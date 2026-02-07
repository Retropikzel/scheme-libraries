(define chunk-size 4000)
(define slash-r (bytevector-u8-ref (string->utf8 "\r") 0))
(define slash-n (bytevector-u8-ref (string->utf8 "\n") 0))

(define remove-leading-whitespate
  (lambda (str)
    (if (or (= (string-length str) 0)
            (not (char-whitespace? (string-ref str 0))))
      str
      (remove-leading-whitespate (string-copy str )))))

(define http-util-header-line->pair
  (lambda (line)
    (letrec* ((split
                (lambda (first rest)
                  (if (or (= (string-length rest) 0)
                          (char=? #\: (string-ref rest 0)))
                    (cons (string->symbol (string-downcase first)) (remove-leading-whitespate (string-copy rest 2)))
                    (split (string-append first (string (string-ref rest 0)))
                           (string-copy rest 1))))))
      (split "" line))))

(define http-util-status-line->list
  (lambda (line)
    (letrec* ((split
                (lambda (first second rest space-count)
                  (cond ((char=? #\space (string-ref rest 0))
                         (split first second (string-copy rest 1) (+ space-count 1)))
                        ((= space-count 0)
                         (split (string-append first (string-copy rest 0 1))
                                second
                                (string-copy rest 1)
                                space-count))
                        ((= space-count 1)
                         (split first
                                (string-append second (string-copy rest 0 1))
                                (string-copy rest 1)
                                space-count))
                        ((= space-count 2)
                         (list first (if (number? second)
                                       (string->number second)
                                       second)
                               rest))))))
      (split "" "" line 0))))

(define read-lines-until-empty
  (lambda (port result)
    (let ((line (read-line port)))
      (if (string=? "" line)
        result
        (read-lines-until-empty port (append result (list line)))))))

(define http-util-headers->string
  (lambda (headers)
    (apply string-append
           (map
             (lambda (header)
               (string-append (symbol->string (car header))
                              ": "
                              (cdr header)
                              "\r\n"))
             headers))))

(define http-util-request-build
  (lambda (type path headers body)
    (string-append (string-upcase type)
                   " "
                   path
                   " "
                   "HTTP/1.1"
                   "\r\n"
                   (http-util-headers->string headers)
                   "\r\n"
                   body
                   "\r\n\r\n")))

(define receive-all-from-socket
  (lambda (socket)
    (letrec ((looper (lambda (result-bytes)
                       (let ((bytes (socket-recv socket 4000)))
                         (if (or (eof-object? bytes)
                                 (= (bytevector-length bytes) 0))
                           result-bytes
                           (looper (bytevector-append result-bytes bytes)))))))
      (looper (bytevector)))))

(define receive-http-request-from-socket
  (lambda (socket)
    (letrec* ((looper (lambda (result-bytes)
                        (let ((bytes (socket-recv socket 8)))
                          (if (or (eof-object? bytes)
                                  (= (bytevector-length bytes) 0))
                            result-bytes
                            (begin
                              (display (utf8->string bytes))
                              (newline)
                              (looper (bytevector-append result-bytes bytes))))))))
      (looper (bytevector)))))

(define read-chunked-body
  (lambda (port)
    (letrec ((looper (lambda (body)
                       (let ((chunk-size (string-append "#x" (read-line port))))
                         (if (= (string->number chunk-size) 0)
                           body
                           (looper (string-append body
                                                  "\r\n"
                                                  (read-string (+ (string->number chunk-size) 2) port))))))))
      (string->utf8 (looper "")))))

(define http-util-read-http-response
  (lambda (socket)
    (let* ((response (receive-all-from-socket socket))
           (port (open-input-string (utf8->string response)))
           (status-line (http-util-status-line->list (read-line port)))
           (headers (map http-util-header-line->pair (read-lines-until-empty port (list))))
           (body (cond ((assoc 'content-length headers)
                        (read-bytevector (string->number (cdr (assoc 'content-length headers))) port))
                       ((and (assoc 'transfer-encoding headers)
                             (string=? (cdr (assoc 'transfer-encoding headers)) "chunked"))
                        (read-chunked-body port))
                       ((and (assoc 'connection headers)
                             (string=? (cdr (assoc 'connection headers)) "close"))
                        (letrec ((looper (lambda (result line)
                                           (if (eof-object? line)
                                             result
                                             (looper (bytevector-append result line)
                                                     (read-bytevector 4000 port))))))
                          (looper (bytevector) (read-bytevector 4000 port))))
                       (else (bytevector)))))
      (list (cons 'status-line status-line)
            (cons 'protocol (list-ref status-line 0))
            (cons 'status-code (string->number (list-ref status-line 1)))
            (cons 'status-text (list-ref status-line 2))
            (cons 'headers headers)
            (cons 'body body)))))

(define socket-receive-line
  (lambda (socket)
    (letrec ((looper (lambda (result-bytes)
                       (let ((byte (socket-recv socket 1)))
                         (cond ((= (bytevector-length byte) 0) result-bytes)
                               ((= (bytevector-u8-ref byte 0) slash-r)
                                (let ((next-byte (socket-recv socket 1)))
                                  (if (or (= (bytevector-length next-byte) 0)
                                          (= (bytevector-u8-ref next-byte 0) slash-n))
                                    result-bytes
                                    (looper (bytevector-append result-bytes byte next-byte)))))
                               (else (looper (bytevector-append result-bytes byte))))))))
      (looper (bytevector)))))

(define socket-receive-headers
  (lambda (socket)
    (letrec ((looper (lambda (result)
                       (let ((line (utf8->string (socket-receive-line socket))))
                         (if (string=? line "")
                           result
                           (looper (append result (list line))))))))
      (map http-util-header-line->pair (looper (list))))))

(define http-util-read-http-request
  (lambda (socket)
    (let* ((status-line (http-util-status-line->list (utf8->string (socket-receive-line socket))))
           (headers (socket-receive-headers socket))
           (body (let ((content-length (assoc 'content-length headers)))
                   (if content-length
                     (socket-recv socket (string->number (cdr content-length)))
                     ""))))
      (list (cons 'status-line status-line)
            (cons 'headers headers)
            (list 'body body)))))

(define http-util-download-file
  (lambda (url path port headers output-file-path)
    (let* ((headers-with-host (append headers
                                      (list
                                        (cons 'host
                                              (string-append url ":" (number->string port))))))
           (request (http-util-request-build "GET" path headers-with-host ""))
           (socket (make-client-socket url (number->string port))))
      (socket-send socket (string->utf8 request))
      (let* ((socket-port (socket-input-port socket))
             (status-line (http-util-status-line->list (read-line socket-port)))
             (headers (map http-util-header-line->pair (read-lines-until-empty socket-port (list))))
             (content-length (if (assoc 'content-length headers)
                               (string->number (cdr (assoc 'content-length headers)))
                               chunk-size))
             (status-code (string->number (list-ref status-line 1))))
        (if (not (= status-code 200))
          (error (string-append "Could not download file from " url "/" path) headers))
        (letrec* ((output-port (open-binary-output-file output-file-path))
                  (looper
                    (lambda (bytes)
                      (if (not (eof-object? bytes))
                        (begin
                          (write-bytevector bytes output-port)
                          (looper (read-bytevector chunk-size socket-port)))))))
          (looper (read-bytevector content-length socket-port))
          (close-port output-port)
          (close-port socket-port)
          (socket-close socket))))))

(define http-util-request-make
  (lambda (type url path port headers body)
    (let* ((headers-with-host (append headers
                                      (list
                                        (cons 'host
                                              (string-append url ":" (number->string port))))))
           (request (http-util-request-build "GET" path headers-with-host body))
           (socket (make-client-socket url (number->string port))))
      (socket-send socket (string->utf8 request))
      (let ((response (http-util-read-http-response socket)))
        (socket-close socket)
        response))))

(define http-util-response-build
  (lambda (code code-text headers body)
    (let ((headers-with-content-length (append headers
                                               (list (cons 'content-length
                                                           (number->string (string-length body)))))))

      (string-append "HTTP/1.1"
                     " "
                     (number->string code)
                     " "
                     code-text
                     "\r\n"
                     (http-util-headers->string headers-with-content-length)
                     "\r\n"
                     body))))

(define http-util-parameters-split
  (lambda (body)
    (cond ((and (string? body) (string=? "" body)) (list))
          ((string? body) (let ((bodylist (string->list body)))
                            (map (lambda (x) (string-split x #\=))
                                 (string-split (list->string
                                                 (if (string=? "?" (string (car bodylist)))
                                                   (cdr bodylist)
                                                   bodylist)) #\&))))
          (else (list)))))

(define http-util-parameter-get
  (lambda (key params)
    (let ((value #f))
      (if (list? params)
        (map (lambda (x)
               (if (and (string? (car (cdr x)))
                        (string? (car x))
                        (string=? key (car x)))
                 (set! value (car (cdr x)))))
             params))
      value)))
