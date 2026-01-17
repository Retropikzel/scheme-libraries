(define buffer-size 4000)

(define get-replacement
  (lambda (key mode)
    (let ((r (if (string=? mode "encode")
               (assoc key encode-replacements)
               (assoc key decode-replacements))))
      (if r (car (cdr r)) key))))

(define scgi-split-by-zero->list
  (lambda (source)
    (let ((result (list))
          (source-size (bytevector-length source)))
      (letrec ((looper
                 (lambda (index last-index key value)
                   (if (< index source-size)
                     (if (and key value)
                       (begin
                         (if (> (bytevector-length key) 0)
                           (set! result
                             (append
                               result
                               (list (cons (utf8->string key)
                                           (if (= (bytevector-length value) 0)
                                             ""
                                             (utf8->string value)))))))
                         (looper index last-index #f #f))
                       (if (= (bytevector-u8-ref source index) 0)
                         (let ((slice (bytevector-copy source last-index index)))
                           (if (not key)
                             (looper (+ index 1) (+ index 1) slice value)
                             (looper (+ index 1) (+ index 1) key slice)))
                         (looper (+ index 1) last-index key value)))))))
        (looper 0 0 #f #f))
      result)))

#;(define scgi-netstring->list
  (lambda (netstring)
    (let ((request (list)))
      (letrec ((get-request
                 (lambda (index)
                   (if (= (bytevector-u8-ref netstring index) 58)
                     (bytevector-copy netstring (+ index 1))
                     (get-request (+ index 1))))))
        (if (> (bytevector-length netstring) 0)
          (scgi-split-by-zero->list (get-request 0))
          (list))))))

#;(define scgi-get-request-body
  (lambda (request-bytes content-length)
    (letrec ((looper
               (lambda (index)
                 (if (and (> (bytevector-length request-bytes) 0)
                          (= (bytevector-u8-ref request-bytes index) 0)
                          (= (bytevector-u8-ref request-bytes (+ index 1)) 44))
                   (bytevector-copy request-bytes (+ index 2))
                   (looper (- index 1))))))
      (looper (- (bytevector-length request-bytes) 1)))))

#;(define read-all-from-socket
  (lambda (socket result)
    (let ((bytes (socket-recv socket buffer-size)))
      (if (or (eof-object? bytes)
              (< (bytevector-length bytes) buffer-size))
        (bytevector-append result bytes)
        (read-all-from-socket socket (bytevector-append result bytes))))))

(define (read-size-from-socket result socket)
  (let ((bytes (socket-recv socket 1)))
    (if (char=? (integer->char (bytevector-u8-ref bytes 0)) #\:)
      (string->number (utf8->string result))
      (read-size-from-socket (bytevector-append result bytes) socket))))

(define (read-headers-from-socket socket)
  (socket-recv socket (read-size-from-socket (bytevector) socket)))

(define (read-body-from-socket socket content-size)
  (socket-recv socket 1) ; Read away ","
  (socket-recv socket content-size))

(define (clean-files)
  (for-each
    (lambda (file)
      (let ((path (cdr file)))
        (when (file-exists? path)
          (delete-file path))))
    files))

(define request (list))
(define files (list))

(define string-split
  (lambda (str mark)
    (let* ((str-l (string->list str))
           (res (list))
           (last-index 0)
           (index 0)
           (splitter (lambda (c)
                       (cond ((char=? c mark)
                              (begin
                                (set! res (append res (list (string-copy str last-index index))))
                                (set! last-index (+ index 1))))
                             ((equal? (length str-l) (+ index 1))
                              (set! res (append res (list (string-copy str last-index (+ index 1)))))))
                       (set! index (+ index 1)))))
      (for-each splitter str-l)
      res)))

(define split-http-parameters
  (lambda (body)
    (cond ((or (not (string? body))
               (string=? "" body))
           (list))
          (else (let ((bodylist (string->list body)))
                  (map (lambda (p)
                         (cons (list-ref p 0)
                               (if (> (length p) 1)
                                 (list-ref p 1)
                                 "")))
                       (map (lambda (x) (string-split x #\=))
                            (string-split (list->string bodylist)
                                          #\&))))))))

(define string-filter
  (lambda (str filter)
    (let ((result (list)))
      (string-for-each
        (lambda (c)
          (if (filter c)
            (set! result (append result (list c)))))
        str)
      (list->string result))))

(define read-binary-port-until
  (lambda (port result until)
    (let ((byte (read-u8 port)))
      (if (or (eof-object? byte)
              (= byte until))
        result
        (read-binary-port-until port (bytevector-append result
                                                        (bytevector byte))
                                until)))))

(define read-bytevector-line
  (lambda (port)
    (let* ((result (utf8->string (read-binary-port-until port
                                                         (bytevector)
                                                         (char->integer #\newline))))
           (result-length (string-length result))
           (ends-in-return? (and (> result-length 0)
                                 (char=? (string-ref result (- result-length 1))
                                         #\return))))
      (cond ((= result-length 0) "")
            (ends-in-return? (string-copy result 0 (- result-length 1)))
            (else result)))))

(define breaker (char->integer #\-))

(define scgi-internal-handle
  (lambda (client-socket thunk)
    (let* ((headers (scgi-split-by-zero->list (read-headers-from-socket client-socket)))
           (request-method (if (not (null? headers)) (cdr (assoc "REQUEST_METHOD" headers)) ""))
           (content-length (if (not (null? headers)) (string->number (cdr (assoc "CONTENT_LENGTH" headers))) 0))
           (content-type-pair (if (assoc "CONTENT_TYPE" headers)
                                (assoc "CONTENT_TYPE" headers)
                                (cons "Content-Type" "text/html")))
           (parameters (list))
           (content-type-data (string-split (cdr content-type-pair) #\;))
           (content-type (list-ref content-type-data 0))
           (body (if (> content-length 0)
                   (if (string=? content-type "multipart/form-data")
                     (bytevector)
                     (read-body-from-socket client-socket content-length))
                   (bytevector))))
      (cond ((and content-type-pair (string=? content-type "multipart/form-data"))
             (letrec* ((boundary (string->utf8 (string-append (list-ref (string-split
                                                                          (list-ref content-type-data 1) #\=) 1))))
                       (boundary-length (bytevector-length boundary))
                       (content (read-body-from-socket client-socket content-length))
                       (content-mark 0)
                       (looper (lambda (index)
                                 (cond ((< index (- content-length 4))
                                        (if (and (= breaker (bytevector-u8-ref content index))
                                                 (= breaker (bytevector-u8-ref content (+ index 1)))
                                                 (equal? boundary (bytevector-copy content (+ index 2) (+ index 2 boundary-length))))
                                          (let* ((part (bytevector-copy content content-mark index))
                                                 (part-length (bytevector-length part))
                                                 (part-port (open-input-bytevector part))
                                                 (part-headers-length 0)
                                                 (part-headers (letrec ((loop (lambda (line result)
                                                                                (if (or (eof-object? line) (string=? line ""))
                                                                                  (map (lambda (p) (string-split p #\:)) result)
                                                                                  (begin
                                                                                    (set! part-headers-length (+ part-headers-length
                                                                                                                 (string-length line)
                                                                                                                 2))
                                                                                    (loop (read-bytevector-line part-port)
                                                                                          (append result (list line))))))))
                                                                 (loop (read-bytevector-line part-port) (list)))))
                                            (if (and (not (null? part-headers))
                                                     (assoc "Content-Disposition" part-headers))
                                              (let* ((content-disposition
                                                       (map
                                                         (lambda (str)
                                                           (let ((split (string-split str #\=)))
                                                             (cons (string-filter (list-ref split 0) (lambda (c) (not (char=? c #\space))))
                                                                   (if (= (length split) 2)
                                                                     (string-filter (list-ref split 1) (lambda (c) (not (char=? c #\"))))
                                                                     ""))))
                                                         (string-split (car (cdr (assoc "Content-Disposition" part-headers))) #\;)))
                                                     (filename (assoc "filename" content-disposition)))
                                                (if (not filename)
                                                  (set! parameters
                                                    (append parameters
                                                            (list
                                                              (cons (cdr (assoc "name" content-disposition))
                                                                    (utf8->string (bytevector-copy content
                                                                                                   (+ (+ content-mark part-headers-length) 2)
                                                                                                   (- index 2)))))))
                                                  (let* ((tmp-file-path (make-temp-filename (cdr filename)))
                                                         (tmp-file-port (begin (when (file-exists? tmp-file-path)
                                                                                 (delete-file tmp-file-path))
                                                                               (open-binary-output-file tmp-file-path))))
                                                    (write-bytevector (bytevector-copy content
                                                                                       (+ (+ content-mark part-headers-length) 2)
                                                                                       (- index 2))
                                                                      tmp-file-port)
                                                    (close-port tmp-file-port)
                                                    (set! files (append files (list
                                                                                (cons (cdr (assoc "name" content-disposition))
                                                                                      tmp-file-path))))))
                                                (set! content-mark index)))
                                            (looper (+ index boundary-length)))
                                          (looper (+ index 1))))))))
               (looper 0)))
            ((string=? request-method "POST")
             (set! parameters (split-http-parameters (url-decode (utf8->string body)))))
            (else (if (not (null? headers))
                    (split-http-parameters (cdr (assoc "QUERY_STRING" headers)))
                    (list))))
      (set! request (list (cons 'headers headers)
                          (cons 'parameters parameters)
                          (cons 'files files)
                          (cons 'body (url-decode (utf8->string body)))))
          (with-exception-handler
            (lambda (ex)
              (socket-send client-socket (string->utf8 "#f")))
            (lambda ()
              (let ((response (parameterize
                                ((current-output-port (open-output-string)))
                                (apply thunk
                                       (list request
                                             headers
                                             parameters
                                             '() ;; TODO Cookies
                                             (url-decode (utf8->string body))
                                             files))
                                (set! request (list))
                                (set! files (list))
                                (get-output-string (current-output-port)))))
                (socket-send client-socket
                             (string->utf8 (if (string? response)
                                             response
                                             ""))))))
      (socket-close client-socket))))

(define scgi-listen
  (lambda (socket thunk)
    (scgi-internal-handle (socket-accept socket) thunk)
    (clean-files)
    (scgi-listen socket thunk)))

(define (handle-request options thunk)
  (let ((port (assoc 'port options)))
    (when (not port)
      (error "handle-request (scgi) requires port to be passed in options, example: '((port . \"3000\"))"))
    (scgi-listen (make-server-socket (cdr port) *af-inet* *sock-stream* *ipproto-ip*) thunk)))
