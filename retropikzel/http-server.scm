(define buffer-size 4000)
(define response-line-end (string->utf8 "\r\n"))
(define response-200 (string->utf8 "HTTP/1.1 200 OK\r\n"))
(define response-404 (string->utf8 "HTTP/1.1 404 Not Found\r\n"))
(define response-501 (string->utf8 "HTTP/1.1 501 Not Implemented\r\n"))
(define header-content-type-text-html (string->utf8 "Content-type: text/html\r\n"))
(define (header-content-length len)
  (string->utf8 (string-append "Content-length: " (number->string len) "\r\n")))

(define (logger msg) (display msg (current-error-port)))

(define (receive-all socket bytes result)
  (if (< (bytevector-length bytes) buffer-size)
    (bytevector-append result bytes)
    (receive-all socket
                 (socket-recv socket buffer-size)
                 (bytevector-append result bytes))))

(define (read-until port character)
  (letrec ((looper (lambda (result)
                     (let ((c (read-char port)))
                       (if (char=? c character)
                         (list->string (reverse result))
                         (looper (cons c result)))))))
    (looper (list))))

(define (read-until-eof port)
  (letrec ((looper (lambda (result)
                     (let ((bytes (read-bytevector buffer-size)))
                       (if (eof-object? bytes)
                         result
                         (looper (bytevector-append result bytes)))))))
    (looper (bytevector))))

(define (send socket bytes)
  ;(display "Sending: ")
  ;(write (utf8->string bytes))
  ;(newline)
  (socket-send socket bytes))

(define (http-server-listen server-socket document-root port)
  (call-with-socket
    (socket-accept server-socket)
    (lambda (client-socket)
      (let ((request-type (utf8->string (socket-recv client-socket 4))))
        (cond
          ((string=? request-type "GET ")
           (logger "GET: ")
           (let* ((request
                    (utf8->string
                      (receive-all client-socket
                                   (socket-recv client-socket buffer-size)
                                   (bytevector))))
                  (request-port (open-input-string request))
                  (request-path (url-decode (read-until request-port #\space)))
                  (request-full-path (string-append document-root request-path)))
             (logger request-path)
             (cond
               ((file-exists? request-full-path)
                (let ((contents
                        (with-input-from-file
                          request-full-path
                          (lambda ()
                            (read-until-eof (current-input-port))))))
                  (send client-socket response-200)
                  (send client-socket header-content-type-text-html)
                  (send client-socket
                               (header-content-length
                                               (bytevector-length contents)))
                  (send client-socket response-line-end)
                  (send client-socket contents)
                  (send client-socket response-line-end)
                  (logger " -> 200 OK")
                  (logger #\newline)))
               (else (send client-socket response-404)
                     (send client-socket response-line-end)
                     (logger " -> 404 Not found")
                     (logger #\newline)))))
          (else (send client-socket response-501))))
      (socket-close client-socket)
      (http-server-listen server-socket document-root port))))

(define (http-server document-root port)
  (let ((server-socket
          (make-server-socket (if (number? port) (number->string port) port))))
    (when (char=? (car (reverse (string->list document-root))) #\/)
      (error "document-root can not end with /" document-root))
    (http-server-listen server-socket document-root port)))
