(define line-end "\r\n")
(define (recv-all socket)
  (letrec*
    ((buffer-size 4000)
     (looper (lambda (bytes result)
               (if (< (bytevector-length bytes) 4000)
                 (bytevector-append bytes result)
                 (looper (socket-recv socket 4000)
                         (bytevector-append result bytes))))))
    (looper (socket-recv socket 4000) (make-bytevector 0))))

(define (read-string-until-eof port)
  (letrec*
    ((looper (lambda (str result)
               (if (eof-object? str)
                 result
                 (looper (read-string 4000 port)
                         (string-append result str))))))
    (looper (read-string 4000 port) "")))

(define (string-trim-left str)
  (letrec*
    ((beyond-whitespace #f)
     (looper (lambda (result rest)
               (if (null? rest)
                 (list->string (reverse result))
                 (if (and (not beyond-whitespace)
                          (char-whitespace? (car rest)))
                   (looper result (cdr rest))
                   (begin
                     (set! beyond-whitespace #t)
                   (looper (cons (car rest) result) (cdr rest))))))))
    (looper '() (string->list str))))

(define (string->status str)
  `((http-version ,(string-copy str 0 8))
    (status-code ,(string-copy str 9 12))
    (status-message ,(string-copy str 13))))

(define (string->headers str)
  (let ((first-punctuation-index 0)
        (index -1))
    (string-for-each
      (lambda (c)
        (set! index (+ index 1))
        (when (and (= first-punctuation-index 0)
                   (char=? c #\:))
          (set! first-punctuation-index index)))
      str)
    (list (string-copy str 0 first-punctuation-index)
          (string-copy str (+ first-punctuation-index 1)))))

(define (read-headers port)
  (letrec*
    ((looper
       (lambda (line result)
         (if (string=? line "")
           result
           (looper (read-line port)
                   (append result
                           (list (map string-trim-left (string->headers line)))))))))
    `(headers ,(looper (read-line port) '()))))

(define (string->http-response str)
  (cond ((not (string? str))
         (error "string->http-reseponse: Argument must be string" str))
        ((and (>= (string-length str) 8)
              (string=? (string-copy str 0 8) "HTTP/1.1"))
         (display "HERE: str ")
         (write str)
         (newline)
         (let* ((port (open-input-string str))
                (status (string->status (read-line port)))
                (headers (read-headers port))
                (body (read-string-until-eof port)))
           (append `(,@status ,headers (body ,body)))
           ))
        (else (error "string->http-response: unsupported HTTP response" str))))

(define (http-request url . options)
  (let* ((uri (string->uri url))
         (host (uri-host uri))
         (port (cond ((equal? (uri-scheme uri) 'http) (or (uri-port uri) "80"))
                     ((equal? (uri-scheme uri) 'https) (or (uri-port uri) "443"))
                     (else (error "http-request: Unknown url scheme" url))))
         (method (cadr (or (assq 'method options) '(#f "GET"))))
         (path (or (uri-path uri) "/"))
         (socket (make-client-socket host port))
         (request
           (string-append
             method " " path " HTTP/1.1" line-end
             "Host: test" line-end
             "Accept: text/html" line-end
             line-end))
         (response
           (begin
             (socket-send socket (string->utf8 request))
             (string->http-response (utf8->string (recv-all socket))))))
    (display "HERE: ")
    (write port)
    (newline)
    (display "HERE: ")
    (write (uri-scheme uri))
    (newline)
    response
  ))
