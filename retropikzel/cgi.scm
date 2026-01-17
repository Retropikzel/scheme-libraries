(define stdin (open-binary-input-file "/dev/fd/0"))
(define buffer-size 4000)

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
                         (cons (string->symbol (list-ref p 0))
                               (if (> (length p) 1)
                                 (list-ref p 1)
                                 "")))
                       (map (lambda (x) (string-split x #\=))
                            (string-split (list->string bodylist)
                                          #\&))))))))


(define read-until-eof
  (lambda (port result)
    (let ((c (read-bytevector buffer-size port)))
      (if (eof-object? c)
        (utf8->string result)
        (read-until-eof port (bytevector-append result c))))))

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

(define string-filter
  (lambda (str filter)
    (let ((result (list)))
      (string-for-each
        (lambda (c)
          (if (filter c)
            (set! result (append result (list c)))))
        str)
      (list->string result))))

(define headers (map (lambda (p)
                       (cons (string->symbol (car p))
                             (cdr p)))
                     (get-environment-variables)))
(define content-type-pair (if (assoc 'CONTENT_TYPE headers)
                            (assoc 'CONTENT_TYPE headers)
                            (cons "Content-Type" "text/html")))
(define content-type-data (string-split (cdr content-type-pair) #\;))
(define content-type (list-ref content-type-data 0))
(define request-method (if (assoc 'REQUEST_METHOD headers)
                         (cdr (assoc 'REQUEST_METHOD headers))
                         "GET"))

(define query-string (if (assoc 'QUERY_STRING headers)
                       (cdr (assoc 'QUERY_STRING headers))
                       ""))
(define parameters (list))
(define cookies (let ((cookie-string (get-environment-variable "HTTP_COOKIE")))
                  (if cookie-string
                    (split-http-parameters cookie-string)
                    (list))))
(define body "")
(define files (list))

(define breaker (char->integer #\-))

(define cgi-clean
  (lambda args
    (for-each (lambda (file)
                (let ((path (cdr file)))
                  (when (file-exists? path)
                    (delete-file path))))
              files)))

(define (cgi)
  (cond ((and content-type-pair (string=? content-type "multipart/form-data"))
         (letrec* ((boundary (string->utf8 (string-append (list-ref (string-split
                                                                      (list-ref content-type-data 1) #\=) 1))))
                   (boundary-length (bytevector-length boundary))
                   (content (letrec ((looper (lambda (bytes result)
                                               (if (eof-object? bytes)
                                                 result
                                                 (looper (read-bytevector buffer-size stdin)
                                                         (bytevector-append result bytes))))))
                              (looper (read-bytevector buffer-size stdin)
                                      (bytevector))))
                   (header-content-length (string->number (cdr (assoc 'CONTENT_LENGTH headers))))
                   (content-length (bytevector-length content))
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
        (else (let ((raw-body (if (string=? request-method "POST")
                                (read-until-eof stdin (bytevector))
                                "")))
                (set! parameters (split-http-parameters (if (string=? request-method "POST")
                                                          raw-body
                                                          query-string)))
                (when (string=? request-method "POST")
                  (set! body raw-body)))))

  (list (cons 'headers headers)
        (cons 'parameters parameters)
        (cons 'cookies cookies)
        (cons 'body body)
        (cons 'files files)))

(define (write-to-string str)
  (let ((port (open-output-string)))
    (write str port)
    (get-output-string port)))

(define (handle-request options thunk)
  (let* ((request (cgi)))
    (with-exception-handler
      (lambda (exn) (cgi-clean) #f)
      (lambda ()
        (display
          (parameterize
            ((current-output-port (open-output-string)))
            (apply thunk
                   (list request
                         (cdr (assq 'headers request))
                         (cdr (assq 'parameters request))
                         (cdr (assq 'cookies request))
                         (cdr (assq 'body request))
                         (cdr (assq 'files request))))
            (get-output-string (current-output-port))))))
    (cgi-clean)))
