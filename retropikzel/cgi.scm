(define stdin (open-binary-input-file "/dev/fd/0"))
(define buffer-size 4000)
(define temporary-directory (if (get-environment-variable "SCHEME_CGI_TMP_PATH")
                              (get-environment-variable "SCHEME_CGI_TMP_PATH")
                              "/tmp"))
(define file-move-buffer-size 4000)
(define encode-replacements
  (list (list " " "%20")
        (list " " "+")
        (list "!" "%21")
        (list "#" "%23")
        (list "$" "%24")
        (list "%" "%25")
        (list "&" "%26")
        (list "'" "%27")
        (list "(" "%28")
        (list ")" "%29")
        (list "*" "%2A")
        (list "+" "%2B")
        (list "," "%2C")
        (list "/" "%2F")
        (list ":" "%3A")
        (list ";" "%3B")
        (list "=" "%3D")
        (list "?" "%3F")
        (list "@" "%40")
        (list "[" "%5B")
        (list "]" "%5D")
        (list "<" "%3C")
        (list ">" "%3E")
        (list "\\" "%5C")
        (list "\"" "%22")
        (list "\n" "%0A")
        (list "\r" "%0D")))
(define decode-replacements (map reverse encode-replacements))

(define make-temp-filename
  (lambda (filename)
    (letrec* ((dev-random (open-binary-input-file "/dev/random"))
              (min-byte (char->integer #\a))
              (max-byte (char->integer #\z))
              (max-length 10)
              (looper (lambda (result count)
                        (if (>= count max-length)
                          result
                          (let ((byte (read-u8 dev-random)))
                            (if (and (> byte min-byte) (< byte max-byte))
                              (looper (bytevector-append result
                                                         (bytevector byte))
                                      (+ count 1))
                              (looper result count))))))
              (result (string-append (utf8->string (looper (bytevector) 0))
                                     "_"
                                     (utf8->string (looper (bytevector) 0))
                                     "_"
                                     filename)))
      (close-port dev-random)
      result)))

#;(define headers->string
  (lambda (headers)
    (apply string-append (map
                           (lambda (key-value)
                             (string-append (car key-value) ": " (cdr key-value) "\r\n"))
                           headers))))

(define get-replacement
  (lambda (key mode)
    (let ((r (if (string=? mode "encode")
               (assoc key encode-replacements)
               (assoc key decode-replacements))))
      (if r (car (cdr r)) key))))

(define endecode
  (lambda (mode s)
    (if (not s)
      ""
      (letrec ((s-length (string-length s))
               (looper
                 (lambda (i result)
                   (if (< i s-length)
                     (let ((key-length (if (and (string=? mode "decode")
                                                (string=? (string-copy s i (+ i 1)) "%")
                                                (> s-length (+ i 2)))
                                         3
                                         1)))
                       (looper (+ i key-length)
                               (string-append result
                                              (get-replacement
                                                (string-copy s i (+ i key-length))
                                                mode))))
                     result))))
        (looper 0 "")))))

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


(define request
  (list (cons 'headers headers)
        (cons 'parameters parameters)
        (cons 'cookies cookies)
        (cons 'body body)
        (cons 'files files)))

(define (get from key)
  (let ((value (assoc (if (string? key)
                        (string->symbol (endecode "encode" key))
                        key)
                      from)))
    (if value (cdr value) #f)))
(define (get-file file)
  (let ((value (assoc (endecode "encode" (if (symbol? file)
                                           (symbol->string file)
                                           file))
                      files)))
    (if value (cdr value) #f)))
(define (move-file from to)
  (letrec* ((input (open-binary-input-file from))
            (output (open-binary-output-file to))
            (looper (lambda (bytes)
                      (when (not (eof-object? bytes))
                        (write-bytevector bytes output)
                        (looper (read-bytevector file-move-buffer-size input))))))
    (looper (read-bytevector file-move-buffer-size input))
    (close-port input)
    (close-port output)))

(define (cgi) request)

(define cgi-exit
  (lambda args
    (for-each (lambda (file)
                (let ((path (cdr file)))
                  (when (file-exists? path)
                    (delete-file path))))
              files)
    (if (null? args)
      (exit 0)
      (exit (car args)))))

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
                                            (let* ((tmp-file-path (string-append temporary-directory
                                                                                 "/"
                                                                                 (make-temp-filename (cdr filename))))
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
