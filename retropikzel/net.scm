(define temporary-directory
  (if (get-environment-variable "NET_TMP_PATH")
    (get-environment-variable "NET_TMP_PATH")
    "/tmp"))

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

(define url-encode
  (lambda (str)
    (cond ((string? str) (endecode "encode" str))
          (else str))))
(define url-decode
  (lambda (str)
    (cond ((string? str) (endecode "decode" str))
          (else str))))

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
