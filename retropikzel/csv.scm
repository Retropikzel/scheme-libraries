(define string-util-replace-count
      (lambda (text replace replace-with count)
        (letrec* ((inner-count 0)
                  (looper (lambda (result text)
                            (if (or (string=? replace "")
                                    (string=? text "")
                                    (= (string-length text) 0)
                                    (= inner-count count))
                              (string-append result text)
                              (if (and (char=? (string-ref replace 0) (string-ref text 0))
                                       (>= (string-length text) (string-length replace))
                                       (string=? (string-copy text 0 (string-length replace))
                                                 replace))
                                (begin
                                  (set! inner-count (+ inner-count 1))
                                  (looper (string-append result replace-with)
                                          (string-copy text (string-length replace))))
                                (looper (string-append result (string-copy text 0 1))
                                        (string-copy text 1)))))))
          (looper "" text))))

(define string-util-contains?
      (lambda (haystack needle)
        (if (string=? needle "")
          #f
          (if (string=? haystack
                        (string-util-replace-count haystack
                                                   needle
                                                   ""
                                                   1))
            #f
            #t))))

(define string-util-replace-all
      (lambda (text replace replace-with)
        (letrec ((looper (lambda (result text)
                           (if (or (string=? replace "")
                                   (string=? text "")
                                   (= (string-length text) 0))
                             result
                             (if (and (char=? (string-ref replace 0) (string-ref text 0))
                                      (>= (string-length text) (string-length replace))
                                      (string=? (string-copy text 0 (string-length replace))
                                                replace))
                               (looper (string-append result replace-with)
                                       (string-copy text (string-length replace)))
                               (looper (string-append result (string-copy text 0 1))
                                       (string-copy text 1)))))))
          (looper "" text))))

(define read-until-char
      (lambda (result port char)
        (let ((c (read-char port)))
          (cond
            ((or (eof-object? c)
                 (eof-object? (peek-char port)))
             (list->string (reverse result)))
            ((and (not (char=? char #\"))
                  (char=? c #\return)
                  (char=? (peek-char port) #\newline))
             (list->string (reverse result)))
            ((and (not (char=? char #\"))
                  (char=? (peek-char port) #\newline))
             (list->string (reverse (cons c result))))
            (else
              ; When reading inside " read "" as "
              (if (and (char=? char #\")
                       (char=? c #\")
                       (char=? (peek-char port) #\"))
                (read-until-char (cons (read-char port) result) port char)
                (if (char=? c char)
                  (list->string (reverse result))
                  (read-until-char (cons c result) port char))))))))
1
(define csv->list ;-> obj
  #|
  Reads csv from given port and transforms it into list of list.

  Each row is a list inside list of rows.
  |#
  (lambda (delimiter port)
    (csv->list-loop (list) (list) delimiter port)))

(define csv->list-loop
  (lambda (result line-list delimiter port)
    (let* ((next-char (peek-char port)))
      (if (eof-object? next-char)
        (reverse result)
        (csv->list-loop
          (if (char=? next-char #\newline)
            (begin
              (read-char port)
              (set! result (cons (reverse line-list) result))
              (set! line-list (list))
              result)
            result)
          (cond
            ((char=? next-char #\return)
             (read-char port)
             line-list)
            ((char=? next-char delimiter)
             (read-char port)
             line-list)
            ((char=? next-char #\")
             (read-char port)
             (cons (read-until-char (list) port #\") line-list))
            (else (cons (read-until-char (list) port delimiter) line-list)))
          delimiter port)))))

(define (csv-from-list delimiter rows) ;-> string
  #| Transform list of lists into CSV string |#
  (let ((item-display
          (lambda (item)
            (let ((write? #f))
              (when (and (string? item)
                         (string-util-contains? item (string delimiter)))
                (set! write? #t))
              (when (and (string? item)
                         (string-util-contains? item "\""))
                (set! item (string-util-replace-all item "\"" "\"\""))
                (set! write? #t))
              (when (and (string? item)
                         (= (string-length item) 0))
                (set! write? #t))
              (if write?
                (write item)
                (display item))))))
    (parameterize
      ((current-output-port
         (open-output-string)))
      (for-each
        (lambda (row)
          (let ((index -1))
            (for-each
              (lambda (item)
                (set! index (+ index 1))
                (cond ((= index 0) (item-display item))
                      (else (display ",")
                            (item-display item))))
              row))
          (newline))
        rows)
      (get-output-string (current-output-port)))))

(define csv-file->list
  (lambda (character file-path)
    (call-with-input-file
      file-path
      (lambda (csv-file-input-port)
        (csv->list character csv-file-input-port)))))
