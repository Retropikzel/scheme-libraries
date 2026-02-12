(define (string-replace str . replacements)
  (letrec*
    ((replacements-list (if (string? (car replacements))
                          (list (list (car replacements) (cadr replacements)))
                          replacements))
     (first-chars (map (lambda (item) (string-ref (car item) 0)) replacements-list))
     (replace-vectors (map (lambda (item) (string->vector (car item))) replacements-list))
     (replace-with-vectors (map (lambda (item) (string->vector( cadr item))) replacements-list))
     (longest-replace-length 32)
     (str-vector (string->vector str))
     (str-length (vector-length str-vector))
     (str-index 0)
     (result-block 4000)
     (result-size result-block)
     (result (make-vector result-size #\X))
     (result-index 0)
     (looper
       (lambda ()
         (when (>= result-index (- result-size longest-replace-length))
           (set! result (vector-append result (make-vector result-block #\X)))
           (set! result-size (+ result-size result-block)))
         (when (< str-index str-length)
           (for-each
             (lambda (first-char replace-vector replace-with-vector)
               (when (and (char=? first-char (vector-ref str-vector str-index))
                          (<= (+ str-index (vector-length replace-vector)) str-length)
                          (equal?  replace-vector
                                   (vector-copy str-vector
                                                str-index
                                                (+ str-index (vector-length replace-vector))))
                          )
                 (vector-copy! result result-index replace-with-vector)
                 (set! result-index (+ result-index (vector-length replace-with-vector)))
                 (set! str-index (+ str-index (vector-length replace-vector)))))
             first-chars
             replace-vectors
             replace-with-vectors)
           (when (< str-index str-length)
             (vector-set! result result-index (vector-ref str-vector str-index))
             (set! str-index (+ str-index 1))
             (set! result-index (+ result-index 1))
             (looper))))))
    (looper)
    (vector->string (vector-copy result 0 result-index))))

(define (string-format str vals)
  (apply string-replace
         (cons str
               (map
                 (lambda (pair)
                   (list (string-append "{" (symbol->string (car pair)) "}")
                         (if (number? (cadr pair))
                           (number->string (cadr pair))
                           (cadr pair))))
                 vals))))

(define (string-capitalize str)
  (string-append (string (char-upcase (string-ref str 0))) (string-copy str 1)))

;; TODO
#;(define (string-center str len . char)
  (let ((c (if (null? char)) #\space (car char)))
    (string-append (string (char-upcase (string-ref str 0))) (string-copy str 1))))

;; TODO
#;(define (string-count str val)
  (letrec*
    ((str-vec (string->vector str))
     (str-len (vector-length str-vec))
     (str-index 0)
     (val-len (string-length val))
     (looper (lambda ()
                      (when (< str-index (- str-len val-len))

                        ))))
    (looper)
    ))

(define (string-ends-with? str end-str)
  (let* ((str-vec (string->vector str))
         (str-len (vector-length str-vec))
         (end-str-vec (string->vector end-str))
         (end-str-len (vector-length end-str-vec)))
    (and (> str-len end-str-len)
         (equal? (vector-copy str-vec (- str-len end-str-len))
                 end-str-vec))))


(define (string-expand-tabs str size)
  (let ((tab (make-string size #\space)))
    (string-replace str (string #\tab) tab)))
