(test-begin "memoize")

(define x 1)

(define (two) (memoize (lambda () (+ 1 x))))
(write (two))
(newline)

(set! x 2)
(write (two))
(newline)

(test-end "memoize")

