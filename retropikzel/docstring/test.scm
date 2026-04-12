;(test-begin "docstring")

(define (plus a b)
  (doc-string "Add a to b")
  (+ a b))


(define (minus a b)
  (- a b))

(write (plus 1 1))
(newline)
(display (doc plus))
(newline)

(write (minus 1 1))
(newline)
(display (doc minus))
(newline)

;(test-end "docstring")
