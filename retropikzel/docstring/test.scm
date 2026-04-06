;(test-begin "docstring")


(define (plus a b)
  (docstring "Adds a to b")
  (+ a b))

(plus 1 2)
(write (doc plus))
(newline)



;(test-end "docstring")
