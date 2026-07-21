(test-begin "debug")

(debug 'lol)
(debug-display 'lol)

(define someval 123)
(debug someval)
(debug-display someval)

(define (plus a b) (+ a b))
(debug plus)
(debug-display plus)
(debug-proc plus 1 2)

(test-end "debug")
