(test-begin "record")


(define player (record (x 10) (y 10) (health 10)))

(write player)
(newline)
(write (player 'x))
(newline)
(player 'x 100)
(write (player 'x))
(newline)

(define tree (immutable-record (x 10) (y 10)))

(write tree)
(newline)
(write (tree 'x))
(newline)

(define dog (typed-record (x 10 (lambda (x) (and (> x 10) (integer? x)))) (y 10 integer?)))

(write dog)
(newline)
(write (dog 'x))
(newline)
(dog 'x 3)
(write (dog 'x))
(newline)

(test-end "record")
