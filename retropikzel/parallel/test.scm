
(write (parallel-map
         ((scheme base))
         (lambda (i)
           (+ i 1))
         '(1 2 3 4 5 6 7 8 9 10)))
(newline)


