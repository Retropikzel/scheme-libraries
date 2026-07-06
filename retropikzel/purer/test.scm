
(define x 5)
(define inc (purer-lambda
              ((scheme base))
              (i)
              (+ i 1)))

(display "HERE: ")
(write (inc 1))
(newline)
