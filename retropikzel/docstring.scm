(define return (make-parameter (lambda (x) x) (lambda (x) x)))

(define doc-string
  (make-parameter
    ""
    (lambda (x)
      (apply (return) (list x)))))

(define (doc procedure)
  (letrec*
    ((looper
       (lambda (args cont)
         (if (> (length args) 100)
           #f
           (with-exception-handler
             (lambda (x)
               (cont (looper (cons (lambda () (error "" '())) args) cont)))
             (lambda ()
               (return cont)
               (apply procedure args)))))))
    (call-with-current-continuation (lambda (cont) (looper '() cont)))))

