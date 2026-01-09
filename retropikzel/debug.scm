(define-syntax debug
  (syntax-rules ()
    ((_ obj)
     (begin
       (display 'obj)
       (display ": ")
       (write obj)
       (newline)
       obj))))
