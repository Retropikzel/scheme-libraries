(define-syntax default-lambda
  (syntax-rules ()
    ((_ ((arg default) ...) body ...)
     (lambda args
       (let ((arg default) ...)
         (for-each
           (lambda (item)
             (when (equal? (car item) 'arg) (set! arg (cadr item))) ...)
           args)
         body ...)))))

(define-syntax default-checked-lambda
  (syntax-rules ()
    ((_ ((arg default check) ...) body ...)
     (lambda args
       (let ((arg default) ...)
         (for-each
           (lambda (item)
             (when (equal? (car item) 'arg)
               (set! arg (cadr item))
               (when (not (check arg))
                 (error
                   (string-append "argument: "
                                  (symbol->string 'arg)
                                  ", failed check: "
                                  (symbol->string 'check)))))
             ...)
           args)
         body ...)))))
