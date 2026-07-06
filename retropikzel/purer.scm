(define-syntax purer-lambda
  (syntax-rules ()
    ((_ env args body ...)
     (eval `(lambda args body ...) (apply environment 'env)))))
