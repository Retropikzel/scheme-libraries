(define-syntax parallel-map
  (syntax-rules ()
    ((_ env (l args body ...) lst)
     (let ((l (eval `(lambda args body ...) (apply environment 'env))))
       (map l lst)))))
