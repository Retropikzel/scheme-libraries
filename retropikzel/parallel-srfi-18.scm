(define thread-proc (lambda (x) #t))

(define (new-thread)
  (make-thread
    (lambda ()
      (thread-specific-set!
        (current-thread)
        (thread-proc (thread-specific (current-thread)))))))

(define threads (make-list thread-count (new-thread)))

(define-syntax parallel-map
  (syntax-rules ()
    ((_ env (l args body ...) lst)
     (let* ((lst-length (length lst))
            (thread-proc (eval `(lambda args body ...)
                               (apply environment 'env))))
       (map thread-proc lst)
       ))))
