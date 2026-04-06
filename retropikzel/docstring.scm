(define show-docstring?
  (make-parameter #f (lambda (x) x)))

(define docstring
  (make-parameter
    ""
    (lambda (str)
      (if (show-docstring?)
        (error (string-append "docstring:" str))
        ""))))

;; TODO: stop at like 100 arguments, it means the procedure has no docstring
;; TODO: Change all arguments to procedures that throw errors
;; TODO: Use eval to run the procedure in empty environment so nothing happens if it does not have docstring?
(define (doc procedure)
  (letrec*
    ((looper (lambda (args)
               (call-with-current-continuation
                 (lambda (k)
                   (with-exception-handler
                     (lambda (x)
                       (let ((msg (error-object-message x)))
                         (if (string=? (string-copy msg 0 10) "docstring:")
                           (k (string-copy msg 10))
                           (k (looper (cons #t args))))))
                     (lambda ()
                       (show-docstring? #t)
                       (apply procedure args))))))))
    (looper '())))
