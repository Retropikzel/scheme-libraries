(define (->string separator arg)
  (cond ((string? arg) (string-append separator arg))
        (else
          (let* ((port (open-output-string))
                 (str (begin
                        (display separator port)
                        (display arg port)
                        (get-output-string port))))
            (close-port port)
            str))))

(define (concat separator . args)
  (apply string-append
         (cons
           (string-copy
             (->string separator (car args))
             (string-length separator))
           (map (lambda (arg) (->string separator arg)) (cdr args)))))
