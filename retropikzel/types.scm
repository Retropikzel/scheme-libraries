(define-syntax define-integer
  (syntax-rules ()
    ((_ name value)
     (cond-expand
       (chicken (begin (: name integer) (define name value)))
       (debug (define name
                (if (not (integer? value))
                  (begin (display "Warning: Invalid assignment"
                                  (current-error-port))
                         value)
                  value)))
       (else (define name value))))))

(define-syntax set-integer!
  (syntax-rules ()
    ((_ name value)
     (cond-expand
       (chicken (set! name value))
       (debug (set! name
                (if (not (integer? value))
                  (begin (display "Warning: Invalid assignment" (current-error-port))
                         value)
                  value)))
       (else (set! name value))))))
