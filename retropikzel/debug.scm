(define print
  (lambda args
    (map (lambda (item) (display item (current-error-port))) args)))

(define-syntax debug-display
  (syntax-rules ()
    ((_ obj)
     (begin
       (print "[DEBUG] " 'obj ": " obj #\newline)
       obj))))

(define-syntax debug
  (syntax-rules ()
    ((_ obj)
     (begin
       (print "[DEBUG] " 'obj ": " obj #\newline)
       obj))))

(define-syntax debug-proc
  (syntax-rules ()
    ((_ proc arg ...)
     (begin
       (print "[DEBUG] Calling: " 'proc ", arguments: " '(arg ...))
       (let ((result (apply proc (list arg ...))))
         (print ", returned: " result #\newline)
         result)))))

