(define-syntax record
  (syntax-rules ()
    ((_ (field value) ...)
     (let ((field value) ...)
       (lambda (name . val)
         (cond ((equal? 'field name)
                (if (null? val)
                  field
                  (set! field (car val)))) ...))))))

(define-syntax immutable-record
  (syntax-rules ()
    ((_ (field value) ...)
     (lambda (name)
       (cond ((equal? 'field name) value) ...)))))

(define-syntax typed-record
  (syntax-rules ()
    ((_ (field value type-check) ...)
     (let ((field value) ...)
       (lambda (name . val)
         (cond ((equal? 'field name)
                (if (null? val)
                  field
                  (if (type-check (car val))
                    (set! field (car val))
                    (error (string-append "type-check fail "
                                          (symbol->string name))
                           `((,name ,(car val))
                             (type-check ,type-check)))
                           )))
               ...))))))
