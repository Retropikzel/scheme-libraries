(define-syntax type->native-type
  (syntax-rules ()
    ((_ type)
     (cond ((equal? type 'i8) 'integer-8)
           ((equal? type 'u8) 'unsigned-8)
           ((equal? type 'i16) 'integer-16)
           ((equal? type 'u16) 'unsigned-16)
           ((equal? type 'i32) 'integer-32)
           ((equal? type 'u32) 'unsigned-32)
           ((equal? type 'i64) 'integer-64)
           ((equal? type 'u64) 'unsigned-64)
           ((equal? type 'char) 'char)
           ((equal? type 'uchar) 'unsigned-8)
           ((equal? type 'short) 'short)
           ((equal? type 'ushort) 'unsigned-short)
           ((equal? type 'int) 'int)
           ((equal? type 'uint) 'unsigned-int)
           ((equal? type 'long) 'long)
           ((equal? type 'ulong) 'unsigned-long)
           ((equal? type 'float) 'float)
           ((equal? type 'double) 'double)
           ((equal? type 'pointer) 'void*)
           ((equal? type 'void) 'void)))))

(define c-bytevector?
  (lambda (object)
    (or (number? object)
        (ftype-pointer? object))))

(define-syntax define-macro!
  (lambda (x)
    (syntax-case x ()
                 [(k (name arg1 ... . args)
                     form1
                     form2
                     ...)
                  #'(k name (arg1 ... . args)
                       form1
                       form2
                       ...)]
                 [(k (name arg1 arg2 ...)
                     form1
                     form2
                     ...)
                  #'(k name (arg1 arg2 ...)
                       form1
                       form2
                       ...)]
                 [(k name args . forms)
                  (identifier? #'name)
                  (letrec ((add-car
                             (lambda (access)
                               (case (car access)
                                 ((cdr) `(cadr ,@(cdr access)))
                                 ((cadr) `(caadr ,@(cdr access)))
                                 ((cddr) `(caddr ,@(cdr access)))
                                 ((cdddr) `(cadddr ,@(cdr access)))
                                 (else `(car ,access)))))
                           (add-cdr
                             (lambda (access)
                               (case (car access)
                                 ((cdr) `(cddr ,@(cdr access)))
                                 ((cadr) `(cdadr ,@(cdr access)))
                                 ((cddr) `(cdddr ,@(cdr access)))
                                 ((cdddr) `(cddddr ,@(cdr access)))
                                 (else `(cdr ,access)))))
                           (parse
                             (lambda (l access)
                               (cond
                                 ((null? l) '())
                                 ((symbol? l) `((,l ,access)))
                                 ((pair? l)
                                  (append!
                                    (parse (car l) (add-car access))
                                    (parse (cdr l) (add-cdr access))))
                                 (else
                                   (syntax-error #'args
                                                 (format "invalid ~s parameter syntax" (datum k))))))))
                    (with-syntax ((proc (datum->syntax-object #'k
                                                              (let ((g (gensym)))
                                                                `(lambda (,g)
                                                                   (let ,(parse (datum args) `(cdr ,g))
                                                                     ,@(datum forms)))))))
                                 #'(define-syntax name
                                     (lambda (x)
                                       (syntax-case x ()
                                                    ((k1 . r)
                                                     (datum->syntax-object #'k1
                                                                           (proc (syntax-object->datum x)))))))))])))

(define-macro!
  define-c-procedure
  (scheme-name shared-object c-name return-type argument-types)
  (let ((native-argument-types
          (map (lambda (type)
                 ;; This is defined in 3 places
                 (cond ((equal? type 'i8) 'integer-8)
                       ((equal? type 'u8) 'unsigned-8)
                       ((equal? type 'i16) 'integer-16)
                       ((equal? type 'u16) 'unsigned-16)
                       ((equal? type 'i32) 'integer-32)
                       ((equal? type 'u32) 'unsigned-32)
                       ((equal? type 'i64) 'integer-64)
                       ((equal? type 'u64) 'unsigned-64)
                       ((equal? type 'char) 'char)
                       ((equal? type 'uhar) 'unsigned-8)
                       ((equal? type 'short) 'short)
                       ((equal? type 'ushort) 'unsigned-short)
                       ((equal? type 'int) 'int)
                       ((equal? type 'uint) 'unsigned-int)
                       ((equal? type 'long) 'long)
                       ((equal? type 'ulong) 'unsigned-long)
                       ((equal? type 'float) 'float)
                       ((equal? type 'double) 'double)
                       ((equal? type 'pointer) 'void*)
                       ((equal? type 'void) 'void)
                       (else type)))
               (if (null? argument-types)
                 '()
                 (cadr argument-types))))
        (native-return-type
          ;; This is defined in 3 places
          (cond ((equal? return-type ''i8) 'integer-8)
                ((equal? return-type ''u8) 'unsigned-8)
                ((equal? return-type ''i16) 'integer-16)
                ((equal? return-type ''u16) 'unsigned-16)
                ((equal? return-type ''i32) 'integer-32)
                ((equal? return-type ''u32) 'unsigned-32)
                ((equal? return-type ''i64) 'integer-64)
                ((equal? return-type ''u64) 'unsigned-64)
                ((equal? return-type ''char) 'char)
                ((equal? return-type ''uhar) 'unsigned-8)
                ((equal? return-type ''short) 'short)
                ((equal? return-type ''ushort) 'unsigned-short)
                ((equal? return-type ''int) 'int)
                ((equal? return-type ''uint) 'unsigned-int)
                ((equal? return-type ''long) 'long)
                ((equal? return-type ''ulong) 'unsigned-long)
                ((equal? return-type ''float) 'float)
                ((equal? return-type ''double) 'double)
                ((equal? return-type ''pointer) 'void*)
                ((equal? return-type ''void) 'void)
                (else return-type))))
    (if (null? argument-types)
      `(define ,scheme-name
         (foreign-procedure #f
                            ,(symbol->string (cadr c-name))
                            ()
                            ,native-return-type))
      `(define ,scheme-name
         (foreign-procedure #f
                            ,(symbol->string (cadr c-name))
                            ,native-argument-types
                            ,native-return-type)))))

(define size-of-type
  (lambda (type)
    (foreign-sizeof (type->native-type type))))

(define align-of-type
  (lambda (type)
    (foreign-alignof (type->native-type type))))

(define shared-object-load
  (lambda (path options)
    (load-shared-object path)))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    (foreign-set! 'unsigned-8 c-bytevector k byte)))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
    (foreign-ref 'unsigned-8 c-bytevector k)))

(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    (foreign-set! 'void* c-bytevector k pointer)))

(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    (foreign-ref 'void* c-bytevector k)))

(define (make-c-null) (make-ftype-pointer void* 0))
(define (c-null? pointer)
  (and (ftype-pointer? pointer)
       (ftype-pointer-null? pointer)))
