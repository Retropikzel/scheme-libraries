(define (primitives-init set-procedure get-procedure) #t)

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'i8) 1)
          ((eq? type 'u8) 1)
          ((eq? type 'i16) 2)
          ((eq? type 'u16) 2)
          ((eq? type 'i32) 4)
          ((eq? type 'u32) 4)
          ((eq? type 'i64) 8)
          ((eq? type 'u64) 8)
          ((eq? type 'char) 1)
          ((eq? type 'uchar) 1)
          ((eq? type 'short) size-of-short)
          ((eq? type 'ushort) size-of-unsigned-short)
          ((eq? type 'int) size-of-int)
          ((eq? type 'uint) size-of-unsigned-int)
          ((eq? type 'long) size-of-long)
          ((eq? type 'ulong) size-of-unsigned-long)
          ((eq? type 'float) size-of-float)
          ((eq? type 'double) size-of-double)
          ((eq? type 'pointer) size-of-pointer)
          ((eq? type 'callback) size-of-pointer)
          ((eq? type 'void) 0)
          (else #f))))

(define align-of-type
  (lambda (type)
    (cond ((eq? type 'i8) 1)
          ((eq? type 'u8) 1)
          ((eq? type 'i16) 2)
          ((eq? type 'u16) 2)
          ((eq? type 'i32) 4)
          ((eq? type 'u32) 4)
          ((eq? type 'i64) 8)
          ((eq? type 'u64) 8)
          ((eq? type 'char) 1)
          ((eq? type 'uchar) 1)
          ((eq? type 'short) align-of-short)
          ((eq? type 'ushort) align-of-short)
          ((eq? type 'int) align-of-int)
          ((eq? type 'uint) align-of-int)
          ((eq? type 'long) align-of-long)
          ((eq? type 'ulong) align-of-unsigned-long)
          ((eq? type 'float) align-of-float)
          ((eq? type 'double) align-of-double)
          ((eq? type 'pointer) align-of-void*)
          ((eq? type 'callback) align-of-void*)
          ((eq? type 'void) 0)
          (else #f))))

(define shared-object-load
  (lambda (path options)
    (open-shared-library path)))

(define c-bytevector?
  (lambda (object)
    (pointer? object)))

(define c-bytevector-u8-set! pointer-set-c-uint8!)
(define c-bytevector-u8-ref pointer-ref-c-uint8)
(define c-bytevector-pointer-set!
  (lambda (pointer offset value)
    (pointer-set-c-pointer! pointer offset value)))
(define c-bytevector-pointer-ref
  (lambda (pointer offset)
    (pointer-ref-c-pointer pointer offset)))

(define type->native-type
  (lambda (type)
    (cond ((equal? type 'i8) 'int8_t)
          ((equal? type 'u8) 'uint8_t)
          ((equal? type 'i16) 'int16_t)
          ((equal? type 'u16) 'uint16_t)
          ((equal? type 'i32) 'int32_t)
          ((equal? type 'u32) 'uint32_t)
          ((equal? type 'i64) 'int64_t)
          ((equal? type 'u64) 'uint64_t)
          ((equal? type 'char) 'char)
          ((equal? type 'uchar) 'char)
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
          ((equal? type 'callback) 'void*)
          (else (error "type->native-type -- No such type" type)))))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (make-c-function shared-object
                        (type->native-type return-type)
                        c-name
                        (map type->native-type argument-types))))))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name
       (make-c-callback (type->native-type return-type)
                        (map type->native-type argument-types)
                        procedure)))))

(define (make-c-null) (integer->pointer 0))
(define c-null? pointer-null?)
