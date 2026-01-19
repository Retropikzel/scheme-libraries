(define (primitives-init set-procedure get-procedure) #t)

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'i8) size-of-int8_t)
          ((eq? type 'u8) size-of-uint8_t)
          ((eq? type 'i16) size-of-int16_t)
          ((eq? type 'u16) size-of-uint16_t)
          ((eq? type 'i32) size-of-int32_t)
          ((eq? type 'u32) size-of-uint32_t)
          ((eq? type 'i64) size-of-int64_t)
          ((eq? type 'u64) size-of-uint64_t)
          ((eq? type 'char) size-of-char)
          ((eq? type 'uchar) size-of-char)
          ((eq? type 'short) size-of-short)
          ((eq? type 'ushort) size-of-unsigned-short)
          ((eq? type 'int) size-of-int)
          ((eq? type 'uint) size-of-unsigned-int)
          ((eq? type 'long) size-of-long)
          ((eq? type 'ulong) size-of-unsigned-long)
          ((eq? type 'float) size-of-float)
          ((eq? type 'double) size-of-double)
          ((eq? type 'pointer) size-of-void*)
          ((eq? type 'void) 0)
          ((eq? type 'callback) size-of-void*)
          (else #f))))

(define align-of-type
  (lambda (type)
    (cond ((eq? type 'i8) align-of-int8_t)
          ((eq? type 'u8) align-of-uint8_t)
          ((eq? type 'i16) align-of-int16_t)
          ((eq? type 'u16) align-of-uint16_t)
          ((eq? type 'i32) align-of-int32_t)
          ((eq? type 'u32) align-of-uint32_t)
          ((eq? type 'i64) align-of-int64_t)
          ((eq? type 'u64) align-of-uint64_t)
          ((eq? type 'char) align-of-char)
          ((eq? type 'uchar) align-of-char)
          ((eq? type 'short) align-of-short)
          ((eq? type 'ushort) align-of-unsigned-short)
          ((eq? type 'int) align-of-int)
          ((eq? type 'uint) align-of-unsigned-int)
          ((eq? type 'long) align-of-long)
          ((eq? type 'ulong) align-of-unsigned-long)
          ((eq? type 'float) align-of-float)
          ((eq? type 'double) align-of-double)
          ((eq? type 'pointer) align-of-void*)
          ((eq? type 'void) 0)
          ((eq? type 'callback) align-of-void*)
          (else #f))))

(define shared-object-load
  (lambda (path options)
    (open-shared-library path)))

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
          ((equal? type 'callback) 'callback)
          (else #f))))

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

(define c-bytevector?
  (lambda (object)
    (pointer? object)))

(define c-bytevector-u8-set! pointer-set-c-uint8_t!)
(define c-bytevector-u8-ref pointer-ref-c-uint8_t)
(define c-bytevector-pointer-set! pointer-set-c-pointer!)
(define c-bytevector-pointer-ref pointer-ref-c-pointer)

(define make-c-null empty-pointer)
(define c-null? null-pointer?)


