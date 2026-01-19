(define (primitives-init set-procedure get-procedure)
  #t)

(define os 'unix)
(define implementation 'guile)
(define arch 'x86_64)
(define libc-name "c")

(define type->native-type
  (lambda (type)
    (cond ((equal? type 'i8) int8)
          ((equal? type 'u8) uint8)
          ((equal? type 'i16) int16)
          ((equal? type 'u16) uint16)
          ((equal? type 'i32) int32)
          ((equal? type 'u32) uint32)
          ((equal? type 'i64) int64)
          ((equal? type 'u64) uint64)
          ((equal? type 'char) int8)
          ((equal? type 'uchar) uint8)
          ((equal? type 'short) short)
          ((equal? type 'ushort) unsigned-short)
          ((equal? type 'int) int)
          ((equal? type 'uint) unsigned-int)
          ((equal? type 'long) long)
          ((equal? type 'ulong) unsigned-long)
          ((equal? type 'float) float)
          ((equal? type 'double) double)
          ((equal? type 'pointer) '*)
          ((equal? type 'void) void)
          ((equal? type 'callback) '*)
          (else #f))))

(define c-bytevector?
  (lambda (object)
    (pointer? object)))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (pointer->procedure (type->native-type return-type)
                           (foreign-library-pointer shared-object
                                                    (symbol->string c-name))
                           (map type->native-type argument-types))))))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name
       (procedure->pointer (type->native-type return-type)
                           procedure
                           (map type->native-type argument-types))))))

(define size-of-type
  (lambda (type)
    (let ((native-type (type->native-type type)))
      (cond ((equal? native-type void) 0)
            (native-type (sizeof native-type))
            (else #f)))))

(define align-of-type
  (lambda (type)
    (let ((native-type (type->native-type type)))
      (cond ((equal? native-type void) 0)
            (native-type (alignof native-type))
            (else #f)))))

(define shared-object-load
  (lambda (path options)
    (load-foreign-library path)))

(define (c-bytevector-u8-set! cbv offset byte)
  (bytevector-u8-set! (pointer->bytevector cbv (+ offset 100)) offset byte))

(define (c-bytevector-u8-ref cbv offset)
  (bytevector-u8-ref (pointer->bytevector cbv (+ offset 100)) offset))

(define (c-bytevector-pointer-set! cbv offset pointer)
  (bytevector-uint-set! (pointer->bytevector cbv (+ offset 100))
                        offset
                        (pointer-address pointer)
                        (native-endianness)
                        (size-of-type 'uint)))

(define (c-bytevector-pointer-ref cbv offset)
  (make-pointer (bytevector-uint-ref (pointer->bytevector cbv (+ offset 100))
                                     offset
                                     (native-endianness)
                                     (size-of-type 'uint))))

(define (make-c-null) (make-pointer (pointer-address %null-pointer)))

(define (c-null? pointer)
  (and (pointer? pointer)
       (null-pointer? pointer)))
