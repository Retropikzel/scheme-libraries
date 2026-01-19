(define c-bytevector-set! #f)
(define c-bytevector-ref #f)
(define (primitives-init set-procedure get-procedure)
  (set! c-bytevector-set! set-procedure)
  (set! c-bytevector-ref get-procedure))

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
    (display "HERE: ")
    (write path)
    (newline)
    (load-foreign-library `(filename ,path))))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    (let ((p (pointer->bytevector c-bytevector (+ k 100))))
      (bytevector-u8-set! p k byte))))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
    (let ((p (pointer->bytevector c-bytevector (+ k 100))))
      (bytevector-u8-ref p k))))

(define c-bytevector-pointer-set!
  (lambda (cbv offset pointer)
    (c-bytevector-set! cbv 'uint offset pointer)))

(define c-bytevector-pointer-ref
  (lambda (cbv offset)
    (make-pointer (c-bytevector-ref cbv 'uint offset))))

(define (make-c-null) (make-pointer (pointer-address %null-pointer)))

(define (c-null? pointer)
  (and (pointer? pointer)
       (null-pointer? pointer)))
