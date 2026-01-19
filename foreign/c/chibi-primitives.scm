(define c-bytevector-ref #f)
(define (primitives-init set-procedure get-procedure)
  (set! c-bytevector-ref get-procedure))

(define type->libffi-type-number
  (lambda (type)
    (cond ((equal? type 'i8) 1)
          ((equal? type 'u8) 2)
          ((equal? type 'i16) 3)
          ((equal? type 'u16) 4)
          ((equal? type 'i32) 5)
          ((equal? type 'u32) 6)
          ((equal? type 'i64) 7)
          ((equal? type 'u64) 8)
          ((equal? type 'char) 9)
          ((equal? type 'uchar) 10)
          ((equal? type 'short) 11)
          ((equal? type 'ushort) 12)
          ((equal? type 'int) 13)
          ((equal? type 'uint) 14)
          ((equal? type 'long) 15)
          ((equal? type 'ulong) 16)
          ((equal? type 'float) 17)
          ((equal? type 'double) 18)
          ((equal? type 'void) 19)
          ((equal? type 'pointer) 20)
          ((equal? type 'pointer-address) 21)
          ((equal? type 'callback) 22)
          (else (error "Undefined type" type)))))

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'i8) (size-of-int8_t))
          ((eq? type 'u8) (size-of-uint8_t))
          ((eq? type 'i16) (size-of-int16_t))
          ((eq? type 'u16) (size-of-uint16_t))
          ((eq? type 'i32) (size-of-int32_t))
          ((eq? type 'u32) (size-of-uint32_t))
          ((eq? type 'i64) (size-of-int64_t))
          ((eq? type 'u64) (size-of-uint64_t))
          ((eq? type 'char) (size-of-char))
          ((eq? type 'uchar) (size-of-char))
          ((eq? type 'short) (size-of-short))
          ((eq? type 'ushort) (size-of-unsigned-short))
          ((eq? type 'int) (size-of-int))
          ((eq? type 'uint) (size-of-unsigned-int))
          ((eq? type 'long) (size-of-long))
          ((eq? type 'ulong) (size-of-unsigned-long))
          ((eq? type 'float) (size-of-float))
          ((eq? type 'double) (size-of-double))
          ((eq? type 'pointer) (size-of-pointer))
          ((eq? type 'pointer-address) (size-of-pointer))
          ((eq? type 'callback) (size-of-pointer))
          ((eq? type 'void) 0)
          (else #f))))

(define align-of-type
  (lambda (type)
    (cond ((eq? type 'i8) (align-of-int8_t))
          ((eq? type 'u8) (align-of-uint8_t))
          ((eq? type 'i16) (align-of-int16_t))
          ((eq? type 'u16) (align-of-uint16_t))
          ((eq? type 'i32) (align-of-int32_t))
          ((eq? type 'u32) (align-of-uint32_t))
          ((eq? type 'i64) (align-of-int64_t))
          ((eq? type 'u64) (align-of-uint64_t))
          ((eq? type 'char) (align-of-char))
          ((eq? type 'uchar) (align-of-char))
          ((eq? type 'short) (align-of-short))
          ((eq? type 'ushort) (align-of-unsigned-short))
          ((eq? type 'int) (align-of-int))
          ((eq? type 'uint) (align-of-unsigned-int))
          ((eq? type 'long) (align-of-long))
          ((eq? type 'ulong) (align-of-unsigned-long))
          ((eq? type 'float) (align-of-float))
          ((eq? type 'double) (align-of-double))
          ((eq? type 'pointer) (align-of-pointer))
          ((eq? type 'pointer-address) (align-of-pointer))
          ((eq? type 'callback) (align-of-pointer))
          ((eq? type 'void) 0)
          (else #f))))

(define shared-object-load
  (lambda (path options)
    (let ((shared-object (dlopen path RTLD-NOW))
          ;(maybe-error (dlerror))
          )
      shared-object)))

(define c-bytevector?
  (lambda (object)
    (or (equal? object #f) ; False can be null pointer
        (pointer? object))))

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
          ((equal? type 'pointer) '(maybe-null pointer void*))
          ((equal? type 'pointer-address) '(maybe-null pointer void*))
          ((equal? type 'void) 'void)
          ((equal? type 'callback) '(maybe-null pointer void*))
          (else (error "type->native-type -- No such pffi type" type)))))

;; define-c-procedure

(define make-c-function
  (lambda (shared-object c-name return-type argument-types)
    ;(dlerror) ;; Clean all previous errors
    (let ((c-function (dlsym shared-object c-name))
          ;(maybe-dlerror (dlerror))
          )
      (lambda arguments
        (let* ((return-pointer
                 (internal-ffi-call (length argument-types)
                                    (type->libffi-type-number return-type)
                                    (map type->libffi-type-number argument-types)
                                    c-function
                                    (size-of-type return-type)
                                    arguments)))
          (when (not (symbol=? return-type 'void))
            (c-bytevector-ref return-pointer return-type 0)))))))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (make-c-function shared-object
                        (symbol->string c-name)
                        return-type
                        argument-types)))))

(define make-c-callback
  (lambda (return-type argument-types procedure)
    (scheme-procedure-to-pointer procedure)))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (error "define-c-callback is not yet supported on Chibi")
     #;(define scheme-name
       (make-c-callback return-type 'argument-types procedure))
     )))

(define (c-null? pointer)
  (or (equal? pointer #f) ;; #f counts as null pointer on chibi
      (and (c-bytevector? pointer)
           (internal-c-null? pointer))))

