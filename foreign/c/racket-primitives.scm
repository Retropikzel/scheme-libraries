(define (primitives-init set-procedure get-procedure) #t)

(define type->native-type
  (lambda (type)
    (cond ((equal? type 'i8) _byte)
          ((equal? type 'u8) _ubyte)
          ((equal? type 'i16) _int16)
          ((equal? type 'u16) _uint16)
          ((equal? type 'i32) _int32)
          ((equal? type 'u32) _uint32)
          ((equal? type 'i64) _int64)
          ((equal? type 'u64) _uint64)
          ((equal? type 'char) _int8)
          ((equal? type 'uchar) _uint8)
          ((equal? type 'short) _short)
          ((equal? type 'ushort) _ushort)
          ((equal? type 'int) _int)
          ((equal? type 'uint) _uint)
          ((equal? type 'long) _long)
          ((equal? type 'ulong) _ulong)
          ((equal? type 'float) _float)
          ((equal? type 'double) _double)
          ((equal? type 'pointer) _pointer)
          ((equal? type 'void) _void)
          ((equal? type 'callback) _pointer)
          (else #f))))

(define c-bytevector?
  (lambda (object)
    (cpointer? object)))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (get-ffi-obj c-name
                    shared-object
                    (_cprocedure (mlist->list (map type->native-type argument-types))
                                 (type->native-type return-type)))))))


(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name (function-ptr procedure
                                       (_cprocedure
                                         (mlist->list (map type->native-type argument-types))
                                         (type->native-type return-type)))))))

(define size-of-type
  (lambda (type)
    (ctype-sizeof (type->native-type type))))

;; FIXME
(define align-of-type
  (lambda (type)
    (ctype-sizeof (type->native-type type))))

(define shared-object-load
  (lambda (path options)
    (if (and (not (null? options))
             (assoc 'additional-versions options))
      (ffi-lib path (mlist->list (append (cadr (assoc 'additional-versions
                                                      options))
                                         (list #f))))
      (ffi-lib path))))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    (ptr-set! c-bytevector _uint8 'abs k byte)))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
    (ptr-ref c-bytevector _uint8 'abs k)))

(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    (ptr-set! c-bytevector _pointer 'abs k pointer)))

(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    (ptr-ref c-bytevector _pointer 'abs k)))

(define (make-c-null) #f)
(define (c-null? pointer) (and (cpointer? pointer) (equal? pointer #f)))

