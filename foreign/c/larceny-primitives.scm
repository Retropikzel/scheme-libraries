(require 'std-ffi)
(require 'ffi-load)
(require 'foreign-ctools)
(require 'foreign-cenums)
(require 'foreign-stdlib)
(require 'foreign-sugar)
;(require 'system-interface)

(define (type->native-type type)
  (cond ((equal? type 'int8) 'char)
        ((equal? type 'uint8) 'uchar)
        ((equal? type 'int16) 'short)
        ((equal? type 'uint16) 'ushort)
        ((equal? type 'int32) 'int)
        ((equal? type 'uint32) 'uint)
        ((equal? type 'int64) 'long)
        ((equal? type 'uint64) 'ulong)
        ((equal? type 'char) 'char)
        ((equal? type 'unsigned-char) 'uchar)
        ((equal? type 'short) 'short)
        ((equal? type 'unsigned-short) 'ushort)
        ((equal? type 'int) 'int)
        ((equal? type 'unsigned-int) 'uint)
        ((equal? type 'long) 'long)
        ((equal? type 'unsigned-long) 'ulong)
        ((equal? type 'float) 'float)
        ((equal? type 'double) 'double)
        ((equal? type 'pointer) 'void*)
        ((equal? type 'void) 'void)
        (error "Unsupported type: " type)))

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'int8) 1)
          ((eq? type 'uint8) 1)
          ((eq? type 'int16) 2)
          ((eq? type 'uint16) 2)
          ((eq? type 'int32) 4)
          ((eq? type 'uint32) 4)
          ((eq? type 'int64) 8)
          ((eq? type 'uint64) 8)
          ((eq? type 'char) 1)
          ((eq? type 'unsigned-char) 1)
          ((eq? type 'short) 2)
          ((eq? type 'unsigned-short) 2)
          ((eq? type 'int) 4)
          ((eq? type 'unsigned-int) 4)
          ((eq? type 'long) 4)
          ((eq? type 'unsigned-long) 4)
          ((eq? type 'float) 4)
          ((eq? type 'double) 8)
          ((eq? type 'pointer) 8)
          ((eq? type 'void) 0)
          (else (error "Can not get size of unknown type" type)))))

(define align-of-type size-of-type)

(define c-bytevector?
  (lambda (object)
    ;(void*? object)
    (number? object)))

(define shared-object-load
  (lambda (path . options)
    (foreign-file path)))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    ;; FIXME
    #;(syscall syscall:poke-bytes c-bytevector k (size-of-type 'uint8) byte)
    #t
    ))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
    ;; FIXME
    #;(syscall syscall:peek-bytes c-bytevector k (size-of-type 'uint8))
    #t
    ))

(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    ;; FIXME
    #;(syscall syscall:poke-bytes c-bytevector k (size-of-type 'pointer) pointer)
    #t
    ))

(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    ;; FIXME
    #;(syscall syscall:peek-bytes c-bytevector k (size-of-type 'pointer))
    #t
    ))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (foreign-procedure (symbol->string c-name)
                          (map type->native-type argument-types)
                          (type->native-type return-type))))))

(define (make-c-null) (foreign-null-pointer))
(define (c-null? pointer) (foreign-null-pointer?))

