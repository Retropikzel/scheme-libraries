(define (primitives-init set-procedure get-procedure) #t)

(define (shared-object-load path options) path)

(define type->native-type
  (lambda (type)
    (cond ((equal? type 'i8) :char)
          ((equal? type 'u8) :char)
          ((equal? type 'i16) :short)
          ((equal? type 'u16) :ushort)
          ((equal? type 'i32) :int)
          ((equal? type 'u32) :uint)
          ((equal? type 'i64) :long)
          ((equal? type 'u64) :ulong)
          ((equal? type 'char) :char)
          ((equal? type 'uchar) :uchar)
          ((equal? type 'short) :short)
          ((equal? type 'ushort) :ushort)
          ((equal? type 'int) :int)
          ((equal? type 'uint) :uint)
          ((equal? type 'long) :long)
          ((equal? type 'ulong) :ulong)
          ((equal? type 'float) :float)
          ((equal? type 'double) :double)
          ((equal? type 'pointer) :pointer)
          ((equal? type 'void) :void)
          ((equal? type 'callback) :pointer)
          (else (error "type->native-type -- No such pffi type" type)))))

(define c-bytevector?
  (lambda (object)
    (and (not (void? object))
         (cpointer? object))))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (begin
       (define type->native-type
         (lambda (type)
           (cond ((equal? type 'i8) :char)
                 ((equal? type 'u8) :char)
                 ((equal? type 'i16) :short)
                 ((equal? type 'u16) :ushort)
                 ((equal? type 'i32) :int)
                 ((equal? type 'u32) :uint)
                 ((equal? type 'i64) :long)
                 ((equal? type 'u64) :ulong)
                 ((equal? type 'char) :char)
                 ((equal? type 'uchar) :char)
                 ((equal? type 'short) :short)
                 ((equal? type 'ushort) :ushort)
                 ((equal? type 'int) :int)
                 ((equal? type 'uint) :uint)
                 ((equal? type 'long) :long)
                 ((equal? type 'ulong) :ulong)
                 ((equal? type 'float) :float)
                 ((equal? type 'double) :double)
                 ((equal? type 'pointer) :pointer)
                 ((equal? type 'void) :void)
                 ((equal? type 'callback) :pointer)
                 (else (error "type->native-type -- No such pffi type" type)))))
       (define scheme-name
         (make-external-function
           (symbol->string c-name)
           (map type->native-type argument-types)
           (type->native-type return-type)
           shared-object))))))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name
       (%make-callback procedure
                       (map type->native-type argument-types)
                       (type->native-type return-type))))))

(define size-of-type
  (lambda (type)
    (cond ((equal? type 'i8) (c-size-of :int8))
          ((equal? type 'u8) (c-size-of :uint8))
          ((equal? type 'i16) (c-size-of :int16))
          ((equal? type 'u16) (c-size-of :uint16))
          ((equal? type 'i32) (c-size-of :int32))
          ((equal? type 'u32) (c-size-of :uint32))
          ((equal? type 'i64) (c-size-of :int64))
          ((equal? type 'u64) (c-size-of :uint64))
          ((equal? type 'char) (c-size-of :char))
          ((equal? type 'uchar) (c-size-of :uchar))
          ((equal? type 'short) (c-size-of :short))
          ((equal? type 'ushort) (c-size-of :ushort))
          ((equal? type 'int) (c-size-of :int))
          ((equal? type 'uint) (c-size-of :uint))
          ((equal? type 'long) (c-size-of :long))
          ((equal? type 'ulong) (c-size-of :ulong))
          ((equal? type 'float) (c-size-of :float))
          ((equal? type 'double) (c-size-of :double))
          ((equal? type 'pointer) (c-size-of :pointer)))))

;; FIXME
(define align-of-type
  (lambda (type)
    (size-of-type type)))

(define c-bytevector-u8-set!
  (lambda (pointer offset value)
    (cpointer-set-abs! pointer :uint8 value offset)))

(define c-bytevector-u8-ref
  (lambda (pointer offset)
    (cpointer-ref-abs pointer :uint8 offset)))

(define c-bytevector-pointer-set!
  (lambda (pointer offset value)
    (cpointer-set-abs! pointer :pointer value offset)))

(define c-bytevector-pointer-ref
  (lambda (pointer offset)
    (cpointer-ref-abs pointer :pointer offset)))

(define (make-c-null) #f) ;; FIXME
(define c-null? cpointer-null?)
