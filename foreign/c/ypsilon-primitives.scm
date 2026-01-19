(define (primitives-init set-procedure get-procedure) #t)

(define size-of-type
  (lambda (type)
    (cond ((eq? type 'i8) (c-sizeof int8_t))
          ((eq? type 'u8) (c-sizeof uint8_t))
          ((eq? type 'i16) (c-sizeof int16_t))
          ((eq? type 'u16) (c-sizeof uint16_t))
          ((eq? type 'i32) (c-sizeof int32_t))
          ((eq? type 'u32) (c-sizeof uint32_t))
          ((eq? type 'i64) (c-sizeof int64_t))
          ((eq? type 'u64) (c-sizeof uint64_t))
          ((eq? type 'char) (c-sizeof char))
          ((eq? type 'uchar) (c-sizeof char))
          ((eq? type 'short) (c-sizeof short))
          ((eq? type 'ushort) (c-sizeof unsigned-short))
          ((eq? type 'int) (c-sizeof int))
          ((eq? type 'uint) (c-sizeof unsigned-int))
          ((eq? type 'long) (c-sizeof long))
          ((eq? type 'ulong) (c-sizeof unsigned-long))
          ((eq? type 'float) (c-sizeof float))
          ((eq? type 'double) (c-sizeof double))
          ((eq? type 'pointer) (c-sizeof void*))
          ((eq? type 'callback) (c-sizeof void*)))))

(define align-of-type
  (lambda (type)
    (cond ((eq? type 'i8) alignof:int8_t)
          ((eq? type 'u8) alignof:int8_t)
          ((eq? type 'i16) alignof:int16_t)
          ((eq? type 'u16) alignof:int16_t)
          ((eq? type 'i32) alignof:int32_t)
          ((eq? type 'u32) alignof:int32_t)
          ((eq? type 'i64) alignof:int64_t)
          ((eq? type 'u64) alignof:int64_t)
          ((eq? type 'char) alignof:int8_t)
          ((eq? type 'uchar) alignof:int8_t)
          ((eq? type 'short) alignof:short)
          ((eq? type 'ushort) alignof:short)
          ((eq? type 'int) alignof:int)
          ((eq? type 'uint) alignof:int)
          ((eq? type 'long) alignof:long)
          ((eq? type 'ulong) alignof:long)
          ((eq? type 'float) alignof:float)
          ((eq? type 'double) alignof:double)
          ((eq? type 'pointer) alignof:void*)
          ((eq? type 'callback) alignof:void*))))

(define c-bytevector?
  (lambda (object)
    (number? object)))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    ;; Ypsilon for some reason does not have bytevector-c-uint8-set!
    ;; or other bytevector-c-u*-set! procedures so we use
    ;; bytevector-c-int8-set!
    (bytevector-c-int8-set! (make-bytevector-mapping (+ c-bytevector k)
                                                     (size-of-type 'uint8))
                            0
                            byte)))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
      (bytevector-c-uint8-ref (make-bytevector-mapping (+ c-bytevector k)
                                                      (size-of-type 'uint8))
                             0)))

(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    (bytevector-c-void*-set! (make-bytevector-mapping (+ c-bytevector k)
                                                      (size-of-type 'pointer))
                             0
                             pointer)))

(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    (bytevector-c-void*-ref (make-bytevector-mapping (+ c-bytevector k)
                                                     (size-of-type 'pointer))
                            0)))

(define shared-object-load
  (lambda (path options)
    (load-shared-object path)))

(define-macro
  (define-c-procedure scheme-name shared-object c-name return-type argument-types)
  (begin
    (let ((type->native-type
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
                    (else (error "type->native-type -- No such type" type))))))
      `(define ,scheme-name
         (c-function ,(type->native-type (cadr return-type))
                     ,(cadr c-name)
                     ,(map type->native-type (cadr argument-types)))))))

(define-macro
  (define-c-callback scheme-name return-type argument-types procedure)
  (let* ((type->native-type
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
         (native-return-type (type->native-type (cadr return-type)))
         (native-argument-types (map type->native-type (cadr argument-types))))
    `(define ,scheme-name
       (c-callback ,native-return-type ,native-argument-types ,procedure))))
