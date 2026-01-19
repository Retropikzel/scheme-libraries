(define (primitives-init set-procedure get-procedure) #t)

(define type->native-type ; Chicken has this procedure in three places
  (lambda (type)
    (cond ((equal? type 'i8) 'byte)
          ((equal? type 'u8) 'unsigned-byte)
          ((equal? type 'i16) 'short)
          ((equal? type 'u16) 'unsigned-short)
          ((equal? type 'i32) 'integer32)
          ((equal? type 'u32) 'unsigned-integer32)
          ((equal? type 'i64) 'integer64)
          ((equal? type 'u64) 'unsigned-integer64)
          ((equal? type 'char) 'char)
          ((equal? type 'uchar) 'unsigned-char)
          ((equal? type 'short) 'short)
          ((equal? type 'ushort) 'unsigned-short)
          ((equal? type 'int) 'int)
          ((equal? type 'uint) 'unsigned-int)
          ((equal? type 'long) 'long)
          ((equal? type 'ulong) 'unsigned-long)
          ((equal? type 'float) 'float)
          ((equal? type 'double) 'double)
          ((equal? type 'pointer) 'c-pointer)
          ((equal? type 'void) 'void)
          ((equal? type 'callback) 'c-pointer)
          ((equal? type 'struct) 'c-pointer)
          (else (error "type->native-type -- No such pffi type" type)))))

(define c-bytevector?
  (lambda (object)
    (pointer? object)))

(define-syntax define-c-procedure
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((type->native-type ; Chicken has this procedure in three places
               (lambda (type)
                 (cond ((equal? type 'i8) 'byte)
                       ((equal? type 'u8) 'unsigned-byte)
                       ((equal? type 'i16) 'short)
                       ((equal? type 'u16) 'unsigned-short)
                       ((equal? type 'i32) 'integer32)
                       ((equal? type 'u32) 'unsigned-integer32)
                       ((equal? type 'i64) 'integer64)
                       ((equal? type 'u64) 'unsigned-integer64)
                       ((equal? type 'char) 'char)
                       ((equal? type 'uchar) 'unsigned-char)
                       ((equal? type 'short) 'short)
                       ((equal? type 'ushort) 'unsigned-short)
                       ((equal? type 'int) 'int)
                       ((equal? type 'uint) 'unsigned-int)
                       ((equal? type 'long) 'long)
                       ((equal? type 'ulong) 'unsigned-long)
                       ((equal? type 'float) 'float)
                       ((equal? type 'double) 'double)
                       ((equal? type 'pointer) 'c-pointer)
                       ((equal? type 'void) 'void)
                       ((equal? type 'callback) 'c-pointer)
                       ((equal? type 'struct) 'c-pointer)
                       (else (error "type->native-type -- No such pffi type" type)))))
             (scheme-name (list-ref expr 1))
             (c-name (symbol->string (cadr (list-ref expr 3))))
             (return-type (type->native-type (cadr (list-ref expr 4))))
             (argument-types (if (null? (cdr (list-ref expr 5)))
                               (list)
                               (map type->native-type
                                    (cadr (list-ref expr 5))))))
        (if (null? argument-types)
          `(define ,scheme-name
             (foreign-safe-lambda ,return-type ,c-name))
          `(define ,scheme-name
             (foreign-safe-lambda ,return-type ,c-name ,@ argument-types)))))))

#;(define-syntax define-c-callback
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((type->native-type ; Chicken has this procedure in three places
               (lambda (type)
                 (cond ((equal? type 'i8) 'byte)
                       ((equal? type 'u8) 'unsigned-byte)
                       ((equal? type 'i16) 'short)
                       ((equal? type 'u16) 'unsigned-short)
                       ((equal? type 'i32) 'integer32)
                       ((equal? type 'u32) 'unsigned-integer32)
                       ((equal? type 'i64) 'integer64)
                       ((equal? type 'u64) 'unsigned-integer64)
                       ((equal? type 'char) 'char)
                       ((equal? type 'uchar) 'unsigned-char)
                       ((equal? type 'short) 'short)
                       ((equal? type 'ushort) 'unsigned-short)
                       ((equal? type 'int) 'int)
                       ((equal? type 'uint) 'unsigned-int)
                       ((equal? type 'long) 'long)
                       ((equal? type 'ulong) 'unsigned-long)
                       ((equal? type 'float) 'float)
                       ((equal? type 'double) 'double)
                       ((equal? type 'pointer) 'c-pointer)
                       ((equal? type 'void) 'void)
                       ((equal? type 'callback) 'c-pointer)
                       ((equal? type 'struct) 'c-pointer)
                       (else (error "type->native-type -- No such pffi type" type)))))
             (scheme-name (list-ref expr 1))
             (return-type (type->native-type (cadr (list-ref expr 2))))
             (argument-types (map type->native-type (cadr (list-ref expr 3))))
             (argument-names (cadr (list-ref expr 4)))
             (arguments (map
                          (lambda (name type)
                            `(,name ,type))
                          argument-types argument-names))
             (procedure-body (cdr (cdr (list-ref expr 4)))))
        `(begin (define-external ,(cons 'external_123456789 arguments)
                                 ,return-type
                                 (begin ,@ procedure-body))
                (define ,scheme-name (location external_123456789)))))))

(define size-of-type
  (lambda (type)
    (cond ((equal? type 'i8) (foreign-value "sizeof(int8_t)" int))
          ((equal? type 'u8) (foreign-value "sizeof(uint8_t)" int))
          ((equal? type 'i16) (foreign-value "sizeof(int16_t)" int))
          ((equal? type 'u16) (foreign-value "sizeof(uint16_t)" int))
          ((equal? type 'i32) (foreign-value "sizeof(int32_t)" int))
          ((equal? type 'u32) (foreign-value "sizeof(uint32_t)" int))
          ((equal? type 'i64) (foreign-value "sizeof(int64_t)" int))
          ((equal? type 'u64) (foreign-value "sizeof(uint64_t)" int))
          ((equal? type 'char) (foreign-value "sizeof(char)" int))
          ((equal? type 'uchar) (foreign-value "sizeof(unsigned char)" int))
          ((equal? type 'short) (foreign-value "sizeof(short)" int))
          ((equal? type 'ushort) (foreign-value "sizeof(unsigned short)" int))
          ((equal? type 'int) (foreign-value "sizeof(int)" int))
          ((equal? type 'uint) (foreign-value "sizeof(unsigned int)" int))
          ((equal? type 'long) (foreign-value "sizeof(long)" int))
          ((equal? type 'ulong) (foreign-value "sizeof(unsigned long)" int))
          ((equal? type 'float) (foreign-value "sizeof(float)" int))
          ((equal? type 'double) (foreign-value "sizeof(double)" int))
          ((equal? type 'pointer) (foreign-value "sizeof(void*)" int))
          ((equal? type 'string) (foreign-value "sizeof(void*)" int))
          ((equal? type 'callback) (foreign-value "sizeof(void*)" int)))))

(define align-of-type
  (lambda (type)
    (cond ((equal? type 'i8) (foreign-value "_Alignof(int8_t)" int))
          ((equal? type 'u8) (foreign-value "_Alignof(uint8_t)" int))
          ((equal? type 'i16) (foreign-value "_Alignof(int16_t)" int))
          ((equal? type 'u16) (foreign-value "_Alignof(uint16_t)" int))
          ((equal? type 'i32) (foreign-value "_Alignof(int32_t)" int))
          ((equal? type 'u32) (foreign-value "_Alignof(uint32_t)" int))
          ((equal? type 'i64) (foreign-value "_Alignof(int64_t)" int))
          ((equal? type 'u64) (foreign-value "_Alignof(uint64_t)" int))
          ((equal? type 'char) (foreign-value "_Alignof(char)" int))
          ((equal? type 'uchar) (foreign-value "_Alignof(unsigned char)" int))
          ((equal? type 'short) (foreign-value "_Alignof(short)" int))
          ((equal? type 'ushort) (foreign-value "_Alignof(unsigned short)" int))
          ((equal? type 'int) (foreign-value "_Alignof(int)" int))
          ((equal? type 'uint) (foreign-value "_Alignof(unsigned int)" int))
          ((equal? type 'long) (foreign-value "_Alignof(long)" int))
          ((equal? type 'ulong) (foreign-value "_Alignof(unsigned long)" int))
          ((equal? type 'float) (foreign-value "_Alignof(float)" int))
          ((equal? type 'double) (foreign-value "_Alignof(double)" int))
          ((equal? type 'pointer) (foreign-value "_Alignof(void*)" int))
          ((equal? type 'string) (foreign-value "_Alignof(void*)" int))
          ((equal? type 'callback) (foreign-value "_Alignof(void*)" int)))))

(define-syntax shared-object-load
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((headers (cadr (car (cdr expr)))))
        `(begin
           ,@ (map
                (lambda (header)
                  `(foreign-declare ,(string-append "#include <" header ">")))
                headers))))))

(define c-bytevector-u8-ref
  (lambda (c-bytevector k)
    (pointer-u8-ref (pointer+ c-bytevector k))))

(define c-bytevector-u8-set!
  (lambda (c-bytevector k byte)
    (pointer-u8-set! (pointer+ c-bytevector k) byte)))

(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    (address->pointer (pointer-u64-ref (pointer+ c-bytevector k)))))

(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    (pointer-u64-set! (pointer+ c-bytevector k) (pointer->address pointer))))

(define (make-c-null) (foreign-value "NULL" c-pointer))

(define c-null?
  (lambda (pointer)
    (if (and (not (pointer? pointer))
             pointer)
      #f
      (or (not pointer) ; #f counts as null pointer on Chicken
          (= (pointer->address pointer) 0)))))
