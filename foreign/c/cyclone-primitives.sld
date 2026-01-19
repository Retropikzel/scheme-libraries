(define-library
  (foreign c primitives-cyclone)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (cyclone foreign)
          (scheme cyclone primitives))
  (export size-of-type
          align-of-type
          shared-object-load
          define-c-procedure
          ;define-c-callback
          c-bytevector?
          c-bytevector-u8-ref
          c-bytevector-u8-set!
          c-bytevector-pointer-ref
          c-bytevector-pointer-set!)
  (begin
    (define type->native-type
      (lambda (type)
        (cond ((equal? type 'int8) int)
              ((equal? type 'uint8) int)
              ((equal? type 'int16) int)
              ((equal? type 'uint16) int)
              ((equal? type 'int32) int)
              ((equal? type 'uint32) int)
              ((equal? type 'int64) int)
              ((equal? type 'uint64) int)
              ((equal? type 'char) char)
              ((equal? type 'unsigned-char) char)
              ((equal? type 'short) int)
              ((equal? type 'unsigned-short) int)
              ((equal? type 'int) int)
              ((equal? type 'unsigned-int) int)
              ((equal? type 'long) int)
              ((equal? type 'unsigned-long) int)
              ((equal? type 'float) float)
              ((equal? type 'double) double)
              ((equal? type 'pointer) opaque)
              ((equal? type 'void) c-void)
              ((equal? type 'callback) opaque)
              (else (error "type->native-type -- No such type" type)))))

    (define c-bytevector?
      (lambda (object)
        (opaque? object)))

    (define-syntax define-c-procedure
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((type->native-type
                   (lambda (type)
                     (cond ((equal? type 'int8) 'int)
                           ((equal? type 'uint8) 'int)
                           ((equal? type 'int16) 'int)
                           ((equal? type 'uint16) 'int)
                           ((equal? type 'int32) 'int)
                           ((equal? type 'uint32) 'int)
                           ((equal? type 'int64) 'int)
                           ((equal? type 'uint64) 'int)
                           ((equal? type 'char) 'char)
                           ((equal? type 'unsigned-char) 'unsigned-char)
                           ((equal? type 'short) 'short)
                           ((equal? type 'unsigned-short) 'unsigned-short)
                           ((equal? type 'int) 'int)
                           ((equal? type 'unsigned-int) 'unsigned-int)
                           ((equal? type 'long) 'long)
                           ((equal? type 'unsigned-long) 'unsigned-long)
                           ((equal? type 'float) 'float)
                           ((equal? type 'double) 'double)
                           ((equal? type 'pointer) 'opaque)
                           ((equal? type 'void) 'c-void)
                           ((equal? type 'callback) 'opaque)
                           (else (error "type->native-type -- No such type" type)))))
                 (scheme-name (cadr expr))
                 (c-name (symbol->string (car (cdr (car (cdr (cdr (cdr expr))))))))
                 (return-type (type->native-type (car (cdr (car (cdr (cdr (cdr (cdr expr)))))))))
                 (argument-types
                   (let ((types (cadr (car (cdr (cdr (cdr (cdr (cdr expr)))))))))
                     (if (null? types)
                       '()
                       (map type->native-type types)))))
            (if (null? argument-types)
              `(c-define ,scheme-name ,return-type ,c-name)
              `(c-define ,scheme-name
                         ,return-type ,c-name ,@argument-types))))))

    (define define-c-callback
      (lambda (scheme-name return-type argument-types procedure)
        (error "define-callback not yet implemented on Cyclone")))

    (define size-of-type
      (lambda (type)
        (cond ((equal? type 'int8) (c-value "sizeof(int8_t)" int))
              ((equal? type 'uint8) (c-value "sizeof(uint8_t)" int))
              ((equal? type 'int16) (c-value "sizeof(int16_t)" int))
              ((equal? type 'uint16) (c-value "sizeof(uint16_t)" int))
              ((equal? type 'int32) (c-value "sizeof(int32_t)" int))
              ((equal? type 'uint32) (c-value "sizeof(uint32_t)" int))
              ((equal? type 'int64) (c-value "sizeof(int64_t)" int))
              ((equal? type 'uint64) (c-value "sizeof(uint64_t)" int))
              ((equal? type 'char) (c-value "sizeof(char)" int))
              ((equal? type 'unsigned-char) (c-value "sizeof(unsigned char)" int))
              ((equal? type 'short) (c-value "sizeof(short)" int))
              ((equal? type 'unsigned-short) (c-value "sizeof(unsigned short)" int))
              ((equal? type 'int) (c-value "sizeof(int)" int))
              ((equal? type 'unsigned-int) (c-value "sizeof(unsigned int)" int))
              ((equal? type 'long) (c-value "sizeof(long)" int))
              ((equal? type 'unsigned-long) (c-value "sizeof(unsigned long)" int))
              ((equal? type 'float) (c-value "sizeof(float)" int))
              ((equal? type 'double) (c-value "sizeof(double)" int))
              ((equal? type 'pointer) (c-value "sizeof(void*)" int)))))

    ;; FIXME
    (define align-of-type size-of-type)

    (define-c pointer-address
              "(void *data, int argc, closure _, object k, object pointer)"
              "make_c_opaque(opq, &(void*)opaque_ptr(pointer));
              return_closcall1(data, k, &opq);")

              (define pointer-null
                (lambda ()
                  (make-opaque)))

              (define-syntax define-c-library
                (syntax-rules ()
                  ((_ scheme-name headers object-name options)
                   (begin
                     (define scheme-name #t)
                     (shared-object-load headers)))))

              (define-syntax shared-object-load
                (er-macro-transformer
                  (lambda (expr rename compare)
                    (let* ((headers (cadr (cadr expr)))
                           (includes (map
                                       (lambda (header)
                                         `(include-c-header ,(string-append "<" header ">")))
                                       headers)))
                      `(,@includes)))))

              (define pointer-null?
                (lambda (pointer)
                  (and (opaque? pointer)
                       (opaque-null? pointer))))

              (define-c pointer-int8-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "int8_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-uint8-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "uint8_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-int16-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "int16_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-uint16-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "uint16_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-int32-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "int32_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-uint32-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "uint32_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-int64-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "int64_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-uint64-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "uint64_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-char-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "char* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2char(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-short-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "short* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-unsigned-short-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "unsigned short* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-int-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "int* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-unsigned-int-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "unsigned int* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-long-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "long* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-unsigned-long-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "unsigned long* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = obj_obj2int(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-float-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "float* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = double_value(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-double-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "double* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = double_value(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c c-bytevector-pointer-set!
                        "(void *data, int argc, closure _, object k, object pointer, object offset, object value)"
                        "uintptr_t* p = opaque_ptr(pointer) + obj_obj2int(offset); *p = (uintptr_t)&opaque_ptr(value); return_closcall1(data, k, make_boolean(boolean_t));")

              (define-c pointer-int8-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "int8_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-uint8-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "uint8_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-int16-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "int16_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-uint16-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "uint16_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-int32-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "int32_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-uint32-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "uint32_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-int64-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "int64_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-uint64-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "uint64_t* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-char-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "char* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_char2obj(*p));")

              (define-c pointer-short-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "short* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-unsigned-short-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "unsigned short* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-int-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "int* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-unsigned-int-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "unsigned int* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-long-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "long* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-unsigned-long-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "unsigned long* p = opaque_ptr(pointer) + obj_obj2int(offset); return_closcall1(data, k, obj_int2obj(*p));")

              (define-c pointer-float-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "float* p = opaque_ptr(pointer) + obj_obj2int(offset); alloca_double(d, *p); return_closcall1(data, k, d);")

              (define-c pointer-double-get
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "double* p = opaque_ptr(pointer) + obj_obj2int(offset); alloca_double(d, *p); return_closcall1(data, k, d);")

              (define-c c-bytevector-pointer-ref
                        "(void *data, int argc, closure _, object k, object pointer, object offset)"
                        "make_c_opaque(opq, (void*)opaque_ptr(pointer) + obj_obj2int(offset)); return_closcall1(data, k, &opq);")

              (define c-bytevector-u8-set! pointer-uint8-set!)
              (define c-bytevector-u8-ref pointer-uint8-get))))
