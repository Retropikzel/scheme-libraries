(define-library
  (foreign c ironscheme-primitives)
  (import (rnrs base)
          (rnrs lists)
          (rnrs control)
          (rnrs files)
          (rnrs io simple)
          (rnrs programs)
          (only (rnrs bytevectors)
                make-bytevector
                bytevector-length
                utf8->string
                string->utf8
                bytevector-u8-ref
                bytevector-u8-set!)
          (only (rnrs r5rs)
                remainder
                quotient)
          (ironscheme)
          (ironscheme clr)
          (ironscheme clr internal)
          (ironscheme ffi)
          (srfi :0))
  (export primitives-init
          size-of-type
          align-of-type
          shared-object-load
          define-c-procedure
          c-bytevector?
          c-bytevector-u8-ref
          c-bytevector-u8-set!
          c-bytevector-pointer-ref
          c-bytevector-pointer-set!
          make-c-null
          c-null?)
  (begin
    (clr-using System.Runtime.InteropServices)

    (define (primitives-init set-procedure get-procedure) #t)

    ;; FIXME
    (define size-of-type
      (lambda (type)
        (cond ((eq? type 'i8) 1)
              ((eq? type 'u8) 1)
              ((eq? type 'i16) 2)
              ((eq? type 'u16) 2)
              ((eq? type 'i32) 4)
              ((eq? type 'u32) 4)
              ((eq? type 'i64) 8)
              ((eq? type 'u64) 8)
              ((eq? type 'char) 1)
              ((eq? type 'uchar) 1)
              ((eq? type 'short) 2)
              ((eq? type 'ushort) 2)
              ((eq? type 'int) 4)
              ((eq? type 'unsigned-int) 4)
              ((eq? type 'long) 8)
              ((eq? type 'ulong) 8)
              ((eq? type 'float) 4)
              ((eq? type 'double) 8)
              ((eq? type 'pointer) 8)
              ((eq? type 'void) 0)
              (else #f))))

    ;; FIXME
    (define align-of-type size-of-type)

    ;; FIXME
    (define (type->native-type type)
      (cond ((equal? type 'i8) 'int8)
            ((equal? type 'u8) 'uint8)
            ((equal? type 'i16) 'int16)
            ((equal? type 'u16) 'uint16)
            ((equal? type 'i32) 'int32)
            ((equal? type 'u32) 'uint32)
            ((equal? type 'i64) 'in64)
            ((equal? type 'u64) 'uint64)
            ((equal? type 'char) 'char)
            ((equal? type 'uchar) 'uchar)
            ((equal? type 'short) 'int16)
            ((equal? type 'ushort) 'uint16)
            ((equal? type 'int) 'int32)
            ((equal? type 'uint) 'uint32)
            ((equal? type 'long) 'int64)
            ((equal? type 'ulong) 'uint64)
            ((equal? type 'float) 'float)
            ((equal? type 'double) 'double)
            ((equal? type 'void) 'void)
            ((equal? type 'pointer) 'void*)
            (error "Unsupported type: " type)))

    (define c-bytevector?
      (lambda (object)
        (pointer? object)))

    (define-syntax define-c-procedure
      (syntax-rules ()
        ((_ scheme-name shared-object c-name return-type argument-types)
         (define scheme-name
           ((make-ffi-callout (type->native-type return-type)
                              (map type->native-type argument-types))
            (cond-expand
              (windows (dlsym shared-object (symbol->string c-name)))
              (else (apply (pinvoke-call libc dlsym void* (void* string))
                           (list shared-object (symbol->string c-name))))))))))

    (define shared-object-load
      (lambda (path options)
        (cond-expand
          (windows (dlopen path))
          (else (apply (pinvoke-call libc dlopen void* (string int))
                       (list path 0))))))

    (define c-bytevector-u8-set!
      (lambda (c-bytevector k byte)
        (clr-static-call Marshal
                         (WriteByte IntPtr Int32 Byte)
                         c-bytevector
                         k
                         (clr-static-call Convert (ToByte Int32) byte))))

    (define c-bytevector-u8-ref
      (lambda (c-bytevector k)
        (clr-static-call Convert
                         (ToInt32 Byte)
                         (clr-static-call Marshal (ReadByte IntPtr Int32) c-bytevector k))))

    (define c-bytevector-pointer-set!
      (lambda (c-bytevector k pointer)
        (write-intptr! c-bytevector k pointer)))

    (define c-bytevector-pointer-ref
      (lambda (c-bytevector k)
        (read-intptr c-bytevector k)))

    (define make-c-null null-pointer)
    (define (c-null? pointer)
      (and (pointer? pointer)
           (null-pointer? pointer)))))
