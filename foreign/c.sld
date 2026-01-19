(define-library
  (foreign c)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme process-context)
          (scheme inexact))
  (cond-expand
    (capyscheme (import (foreign c capyscheme-primitives)))
    (chezscheme (import (foreign c chezscheme-primitives))
                (export foreign-procedure))
    (chibi (import (foreign c chibi-primitives)))
    (chicken (import (foreign c chicken-primitives)
                     (chicken base)
                     (chicken foreign))
             (export foreign-declare
                     foreign-safe-lambda
                     foreign-value))
    ;(cyclone (import (foreign c cyclone-primitives)))
    ;(gambit (import (foreign c gambit-primitives)))
    (gauche (import (foreign c gauche-primitives)))
    (guile (import (foreign c guile-primitives)))
    (ikarus (import (foreign c ikarus-primitives)))
    (ironscheme (import (foreign c ironscheme-primitives)))
    (kawa (import (foreign c kawa-primitives)))
    ;(mit-scheme (import (foreign c mit-scheme-primitives)))
    ;(larceny (import (foreign c larceny-primitives)))
    (mosh (import (foreign c mosh-primitives)))
    (racket (import (foreign c racket-primitives)))
    (sagittarius (import (foreign c sagittarius-primitives)))
    (stklos (import (foreign c stklos-primitives))
            (export make-external-function
                    free-bytes
                    file-exists?
                    c-bytevector-pointer-set!
                    c-bytevector-pointer-ref))
    (ypsilon (import (foreign c ypsilon-primitives))
             (export c-function
                     bytevector-c-int8-set!
                     bytevector-c-uint8-ref)))
  (export
    ;; Types
    c-type-size
    c-type-align

    ;; Libraries and procedures
    define-c-library
    define-c-procedure
    ;define-c-callback ;; TODO

    ;; c-bytevectors
    make-c-bytevector
    c-bytevector
    c-bytevector?
    c-free
    make-c-null
    c-null?
    c-bytevector-set!
    c-bytevector-ref
    bytevector->c-bytevector
    c-bytevector->bytevector
    c-bytevector->integer
    integer->c-bytevector

    ;; Strings
    string->c-utf8
    c-utf8->string

    ;; Pass pointer by address
    call-with-address-of

    ;; Structs
    define-c-struct

    ;; Utilities
    libc-name

    ;; endianness
    native-endianness)
  (cond-expand
    (chezscheme
      (import (only (rnrs bytevectors) native-endianness)))
    (r6rs
      (import (only (rnrs bytevectors) native-endianness)))
    (guile
      (import (only (rnrs bytevectors) native-endianness)))
    (else
      (begin
        (define (native-endianness)
          (cond-expand (big-endian 'big) (else 'little))))))
  (cond-expand
    (chicken
      (begin
        (define-syntax define-c-library
          (syntax-rules ()
            ((_ scheme-name headers object-name options)
             (begin
               (define scheme-name #t)
               (shared-object-load headers)))))))
    (else (include "c/define-c-library.scm")))
  (cond-expand
    (chicken
      (begin
        (define libc-name
          (cond-expand
            (windows "ucrtbase")
            (haiku "root")
            (else "c")))
        (define-c-library libc
                          '("stdlib.h" "stdio.h" "string.h")
                          libc-name
                          '((additional-versions ("0" "6"))))

        (define-c-procedure c-malloc libc 'malloc 'pointer '(int))
        (define-c-procedure c-free libc 'free 'void '(pointer))
        (define-c-procedure c-strlen libc 'strlen 'int '(pointer))
        (define-c-procedure c-calloc libc 'calloc 'pointer '(int int))
        (define-c-procedure c-perror libc 'perror 'void '(pointer))
        (define (c-memset-address->pointer address value offset) (address->pointer address))
        (define (c-memset-pointer->address pointer value offset) (pointer->address pointer))))
    (else (include "c/libc.scm")))
  (cond-expand
    (chicken
      ;; FIXME These are in primitives too but error
      (begin
        (define (make-c-null) (foreign-value "NULL" c-pointer))
        (define c-null?
          (lambda (pointer)
            (if (and (not (pointer? pointer))
                     pointer)
              #f
              (or (not pointer) ; #f counts as null pointer on Chicken
                  (= (pointer->address pointer) 0)))))))
    (else))
  (include "c.scm"))

