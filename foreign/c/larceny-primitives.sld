(define-library
  (foreign c larceny-primitives)
  (cond-expand
    (r6rs (import (rnrs base)
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
                  (rename (primitives r5rs:require) (r5rs:require require))
                  (primitives std-ffi)
                  (primitives foreign-procedure)
                  (primitives foreign-file)
                  (primitives foreign-stdlib)
                  (primitives system-interface)))
    (else
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (rename (primitives r5rs:require) (r5rs:require require))
              (primitives std-ffi)
              (primitives foreign-procedure)
              (primitives foreign-file)
              (primitives foreign-stdlib)
              (primitives system-interface))))
  (export size-of-type
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
  (include "larceny-primitives.scm"))
