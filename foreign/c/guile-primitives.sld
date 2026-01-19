(define-library
  (foreign c guile-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (system foreign)
          (system foreign-library))
  (export primitives-init
          size-of-type
          align-of-type
          shared-object-load
          define-c-procedure
          ;define-c-callback
          c-bytevector?
          c-bytevector-u8-ref
          c-bytevector-u8-set!
          c-bytevector-pointer-ref
          c-bytevector-pointer-set!
          make-c-null
          c-null?
          ;; Guile specific
          implementation
          os
          arch
          libc-name)
  (include "guile-primitives.scm"))
