(define-library
  (foreign c capyscheme-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (core foreign)
          (core foreign-library))
  (export size-of-type
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
          c-null?)
  (include "capyscheme-primitives.scm"))
