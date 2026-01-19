(define-library
  (foreign c chezscheme-primitives)
  (import (chezscheme))
  (export size-of-type
          align-of-type
          shared-object-load
          define-c-procedure
          c-bytevector?
          c-bytevector-u8-ref
          c-bytevector-u8-set!
          c-bytevector-pointer-ref
          c-bytevector-pointer-set!
          ;; Chez specific
          foreign-procedure
          type->native-type
          make-c-null
          c-null?)
  (include "chezscheme-primitives.scm"))
