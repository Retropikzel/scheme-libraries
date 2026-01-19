(define-library
  (foreign c sagittarius-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (except (sagittarius ffi) c-free c-malloc define-c-struct)
          (sagittarius))
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
          c-null?)
  (include "sagittarius-primitives.scm"))
