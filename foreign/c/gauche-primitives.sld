(define-library
  (foreign c gauche-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (gauche ffi))
  (export primitives-init
          internal-size-of-type
          align-of-type
          shared-object-load
          define-c-procedure
          c-bytevector?
          c-bytevector-u8-ref
          c-bytevector-u8-set!
          c-bytevector-pointer-ref
          c-bytevector-pointer-set!
          make-c-null
          c-null?
          (rename internal-size-of-type size-of-type)
          (rename internal-align-of-type align-of-type))
  (include "gauche-primitives.scm"))
