(define-library
  (foreign c gambit-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context)
          (only (gambit) c-declare c-lambda c-define define-macro))
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
  (include "gambit-primitives.scm"))
