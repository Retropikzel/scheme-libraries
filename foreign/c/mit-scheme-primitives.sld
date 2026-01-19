(define-library
  (foreign c mit-scheme-primitives)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context))
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

(declare (usual-integrations))
(load-option 'ffi)

;(define lib (dld-load-file "mit-scheme-foreign-c-shim.so"))
(C-include "mit-scheme-foreign-c")

(define (hello)
  (puts "Hello from puts")
  ;(display "Not from puts")
  (newline)
  )
;(C-call "puts" "Hello world")
    )
  )
