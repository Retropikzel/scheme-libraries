(define-library
  (foreign c racket-primitives)
  (cond-expand
    (r6rs
      (import (except (rnrs)
                      native-endianness)
              (only (racket base)
                    system-type
                    system-big-endian?)
              (ffi winapi)
              (compatibility mlist)
              (ffi unsafe)
              (ffi vector)))
    (else
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (only (racket base)
                    system-type
                    system-big-endian?)
              (ffi winapi)
              (compatibility mlist)
              (ffi unsafe)
              (ffi vector))))
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
          make-c-null ;; FIXME
          c-null? ;; FIXME
          )
  (include "racket-primitives.scm"))
