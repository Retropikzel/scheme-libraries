(define-library
  (foreign c struct)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context))
  (export define-c-struct
          c-struct->alist
          ;pffi-define-struct;define-c-struct
          ;pffi-struct-pointer;c-struct-bytevector
          ;pffi-struct-offset-get;c-struct-offset
          ;pffi-struct-set!;c-struct-set!
          ;pffi-struct-get;c-struct-get
          )
  (include "struct.scm"))
