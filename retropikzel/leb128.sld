(define-library
  (retropikzel leb128)
  (import (scheme base)
          (scheme write)
          (srfi 60))
  (export integer->leb128
          leb128->integer
          leb128->integer-and-length
          integer->uleb128
          uleb128->integer
          uleb128->integer-and-length)
  (include "leb128.scm"))
