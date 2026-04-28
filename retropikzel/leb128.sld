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
          uleb128->integer-and-length
          read-leb128
          read-leb128-and-length
          read-uleb128
          read-uleb128-and-length)
  (include "leb128.scm"))
