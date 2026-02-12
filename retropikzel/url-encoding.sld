(define-library
  (retropikzel url-encoding)
  (import (scheme base)
          (scheme write)
          (scheme char))
  (export encode-url
          ;decode-url
          )
  (include "url-encoding.scm"))
