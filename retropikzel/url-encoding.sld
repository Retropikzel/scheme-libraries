(define-library
  (retropikzel url-encoding)
  (import (scheme base))
  (export url-encode
          url-decode)
  (include "url-encoding.scm"))
