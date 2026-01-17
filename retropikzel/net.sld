(define-library
  (retropikzel net)
  (import (scheme base)
          (scheme file)
          (scheme process-context))
  (export url-encode
          url-decode
          make-temp-filename)
  (include "net.scm"))
