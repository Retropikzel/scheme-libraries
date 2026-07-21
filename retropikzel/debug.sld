(define-library
  (retropikzel debug)
  (import (scheme base)
          (scheme write))
  (export debug
          debug-display
          debug-proc)
  (include "debug.scm"))
