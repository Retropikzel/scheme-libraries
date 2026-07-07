(define-library
  (retropikzel hardware-info)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme file))
  (export cpu-count)
  (include "hardware-info.scm"))
