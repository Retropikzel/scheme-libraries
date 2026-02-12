(define-library
  (retropikzel mouth)
  (import (scheme base)
          (scheme write)
          (scheme file))
  (export slurp
          spit)
  (include "mouth.scm"))
