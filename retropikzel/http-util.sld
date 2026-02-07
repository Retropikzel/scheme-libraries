(define-library
  (retropikzel http-util)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme char)
          (scheme file)
          (srfi 106))
  (export http-util-headers->string
          http-util-header-line->pair
          http-util-status-line->list
          http-util-request-build
          http-util-request-make
          http-util-response-build
          http-util-download-file
          http-util-read-http-request
          http-util-read-http-response
          http-util-parameters-split
          http-util-parameter-get)
  (include "http-util.scm"))
