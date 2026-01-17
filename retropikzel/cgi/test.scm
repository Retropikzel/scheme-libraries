(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (retropikzel cgi)
        (srfi 64))

(handle-request
  '()
  (lambda (request headers parameters cookies body files)
    (display "Content-type: text/html")
    (display "\r\n")
    (display "\r\n")
    (display "Hello")))

