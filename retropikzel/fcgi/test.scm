(define count 1)

(display "Starting fcgi server")
(newline)

(handle-request
  '((port . "3002"))
  (lambda (request headers parameters cookies body files)
    (display "Content-type: text/html")
    (display "\r\n")
    (display "\r\n")
    (display "Hello")
    (display "Count: ")
    (display count)
    (set! count (+ count 1))))
