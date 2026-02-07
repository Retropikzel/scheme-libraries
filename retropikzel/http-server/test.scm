(define document-root (string-append (get-environment-variable "PWD") "/retropikzel/http-server/www"))
(http-server document-root "3005")
