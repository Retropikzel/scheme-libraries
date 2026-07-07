
(test-begin "hardware-info")


(define cpus (cpu-count))

(test-assert (number? cpus))

(write cpus)
(newline)

(test-end "hardware-info")
