(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme char)
        (scheme file)
        (scheme process-context)
        (retropikzel tap)
        (retropikzel junit)
        (retropikzel LIBRARY)
        (srfi 64))

(cond
  ;; In Jenkins
  ((get-environment-variable "WORKSPACE")
   (let ((junit-file "junit-result.xml"))
     (test-runner-current (junit-runner))
     (when (file-exists? junit-file) (delete-file junit-file))
     (set-junit-runner-output-port! (open-output-file junit-file))))
  (else (test-runner-current (tap-runner))))
