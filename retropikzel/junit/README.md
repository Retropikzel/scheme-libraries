JUnit, output for SRFI-64

[JUnit](https://junit.org/)



## Usage

    (import (scheme base)
            (srfi 64)
            (retropikzel junit))

    (test-runner-current (junit-runner))



## Jenkins usage tip

Use JUnit test runner in Jenkins and output to file, use TAP anywhere else and
output to stdout.

(cond
  ;; In Jenkins
  ((get-environment-variable "JENKINS_URL")
   (let ((junit-file "junit-result.xml"))
     (test-runner-current (junit-runner))
     (when (file-exists? junit-file) (delete-file junit-file))
     (set-junit-runner-output-port! (open-output-file junit-file))))
  (else (test-runner-current (tap-runner))))



## Reference

(**junit-runner**)

Get the JUnit test runner.

(**set-junit-runner-output-port!** port)

Set the output port of the test runner. To output to file you can do:

    (define junit-file "junit-result.xml")
    (when (file-exists? junit-file) (delete-file junit-file))
    (set-junit-runner-output-port! (open-output-file junit-file))

