JUnit, output for SRFI-64

[JUnit](https://junit.org/)



## Usage

    (import (scheme base)
            (srfi 64)
            (retropikzel junit))

    (test-runner-current (junit-runner))



## Reference

(**junit-runner**)

Get the JUnit test runner.

(**set-junit-runner-output-port!** port)

Set the output port of the test runner. To output to file you can do:

    (define junit-file "junit-result.xml")
    (when (file-exists? junit-file) (delete-file junit-file))
    (set-junit-runner-output-port! (open-output-file junit-file))

