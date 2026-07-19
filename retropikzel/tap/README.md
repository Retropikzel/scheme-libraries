TAP output for SRFI-64

[Test Anything Protocol](https://testanything.org/)



## Usage

    (import (scheme base)
            (srfi 64)
            (retropikzel tap))

    (test-runner-current (tap-runner))



## Reference

(**tap-runner**)

Get the TAP test runner.

(**set-tap-runner-output-port!** port)

Set the output port of the test runner. To output to file you can do:

    (define tap-file "tap-result.txt")
    (when (file-exists? tap-file) (delete-file tap-file))
    (set-tap-runner-output-port! (open-output-file tap-file))
