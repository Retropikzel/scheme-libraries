Test-runner for SRFI-64 that outputs
[Common Test Report Format](https://ctrf.io/).


Usage:


    (import (scheme base)
            (srfi 64)
            (retropikzel ctrf))

    (test-runner-current (ctrf-runner))


Then run tests as usual. The CTRF output will be outputted into JSON file
named as ${SCHEME}-${FIRST\_TEST\_GROUP\_NAME}.ctrf.json.

Any failing tests and summary will be printed into stdout as list of JSON
objects.

Exit code is the amount of failed tests.
