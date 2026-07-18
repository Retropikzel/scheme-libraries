TAP, the Test Anything Protocol, is a simple text-based interface between testing modules in a test harness.

[Test Anything Protocol](https://testanything.org/)

Usage:


    (import (scheme base)
            (srfi 64)
            (retropikzel tap))

    (test-runner-current (tap-runner))
