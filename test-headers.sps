#!r6rs
(import (rnrs)
        (srfi :64)
        (srfi :98)
        (retropikzel mouth)
        (retropikzel ctrf)
        (retropikzel LIBRARY))

(test-runner-current (ctrf-runner))
