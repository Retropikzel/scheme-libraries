(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (retropikzel concat)
        (srfi 64))

(test-begin "concat")

(test-equal "1 2 3 foo bar (1 2 3 4)"
            (concat " " 1 2 3 "foo" 'bar '(1 2 3 4)))

(test-end "concat")

