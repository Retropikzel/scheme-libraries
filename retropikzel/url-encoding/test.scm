(test-begin "url-encoding")

(test-assert "url-encode-1"
             (string=? (encode-url "https://retropikzel.neocities.org/blog/2025-12-24 - Making a Scheme script on windows.html")
                       "https://retropikzel.neocities.org/blog/2025-12-24%20-%20Making%20a%20Scheme%20script%20on%20windows.html"))

(write (encode-url "https://retropikzel.neocities.org/blog/2025-12-24 - Making a Scheme script on windows.html"))
(newline)

(define long-text (slurp "retropikzel/url-encoding/long-test-string.txt"))

(test-assert "url-encode long-text" (string? (encode-url long-text)))

(define long-text1 (slurp "retropikzel/url-encoding/long-test-string1.txt"))
(test-assert "url-encode long-text1" (string? (encode-url long-text1)))

(test-end "url-encoding")
