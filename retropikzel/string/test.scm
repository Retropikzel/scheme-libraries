(test-begin "string")

(test-equal
  "foo456bar"
  (string-replace "foo123bar" "123" "456"))

(test-equal
  "foo456bar-yes"
  (string-replace "foo123bar-no" '("123" "456") '("no" "yes")))

(define long-text (slurp "retropikzel/string/long-test-string.txt"))
(test-assert
  "string-replace long-text"
  (string? (string-replace long-text '("irure" "foobar"))))

(test-equal
  "Hello Schemer, I count 7 parenthesis"
  (string-format "Hello {name}, I count {n} parenthesis"
                 '((name "Schemer")
                   (n 7))))

;(define long-text1 (slurp "retropikzel/string/long-test-string1.txt"))
;(test-assert "string-replace long-text1" (string? (string-replace long-text1 "irure" "foobar")))

(test-equal "foo456bar-yes" (string-replace "foo123bar-no" '("123" "456") '("no" "yes")))

(test-equal "Hello" (string-capitalize "hello"))

(test-assert (string-ends-with? "hello" "lo"))

(test-assert (not (string-ends-with? "hello" "e")))

(test-equal "  hello" (string-expand-tabs "\thello" 2))
(test-equal "    hello    " (string-expand-tabs "\thello\t" 4))

(test-end "string")
