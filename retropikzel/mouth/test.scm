
(test-begin "mouth")

(spit "/tmp/mouthtestfile" "Hello world")

(test-assert (string=? (slurp "/tmp/mouthtestfile") "Hello world"))

(spit "/tmp/mouthtestfile" ", and append" #t)

(test-assert (string=? (slurp "/tmp/mouthtestfile") "Hello world, and append"))

(test-end "mouth")


