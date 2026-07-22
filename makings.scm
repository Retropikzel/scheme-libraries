(let ((init
        '("apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
          "git clone https://github.com/ashinn/chibi-scheme.git --depth=1"
          "make -j8 -C chibi-scheme"
          "make -j8 -C chibi-scheme install"
          "snow-chibi install retropikzel.compile-r7rs"))
      (tap
        (lambda (scheme)
          (list
            (string-append "COMPILE_R7RS="
                           scheme
                           " compile-r7rs -o tap-test-program retropikzel/tap/test.scm")
            "./tap-test-program"))))

  (map (lambda (scheme)
         `(makings ((name ,scheme)
                     (image ,(string-append "schemers/" scheme ":head"))
                     (stages
                       (init ,@init)
                       (tap ,@(tap scheme))))))
       '("chibi" "sagittarius")))
