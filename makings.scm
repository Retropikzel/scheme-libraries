(let ((init
        '("apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
          "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
          "make -j8 -C chibi-scheme"
          "make -j8 -C chibi-scheme install"
          "snow-chibi install retropikzel.compile-r7rs"))
      (library-stage
        (lambda (scheme library-name)
          (list (string->symbol library-name)
                (string-append "make SCHEME="
                               scheme
                               " LIBRARY="
                               library-name
                               " all install test")))))

  `((jenkinsfile
      (agent "label 'docker-x86_64'")
      (options "disableConcurrentBuilds()"
               "buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))"))
    (makings ,@(map (lambda (scheme)
                      `((name ,scheme)
                        (image ,(string-append "schemers/" scheme ":head"))
                        (stages
                          (init ,@init)
                          ,@(map
                              (lambda (library-name)
                                (library-stage scheme library-name))
                              '("tap" "debug")))))
                    '("chibi" "sagittarius")))))
