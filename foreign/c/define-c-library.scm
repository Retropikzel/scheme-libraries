(define-syntax define-c-library
  (syntax-rules ()
    ((_ scheme-name headers object-name options)
     (define scheme-name
       (let* ((os (cond-expand (windows 'windows) (guile 'unix) (else 'unix)))
              (arch (cond-expand (i386 'i386) (guile 'x86_64) (else 'x86_64)))
              (string-split
                (lambda (str mark)
                  (let* ((str-l (string->list str))
                         (res (list))
                         (last-index 0)
                         (index 0)
                         (splitter (lambda (c)
                                     (cond ((char=? c mark)
                                            (begin
                                              (set! res (append res (list (substring str last-index index))))
                                              (set! last-index (+ index 1))))
                                           ((equal? (length str-l) (+ index 1))
                                            (set! res (append res (list (substring str last-index (+ index 1)))))))
                                     (set! index (+ index 1)))))
                    (for-each splitter str-l)
                    res)))
              (internal-options (if (null? 'options)
                                  (list)
                                  (cadr 'options)))
              (additional-paths (if (assoc 'additional-paths internal-options)
                                  (cadr (assoc 'additional-paths internal-options))
                                  (list)))
              (additional-versions (if (assoc 'additional-versions internal-options)
                                     (map (lambda (version)
                                            (if (number? version)
                                              (number->string version)
                                              version))
                                          (cadr (assoc 'additional-versions internal-options)))
                                     (list)))
              (slash (if (symbol=? os 'windows) "\\" "/"))
              (auto-load-paths
                (if (symbol=? os 'windows)
                  (append
                    (if (get-environment-variable "FOREIGN_C_LOAD_PATH")
                      (string-split (get-environment-variable "FOREIGN_C_LOAD_PATH") (string-ref ";" 0))
                      (list))
                    (if (get-environment-variable "SYSTEM")
                      (list (get-environment-variable "SYSTEM"))
                      (list))
                    (if (get-environment-variable "WINDIR")
                      (list (get-environment-variable "WINDIR"))
                      (list))
                    (if (get-environment-variable "WINEDLLDIR0")
                      (list (get-environment-variable "WINEDLLDIR0"))
                      (list))
                    (if (get-environment-variable "SystemRoot")
                      (list (string-append
                              (get-environment-variable "SystemRoot")
                              slash
                              "system32"))
                      (list))
                    (list ".")
                    (if (get-environment-variable "PATH")
                      (string-split (get-environment-variable "PATH") (string-ref ";" 0))
                      (list))
                    (if (get-environment-variable "PWD")
                      (list (get-environment-variable "PWD"))
                      (list)))
                  (append
                    (if (get-environment-variable "FOREIGN_C_LOAD_PATH")
                      (string-split (get-environment-variable "FOREIGN_C_LOAD_PATH") (string-ref ":" 0))
                      (list))
                    ; Guix
                    (list (if (get-environment-variable "GUIX_ENVIRONMENT")
                            (string-append (get-environment-variable "GUIX_ENVIRONMENT") slash "lib")
                            "")
                          "/run/current-system/profile/lib")
                    ; Debian
                    (if (get-environment-variable "LD_LIBRARY_PATH")
                      (string-split (get-environment-variable "LD_LIBRARY_PATH") (string-ref ":" 0))
                      (list))
                    (if (symbol=? arch 'i386)
                      (list
                        "/lib/i386-linux-gnu"
                        "/usr/lib/i386-linux-gnu"
                        "/lib32"
                        "/usr/lib32")
                      (list
                        ;;; x86-64
                        ; Debian
                        "/lib/x86_64-linux-gnu"
                        "/usr/lib/x86_64-linux-gnu"
                        "/usr/local/lib"
                        ; Fedora/Alpine
                        "/usr/lib"
                        "/usr/lib64"
                        ;;; aarch64
                        ; Debian
                        "/lib/aarch64-linux-gnu"
                        "/usr/lib/aarch64-linux-gnu"
                        "/usr/local/lib"
                        ; Fedora/Alpine
                        "/usr/lib"
                        "/usr/lib64"
                        ; NetBSD
                        "/usr/pkg/lib"
                        ; Haiku
                        "/boot/system/lib")))))
              (auto-load-versions (list ""))
              (paths (append auto-load-paths additional-paths))
              (versions (append additional-versions auto-load-versions))
              (platform-lib-prefix (if (symbol=? os 'windows) "" "lib"))
              (platform-file-extension (if (symbol=? os 'windows) ".dll" ".so"))
              (shared-object #f)
              (searched-paths (list)))
         (for-each
           (lambda (path)
             (for-each
               (lambda (version)
                 (let ((library-path
                         (string-append path
                                        slash
                                        platform-lib-prefix
                                        object-name
                                        (if (symbol=? os 'windows)
                                          ""
                                          platform-file-extension)
                                        (if (string=? version "")
                                          ""
                                          (string-append
                                            (if (symbol=? os 'windows)
                                              "-"
                                              ".")
                                            version))
                                        (if (symbol=? os 'windows)
                                          platform-file-extension
                                          "")))
                       (library-path-without-suffixes (string-append path
                                                                     slash
                                                                     platform-lib-prefix
                                                                     object-name)))
                   (set! searched-paths (append searched-paths (list library-path)))
                   (when (and (not shared-object)
                              (file-exists? library-path))
                     (set! shared-object
                       (cond-expand
                         (gauche library-path-without-suffixes)
                         (racket library-path-without-suffixes)
                         (guile library-path)
                         (else library-path))))))
               versions))
           paths)
         (if (not shared-object)
           (error "Could not load shared object: "
                  (list (cons 'object object-name)
                        (cons 'searched-paths searched-paths)
                        (cons 'platform-file-extension platform-file-extension)
                        (cons 'versions versions)))
           (cond-expand
             (stklos shared-object)
             (guile (shared-object-load shared-object
                                        `((additional-versions ,additional-versions))))
             (else (shared-object-load shared-object
                                       `((additional-versions ,additional-versions)))))))))))
