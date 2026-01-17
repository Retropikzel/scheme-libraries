(define FCGI-BEGIN-REQUEST 1)
(define FCGI-ABORT-REQUEST 2)
(define FCGI-END-REQUEST 7)
(define FCGI-PARAMS 4)
(define FCGI-STDIN 5)
(define FCGI-STDOUT 6)
(define FCGI-STDERR 7)
(define FCGI-DATA 8)
(define FCGI-GET-VALUES 9)
(define FCGI-GET-VALUES-RESULT 10)
(define FCGI-UNKNOWN-TYPE 11)

(define (fcgi-type->symbol type)
  (cond ((= type FCGI-BEGIN-REQUEST) 'FCGI-BEGIN-REQUEST)
        ((= type FCGI-ABORT-REQUEST) 'FCGI-ABORT-REQUEST)
        ((= type FCGI-END-REQUEST) 'FCGI-END-REQUEST)
        ((= type FCGI-PARAMS) 'FCGI-PARAMS)
        ((= type FCGI-STDIN) 'FCGI-STDIN)
        ((= type FCGI-STDOUT) 'FCGI-STDOUT)
        ((= type FCGI-STDERR) 'FCGI-STDERR)
        ((= type FCGI-DATA) 'FCGI-DATA)
        ((= type FCGI-GET-VALUES) 'FCGI-GET-VALUES)
        ((= type FCGI-GET-VALUES-RESULT) 'FCGI-GET-VALUES-RESULT)
        ((= type FCGI-UNKNOWN-TYPE) 'FCGI-UNKNOWN-TYPE)))

(define FCGI-KEEP-CONN 1)

(define FCGI-RESPONDER 1)
(define FCGI-AUTHORIZER 2)
(define FCGI-FILTER 3)

(define (fcgi-role->symbol role)
  (cond ((= role FCGI-RESPONDER) 'FCGI-RESPONDER)
        ((= role FCGI-AUTHORIZER) 'FCGI-AUTHORIZER)
        ((= role FCGI-FILTER) 'FCGI-FILTER)))

(define (b1+b0 b1 b0)
  ;; https://web.archive.org/web/20160119141816/http://www.fastcgi.com/drupal/node/6?q=node%2F22
  ;; FROM SPECIFICATION:
  ;; When two adjacent structure components are named identically except for
  ;; the suffixes "B1" and "B0," it means that the two components may be viewed
  ;; as a single number, computed as B1<<8 + B0
  ;; BUT IN CODE THEY DO: (B1<<8) + B0
  (+ (arithmetic-shift b1 8) b0))

(define (integer->b1-b0 int)
  (let ((b1 (bitwise-and (arithmetic-shift int -8) 255))
        (b0 (bitwise-and int 255)))
    `((b1 . ,b1) (b0 . ,b0))))

(define (parse-request-content type content)
  (cond
    ((symbol=? type 'FCGI-BEGIN-REQUEST)
     (let ((role-b1 (bytevector-u8-ref content 0))
           (role-b0 (bytevector-u8-ref content 1))
           (flags (bytevector-u8-ref content 2)))
       `((role . ,(b1+b0 role-b1 role-b0))
         (flags . ,flags))))
    ((symbol=? type 'FCGI-PARAMS)
     ;; https://web.archive.org/web/20160119141816/http://www.fastcgi.com/drupal/node/6?q=node%2F22
     ;; 3.4 Name-Value Pairs
     (letrec*
       ((>>7 (lambda (n) (exact (floor (* n (expt 2 -7))))))
        (read-length
          (lambda (bv start-index)
            (let*
              ((b0 (bytevector-u8-ref bv (+ start-index 0)))
               (b0>>7 (>>7 b0))
               (b3 (if (= b0>>7 0) 0 b0))
               (b2 (if (= b0>>7 0) 0 (bytevector-u8-ref bv (+ start-index 1))))
               (b1 (if (= b0>>7 0) 0 (bytevector-u8-ref bv (+ start-index 2))))
               ;; Notice redefinition of b0
               (b0 (if (= b0>>7 0) b0 (bytevector-u8-ref bv (+ start-index 3))))
               (bytes-in-length (if (= b0>>7 0) 1 4)))
              (cons bytes-in-length
                    (if (= b0>>7 0)
                      b0
                      ;; ((B3 & 0x7f) << 24) + (B2 << 16) + (B1 << 8) + B0
                      (+ (arithmetic-shift (bitwise-and b3 127) 24)
                         (arithmetic-shift b2 16)
                         (arithmetic-shift b1 8)
                         b0))))))
        (content-length (bytevector-length content))
        (looper
          (lambda (start-index result)
            (if (>= start-index content-length)
              result
              (let*
                ((name-length (read-length content start-index))
                 (value-length
                   (read-length content (+ start-index (car name-length))))
                 (lengths-length (+ (car name-length) (car value-length)))
                 (name (string->symbol
                         (utf8->string
                           (bytevector-copy content
                                            (+ start-index lengths-length)
                                            (+ start-index
                                               lengths-length
                                               (cdr name-length))))))
                 (value (utf8->string
                          (bytevector-copy content
                                           (+ start-index
                                              lengths-length
                                              (cdr name-length))
                                           (+ start-index
                                              lengths-length
                                              (cdr name-length)
                                              (cdr value-length))))))
                (looper (+ start-index
                           lengths-length
                           (cdr name-length)
                           (cdr value-length))
                        (append result (list (cons name value)))))))))
       (if (= content-length 0)
         (bytevector)
         (looper 0 '()))))
    ((symbol=? type 'FCGI-STDIN)
     (utf8->string content))
    (else content)))

(define (read-request socket)
  (let* ((fields (socket-recv socket 8))
         (version (bytevector-u8-ref fields 0))
         (type (bytevector-u8-ref fields 1))
         (type-symbol (fcgi-type->symbol type))
         (request-id-b1 (bytevector-u8-ref fields 2))
         (request-id-b0 (bytevector-u8-ref fields 3))
         (request-id (b1+b0 request-id-b1 request-id-b0))
         (content-length-b1 (bytevector-u8-ref fields 4))
         (content-length-b0 (bytevector-u8-ref fields 5))
         (content-length (b1+b0 content-length-b1 content-length-b0))
         (padding-length (bytevector-u8-ref fields 6))
         (reserved (bytevector-u8-ref fields 7))
         (content-data (if (> content-length 0)
                         (socket-recv socket content-length)
                         (bytevector)))
         (padding-data (if (> padding-length 0)
                         (socket-recv socket padding-length)
                         (bytevector))))
    (when (not (= version 1)) (error "Unsupported fastcgi version" version))
    `(,type-symbol
       (id . ,request-id)
       (content . ,(parse-request-content type-symbol content-data)))))

(define (read-requests socket result)
  (let ((request (read-request socket)))
    (if (symbol=? (car request) 'FCGI-STDIN)
      (reverse (cons request result))
      (read-requests socket (cons request result)))))

(define (write-response socket type id response-bytes)
  (let* ((version 1)
         (header-bytes
           (let* ((bytes (make-bytevector 8 0))
                  (id-b1-b0 (integer->b1-b0 id))
                  (content-length (bytevector-length response-bytes))
                  (content-length-b1-b0 (integer->b1-b0 content-length))
                  (padding-length 0)
                  (reserved 0))
             (bytevector-u8-set! bytes 0 version)
             (bytevector-u8-set! bytes 1 type)
             (bytevector-u8-set! bytes 2 (cdr (assoc 'b1 id-b1-b0)))
             (bytevector-u8-set! bytes 3 (cdr (assoc 'b0 id-b1-b0)))
             (bytevector-u8-set! bytes 4 (cdr (assoc 'b1 content-length-b1-b0)))
             (bytevector-u8-set! bytes 5 (cdr (assoc 'b0 content-length-b1-b0)))
             (bytevector-u8-set! bytes 6 padding-length)
             (bytevector-u8-set! bytes 7 reserved)
             bytes))
         (response (bytevector-append header-bytes response-bytes)))
    (socket-send socket response)))

(define (check-role role)
  (if (= role FCGI-RESPONDER)
    role
    (error "Unsupported fastcgi role" (fcgi-role->symbol role))))

(define fcgi-internal-handle
  (lambda (client-socket thunk)
    (let*
      ((requests (read-requests client-socket '()))
       (begin-request (cdr (assoc 'FCGI-BEGIN-REQUEST requests)))
       (begin-request-content (cdr (assoc 'content begin-request)))
       (id (cdr (assoc 'id begin-request)))
       (role (check-role (cdr (assoc 'role begin-request-content))))
       (flags (cdr (assoc 'flags begin-request-content)))
       (params-request (cdr (assoc 'FCGI-PARAMS requests)))
       (headers (cdr (assoc 'content params-request)))
       (content-length (string->number (cdr (assoc 'CONTENT_LENGTH headers))))
       (query-string (cdr (assoc 'QUERY_STRING headers)))
       (parameters '()) ;; TODO
       (stdin-request (cdr (assoc 'FCGI-STDIN requests)))
       (body (cdr (assoc 'content stdin-request)))
       (files '()) ;; TODO
       (request '())
       (response (parameterize
                   ((current-output-port (open-output-string)))
                   (apply thunk
                          (list request
                                headers
                                parameters
                                '() ;; TODO Cookies
                                body
                                files))
                   (get-output-string (current-output-port)))))
      (write-response client-socket FCGI-STDOUT id (string->utf8 response))
      (write-response client-socket FCGI-STDERR id (make-bytevector 0))
      (write-response client-socket FCGI-END-REQUEST id (make-bytevector 0))
      (socket-close client-socket))))

(define fcgi-listen
  (lambda (socket thunk)
    (fcgi-internal-handle (socket-accept socket) thunk)
    (fcgi-listen socket thunk)))

(define (handle-request options thunk)
  (let ((port (assoc 'port options)))
    (when (not port)
      (error "handle-request (fcgi) requires port to be passed in options, example: '((port . \"3000\"))"))
    (fcgi-listen (make-server-socket (cdr port) *af-inet* *sock-stream* *ipproto-ip*) thunk)))
