(test-begin "leb128")

;; Cant use #u8(...) or #vu8(...) because tests need to work on both R6RS and R7RS
(define bv-42 (let ((bv (make-bytevector 1))) (bytevector-u8-set! bv 0 #x2A) bv))
(define bv-minus-42 (let ((bv (make-bytevector 1))) (bytevector-u8-set! bv 0 #x56) bv))
(define bv-123456
  (let ((bv (make-bytevector 3)))
    (bytevector-u8-set! bv 0 #xC0)
    (bytevector-u8-set! bv 1 #xC4)
    (bytevector-u8-set! bv 2 #x07)
    bv))
(define bv-minus-123456
  (let ((bv (make-bytevector 3)))
    (bytevector-u8-set! bv 0 #xC0)
    (bytevector-u8-set! bv 1 #xBB)
    (bytevector-u8-set! bv 2 #x78)
    bv))

(test-begin "integer->leb128")
(test-equal bv-42 (integer->leb128 42))
(test-equal bv-minus-42 (integer->leb128 -42))
(test-equal bv-123456 (integer->leb128 123456))
(test-equal bv-minus-123456 (integer->leb128 -123456))
(test-end "integer->leb128")

(test-begin "leb128->integer")
(test-equal 42 (leb128->integer (integer->leb128 42)))
(test-equal 123456 (leb128->integer (integer->leb128 123456)))
(test-equal -42 (leb128->integer (integer->leb128 -42)))
(test-equal -123456 (leb128->integer (integer->leb128 -123456)))
(test-end "leb128->integer")

(test-begin "leb128->integer-and-length")
(test-equal 123456 (car (leb128->integer-and-length bv-123456)))
(test-equal 3 (cdr (leb128->integer-and-length bv-123456)))
(test-equal -123456 (car (leb128->integer-and-length bv-minus-123456)))
(test-equal 3 (cdr (leb128->integer-and-length bv-minus-123456)))
(test-end "leb128->integer-and-length")

(test-begin "integer->uleb128")
(test-equal bv-42 (integer->uleb128 42))
(test-equal bv-123456 (integer->uleb128 123456))
(test-end "integer->uleb128")

(test-begin "uleb128->integer")
(test-equal 42 (uleb128->integer (integer->uleb128 42)))
(test-equal 123456 (uleb128->integer (integer->uleb128 123456)))
(test-end "uleb128->integer")

(test-begin "uleb128->integer-and-length")
(test-equal 123456 (car (uleb128->integer-and-length bv-123456)))
(test-equal 3 (cdr (uleb128->integer-and-length bv-123456)))
(test-end "uleb128->integer-and-length")

(test-end "leb128")
