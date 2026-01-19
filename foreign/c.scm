;; (Heavily modified) Parts from (r6rs bytevectors) library begins
;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;

(define c-bytevector:single-maxexponent 255)
(define c-bytevector:single-bias (quotient c-bytevector:single-maxexponent 2))
(define c-bytevector:single-hidden-bit (expt 2 23))
(define c-bytevector:double-maxexponent 2047)
(define c-bytevector:double-bias (quotient c-bytevector:double-maxexponent 2))
(define c-bytevector:double-hidden-bit (expt 2 52))    ; must be exact integer

(define two^48 (expt 2 48))
(define two^40 (expt 2 40))
(define two^32 (expt 2 32))
(define two^24 (expt 2 24))
(define two^16 (expt 2 16))
(define two^8  (expt 2 8))

(define-syntax s8->u8
  (syntax-rules ()
    ((_ val0)
     (let ((val val0))
       (if (negative? val)
         (+ val 256)
         val)))))

(define-syntax u8->s8
  (syntax-rules ()
    ((_ octet0)
     (let ((octet octet0))
       (if (> octet 127)
         (- octet 256)
         octet)))))

(define-syntax unspecified
  (syntax-rules ()
    ((_) (if #f #f))))

(define (c-bytevector-s8-ref b k)
  (u8->s8 (c-bytevector-u8-ref b k)))

(define (c-bytevector-s8-set! b k val)
  (c-bytevector-u8-set! b k (s8->u8 val)))

;;; Given exact positive integers p and q,
;;; returns three values:
;;; exact integers exponent, p2, and q2 such that
;;;     q2 <= p2 < q2+q2
;;;     p / q = (p2 * 2^exponent) / q2

(define (c-bytevector:normalized-ieee-parts p q)
  (cond ((< p q)
         (do ((p p (+ p p))
              (e 0 (- e 1)))
           ((>= p q)
            (values e p q))))
        ((<= (+ q q) p)
         (do ((q q (+ q q))
              (e 0 (+ e 1)))
           ((< p (+ q q))
            (values e p q))))
        (else
          (values 0 p q))))

;;; Given an inexact real x, an exponent bias, and an exact positive
;;; integer q that is a power of 2 representing the integer value of
;;; the hidden bit, returns three exact integers:
;;;
;;; sign
;;; biased-exponent
;;; p
;;;
;;; If x is normalized, then 0 < biased-exponent <= bias+bias,
;;; q <= p < 2*q, and
;;;
;;;     x = (-1)^sign * (2^(biased-exponent - bias)) * p/q
;;;
;;; If x is denormalized, then p < q and the equation holds.
;;; If x is zero, then biased-exponent and p are zero.
;;; If x is infinity, then biased-exponent = bias+bias+1 and p=0.
;;; If x is a NaN, then biased-exponent = bias+bias+1 and p>0.
;;;

(define (c-bytevector:ieee-parts x bias q)
  (cond ((nan? x)
         (values 0 (+ bias bias 1) (- q 1)))
        ((infinite? x)
         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
        ((zero? x)
         (values (if (eqv? x -0.0) 1 0) 0 0))
        (else
          (let* ((sign (if (negative? x) 1 0))
                 (y (exact (abs x)))
                 (num (numerator y))
                 (den (denominator y)))
            (call-with-values
              (lambda () (c-bytevector:normalized-ieee-parts num den))
              (lambda (exponent num den)
                (let ((biased-exponent (+ exponent bias)))
                  (cond ((< 0 biased-exponent (+ bias bias 1))
                         ; within the range of normalized numbers
                         (if (<= den q)
                           (let* ((factor (/ q den))
                                  (num*factor (* num factor)))
                             (if (integer? factor)
                               (values sign biased-exponent num*factor)
                               (error 'c-bytevector:ieee-parts
                                      "this shouldn't happen: " x bias q)))
                           (let* ((factor (/ den q))
                                  (num*factor (/ num factor)))
                             (values sign
                                     biased-exponent
                                     (round num*factor)))))
                        ((>= biased-exponent (+ bias bias 1))
                         ; infinity
                         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
                        (else
                          ; denormalized
                          ; FIXME: this has the double rounding bug
                          (do ((biased biased-exponent (+ biased 1))
                               (num (round (/ (* q num) den))
                                    (round (quotient num 2))))
                            ((and (< num q) (= biased 1))
                             (values sign biased num))))))))))))

(define (c-bytevector-uint-set! c-bytevector index val size)
  (cond
    ((symbol=? (native-endianness) 'little)
     (do ((i 0 (+ i 1))
          (val val (quotient val 256)))
       ((>= i size)
        (unspecified))
       (c-bytevector-u8-set! c-bytevector (+ index i) (quotient val 256))))
    ((symbol=? (native-endianness) 'big)
     (do ((i (- size 1) (- i 1))
          (val val (quotient val 256)))
       ((< i 0)
        (unspecified))
       (c-bytevector-u8-set! c-bytevector (+ index i) (remainder val 256))))
    (else
      (c-bytevector-uint-set! c-bytevector index val size))))

(define (c-bytevector-uint-ref c-bytevector index size)
  (cond ((equal? (native-endianness) 'big)
         (do ((i 0 (+ i 1))
              (result 0 (+ (* 256 result)
                           (c-bytevector-u8-ref c-bytevector (+ index i)))))
           ((>= i size)
            result)))
        ((equal? (native-endianness) 'little)
         (do ((i (- size 1) (- i 1))
              (result 0 (+ (* 256 result)
                           (c-bytevector-u8-ref c-bytevector (+ index i)))))
           ((< i 0)
            result)))
        (else
          (c-bytevector-uint-ref c-bytevector index size))))

(define (c-bytevector-sint-set! c-bytevector index val size)
  (let ((uval (if (< val 0)
                (+ val (expt 256 size))
                val)))
    (c-bytevector-uint-set! c-bytevector index uval size)))

(define (c-bytevector-sint-ref c-bytevector index size)
  (let* ((high-byte (c-bytevector-u8-ref c-bytevector
                                         (if (eq? (native-endianness) 'big)
                                           index
                                           (+ index size -1))))
         (uresult (c-bytevector-uint-ref c-bytevector index size)))
    (if (> high-byte 127)
      (- uresult (expt 256 size))
      uresult)))

;;; Given
;;;
;;;     the sign bit
;;;     biased exponent
;;;     integer value of the 23-bit mantissa without the hidden bit
;;;
;;; returns an inexact real approximating the IEEE single precision
;;; number with the given representation.  If an implementation
;;; implements inexact reals using IEEE single or double precision,
;;; and implements IEEE-754 arithmetic correctly, and the arguments
;;; do not imply a NaN, then the inexact real that's returned
;;; should be exactly right.

(define (make-ieee-single sign biased-exponent bits)
  (cond ((= biased-exponent c-bytevector:single-maxexponent)
         (if (zero? bits)
           (if (= 0 sign)
             +inf.0
             -inf.0)
           (if (= 0 sign)
             +nan.0
             -nan.0)))
        ((= 0 biased-exponent)
         (if (= 0 bits)
           (if (= 0 sign)
             +0.0
             -0.0)
           (let* ((x (inexact bits))
                  (two^22 4194304.0)
                  (x (/ x two^22))
                  (x (* x (expt 2.0 (- c-bytevector:single-bias)))))
             (if (= 0 sign)
               x
               (- x)))))
        (else
          (let* ((bits (+ #x800000   ; hidden bit
                          bits))
                 (x (inexact bits))
                 (two^23 8388608.0)
                 (x (/ x two^23))
                 (x (* x (expt 2.0
                               (- biased-exponent c-bytevector:single-bias)))))
            (if (= 0 sign)
              x
              (- x))))))

(define (c-bytevector-ieee-single-big-endian-ref c-bytevector k)
  (let* ((byte0 (c-bytevector-u8-ref c-bytevector (+ k 0)))
         (byte1 (c-bytevector-u8-ref c-bytevector (+ k 1)))
         (byte2 (c-bytevector-u8-ref c-bytevector (+ k 2)))
         (byte3 (c-bytevector-u8-ref c-bytevector (+ k 3)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 2 (remainder byte0 128))
                             (quotient byte1 128)))
         (bits (+ (* 65536 (remainder byte1 128))
                  (* 256 byte2)
                  byte3)))
    (make-ieee-single sign biased-exponent bits)))

(define (c-bytevector-ieee-single-little-endian-ref c-bytevector k)
  (let* ((byte0 (c-bytevector-u8-ref c-bytevector (+ k 3)))
         (byte1 (c-bytevector-u8-ref c-bytevector (+ k 2)))
         (byte2 (c-bytevector-u8-ref c-bytevector (+ k 1)))
         (byte3 (c-bytevector-u8-ref c-bytevector (+ k 0)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 2 (remainder byte0 128))
                             (quotient byte1 128)))
         (bits (+ (* 65536 (remainder byte1 128))
                  (* 256 byte2)
                  byte3)))
    (make-ieee-single sign biased-exponent bits)))

(define (c-bytevector-ieee-single-set! c-bytevector k x endianness)
  (call-with-values
    (lambda ()
      (c-bytevector:ieee-parts x
                               c-bytevector:single-bias
                               c-bytevector:single-hidden-bit))
    (lambda (sign biased-exponent frac)
      (define (store! sign biased-exponent frac)
        (if (eq? 'big endianness)
          (begin
            (c-bytevector-u8-set! c-bytevector k
                                  (+ (* 128 sign)
                                     (quotient biased-exponent 2)))
            (c-bytevector-u8-set! c-bytevector (+ k 1)
                                  (+ (* 128 (remainder biased-exponent 2))
                                     (quotient frac (* 256 256))))
            (c-bytevector-u8-set! c-bytevector (+ k 2)
                                  (quotient
                                    (remainder frac (* 256 256)) 256))
            (c-bytevector-u8-set! c-bytevector (+ k 3)
                                  (remainder frac 256)))
          (begin
            (c-bytevector-u8-set! c-bytevector (+ k 3)
                                  (+ (* 128 sign)
                                     (quotient biased-exponent 2)))
            (c-bytevector-u8-set! c-bytevector (+ k 2)
                                  (+ (* 128 (remainder biased-exponent 2))
                                     (quotient frac (* 256 256))))
            (c-bytevector-u8-set! c-bytevector (+ k 1)
                                  (quotient
                                    (remainder frac (* 256 256)) 256))
            (c-bytevector-u8-set! c-bytevector k
                                  (remainder frac 256))))
        (unspecified))
      (cond ((= biased-exponent c-bytevector:single-maxexponent)
             (store! sign biased-exponent frac))
            ((< frac c-bytevector:single-hidden-bit)
             (store! sign 0 frac))
            (else
              (store! sign biased-exponent
                      (- frac c-bytevector:single-hidden-bit)))))))

(define (c-bytevector-ieee-single-native-set! c-bytevector k x)
  (cond
    ((equal? (native-endianness) 'little)
     (if (not (= 0 (remainder k 4)))
       (error "c-bytevector-ieee-single-native-set!" (list c-bytevector k x)))
     (c-bytevector-ieee-single-set! c-bytevector k x 'little))
    (else
      (if (not (= 0 (remainder k 4)))
        (error "c-bytevector-ieee-single-native-set!" (list c-bytevector k x)))
      (c-bytevector-ieee-single-set! c-bytevector k x 'big))))

(define (c-bytevector-ieee-single-native-ref c-bytevector k)
  (cond
    ((equal? (native-endianness) 'little)
     (if (not (= 0 (remainder k 4)))
       (error "c-bytevector-ieee-single-native-ref" (list c-bytevector k)))
     (c-bytevector-ieee-single-little-endian-ref c-bytevector k))
    (else
      (if (not (= 0 (remainder k 4)))
        (error "c-bytevector-ieee-single-native-ref" (list c-bytevector k)))
      (c-bytevector-ieee-single-big-endian-ref c-bytevector k))))


;;; Given
;;;
;;;     the sign bit
;;;     biased exponent
;;;     integer value of the 20 high order bits without the hidden bit
;;;     integer value of the 16 mid-order bits
;;;     integer value of the 16 low-order bits
;;;
;;; returns an inexact real approximating the IEEE double precision
;;; number with the given representation.  If an implementation
;;; implements inexact reals using IEEE double precision, and
;;; implements IEEE-754 arithmetic correctly, and the arguments
;;; do not imply a NaN, then the inexact real that's returned
;;; should be exactly right.

(define (make-ieee-double sign biased-exponent hibits midbits lobits)
  (cond ((= biased-exponent c-bytevector:double-maxexponent)
         (if (zero? (+ hibits midbits lobits))
           (if (= 0 sign)
             +inf.0
             -inf.0)
           (if (= 0 sign)
             +nan.0
             -nan.0)))
        ((= 0 biased-exponent)
         (if (and (= 0 hibits)
                  (= 0 midbits)
                  (= 0 lobits))
           (if (= 0 sign)
             +0.0
             -0.0)
           (let* ((x (inexact hibits))
                  (x (+ (* 65536.0 x)
                        (inexact midbits)))
                  (x (+ (* 65536.0 x)
                        (inexact lobits)))
                  (two^51 2.251799813685248e15)
                  (x (/ x two^51))
                  (x (* x (expt 2.0 (- c-bytevector:double-bias)))))
             (if (= 0 sign)
               x
               (- x)))))
        (else
          (let* ((hibits (+ #x100000    ; hidden bit
                            hibits))
                 (x (inexact hibits))
                 (x (+ (* 65536.0 x)
                       (inexact midbits)))
                 (x (+ (* 65536.0 x)
                       (inexact lobits)))
                 (two^52 4.503599627370496e15)
                 (x (/ x two^52))
                 (x (* x (expt 2.0
                               (- biased-exponent c-bytevector:double-bias)))))
            (if (= 0 sign)
              x
              (- x))))))

(define (c-bytevector-ieee-double-set! c-bytevector k x endianness)
  (call-with-values
    (lambda ()
      (c-bytevector:ieee-parts x
                               c-bytevector:double-bias
                               c-bytevector:double-hidden-bit))
    (lambda (sign biased-exponent frac)

      (define (store! sign biased-exponent frac)
        (c-bytevector-u8-set! c-bytevector (+ k 7)
                              (+ (* 128 sign)
                                 (quotient biased-exponent 16)))
        (c-bytevector-u8-set! c-bytevector (+ k 6)
                              (+ (* 16 (remainder biased-exponent 16))
                                 (quotient frac two^48)))
        (c-bytevector-u8-set! c-bytevector (+ k 5)
                              (quotient (remainder frac two^48) two^40))
        (c-bytevector-u8-set! c-bytevector (+ k 4)
                              (quotient (remainder frac two^40) two^32))
        (c-bytevector-u8-set! c-bytevector (+ k 3)
                              (quotient (remainder frac two^32) two^24))
        (c-bytevector-u8-set! c-bytevector (+ k 2)
                              (quotient (remainder frac two^24) two^16))
        (c-bytevector-u8-set! c-bytevector (+ k 1)
                              (quotient (remainder frac two^16) 256))
        (c-bytevector-u8-set! c-bytevector k (remainder frac 256))
        (if (not (eq? endianness 'little))
          (begin (swap! (+ k 0) (+ k 7))
                 (swap! (+ k 1) (+ k 6))
                 (swap! (+ k 2) (+ k 5))
                 (swap! (+ k 3) (+ k 4))))
        (unspecified))

      (define (swap! i j)
        (let ((bi (c-bytevector-u8-ref c-bytevector i))
              (bj (c-bytevector-u8-ref c-bytevector j)))
          (c-bytevector-u8-set! c-bytevector i bj)
          (c-bytevector-u8-set! c-bytevector j bi)))

      (cond ((= biased-exponent c-bytevector:double-maxexponent)
             (store! sign biased-exponent frac))
            ((< frac c-bytevector:double-hidden-bit)
             (store! sign 0 frac))
            (else
              (store! sign biased-exponent
                      (- frac c-bytevector:double-hidden-bit)))))))

(define (c-bytevector-ieee-double-big-endian-ref c-bytevector k)
  (let* ((byte0 (c-bytevector-u8-ref c-bytevector (+ k 0)))
         (byte1 (c-bytevector-u8-ref c-bytevector (+ k 1)))
         (byte2 (c-bytevector-u8-ref c-bytevector (+ k 2)))
         (byte3 (c-bytevector-u8-ref c-bytevector (+ k 3)))
         (byte4 (c-bytevector-u8-ref c-bytevector (+ k 4)))
         (byte5 (c-bytevector-u8-ref c-bytevector (+ k 5)))
         (byte6 (c-bytevector-u8-ref c-bytevector (+ k 6)))
         (byte7 (c-bytevector-u8-ref c-bytevector (+ k 7)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 16 (remainder byte0 128))
                             (quotient byte1 16)))
         (hibits (+ (* 65536 (remainder byte1 16))
                    (* 256 byte2)
                    byte3))
         (midbits (+ (* 256 byte4) byte5))
         (lobits (+ (* 256 byte6) byte7)))
    (make-ieee-double sign biased-exponent hibits midbits lobits)))

(define (c-bytevector-ieee-double-little-endian-ref c-bytevector k)
  (let* ((byte0 (c-bytevector-u8-ref c-bytevector (+ k 7)))
         (byte1 (c-bytevector-u8-ref c-bytevector (+ k 6)))
         (byte2 (c-bytevector-u8-ref c-bytevector (+ k 5)))
         (byte3 (c-bytevector-u8-ref c-bytevector (+ k 4)))
         (byte4 (c-bytevector-u8-ref c-bytevector (+ k 3)))
         (byte5 (c-bytevector-u8-ref c-bytevector (+ k 2)))
         (byte6 (c-bytevector-u8-ref c-bytevector (+ k 1)))
         (byte7 (c-bytevector-u8-ref c-bytevector (+ k 0)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 16 (remainder byte0 128))
                             (quotient byte1 16)))
         (hibits (+ (* 65536 (remainder byte1 16))
                    (* 256 byte2)
                    byte3))
         (midbits (+ (* 256 byte4) byte5))
         (lobits (+ (* 256 byte6) byte7)))
    (make-ieee-double sign biased-exponent hibits midbits lobits)))

(define (c-bytevector-ieee-double-native-set! c-bytevector k x)
  (cond
    ((equal? (native-endianness) 'little)
     (if (not (= 0 (remainder k 4)))
       (if (not (= 0 (remainder k 8)))
         (error "c-bytevector-ieee-double-native-set!" (list c-bytevector k x)))
       (c-bytevector-ieee-double-set! c-bytevector k x 'little)))
    (else
      (if (not (= 0 (remainder k 8)))
        (error "c-bytevector-ieee-double-native-set!" (list c-bytevector k x)))
      (c-bytevector-ieee-double-set! c-bytevector k x 'big))))

(define (c-bytevector-ieee-double-native-ref c-bytevector k)
  (cond
    ((equal? (native-endianness) 'little)
     (if (not (= 0 (remainder k 8)))
       (error "c-bytevector-ieee-double-native-ref" (list c-bytevector k)))
     (c-bytevector-ieee-double-little-endian-ref c-bytevector k))
    (else
      (if (not (= 0 (remainder k 8)))
        (error "c-bytevector-ieee-double-native-ref" (list c-bytevector k)))
      (c-bytevector-ieee-double-big-endian-ref c-bytevector k))))

;; Parts from (r6rs bytevectors) library ends

(define (c-type-size type)
  (cond ((not (symbol? type)) (error "c-type-size: Type must be symbol" type))
        ((symbol=? type 'void) 0)
        ((or (symbol=? type 'i8)
             (symbol=? type 'u8)
             (symbol=? type 'i16)
             (symbol=? type 'u16)
             (symbol=? type 'i32)
             (symbol=? type 'u32)
             (symbol=? type 'i64)
             (symbol=? type 'u64)
             (symbol=? type 'char)
             (symbol=? type 'uchar)
             (symbol=? type 'short)
             (symbol=? type 'ushort)
             (symbol=? type 'int)
             (symbol=? type 'uint)
             (symbol=? type 'long)
             (symbol=? type 'ulong)
             (symbol=? type 'float)
             (symbol=? type 'double)
             (symbol=? type 'pointer))
         (size-of-type type))
        (else (error "Unknown type" type))))

(define (c-type-align type)
  (cond ((not (symbol? type)) (error "c-type-align: Type must be symbol" type))
        ((symbol=? type 'void) 0)
        ((or (symbol=? type 'i8)
             (symbol=? type 'u8)
             (symbol=? type 'i16)
             (symbol=? type 'u16)
             (symbol=? type 'i32)
             (symbol=? type 'u32)
             (symbol=? type 'i64)
             (symbol=? type 'u64)
             (symbol=? type 'char)
             (symbol=? type 'uchar)
             (symbol=? type 'short)
             (symbol=? type 'ushort)
             (symbol=? type 'int)
             (symbol=? type 'uint)
             (symbol=? type 'long)
             (symbol=? type 'ulong)
             (symbol=? type 'float)
             (symbol=? type 'double)
             (symbol=? type 'pointer))
         (align-of-type type))
        (else (error "Unknown type" type))))

(define (make-c-bytevector size . byte)
  (when (not (integer? size))
    (error "make-c-bytevector: Size must be integer" size))
  (let ((cbv (cond ((null? byte) (c-malloc size))
                   ((= (car byte) 0) (c-calloc 1 size))
                   (else (bytevector->c-bytevector (make-bytevector size (car byte)))))))
    (when (c-null? cbv)
      (c-perror (string->c-utf8 "make-c-bytevector error"))
      (error "make-c-bytevector error: malloc returned null" size))
    cbv))

(define c-bytevector
  (lambda bytes
    (bytevector->c-bytevector
      (apply (lambda (b) (make-bytevector 1 b)) bytes))))

(define (c-bytevector-set! cbv type offset value)
  (when (not (c-bytevector? cbv))
    (error "c-bytevector-set!: cbv argument must be c-bytevector" cbv))
  (when (not (symbol? type))
    (error "c-bytevector-set!: type must be symbol" type))
  (when (not (integer? offset))
    (error "c-bytevector-set!: offset argument must be integer" offset))
  (cond ((not (symbol? type)) (error "c-bytevector-set!: type must be symbol" type))
        ((symbol=? type 'i8)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-s8-set! cbv offset value))
        ((symbol=? type 'u8)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-u8-set! cbv offset value))
        ((symbol=? type 'i16)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value 2))
        ((symbol=? type 'u16)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-uint-set! cbv offset value 2))
        ((symbol=? type 'i32)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value 4))
        ((symbol=? type 'u32)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-uint-set! cbv offset value 4))
        ((symbol=? type 'i64)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value 8))
        ((symbol=? type 'u64)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-uint-set! cbv offset value 8))
        ((symbol=? type 'char)
         (when (not (char? value))
           (error "c-bytevector-set!: value for given type must be char"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-s8-set! cbv offset (char->integer value)))
        ((symbol=? type 'uchar)
         (when (not (char? value))
           (error "c-bytevector-set!: value for given type must be char"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-u8-set! cbv offset (char->integer value)))
        ((symbol=? type 'short)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'short)))
        ((symbol=? type 'ushort)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'ushort)))
        ((symbol=? type 'int)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'int)))
        ((symbol=? type 'uint)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'uint)))
        ((symbol=? type 'long)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'long)))
        ((symbol=? type 'ulong)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-sint-set! cbv offset value (c-type-size 'ulong)))
        ((symbol=? type 'float)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-ieee-single-native-set! cbv offset value))
        ((symbol=? type 'double)
         (when (not (number? value))
           (error "c-bytevector-set!: value for given type must be number"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-ieee-double-native-set! cbv offset value))
        ((symbol=? type 'pointer)
         (when (not (c-bytevector? value))
           (error "c-bytevector-set!: value for given type must be pointer"
                  `((type ,type)
                    (value ,value))))
         (c-bytevector-pointer-set! cbv offset value))
        (else (error "c-bytevector-set!: Unknown type" type))))

(define (c-bytevector-ref cbv type offset)
  (when (not (c-bytevector? cbv))
    (error "c-bytevector-ref: cbv argument must be c-bytevector" cbv))
  (when (not (symbol? type))
    (error "c-bytevector-ref: type must be symbol" type))
  (when (not (integer? offset))
    (error "c-bytevector-ref: offset argument must be integer" offset))
  (cond ((symbol=? type 'i8) (c-bytevector-s8-ref cbv offset))
        ((symbol=? type 'u8) (c-bytevector-u8-ref cbv offset))
        ((symbol=? type 'i16) (c-bytevector-sint-ref cbv offset 2))
        ((symbol=? type 'u16) (c-bytevector-uint-ref cbv offset 2))
        ((symbol=? type 'i32) (c-bytevector-sint-ref cbv offset 4))
        ((symbol=? type 'u32) (c-bytevector-uint-ref cbv offset 4))
        ((symbol=? type 'i64) (c-bytevector-sint-ref cbv offset 8))
        ((symbol=? type 'u64) (c-bytevector-uint-ref cbv offset 8))
        ((symbol=? type 'char) (integer->char (c-bytevector-s8-ref cbv offset)))
        ((symbol=? type 'uchar) (integer->char (c-bytevector-u8-ref cbv offset)))
        ((symbol=? type 'short) (c-bytevector-sint-ref cbv offset (c-type-size 'short)))
        ((symbol=? type 'ushort) (c-bytevector-uint-ref cbv offset (c-type-size 'ushort)))
        ((symbol=? type 'int) (c-bytevector-sint-ref cbv offset (c-type-size 'int)))
        ((symbol=? type 'uint) (c-bytevector-uint-ref cbv offset (c-type-size 'uint)))
        ((symbol=? type 'long) (c-bytevector-sint-ref cbv offset (c-type-size 'long)))
        ((symbol=? type 'ulong) (c-bytevector-uint-ref cbv offset (c-type-size 'ulong)))
        ((symbol=? type 'float) (c-bytevector-ieee-single-native-ref cbv offset))
        ((symbol=? type 'double) (c-bytevector-ieee-double-native-ref cbv offset))
        ((equal? type 'pointer) (c-bytevector-pointer-ref cbv offset))
        (else (error "c-bytevector-ref: Unknown type" type))))

(define (bytevector->c-bytevector bv)
  (when (not (bytevector? bv))
    (error "bytevector->c-bytevector: bv argument must be bytevector" bv))
  (letrec* ((bytes-length (bytevector-length bv))
            (pointer (make-c-bytevector bytes-length))
            (looper (lambda (index)
                      (when (< index bytes-length)
                        (c-bytevector-u8-set! pointer
                                              index
                                              (bytevector-u8-ref bv index))
                        (looper (+ index 1))))))
    (looper 0)
    pointer))

(define (c-bytevector->bytevector cbv size)
  (when (not (c-bytevector? cbv))
    (error "c-bytevector->bytevector: cbv argument must be c-bytevector" cbv))
  (when (not (integer? size))
    (error "c-bytevector->bytevector: size argument must be integer" size))
  (letrec* ((bv (make-bytevector size))
            (looper (lambda (index)
                      (let ((byte (c-bytevector-u8-ref cbv index)))
                        (if (= index size)
                          bv
                          (begin
                            (bytevector-u8-set! bv index byte)
                            (looper (+ index 1))))))))
    (looper 0)))

(define (c-utf8->string cbv)
  (when (not (c-bytevector? cbv))
    (error "c-utf8->string: cbv argument must be c-bytevector" cbv))
  (let ((size (c-strlen cbv)))
    (utf8->string (c-bytevector->bytevector cbv size))))

(define (string->c-utf8 str)
  (when (not (string? str))
    (error "string->c-utf8-: str argument must be string" str))
  (bytevector->c-bytevector
    (string->utf8
      (string-append str (string (integer->char 0))))))

(define (c-bytevector->integer cbv . offset)
  (when (not (c-bytevector? cbv))
    (error "c-bytevector->integer cbv argument must be c-bytevector" cbv))
  (let ((internal-offset (if (null? offset) 0 (car offset))))
    (when (not (integer? internal-offset))
      (error "c-bytevector->integer offset argument must be integer" (car offset)))
    (+ (c-memset-pointer->address cbv 0 0) internal-offset)))

(define (integer->c-bytevector address)
  (when (not (integer? address))
    (error "c-bytevector->string: address argument must be integer" address))
  (c-memset-address->pointer address 0 0))

(define-syntax call-with-address-of
  (syntax-rules ()
    ((_ cbv thunk)
     (let ((address-cbv (make-c-bytevector (c-type-size 'pointer))))
       (c-bytevector-pointer-set! address-cbv 0 cbv)
       (when (not (c-bytevector? cbv))
         (error "call-with-address-of: cbv argument must be c-bytevector"))
       (when (not (procedure? thunk))
         (error "call-with-address-of: thunk argument must be procedure"))
       (let ((result (apply thunk (list address-cbv))))
         (set! cbv (c-bytevector-pointer-ref address-cbv 0))
         (c-free address-cbv)
         result)))))

(define (round-to-next-modulo-of to-round roundee)
  (if (= (modulo to-round roundee) 0)
    to-round
    (round-to-next-modulo-of (+ to-round 1) roundee)))

(define (calculate-struct-members members . return-just-size)
  (let*
    ((size 0)
     (largest-member-size 0)
     (data (map (lambda (member)
                  (let* ((name (list-ref member 0))
                         (type (list-ref member 1))
                         (accessor (list-ref member 2))
                         (type-alignment (c-type-align type)))
                    (when (> (size-of-type type) largest-member-size)
                      (set! largest-member-size (size-of-type type)))
                    (if (or (= size 0)
                            (= (modulo size type-alignment) 0))
                      (begin
                        (set! size (+ size type-alignment))
                        (list name type (- size type-alignment) accessor))
                      (let ((next-alignment
                              (round-to-next-modulo-of size type-alignment)))
                        (set! size (+ next-alignment type-alignment))
                        (list name type next-alignment accessor)))))
                members)))
    (if (null? return-just-size)
      data
      size)))

(define calculate-struct-size
  (lambda (members)
    (calculate-struct-members members #t)))

(define-syntax define-c-struct
  (syntax-rules ()
    ((_ name members struct-size-variable struct-cbv (field-name field-type accessor modifier) ...)
     (begin
       (when (not (or (equal? struct-cbv #f)
                      (c-bytevector? struct-cbv)))
         (error "define-c-struct: struct-cbv argument must be c-bytevector or #f"))
       (define accessor
         (lambda (cbv)
           (let ((offset (let ((offset 0)
                               (before? #t))
                           (for-each
                             (lambda (member)
                               (when (equal? (list-ref member 0) 'field-name)
                                 (set! before? #f))
                               (when before?
                                 (set! offset
                                   (+ offset
                                      (c-type-align (list-ref member 1))))))
                             members)
                           offset)))
             (c-bytevector-ref cbv field-type offset))))
       ...
       (define modifier
         (lambda (cbv value)
           (let ((offset (let ((offset 0)
                               (before? #t))
                           (for-each
                             (lambda (member)
                               (when (equal? (list-ref member 0) 'field-name)
                                 (set! before? #f))
                               (when before?
                                 (set! offset
                                   (+ offset
                                      (c-type-align (list-ref member 1))))))
                             members)
                           offset)))
             (c-bytevector-set! cbv field-type offset value))))
       ...
       (define members (calculate-struct-members
                         (list (list 'field-name field-type accessor) ...)))
       (define struct-size-variable (calculate-struct-size
                                      (list (list 'field-name field-type accessor) ...)))
       (define name
         (if (not struct-cbv)
           (make-c-bytevector (+ (c-type-size field-type) ...) 0)
           struct-cbv))))))

(cond-expand
  (capyscheme (primitives-init c-bytevector-set! c-bytevector-ref))
  (chibi (primitives-init c-bytevector-set! c-bytevector-ref))
  (else))
