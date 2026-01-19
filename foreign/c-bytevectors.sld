;;; Copyright 2025 Retropikzel
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; This is R6RS c-Bytevectors library, modified to work with C pointers.
;;; Mostly just by adding c- prefix to each word "bytevector".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This R7RS-portable implementation of (rnrs bytevectors) is
;;; mostly derived from Larceny's src/Lib/Common/bytevector.sch.
;;;
;;; The R6RS requires implementations to select a native endianness.
;;; That choice is arbitrary, intended to affect performance but not
;;; behavior.  In this implementation, the native endianness is
;;; obtained via cond-expand, which should coincide with the
;;; endianness obtained by calling the features procedure.  Of the
;;; R7RS systems I've tested, only one omits endianness from its
;;; (features), and it's a slow interpreter for which the native
;;; endianness probably won't affect performance.
;;;
;;; This implementation defines a 53-bit exact integer constant,
;;; and the procedures that work with byte fields of arbitrary
;;; width may create even larger exact integers.
;;;
;;; FIXME: It should be possible to delay creation of that 53-bit
;;; constant until it's needed, which might be better for systems
;;; that don't support exact 53-bit integers.  It looks as though
;;; most systems R7RS systems either support exact 53-bit integers
;;; or overflow into inexact 53-bit integers; if the constant turns
;;; out to be inexact, then the procedure that needs it will fail
;;; when it is called, which is what would happen if creation of
;;; that constant were delayed.

(define-library
  (foreign c-bytevectors)
  (cond-expand
    (chezscheme
      (import (rnrs base)
              (rnrs control)
              (only (rnrs r5rs)
                    remainder
                    quotient)
              (only (rnrs bytevectors) native-endianness)))
    (r6rs
      (import (rnrs base)
              (rnrs control)
              (only (rnrs r5rs)
                    remainder
                    quotient)
              (only (rnrs bytevectors) native-endianness)))
    (else
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme process-context)
              (scheme inexact))
      (begin
        (define (native-endianness)
          (cond-expand (little-endian 'little)
                       (else 'big))))))
  (export c-bytevectors-init
    ;; TODO endianness
    native-endianness
    ;make-c-bytevector
    ;c-bytevector->address
    ;; TODO c-bytevector=?
    ;; TODO c-bytevector-fill!
    ;; TODO c-bytevector-copy!
    ;; TODO c-bytevector-copy
    c-bytevector-s8-set!
    c-bytevector-s8-ref
    ;; TODO c-bytevector->u8-list
    ;; TODO u8-list->c-bytevector

    c-bytevector-uint-ref
    c-bytevector-sint-ref
    c-bytevector-sint-set!
    c-bytevector-uint-set!
    ;; TODO bytevector->uint-list
    ;; TODO bytevector->sint-list
    ;; TODO uint-list->bytevector
    ;; TODO sint-list->bytevector

    c-bytevector-u16-ref
    c-bytevector-s16-ref
    c-bytevector-u16-native-ref
    c-bytevector-s16-native-ref
    c-bytevector-u16-set!
    c-bytevector-s16-set!
    c-bytevector-u16-native-set!
    c-bytevector-s16-native-set!

    c-bytevector-u32-ref
    c-bytevector-s32-ref
    c-bytevector-u32-native-ref
    c-bytevector-s32-native-ref
    c-bytevector-u32-set!
    c-bytevector-s32-set!
    c-bytevector-u32-native-set!
    c-bytevector-s32-native-set!

    c-bytevector-u64-ref
    c-bytevector-s64-ref
    c-bytevector-s64-native-ref
    c-bytevector-u64-native-ref
    c-bytevector-u64-set!
    c-bytevector-s64-set!
    c-bytevector-u64-native-set!
    c-bytevector-s64-native-set!

    c-bytevector-ieee-single-native-ref
    c-bytevector-ieee-single-ref

    c-bytevector-ieee-double-native-ref
    c-bytevector-ieee-double-ref

    c-bytevector-ieee-single-native-set!
    c-bytevector-ieee-single-set!

    c-bytevector-ieee-double-native-set!
    c-bytevector-ieee-double-set!

    ;string->c-utf8
    ;; TODO string->c-utf16
    ;; TODO string->c-utf32

    ;c-utf8->string
    ;; TODO c-utf16->string
    ;; TODO c-utf32->string
          )
  (begin

    (define make-c-bytevector #f)
    (define c-bytevector-u8-set! #f)
    (define c-bytevector-u8-ref #f)
    (define c-type-size #f)
    (define (c-bytevectors-init make u8-set! u8-ref size-of)
      (set! make-c-bytevector make)
      (set! c-bytevector-u8-set! u8-set!)
      (set! c-bytevector-u8-ref u8-ref)
      (set! c-type-size size-of))

    ;;; Local stuff.

    (define (complain who . irritants)
      (apply error
             (string-append "illegal arguments passed to "
                            (symbol->string who))
             irritants))

    ; Help syntax and procedures; not exported.

    (define-syntax unspecified
      (syntax-rules ()
        ((_) (if #f #f))))

    (define-syntax c-bytevector:div
      (syntax-rules ()
        ((_ x y) (quotient x y))))

    (define-syntax c-bytevector:mod
      (syntax-rules ()
        ((_ x y) (remainder x y))))

    (define-syntax u8->s8
      (syntax-rules ()
        ((_ octet0)
         (let ((octet octet0))
           (if (> octet 127)
             (- octet 256)
             octet)))))

    (define-syntax s8->u8
      (syntax-rules ()
        ((_ val0)
         (let ((val val0))
           (if (negative? val)
             (+ val 256)
             val)))))

    (define (make-uint-ref size)
      (lambda (c-bytevector k endianness)
        (c-bytevector-uint-ref c-bytevector k endianness size)))

    (define (make-sint-ref size)
      (lambda (c-bytevector k endianness)
        (c-bytevector-sint-ref c-bytevector k endianness size)))

    (define (make-uint-set! size)
      (lambda (c-bytevector k n endianness)
        (c-bytevector-uint-set! c-bytevector k n endianness size)))

    (define (make-sint-set! size)
      (lambda (c-bytevector k n endianness)
        (c-bytevector-sint-set! c-bytevector k n endianness size)))

    (define (make-ref/native base base-ref)
      (lambda (c-bytevector index)
        (ensure-aligned index base)
        (base-ref c-bytevector index (native-endianness))))

    (define (make-set!/native base base-set!)
      (lambda (c-bytevector index val)
        (ensure-aligned index base)
        (base-set! c-bytevector index val (native-endianness))))

    (define (ensure-aligned index base)
      (if (not (zero? (c-bytevector:mod index base)))
        (error "non-aligned c-bytevector access" index base)))

    #;(define (make-c-bytevector->int-list c-bytevector-ref)
    (lambda (b endness size)
      (let ((ref (lambda (i) (c-bytevector-ref b i endness size)))
            (length (c-bytevector-length b)))
        (let loop ((i 0) (r '()))
          (if (>= i length)
            (reverse r)
            (loop (+ i size)
                  (cons (ref i) r)))))))

  (define (make-int-list->c-bytevector c-bytevector-set!)
    (lambda (l endness size)
      (let* ((c-bytevector (make-c-bytevector (* size (length l))))
             (setter! (lambda (i n)
                        (c-bytevector-set! c-bytevector i n endness size))))
        (let loop ((i 0) (l l))
          (if (null? l)
            c-bytevector
            (begin
              (setter! i (car l))
              (loop (+ i size) (cdr l))))))))

  ;;; Magic numbers for IEEE-754 single and double precision:
  ;;;
  ;;;     the largest biased exponent (255 or 2047)
  ;;;     the exponent bias (127 or 1023)
  ;;;     the integer value of the hidden bit (2^23 or 2^52)

  (define c-bytevector:single-maxexponent 255)
  (define c-bytevector:single-bias
    (c-bytevector:div c-bytevector:single-maxexponent 2))
  (define c-bytevector:single-hidden-bit (expt 2 23))

  (define c-bytevector:double-maxexponent 2047)
  (define c-bytevector:double-bias
    (c-bytevector:div c-bytevector:double-maxexponent 2))
  (define c-bytevector:double-hidden-bit (expt 2 52))    ; must be exact integer

  (define two^48 (expt 2 48))
  (define two^40 (expt 2 40))
  (define two^32 (expt 2 32))
  (define two^24 (expt 2 24))
  (define two^16 (expt 2 16))
  (define two^8  (expt 2 8))

  ;;; Given four exact integers, returns
  ;;;
  ;;;     (-1)^sign * (2^exponent) * p/q
  ;;;
  ;;; as an inexact real.
  ;;;
  ;;; FIXME: this procedure is not used, but it might eventually
  ;;; become relevant to a rewrite of this implementation so I'm
  ;;; just commenting it out.

  #;
  (define (c-bytevector:normalized sign exponent p q)
    (let* ((p/q (inexact (/ p q)))
           (x (* p/q (expt 2.0 exponent))))
      (cond ((= sign 0) x)
            ((= x 0.0) -0.0)
            (else (- x)))))

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
                                      (round (c-bytevector:div num 2))))
                              ((and (< num q) (= biased 1))
                               (values sign biased num))))))))))))

  ;;; This procedure should work even if
  ;;;     exact integers are limited to as little as 20 bits
  ;;;     inexact reals are limited to IEEE single precision
  ;;;
  ;;; If inexact reals are limited to single precision, then
  ;;; the result might overflow, but we can't help that.

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

  ;;; This procedure should work even if
  ;;;     exact integers are limited to as little as 23 bits
  ;;;     inexact reals are limited to IEEE single precision

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Exported stuff.
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; The R6RS allows implementations to support other symbols as well.

  (define-syntax endianness
    (syntax-rules ()
      ((_ big)
       (quote big))
      ((_ little)
       (quote little))))

  #;(define (c-bytevector=? bv1 bv2)
    (if (and (c-bytevector? bv1)
             (c-bytevector? bv2))
      (equal? bv1 bv2)
      (complain 'c-bytevector=? bv1 bv2)))

  #;(define (c-bytevector-fill! b fill)
  (if (<= -128 fill -1)
    (c-bytevector-fill! b (+ fill 256))
    (let ((n (c-bytevector-length b)))
      (do ((i 0 (+ i 1)))
        ((= i n))
        (c-bytevector-u8-set! b i fill)))))

(define (r6rs:c-bytevector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
    (do ((i 0 (+ i 1)))
      ((>= i count))
      (c-bytevector-u8-set! target
                            (+ target-start i)
                            (c-bytevector-u8-ref source (+ source-start i))))
    (do ((i (- count 1) (- i 1)))
      ((< i 0))
      (c-bytevector-u8-set! target
                            (+ target-start i)
                            (c-bytevector-u8-ref source (+ source-start i))))))

;;; Already defined by (scheme base), perhaps in greater generality:
;;;
;;;     c-bytevector-copy
;;;     c-bytevector-u8-ref
;;;     c-bytevector-u8-set!

(define (c-bytevector-s8-ref b k)
  (u8->s8 (c-bytevector-u8-ref b k)))

(define (c-bytevector-s8-set! b k val)
  (c-bytevector-u8-set! b k (s8->u8 val)))

#;(define (c-bytevector->u8-list b)
(let ((n (c-bytevector-length b)))
  (do ((i (- n 1) (- i 1))
       (result '() (cons (c-bytevector-u8-ref b i) result)))
    ((< i 0)
     result))))

(define (u8-list->c-bytevector vals)
  (let* ((n (length vals))
         (b (make-c-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
      ((null? vals))
      (c-bytevector-u8-set! b i (car vals)))
    b))

(define (c-bytevector-uchar-ref c-bytevector index)
  (integer->char (c-bytevector-u8-ref c-bytevector index)))

(define (c-bytevector-uchar-set! c-bytevector index char)
  (c-bytevector-u8-set! c-bytevector index (char->integer char)))

(define (c-bytevector-uint-ref c-bytevector index endness size)
  (cond ((equal? endness 'big)
         (do ((i 0 (+ i 1))
              (result 0 (+ (* 256 result)
                           (c-bytevector-u8-ref c-bytevector (+ index i)))))
           ((>= i size)
            result)))
        ((equal? endness 'little)
         (do ((i (- size 1) (- i 1))
              (result 0 (+ (* 256 result)
                           (c-bytevector-u8-ref c-bytevector (+ index i)))))
           ((< i 0)
            result)))
        (else
          (c-bytevector-uint-ref c-bytevector index (native-endianness) size))))

(define (c-bytevector-sint-ref c-bytevector index endness size)
  (let* ((high-byte (c-bytevector-u8-ref c-bytevector
                                         (if (eq? endness 'big)
                                           index
                                           (+ index size -1))))
         (uresult (c-bytevector-uint-ref c-bytevector index endness size)))
    (if (> high-byte 127)
      (- uresult (expt 256 size))
      uresult)))

; FIXME: Some of these procedures may not do enough range checking.

(define (c-bytevector-uint-set! c-bytevector index val endness size)
  (case endness
    ((little)
     (do ((i 0 (+ i 1))
          (val val (c-bytevector:div val 256)))
       ((>= i size)
        (unspecified))
       (c-bytevector-u8-set! c-bytevector (+ index i) (c-bytevector:mod val 256))))
    ((big)
     (do ((i (- size 1) (- i 1))
          (val val (c-bytevector:div val 256)))
       ((< i 0)
        (unspecified))
       (c-bytevector-u8-set! c-bytevector (+ index i) (c-bytevector:mod val 256))))
    (else
      (c-bytevector-uint-set! c-bytevector index val (native-endianness) size))))

(define (c-bytevector-sint-set! c-bytevector index val endness size)
  (let ((uval (if (< val 0)
                (+ val (expt 256 size))
                val)))
    (c-bytevector-uint-set! c-bytevector index uval endness size)))

;(define c-bytevector->uint-list (make-c-bytevector->int-list c-bytevector-uint-ref))
;(define c-bytevector->sint-list (make-c-bytevector->int-list c-bytevector-sint-ref))

;(define uint-list->c-bytevector (make-int-list->c-bytevector c-bytevector-uint-set!))
;(define sint-list->c-bytevector (make-int-list->c-bytevector c-bytevector-sint-set!))

(define c-bytevector-u16-ref (make-uint-ref 2))
(define c-bytevector-s16-ref (make-sint-ref 2))
(define c-bytevector-u16-set! (make-uint-set! 2))
(define c-bytevector-s16-set! (make-sint-set! 2))
(define c-bytevector-u16-native-ref (make-ref/native 2 c-bytevector-u16-ref))
(define c-bytevector-s16-native-ref (make-ref/native 2 c-bytevector-s16-ref))
(define c-bytevector-u16-native-set! (make-set!/native 2 c-bytevector-u16-set!))
(define c-bytevector-s16-native-set! (make-set!/native 2 c-bytevector-s16-set!))

(define c-bytevector-u32-ref (make-uint-ref 4))
(define c-bytevector-s32-ref (make-sint-ref 4))
(define c-bytevector-u32-set! (make-uint-set! 4))
(define c-bytevector-s32-set! (make-sint-set! 4))
(define c-bytevector-u32-native-ref (make-ref/native 4 c-bytevector-u32-ref))
(define c-bytevector-s32-native-ref (make-ref/native 4 c-bytevector-s32-ref))
(define c-bytevector-u32-native-set! (make-set!/native 4 c-bytevector-u32-set!))
(define c-bytevector-s32-native-set! (make-set!/native 4 c-bytevector-s32-set!))

(define c-bytevector-u64-ref (make-uint-ref 8))
(define c-bytevector-s64-ref (make-sint-ref 8))
(define c-bytevector-u64-set! (make-uint-set! 8))
(define c-bytevector-s64-set! (make-sint-set! 8))
(define c-bytevector-u64-native-ref (make-ref/native 8 c-bytevector-u64-ref))
(define c-bytevector-s64-native-ref (make-ref/native 8 c-bytevector-s64-ref))
(define c-bytevector-u64-native-set! (make-set!/native 8 c-bytevector-u64-set!))
(define c-bytevector-s64-native-set! (make-set!/native 8 c-bytevector-s64-set!))

;(cond-expand
  ;(little-endian
    (define (c-bytevector-ieee-single-native-ref c-bytevector k)
      (cond
        ((equal? (native-endianness) 'little)
         (if (not (= 0 (remainder k 4)))
           (complain 'c-bytevector-ieee-single-native-ref c-bytevector k))
         (c-bytevector-ieee-single-little-endian-ref c-bytevector k))
        (else
          (if (not (= 0 (remainder k 4)))
            (complain 'c-bytevector-ieee-single-native-ref c-bytevector k))
          (c-bytevector-ieee-single-big-endian-ref c-bytevector k))))
    (define (c-bytevector-ieee-double-native-ref c-bytevector k)
      (cond
        ((equal? (native-endianness) 'little)
         (if (not (= 0 (remainder k 8)))
           (complain 'c-bytevector-ieee-double-native-ref c-bytevector k))
         (c-bytevector-ieee-double-little-endian-ref c-bytevector k))
        (else
          (if (not (= 0 (remainder k 8)))
            (complain 'c-bytevector-ieee-double-native-ref c-bytevector k))
          (c-bytevector-ieee-double-big-endian-ref c-bytevector k))))
    (define (c-bytevector-ieee-single-native-set! c-bytevector k x)
      (cond
        ((equal? (native-endianness) 'little)
         (if (not (= 0 (remainder k 4)))
           (complain 'c-bytevector-ieee-single-native-set! c-bytevector k x))
         (c-bytevector-ieee-single-set! c-bytevector k x 'little))
        (else
          (if (not (= 0 (remainder k 4)))
            (complain 'c-bytevector-ieee-single-native-set! c-bytevector k x))
          (c-bytevector-ieee-single-set! c-bytevector k x 'big))))
    (define (c-bytevector-ieee-double-native-set! c-bytevector k x)
      (cond
        ((equal? (native-endianness) 'little)
         (if (not (= 0 (remainder k 4)))
           (if (not (= 0 (remainder k 8)))
             (complain 'c-bytevector-ieee-double-native-set! c-bytevector k x))
           (c-bytevector-ieee-double-set! c-bytevector k x 'little)))
        (else
          (if (not (= 0 (remainder k 8)))
            (complain 'c-bytevector-ieee-double-native-set! c-bytevector k x))
          (c-bytevector-ieee-double-set! c-bytevector k x 'big))))
  #;(else
    (define (c-bytevector-ieee-single-native-ref c-bytevector k)
      (if (not (= 0 (remainder k 4)))
        (complain 'c-bytevector-ieee-single-native-ref c-bytevector k))
      (c-bytevector-ieee-single-big-endian-ref c-bytevector k))
    (define (c-bytevector-ieee-double-native-ref c-bytevector k)
      (if (not (= 0 (remainder k 8)))
        (complain 'c-bytevector-ieee-double-native-ref c-bytevector k))
      (c-bytevector-ieee-double-big-endian-ref c-bytevector k))
    (define (c-bytevector-ieee-single-native-set! c-bytevector k x)
      (if (not (= 0 (remainder k 4)))
        (complain 'c-bytevector-ieee-single-native-set! c-bytevector k x))
      (c-bytevector-ieee-single-set! c-bytevector k x 'big))
    (define (c-bytevector-ieee-double-native-set! c-bytevector k x)
      (if (not (= 0 (remainder k 8)))
        (complain 'c-bytevector-ieee-double-native-set! c-bytevector k x))
      (c-bytevector-ieee-double-set! c-bytevector k x 'big)))
;)

(define (c-bytevector-ieee-single-ref c-bytevector k endianness)
  (case endianness
    ((big)
     (c-bytevector-ieee-single-big-endian-ref c-bytevector k))
    ((little)
     (c-bytevector-ieee-single-little-endian-ref c-bytevector k))
    (else
      (complain 'c-bytevector-ieee-single-ref c-bytevector k endianness))))

(define (c-bytevector-ieee-double-ref c-bytevector k endianness)
  (case endianness
    ((big)
     (c-bytevector-ieee-double-big-endian-ref c-bytevector k))
    ((little)
     (c-bytevector-ieee-double-little-endian-ref c-bytevector k))
    (else
      (complain 'c-bytevector-ieee-double-ref c-bytevector k endianness))))

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
                                     (c-bytevector:div biased-exponent 2)))
            (c-bytevector-u8-set! c-bytevector (+ k 1)
                                  (+ (* 128 (c-bytevector:mod biased-exponent 2))
                                     (c-bytevector:div frac (* 256 256))))
            (c-bytevector-u8-set! c-bytevector (+ k 2)
                                  (c-bytevector:div
                                    (c-bytevector:mod frac (* 256 256)) 256))
            (c-bytevector-u8-set! c-bytevector (+ k 3)
                                  (c-bytevector:mod frac 256)))
          (begin
            (c-bytevector-u8-set! c-bytevector (+ k 3)
                                  (+ (* 128 sign)
                                     (c-bytevector:div biased-exponent 2)))
            (c-bytevector-u8-set! c-bytevector (+ k 2)
                                  (+ (* 128 (c-bytevector:mod biased-exponent 2))
                                     (c-bytevector:div frac (* 256 256))))
            (c-bytevector-u8-set! c-bytevector (+ k 1)
                                  (c-bytevector:div
                                    (c-bytevector:mod frac (* 256 256)) 256))
            (c-bytevector-u8-set! c-bytevector k
                                  (c-bytevector:mod frac 256))))
        (unspecified))
      (cond ((= biased-exponent c-bytevector:single-maxexponent)
             (store! sign biased-exponent frac))
            ((< frac c-bytevector:single-hidden-bit)
             (store! sign 0 frac))
            (else
              (store! sign biased-exponent
                      (- frac c-bytevector:single-hidden-bit)))))))

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
                                 (c-bytevector:div biased-exponent 16)))
        (c-bytevector-u8-set! c-bytevector (+ k 6)
                              (+ (* 16 (c-bytevector:mod biased-exponent 16))
                                 (c-bytevector:div frac two^48)))
        (c-bytevector-u8-set! c-bytevector (+ k 5)
                              (c-bytevector:div (c-bytevector:mod frac two^48)
                                                two^40))
        (c-bytevector-u8-set! c-bytevector (+ k 4)
                              (c-bytevector:div (c-bytevector:mod frac two^40)
                                                two^32))
        (c-bytevector-u8-set! c-bytevector (+ k 3)
                              (c-bytevector:div (c-bytevector:mod frac two^32)
                                                two^24))
        (c-bytevector-u8-set! c-bytevector (+ k 2)
                              (c-bytevector:div (c-bytevector:mod frac two^24)
                                                two^16))
        (c-bytevector-u8-set! c-bytevector (+ k 1)
                              (c-bytevector:div (c-bytevector:mod frac two^16)
                                                256))
        (c-bytevector-u8-set! c-bytevector k
                              (c-bytevector:mod frac 256))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversions between c-bytevectors and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Already defined by (scheme base), with greater generality:
;;;
;;;     string->utf8
;;;     utf8->string

; (utf-16-codec) might write a byte order mark,
; so it's better not to use textual i/o for this.

(define (string->utf16 string . rest)
  (let* ((endianness (cond ((null? rest)
                            'big)
                           ((not (null? (cdr rest)))
                            (apply complain 'string->utf16 string rest))
                           ((eq? (car rest) 'big)
                            'big)
                           ((eq? (car rest) 'little)
                            'little)
                           (else
                             (apply complain 'string->utf16 string rest))))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (n (string-length string)))

    (define (result-length)
      (do ((i 0 (+ i 1))
           (k 0 (let ((sv (char->integer (string-ref string i))))
                  (if (< sv #x10000) (+ k 2) (+ k 4)))))
        ((= i n) k)))

    (let ((bv (make-c-bytevector (result-length))))

      (define (loop i k)
        (if (< i n)
          (let ((sv (char->integer (string-ref string i))))
            (if (< sv #x10000)
              (let ((hibits (quotient sv 256))
                    (lobits (remainder sv 256)))
                (c-bytevector-u8-set! bv (+ k hi) hibits)
                (c-bytevector-u8-set! bv (+ k lo) lobits)
                (loop (+ i 1) (+ k 2)))
              (let* ((x (- sv #x10000))
                     (hibits (quotient x 1024))
                     (lobits (remainder x 1024))
                     (hi16 (+ #xd800 hibits))
                     (lo16 (+ #xdc00 lobits))
                     (hi1 (quotient hi16 256))
                     (lo1 (remainder hi16 256))
                     (hi2 (quotient lo16 256))
                     (lo2 (remainder lo16 256)))
                (c-bytevector-u8-set! bv (+ k hi) hi1)
                (c-bytevector-u8-set! bv (+ k lo) lo1)
                (c-bytevector-u8-set! bv (+ k hi 2) hi2)
                (c-bytevector-u8-set! bv (+ k lo 2) lo2)
                (loop (+ i 1) (+ k 4)))))))

      (loop 0 0)
      bv)))

;;; The second argument to utf16->string should be optional,
;;; and was optional in the R5.94RS draft, but was made mandatory
;;; in the R5.95RS draft by someone who misinterpreted John Cowan's
;;; response of 27 May 2007 to an ambiguous question posed by
;;; Mike Sperber.  This error was not spotted by anyone, and
;;; made its way into the ratified R6RS.
;;;
;;; This implementation does not perpetuate that error.  In this
;;; implementation, the second argument is optional.
;;;
;;; The R6RS also contradicts itself by saying the c-bytevector
;;; will be decoded according to UTF-16BE or UTF-16LE, which
;;; implies any BOM must be ignored.  I believe the intended
;;; specification was along these lines:
;;;
;;;    c-Bytevector is decoded acccording to UTF-16, UTF-16BE,
;;;    UTF-16LE, or a fourth encoding scheme that differs from
;;;    all three of those, depending upon the optional arguments
;;;    endianness and endianness-mandatory.  If endianness
;;;    is the symbol big and endianness-mandatory is absent
;;;    or false, then c-bytevector is decoded according to
;;;    UTF-16.  If endianness is the symbol big and
;;;    endianness-mandatory is #t, then c-bytevector is decoded
;;;    according to UTF-16BE.  If endianness is the symbol
;;;    little and endianness-mandatory is #t, then c-bytevector
;;;    is decoded according to UTF-16LE.  If endianness is
;;;    the symbol little and endianness-mandatory is absent
;;;    or #f, then the c-bytevector is decoded according to
;;;    UTF-16 if it begins with a BOM but is decoded according
;;;    to UTF-16LE if it does not begin with a BOM; note that
;;;    this fourth decoding does not correspond to any of the
;;;    Unicode encoding schemes that are defined by the Unicode
;;;    standard.
;;;
;;; That is the specification implemented here.

#;(define (utf16->string c-bytevector . rest)
(let* ((n (c-bytevector-length c-bytevector))

       (begins-with-bom?
         (and (<= 2 n)
              (let ((b0 (c-bytevector-u8-ref c-bytevector 0))
                    (b1 (c-bytevector-u8-ref c-bytevector 1)))
                (or (and (= b0 #xfe) (= b1 #xff) 'big)
                    (and (= b0 #xff) (= b1 #xfe) 'little)))))

       (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                          #f)
                         ((and (null? (cddr rest))
                               (boolean? (cadr rest)))
                          (cadr rest))
                         (else
                           (apply complain 'utf16->string c-bytevector rest))))

       (endianness (cond ((null? rest)
                          (or begins-with-bom? 'big))
                         ((eq? (car rest) 'big)
                          (if mandatory?
                            'big
                            (or begins-with-bom? 'big)))
                         ((eq? (car rest) 'little)
                          (if mandatory?
                            'little
                            (or begins-with-bom? 'little)))
                         (else (apply complain
                                      'utf16->string
                                      c-bytevector rest))))

       (begins-with-bom? (if mandatory? #f begins-with-bom?))

       (endianness (if mandatory? (car rest) endianness))

       ; endianness-dependent adjustments to indexing

       (hi (if (eq? 'big endianness) 0 1))
       (lo (- 1 hi))

       (replacement-character (integer->char #xfffd)))

  ; computes the length of the encoded string

  (define (result-length)
    (define (loop i k)
      (if (>= i n)
        k
        (let ((octet (c-bytevector-u8-ref c-bytevector i)))
          (cond ((< octet #xd8)
                 (loop (+ i 2) (+ k 1)))
                ((< octet #xdc)
                 (let* ((i2 (+ i 2))
                        (octet2 (if (< i2 n)
                                  (c-bytevector-u8-ref c-bytevector i2)
                                  0)))
                   (if (<= #xdc octet2 #xdf)
                     (loop (+ i 4) (+ k 1))
                     ; bad surrogate pair, becomes replacement character
                     (loop i2 (+ k 1)))))
                (else (loop (+ i 2) (+ k 1)))))))
    (if begins-with-bom?
      (loop (+ hi 2) 0)
      (loop hi 0)))

  (if (odd? n)
    (error "c-bytevector passed to utf16->string has odd length" c-bytevector))

  (let ((s (make-string (result-length))))
    (define (loop i k)
      (if (< i n)
        (let ((hibits (c-bytevector-u8-ref c-bytevector (+ i hi)))
              (lobits (c-bytevector-u8-ref c-bytevector (+ i lo))))
          (cond ((< hibits #xd8)
                 (let ((c (integer->char
                            (+ (* hibits 256)
                               lobits))))
                   (string-set! s k c))
                 (loop (+ i 2) (+ k 1)))
                ((< hibits #xdc)
                 (let* ((i2 (+ i hi 2))
                        (i3 (+ i lo 2))
                        (octet2 (if (< i2 n)
                                  (c-bytevector-u8-ref c-bytevector i2)
                                  0))
                        (octet3 (if (< i2 n)
                                  (c-bytevector-u8-ref c-bytevector i3)
                                  0)))
                   (if (<= #xdc octet2 #xdf)
                     (let* ((sv (+ #x10000
                                   (* #x0400
                                      (remainder
                                        (+ (* hibits 256)
                                           lobits)
                                        #x0400))
                                   (remainder
                                     (+ (* octet2 256)
                                        octet3)
                                     #x0400)))
                            (c (if (<= #x10000 sv #x10ffff)
                                 (integer->char sv)
                                 replacement-character)))
                       (string-set! s k c)
                       (loop (+ i 4) (+ k 1)))
                     ; bad surrogate pair
                     (begin (string-set! s k replacement-character)
                            (loop (+ i 2) (+ k 1))))))
                ((< hibits #xe0)
                 ; second surrogate not preceded by a first surrogate
                 (string-set! s k replacement-character)
                 (loop (+ i 2) (+ k 1)))
                (else
                  (let ((c (integer->char
                             (+ (* hibits 256)
                                lobits))))
                    (string-set! s k c))
                  (loop (+ i 2) (+ k 1)))))))
    (if begins-with-bom?
      (loop 2 0)
      (loop 0 0))
    s)))

;;; There is no utf-32-codec, so we can't use textual i/o for this.

#;(define (string->utf32 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (apply complain
                                        'string->utf32
                                        string
                                        rest))))
         (n (string-length string))
         (result (make-c-bytevector (* 4 n))))
    (do ((i 0 (+ i 1)))
      ((= i n) result)
      (c-bytevector-u32-set! result
                             (* 4 i)
                             (char->integer (string-ref string i))
                             endianness))))

;;; There is no utf-32-codec, so we can't use textual i/o for this.

#;(define (utf32->string c-bytevector . rest)
(let* ((n (c-bytevector-length c-bytevector))

       (begins-with-bom?
         (and (<= 4 n)
              (let ((b0 (c-bytevector-u8-ref c-bytevector 0))
                    (b1 (c-bytevector-u8-ref c-bytevector 1))
                    (b2 (c-bytevector-u8-ref c-bytevector 2))
                    (b3 (c-bytevector-u8-ref c-bytevector 3)))
                (or (and (= b0 0) (= b1 0) (= b2 #xfe) (= b3 #xff)
                         'big)
                    (and (= b0 #xff) (= b1 #xfe) (= b2 0) (= b3 0)
                         'little)))))

       (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                          #f)
                         ((and (null? (cddr rest))
                               (boolean? (cadr rest)))
                          (cadr rest))
                         (else
                           (apply complain 'utf32->string c-bytevector rest))))

       (endianness (cond ((null? rest)
                          (or begins-with-bom? 'big))
                         ((eq? (car rest) 'big)
                          (if mandatory?
                            'big
                            (or begins-with-bom? 'big)))
                         ((eq? (car rest) 'little)
                          (if mandatory?
                            'little
                            (or begins-with-bom? 'little)))
                         (else (apply complain
                                      'utf32->string
                                      c-bytevector
                                      rest))))

       (begins-with-bom? (if mandatory? #f begins-with-bom?))

       (endianness (if mandatory? (car rest) endianness))

       (i0 (if begins-with-bom? 4 0))

       (result (if (zero? (remainder n 4))
                 (make-string (quotient (- n i0) 4))
                 (complain
                   "c-bytevector passed to utf32->string has bad length"
                   c-bytevector))))

  (do ((i i0 (+ i 4))
       (j 0 (+ j 1)))
    ((= i n) result)
    (let* ((sv (c-bytevector-u32-ref c-bytevector i endianness))
           (sv (cond ((< sv #xd800) sv)
                     ((< sv #xe000) #xfffd) ; replacement character
                     ((< sv #x110000) sv)
                     (else #xfffd)))        ; replacement character
           (c (integer->char sv)))
      (string-set! result j c)))))
)
)
