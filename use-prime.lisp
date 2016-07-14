;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-prime.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Computational Number Theory.@EOL
;; @std       Common Lisp
;; @see       tst-prime.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1992,1994,1997,1998,2004,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;; @todo      mjr_prime_pimep-miller-rabin-deterministic: Correct refs and fix name.@EOL@EOL
;; @todo      mjr_prime_pimep-miller-rabin-deterministic: Correct refs.@EOL@EOL
;; @todo      mjr_prime_probable-primep-lucas-selfridge: Finish documentation string.@EOL@EOL
;; @todo      mjr_prime_probable-primep-lucas-selfridge: Add alternative selection for D.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PRIME
  (:USE :COMMON-LISP
        :MJR_INTU
        :MJR_PRNG)
  (:DOCUMENTATION "Brief: Computational Number Theory.;")
  (:EXPORT #:mjr_prime_help

           #:mjr_prime_init-small-prime-list

           #:mjr_prime_pollard-rho #:mjr_prime_find-a-big-prime-factor

           #:mjr_prime_strong-probable-primep

           #:mjr_prime_probable-primep-lucas-selfridge #:mjr_prime_probable-primep-miller-rabin #:mjr_prime_probable-primep-bpsw

           #:mjr_prime_probable-primep

           #:mjr_prime_pimep-miller-rabin-deterministic #:mjr_prime_primep-trial-div #:mjr_prime_primep-small

           #:mjr_prime_primep

           #:mjr_prime_prime-factorization #:mjr_prime_big-prime-factorization #:mjr_prime_small-prime-factorization

           #:mjr_prime_nth-small-prime #:mjr_prime_random-small-prime #:mjr_prime_small-prime-index #:mjr_prime_next #:mjr_prime_prev

           #:mjr_prime_num-factors #:mjr_prime_phi-func #:mjr_prime_n-from-factors #:mjr_prime_all-factors #:mjr_prime_square-free?

           #:mjr_prime_perfect? #:mjr_prime_abundant? #:mjr_prime_deficient?

           #:mjr_prime_legendre-symbol #:mjr_prime_jacobi-symbol

           #:mjr_prime_pi-func

           #:mjr_prime_lucas-sequence #:mjr_prime_lucas-number #:mjr_prime_fibonacci-number #:mjr_prime_pell-number
           #:mjr_prime_pell-lucas-number #:mjr_prime_jacobsthal-lucas-number #:mjr_prime_jacobsthal-number
           ))

(in-package :MJR_PRIME)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_help ()
"Help for MJR_PRIME:

A great deal of intellectual effort has been expended to develop efficient computational number theory algorithms that are capable of efficiently manipulating
very large integers, and the result is the availability today of several very high quality software packages targeted at this space; however, the efficiency
and capability of these packages comes at a price of usability and simplicity.  The result is that very few high quality packages exist that are both
optimized and simple to use for the much less sexy problem of smaller number computational number theory.  This library aims to fill this gap and provide very
fast solutions to 'very small' (less than 10 decimal digits) number problems like factorization and provide reasonably useful, but by no means optimal,
routines for slightly larger (less than 20 decimal digits).  Outside of number theory and cryptographic circles, such smallish numbers are the rule rather
than the exception so this library fits nicely into such applications as finding the rational roots of a typical integer polynomial -- how many polynomials do
you see with coefficients containing thousands of digits? :)

  * Small prime list utilities ......... init-small-prime-list
  * Single factor/divisor finding ...... pollard-rho find-a-big-prime-factor
  * Primality Testing .................. primep primep-miller-rabin primep-trial-div primep-small
  * Prime Factoring .................... prime-factorization big-prime-factorization small-prime-factorization
  * Prime Number sequence .............. nth-small-prime random-small-prime small-prime-index next prev
  * Tricks with prime factorizations ... num-factors phi-func n-from-factors all-factors
  * Number Theory Functions ............ pi-func"
  (documentation 'mjr_prime_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_prime_small-list* nil
  "An array of the first few prime numbers.")
(defvar *mjr_prime_small-bitmap* nil
  "An bitmap of the first few prime numbers.")
(defvar *mjr_prime_small-count* 0
  "Number of prime numbers in the *mjr_prime_small-list* array.  Normally 6 million, but may be smaller")
(defvar *mjr_prime_small-max* 0
  "Largest prime number in the *mjr_prime_small-list* array")
(proclaim '(type fixnum *mjr_prime_small-max* *mjr_prime_small-count*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_sieve-int2idx (n)
  "Return the index by which the number may be found in the prime number bitmap or nil if it isn't in the bitmap"
  (declare (type fixnum n))
  (if (and (oddp n) (> n 6))
      (let* ((tmp      (mod n 30))
             (bit-idx  (case tmp
                         (1         7)
                         (7         0)
                         (11        1)
                         (13        2)
                         (17        3)
                         (19        4)
                         (23        5)
                         (29        6)
                         (otherwise nil))))
        (if bit-idx
            (let ((byte-idx (truncate (- n 7) 30)))
              (+ (* 8 byte-idx) bit-idx))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_init-small-prime-list (&optional (max-prime-can 104395301))
  "Initialize *MJR_PRIME_SMALL-LIST*, *MJR_PRIME_SMALL-COUNT*, and *MJR_PRIME_SMALL-MAX*.

It is not necessary to initialize these variables; however, several of the functions in this package take advantage of these variables to achieve significant
performance gains.

The default value for MAX-PRIME-CAN will attempt to compute the first 6 MILLION prime numbers, or at least as many as possible based upon
ARRAY-DIMENSION-LIMIT. This should take but a moment with modern hardware."
  (declare (type fixnum max-prime-can))
  (let* ((max-prime-can (loop for i from (max max-prime-can 36)
                              for j = (mjr_prime_sieve-int2idx i)
                              when (and j (zerop (mod (1+ j) 8)))
                              do (return i)))
         (m             (1+ (mjr_prime_sieve-int2idx max-prime-can))))
    (if (> m array-dimension-limit)
        (error "mjr_prime_init-small-prime-list: The array size limit is too small for such a large sieve!"))
    (let ((abv (make-array m :element-type 'bit  :initial-element 1)))
      (loop with wheel = #(4 2 4 2 4 6 2 6)
            with max-i = (+ 1 (isqrt max-prime-can))
            for k from -1
            for i fixnum = 7 then (+ i (aref wheel (mod k 8)))
            for iidx = (mjr_prime_sieve-int2idx i)
            until (> i max-i)
            when iidx
            do (if (= 1 (bit abv iidx))
                   (loop for j fixnum from (* i 3) upto max-prime-can by (* 2 i)
                         for jidx = (mjr_prime_sieve-int2idx j)
                         when jidx
                         do (setf (bit abv jidx) 0))))
      (let ((bpl (make-array (+ 3 (loop for b of-type bit across abv when (= b 1) count 1)) :element-type 'fixnum :initial-element 0)))
        (dotimes (i 3)
          (setf (aref bpl i) (nth i (list 2 3 5))))
        (loop with bpli = 2
              for i fixnum from 7 upto max-prime-can by 2
              for iidx = (mjr_prime_sieve-int2idx i)
              when (and iidx (= (aref abv iidx) 1))
              do (setf (aref bpl (incf bpli)) i))
        (setq *mjr_prime_small-list* bpl)
        (setq *mjr_prime_small-count* (length *mjr_prime_small-list*))
        (setq *mjr_prime_small-max* (aref *mjr_prime_small-list* (1- *mjr_prime_small-count*)))
        (setq *mjr_prime_small-bitmap* abv)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_pollard-rho (n &key (max-seq 10) (max-seq-len nil) (mon-seq-c nil) (seq-0-0 nil) (show-progress nil))
  "Use the Pollard-Rho algorithm to search for a factor of n.  Returns nil if no factor is found.

Set :SHOW-PROGRESS non-NIL to print progress information as the algorithm runs.
Set :MON-SEQ-C non-NIL to use use a fixed monotone sequence for c (1, 2, 3) instead of a random value
Set :SEQ-0-0 non-NIL to always start each sequence at zero instead of a random value
Set :MAX-SEQ to the maximum number of sequences to try before giving up.  Set it to nil to never stop trying.
Set :MAX-SEQ-LEN to the maximum number of iterations in each sequence.  Set it to nil to exhaust each sequence.

Notes:
  * A NIL return doesn't mean the number is prime, just that MJR_PRIME_POLLARD-RHO didn't find a factor.
  * If MJR_PRIME_POLLARD-RHO fails to find a factor, then just try again a few times.
  * If MJR_PRIME_POLLARD-RHO repeatedly fails to find a factor, then try :MON-SEQ-C, :SEQ-0-0, and/or :MAX-SEQ.
  * The factor returned is NOT always prime!
  * If N<=3, nil is ALWAYS the result!
  * If N is prime, then nil is ALWAYS the result.

References:
  Crandall and Pomerance (2005); Prime numbers: A computational perspective 2nd Ed; ISBN: 0387252827; p230"
  (if (> n 4)
      (loop for i fixnum from 1
            for c = (if mon-seq-c i (mjr_prng_int-co 1 (- n 2))) ;; WAS: (1+ (Random (- n 3)))
            until (and max-seq (> i max-seq))
            until (and mon-seq-c (> c (- n 3)))
            do (flet ((nxt-rnd (x) (mod (+ (* x  x) c) n)))
                 (loop with x0 = (if seq-0-0 0 (mjr_prng_random n))
                       for k from 1
                       for x = (nxt-rnd x0)           then (nxt-rnd x)
                       for y = (nxt-rnd (nxt-rnd x0)) then (nxt-rnd (nxt-rnd y))
                       for g = (gcd (- x y) n)
                       do (if (and max-seq-len (> k max-seq-len)) (return nil))
                       do (if show-progress (format 't "~30d ~5d ~30d ~30d ~5d ~%" c k x y g))
                       do (if (= x y) (return nil))
                       do (if (not (= g 1)) (return-from mjr_prime_pollard-rho g)))))
      (if (= n 4) 2)))

; Test runs for MJR_PRIME_POLLARD-RHO:
;
; n=1241                   n= 527
; c  k    x     y   gcd    c  k    x     y   gcd
; 1  1    1     2     1    1  1    1     2     1
; 1  2    2    26     1    1  2    2    26     1
; 1  3    5   401     1    1  3    5   367     1
; 1  4   26   801     1    1  4   26   274    31
; 1  5  677    26     1
; 1  6  401   401  1241
; 2  1    2     6     1
; 2  2    6   205     1
; 2  3   38   589     1
; 2  4  205     1    17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_strong-probable-primep (n a &optional s d)
  "Return 't if $n$, an odd integer greater than 3, is a strong probable prime base $a$.

The $s$ and $d$ arguments should be integers such that $n=1+2^s\\cdot d$ and $d$ is odd.  If $s$ or $d$ is nil, then they will BOTH be computed.

If a is a list, then 't is returned if and only if n is a strong probable prime with respect to each element of the list.
If a negative, then 't is returned if and only if n is a strong probable prime with respect to each integer in [2,k+2].

Artjuhov (1966)
Artjuhov (1967)"
  (if (< n 4)
      (or (= n 2) (= n 3))
      (if (oddp n)
          (if (not (and s d))
              (multiple-value-bind (s d) (loop for s from 0
                                               for d = (1- n) then (/ d 2)
                                               when (oddp d)
                                               do (return (values s d)))
                (mjr_prime_strong-probable-primep n a s d))
              (if (integerp a)
                  (if (< 0 a)
                      (let ((b (mjr_intu_mod-expt a d n)))
                        (or (= b 1)
                            (= b (1- n))
                            (loop for j from 1 upto (- s 1)
                                  do (setq b (mjr_intu_mod-expt b 2 n))
                                  do (if (= b (1- n)) (return 't)))))
                      (not (loop for i from 1 upto (- a)
                                 do (if (not (mjr_prime_strong-probable-primep n (1+ i) s d))
                                        (return 't)))))
                  (every (lambda (a) (mjr_prime_strong-probable-primep n a s d)) a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_pimep-miller-rabin-deterministic (n &optional assume-riemann-hypothesis)
  "Deterministic Miller-Rabin primality test.  Return is non-NIL if n is PROVED prime, and NIL otherwise.

For n<3825123056546413051, n will be tested via mjr_prime_strong-probable-primep on basis values known to prove primality.  For larger n,
mjr_prime_strong-probable-primep will still be used but for all bases in the interval [2, min(2*(ln(N))^2, N-1)] if assume-riemann-hypothesis is NIL and in
the interval [2, min(isqrt(N), N-1)] otherwise.  Note that things get quite slow when n>=3825123056546413051.

References:
  Jonathan P. Sorenson and Jonathan Webster (2015); Strong Pseudoprimes to Twelve Prime Bases; arXiv:1509.00864
  Yupeng Jiang and Yingpu Deng (2012); Strong pseudoprimes to the first 9 prime bases; arXiv:1207.0063v1
  Zhenxiang Zhang and Min Tang (2003); Finding strong pseudoprimes to several bases. II; Mathematics of Computation
  Crandall and Pomerance (2005); Prime numbers: A computational perspective 2nd Ed; ISBN: 0387252827; p230
  Gary Miller (1976); Riemann's Hypothesis and Tests for Primality; Journal of Computer and System Sciences 13; DOI: 10.1145/800116.803773
  Pomerance, Selfridge, and Wagstaff (1980); The pseudoprimes to 25*10^9; Mathematics of Computation 35; DOI: 10.2307/2006210
  Gerhard Jaeschke (1993); On strong pseudoprimes to several bases; Mathematics of Computation 61; DOI: 10.2307/2153262"
  (mjr_prime_strong-probable-primep n (cond ((< n 2047)                      (list 2))                                  ;; ?       <=10-bits
                                            ((< n 1373653)                   (list 2 3))                                ;; PSW1980 <=20-bits n!=3
                                            ((< n 9080191)                   (list 31 73))                              ;; J1993   <=23-bits n!=31 & n!=73
                                            ((< n 4759123141)                (list 2 7 61))                             ;; J1993   <=32-bits
                                            ((< n 2152302898747)             (list 2 3 5 7 11))                         ;; J1993   <=40-bits
                                            ((< n 3474749660383)             (list 2 3 5 7 11 13))                      ;; J1993   <=41-bits
                                            ((< n 341550071728321)           (list 2 3 5 7 11 13 17))                   ;; J1993   <=48-bits
                                            ((< n 3825123056546413051)       (list 2 3 5 7 11 13 17 19 23 29 31))       ;; ZT2003  <=61-bits
                                          ;;((< n 318665857834031151167461)  (list 2 3 5 7 11 13 17 19 23 29 31 37)     ;; SW2015  <=78-bits  Result too new to trust. ;)
                                          ;;((< n 3317044064679887385961981) (list 2 3 5 7 11 13 17 19 23 29 31 37 41)  ;; SW2015  <=81-bits  Result too new to trust. ;)
                                            (assume-riemann-hypothesis (- (min (ceiling (* 2 (expt (log n) 2))) (1- n))))
                                            ('t                        (- (min (ceiling (isqrt n)) (1- n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_probable-primep-miller-rabin (n k)
  "Classical, randomized Miller-Rabin primality test.  Return 't if n is probably prime, and NIL if it is PROVED composite.

When 't is returned, the number prime with probability no less than $4^{2-k}$ assuming none of the tested bases were equal.

Algorithm: Select k bases at random from [2, n-1] and tested via mjr_prime_strong-probable-primep.

References:
  Crandall and Pomerance (2005); Prime numbers: A computational perspective 2nd Ed; ISBN: 0387252827; p230
  Michael Rabin (1980); Probabilistic algorithm for testing primality; Journal of Number Theory 12; DOI: 10.1016/0022-314X(80)90084-0"
  (multiple-value-bind (s d) (loop for s from 0
                                   for d = (1- n) then (/ d 2)
                                   when (oddp d)
                                   do (return (values s d)))
    (not (loop for i from 1 upto k
               do (if (not (mjr_prime_strong-probable-primep n (mjr_prng_int-co 2 (- n 2)) s d))
                      (return 't))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_find-a-big-prime-factor (n)
  "Find a prime factor, or return NIL.

The 'big' in the name indicates that the techniques used work for numbers with factors larger than *MJR_PRIME_SMALL-MAX*, and that *mjr_prime_small-list* is
never used.  That said this function will find small factors too! The current implementation uses MJR_PRIME_POLLARD-RHO and
MJR_PRIME_PRIMEP-MILLER-RABIN-DETERMINISTIC recursively."
  (let ((f (mjr_prime_pollard-rho n)))
    (if f
        (if (mjr_prime_pimep-miller-rabin-deterministic f)
            f
            (mjr_prime_find-a-big-prime-factor f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_big-prime-factorization (n &key show-progress)
  "Return the complete prime factorization of N, as an assoc array with (prime . power) and 1 as the second return value.

The second return value exists to provide compatibility with the return from mjr_prime_small-prime-factorization.

The 'big' in the name indicates that the techniques used work for numbers with factors larger than *MJR_PRIME_SMALL-MAX*, and
THAT *MJR_PRIME_SMALL-LIST* is never used.  That said this function will find all the small factors too! The current implementation uses
MJR_PRIME_FIND-A-BIG-PRIME-FACTOR to find factors and MJR_PRIME_PRIMEP-MILLER-RABIN-DETERMINISTIC to test potential primes"
  (values (loop with n-left = n
                for p = (if (mjr_prime_pimep-miller-rabin-deterministic n-left)
                            n-left
                            (mjr_prime_find-a-big-prime-factor n-left))
                do (if show-progress (format 't "PROGRESS: mjr_prime_small-prime-factorization: ~d ~s~%" n-left p))
                until (= n-left 1)
                when (and p (mjr_intu_divides? p n-left))
                collect (cons p (loop until (not (and (= 0 (mod n-left p)) (setq n-left (/ n-left p))))
                                      count 't)))
          1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_primep-small (n)
  "For N <= *MJR_PRIME_SMALL-MAX*, return NIL if the number is composite and 'T if it is prime.

The second value of the return is to provide compatibility with the return of other primep-like functions.

This function is O(1) with a run time typically measured in micro-seconds with 2008 vintage hardware."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_primep-small: Could not initialize *mjr_prime_small-list*!"))
        ((> n *mjr_prime_small-max*)   (error "mjr_prime_primep-small: The value of N must be less than *mjr_prime_small-max*")))
  (if (> n 0)
      (let ((idx (mjr_prime_sieve-int2idx n)))
        (if (or (and idx (= (aref *mjr_prime_small-bitmap* idx) 1)) (= n 2) (= n 3) (= n 5)) 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_small-prime-factorization (n)
  "Find all the small (<= *MJR_PRIME_SMALL-MAX*) prime factors of n.  The return is an assoc array with (prime . power) and the remaining part of N left
unfactored (if this second part is 1, then the number was completely factored by small primes"
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_small-prime-factorization: Could not initialize *mjr_prime_small-list*!"))
        ((<= n 0)                      (error "mjr_prime_small-prime-factorization: The value of N must be positive")))
  (if (and (<= n *mjr_prime_small-max*) (mjr_prime_primep-small n)) ;; mjr_prime_primep-small has almost no run time cost, so we check..
      (list (cons n 1))
      (let* ((n-left  n)
             (p-list  (loop for p across *mjr_prime_small-list*
                            until (= n-left 1)
                            when (mjr_intu_divides? p n-left)
                            collect (cons p (loop until (not (and (= 0 (mod n-left p)) (setq n-left (/ n-left p))))
                                                  count 't)))))
        (values p-list n-left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_prime-factorization (n)
  "Return the prime factorization of N as an assoc array with (prime . power) and 1 as the second return value.

Use MJR_PRIME_SMALL-PRIME-FACTORIZATION when N is small enough, and MJR_PRIME_BIG-PRIME-FACTORIZATION otherwise."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_prime-factorization: Could not initialize *mjr_prime_small-list*!")))
  (if (<= n *mjr_prime_small-max*)
      (mjr_prime_small-prime-factorization n)
      (mjr_prime_big-prime-factorization n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_primep-trial-div (n)
  "Use trial division to determine if N is prime.  Return NIL if N is composite, return 'T and 1 if it is prime.

The second value of the return is to provide compatibility with the return of other primep-like functions.

This function is quite slow, but it provides a simplistic implementation of a primep-like function with which we may regression test more useful (faster)
implementations."
  (if (> n 0)
      (if (< n 5)
          (if (or (= n 2) (= n 3)) 't)
          (if (oddp n)
              (loop for i from 3 upto (isqrt n) by 2
                    do (if (mjr_intu_divides? i n) (return nil))
                    finally                        (return 't))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_primep (n)
  "Return NIL if N is composite, and 'T if it is prime.

This function uses MJR_PRIME_PRIMEP-SMALL or MJR_PRIME_PRIMEP-MILLER-RABIN-DETERMINISTIC depending on the size of N."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_primep: Could not initialize *mjr_prime_small-list*!")))
  (if (> n 0)
      (if (<= n *mjr_prime_small-max*)
          (mjr_prime_primep-small n)
          (mjr_prime_pimep-miller-rabin-deterministic n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_nth-small-prime (n &optional (end nil))
  "If END is NIL, return the N-th prime.  If END is non-NIL, return an array of the N-th to the end-th prime numbers.
N and END are zero based, and 2 is the zero'th prime. Note that N, END < *MJR_PRIME_SMALL-COUNT*.  NIL is returned if N is out of bounds or if end is a number
and END<N.  If END is too large, then it is clipped to (1-*MJR_PRIME_SMALL-COUNT*)."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_nth-small-prime: Could not initialize *mjr_prime_small-list*!")))
  (if (and (>= n 0) (< n *mjr_prime_small-count*))
      (if end
          (and (<= n end) (subseq *mjr_prime_small-list* n (min end (1- *mjr_prime_small-count*))))
          (aref *mjr_prime_small-list* n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_random-small-prime ()
  "Return a random prime number less than or equal to *MJR_PRIME_SMALL-MAX*."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_random-small-prime: Could not initialize *mjr_prime_small-list*!")))
  (mjr_prime_nth-small-prime (mjr_prng_random *mjr_prime_small-count*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_small-prime-index (p)
  "Return the index of the N-th small prime, or NIL if P is not in *MJR_PRIME_SMALL-MAX*.

Examples:
  * (mjr_prime_small-prime-index 0) => 2
  * (mjr_prime_small-prime-index 1) => 3

The only error case is when *mjr_prime_small-max* can not be initialized."
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_small-prime-index: Could not initialize *mjr_prime_small-list*!")))
  (if (and (>= p 2) (< p *mjr_prime_small-max*))
      (loop with i-low = 0
            with i-hgh = (1- mjr_prime::*mjr_prime_small-count*)
            for  i-gus = (truncate (+ i-hgh i-low) 2)
            for  v-gus = (aref mjr_prime::*mjr_prime_small-list* i-gus)
            when (= v-gus p)           do (return i-gus)
            when (>= (1+ i-low) i-hgh) do (return (error "mjr_prime_small-prime-index: The value of P must be a prime."))
            when (< v-gus p)           do (setf i-low i-gus)
            when (> v-gus p)           do (setf i-hgh i-gus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_next (n)
  "Return the smallest prime number greater than N."
  (loop for i from (1+ n) do (if (mjr_prime_primep i) (return i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_prev (n)
  "Return the largest prime number less than N."
  (loop for i downfrom (1- n) do (if (mjr_prime_primep i) (return i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_pi-func (n)
  "Return pi function value at N.

Also called the prime-counting function.  The result is the number of prime numbers less than or equal to some real number N.

Let $P=\\{p : (p\\leq n) \\wedge (p\\,\\,\\mathrm{prime})\\}$, then $\\pi(n)=\\vert P\\vert$.

Performance notes:
 * For N < *MJR_PRIME_SMALL-MAX*, this function is relatively efficient because it can make use of the recomputed sieve
 * N that are a power of 10 with the exponent power in [0, 23], a look-up table is used
 * N that are a power of 2 with the exponent power in [0, 52], a look-up table is used
 * For other N larger than *MJR_PRIME_SMALL-MAX*, this function is quite slow

References:
  Crandall & Pomerance (2005); Prime numbers: A computational perspective 2nd Ed; ISBN: 0387252827; p152"
  (let ((pow10 (mjr_intu_log n 10)))
    (if (and pow10 (<= pow10 23))
        (nth pow10 (list 0 4 25 168 1229 9592 78498 664579 5761455 50847534 455052511 4118054813 37607912018
                         346065536839 3204941750802 29844570422669 279238341033925 2623557157654233
                         24739954287740860 234057667276344607 2220819602560918840
                         21127269486018731928 201467286689315906290 1925320391606803968923))
        (let ((pow2 (mjr_intu_log n 2)))
          (if (and pow2 (<= pow2 52))
              (nth pow2  (list 0 1 2 4 6 11 18 31 54 97 172 309 564 1028 1900 3512 6542 12251 23000 43390 82025
                               155611 295947 564163 1077871 2063689 3957809 7603553 14630843 28192750 54400028
                               105097565 203280221 393615806 762939111 1480206279 2874398515 5586502348 10866266172
                               21151907950 41203088796 80316571436 156661034233 305761713237 597116381732
                               1166746786182 2280998753949 4461632979717 8731188863470 17094432576778 33483379603407
                               65612899915304 128625503610475))
              (loop for i from 2 upto n when (mjr_prime_primep i) count 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_num-factors (n)
  "Return a list of all the factors (prime and composite) N.  N may be a number or an associative-style prime factorization."
  (if (listp n)
      (reduce #'* (mapcar (lambda (x) (1+ (cdr x))) n))
      (mjr_prime_num-factors (mjr_prime_prime-factorization n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_phi-func (n)
  "Find the number of factors of N.  N may be a number or an associative-style prime factorization.

    * Euler totient function
    * phi(n): count numbers <= n and prime to n.
    * Number of distinct generators of a cyclic group of order n.
    * Number of automorphisms of the cyclic group of order n"
  (if (listp n)
      (reduce #'* (mapcar (lambda (x) (* (expt (car x) (1- (cdr x))) (1- (car x)))) n))
      (mjr_prime_phi-func (mjr_prime_prime-factorization n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_n-from-factors (factor-list)
  "Find the composite number for the given associative-style prime factorization."
  (reduce #'* (mapcar (lambda (x) (expt (car x) (cdr x))) factor-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_all-factors (n)
  "Find all factors of N.   N may be a number or an associative-style prime factorization."
  (if (listp n)
      (let* ((num-fac    (length n))
             (primes     (make-array num-fac :initial-contents (mapcar #'car n)))
             (powers     (make-array num-fac :initial-contents (mapcar #'cdr n)))
             (curpow     (make-array num-fac :initial-element  0)))
        (sort (loop collect (reduce '* (map 'vector 'expt primes curpow))
                    while (loop for i from 0
                                do (cond ((= i num-fac)                       (return nil))
                                         ((= (aref curpow i) (aref powers i)) (setf (aref curpow i) 0))
                                         ('t                                  (return (incf (aref curpow i))))))) #'<))
      (mjr_prime_all-factors (mjr_prime_prime-factorization n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_square-free? (n)
  "Non-NIL if N is square free -- .i.e. not divisible by the square of any prime number."
  (every (lambda (x) (< (cdr x) 2)) (mjr_prime_prime-factorization n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_perfect? (n)
  "Non-NIL if N is square free -- .i.e. it equals the sum of all of its proper divisors

Proper divisors are divisors that are strictly less than N -- contrary to some sources, we call 1 a proper divisor."
  (= n (- (reduce #'+ (mjr_prime_all-factors n)) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_deficient? (n)
  "Non-NIL if N is square free -- .i.e. it is greater than the sum of all of its proper divisors.

Proper divisors are divisors that are strictly less than N -- contrary to some sources, we call 1 a proper divisor."
  (> n (- (reduce #'+ (mjr_prime_all-factors n)) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_abundant? (n)
  "Non-NIL if N is square free -- .i.e. it is less than the sum of all of its proper divisors.

Proper divisors are divisors that are strictly less than N -- contrary to some sources, we call 1 a proper divisor."
  (< n (- (reduce #'+ (mjr_prime_all-factors n)) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_legendre-symbol (a p)
  "Return the Jacobi-Symbol $\left(\frac{a}{p}\right)$ for $a$, an integer, and $p$, a odd, prime integer."
  (if (< a 0)
      (if (= 1 (mod p 4))
          (mjr_prime_legendre-symbol (- a) p)
          (- (mjr_prime_legendre-symbol (- a) p)))
      (if (= 0 (mod a p))
          0
          (if (mjr_intu_quadratic-residue? a p)
              1
              -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_jacobi-symbol (a n)
  "Return the Jacobi-Symbol $\left(\frac{a}{n}\right)$ for $a$, an integer, and $n$, a positive, odd integer."
  (let ((j 1))
    (if (< a 0)                                      ;; Make top positive. Note: (-a/b)=-(a/b) iff n=3(mod 4)
        (progn
          (setq a (- a))
          (if (= (mod n 4) 3)
              (setf j (- j)))))
    (setq a (mod a n))                               ;; Reduce a mod n.  Note: (a/n)=(b/n) if a=b(mod n)
    (loop while (not (zerop a))
          do (loop with nm8 = (mod n 8)              ;; Remove factors of 2 from top. Note: (2/n)=-1 iff n=3(mod 8) or n=5(mod 8)
                   while (evenp a)
                   do (setq a (/ a 2))
                   when (or (= nm8 3) (= nm8 5))
                   do (setf j (- j)))
          do (rotatef a n)                           ;; Swap a & n. Note: (a/b)=-(b/a) iff a=3(mod 4) & n=3(mod 4)
          when (and (= (mod a 4) 3) (= (mod n 4) 3))
          do (setf j (- j))
          do (setq a (mod a n)))                     ;; Reduce a mod n.  Note: (a/n)=(b/n) if a=b(mod n)
    (if (= n 1)
        j
        0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_lucas-sequence (k p q modn retu)
  "Compute the kth number in the Lucas sequence with parameters P & Q.
References:
  Joye & Quisquater (1996); Efficient computation of full Lucas Sequences; electronics letters"
  (flet ((modit (k) (if modn (mod k modn) k)))
    (let ((ui   (modit 1))                      ;; $U_{1}=1$
          (ui+1 (modit P))                      ;; $U_{2}=P$
          (vi   (modit P))                      ;; $V_{1}=P$
          (vi+1 (modit (- (* P P) (* 2 Q)))))   ;; $V_{2}=P^2-2Q$
      (cond ((= k 0) (if retu 0    2))          ;; $U_{0}=0$, $V_{0}=2$
            ((= k 1) (if retu ui   vi))
            ((= k 2) (if retu ui+1 vi+1))
            ((> k 1) (let ((n  (integer-length k))
                           (s  (loop for j from 0
                                     while (not (logbitp j k))
                                     count 1))
                           (Uh 1)
                           (Vl 2)
                           (Vh P)
                           (Ql 1)
                           (Qh 1))
                       (loop for j from (1- n) downto (1+ s)
                             do (setq Ql (modit (* Ql Qh)))
                             do (if (logbitp j k)
                                    (setq Qh (modit (* Ql  Q))
                                          Uh (modit (* Uh  Vh))
                                          Vl (modit (- (* Vh Vl) (* P Ql)))
                                          Vh (modit (- (* Vh Vh) (* 2 Qh))))
                                    (setq Uh (modit (- (* Uh Vl) Ql))
                                          Vh (modit (- (* Vh Vl) (* P Ql)))
                                          Vl (modit (- (* Vl Vl) (* 2 Ql)))
                                          Qh (modit Ql)))
                             finally (setq Ql (modit (* Ql Qh))
                                           Qh (modit (* Ql Q))
                                           Uh (modit (- (* Uh Vl) Ql))
                                           Vl (modit (- (* Vh Vl) (* P Ql)))
                                           Ql (modit (* Ql Qh))))
                       (loop for j from 1 upto s
                             do (setq Uh (modit (* Uh Vl))
                                      Vl (modit (- (* Vl Vl) (* 2 Ql)))
                                      Ql (modit (* Ql Ql))))
                       (if retu
                           (modit Uh)
                           (modit Vl))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_probable-primep-lucas-selfridge (n)
  "

References:
  Crandall & Pomerance (2005); Prime numbers: A computational perspective 2nd Ed; ISBN: 0387252827; p142
     /MJRDOI/9ed1c1463034bf01d90d2dbd3bb1df26
  Baillie & Wagstaff (1980); Lucas Pseudoprimes; Math. Comput. 35, 1391-1417
     /MJRDOI/b02ea0dcd7628245b08775d4893779d2"
  (if (< n 5)
      (or (= n 2) (= n 3))
      (if (and (oddp n) (not (mjr_intu_square? n)) (not (zerop (mod n 3))))  ;; Check for even, perfect squares, and multiples of 3
          (let* ((d (loop for i from 5 by 2
                          for j from 1
                          for dc = (if (oddp j) i (- i))
                          for g = (gcd n dc)
                          for jac = (mjr_prime_jacobi-symbol dc n)
                          do (if (and (< 1 g) (< g n)) (return-from mjr_prime_probable-primep-lucas-selfridge nil))
                          when (= -1 jac)
                          do (return dc)))
                 (q (/ (- 1 d) 4)))
            (and (not (zerop (rem q n)))
                 (zerop (rem (mjr_prime_lucas-sequence (1+ n) 1 q n 't) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_lucas-number (n)
  "Return the L(n) -- the Lucas number for n.  n starts at 1. Sloan A000204"
  (mjr_prime_lucas-sequence n 1 -1 nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_fibonacci-number (n)
  "Return the F(n) -- the Fibonacci number for n. n starts at 0. Sloan A000045"
  (mjr_prime_lucas-sequence n 1 -1 nil 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_pell-number (n)
  "Return the P(n) -- the Pell number for n. n starts at 0. Sloan A000129"
  (mjr_prime_lucas-sequence n 2 -1 nil 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_pell-lucas-number (n)
  "Return the PL(n) -- the Pell number for n. n starts at 0. Sloan A002203
Note: Some sources call these companion pell numbers."
  (mjr_prime_lucas-sequence n 2 -1 nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_jacobsthal-number (n)
  "Return the J(n) -- the Jacobsthal number for n. n starts at 0. Sloan A001045"
  (mjr_prime_lucas-sequence n 1 -2 nil 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_jacobsthal-lucas-number (n)
  "Return the PJ(n) -- the Jacobsthal number for n. n starts at 0. Sloan A014551
Note: Some sources call these pell-jacobsthal numbers."
  (mjr_prime_lucas-sequence n 1 -2 nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_probable-primep-bpsw (n)
  "The BPSW (Baillie, Pomerance, Selfridge, Wagstaff) probable prime test

Algorithm:
 * Process all N < 3 and all even N.
 * Check N for any small prime divisors p < 1000.
 * Perform a Miller-Rabin (strong probable prime) test, base 2, on N.
 * Perform a Lucas-Selfridge test on N, using Lucas sequences with the parameters suggested by Selfridge.

Multiple sources have reported that this test PROVES prime for n<2^64; however, I don't have a good journal article reference for this.  Interestingly enough,
the test implemented here is essentially the same test used in Mathematica for PrimeQ.

References:
    Baillie, Robert, & Wagstaff (1980); Lucas pseudoprimes; Math. Comp. 35
    Pomerance, Selfridge, and Wagstaff (1980); The pseudoprimes to 25*10^9; Mathematics of Computation 35; DOI: 10.2307/2006210
    Pomerance; Are there counterexamples to the Baillie-PSW primality test?; 1984
    Arnault (1997); The Rabin-Monier theorem for Lucas pseudoprimes; Math. Comp. 66"
  (declare (type integer n))
  (if (zerop *mjr_prime_small-count*)
      (mjr_prime_init-small-prime-list))
  (cond ((zerop *mjr_prime_small-count*) (error "mjr_prime_probable-primep-bpsw: Could not initialize *mjr_prime_small-list*!")))
  (if (< n 5)
      (or (= n 2) (= n 3))
      (and (loop for i from 1 upto 1000
                 for p across mjr_prime::*mjr_prime_small-list*
                 while (< p n)
                 when (zerop (rem n p))
                 return nil
                 finally (return 't))
           (mjr_prime_strong-probable-primep n 2)
           (mjr_prime_probable-primep-lucas-selfridge n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_probable-primep (n)
  "Return NIL if N is composite, and 'T if it is probably prime.

Currently this function uses MJR_PRIME_PROBABLE-PIMEP-BPSW"
  (if (and (integerp n) (> n 0))
          (mjr_prime_probable-primep-bpsw n)))
