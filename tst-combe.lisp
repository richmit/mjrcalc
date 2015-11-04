;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-combe.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-combe.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_COMBE-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_COMBE :MJR_PRNG))

(in-package :MJR_COMBE-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_partitions-naive (n)
  "The number of partitions of N.

A partition of a number, $n$, is a sum of positive integers equaling $n$. Sums differing only in the order of the summands aren't counted as distinct."
  (cond ((not (integerp n)) (error "mjr_combe_partition: Argument must be an integer!"))
        ((< n 0)            (error "mjr_combe_partition: Argument must be non-negative!")))
  (mjr_combe_partition-into-parts-of-size-ge-k n 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_perm-naive (n k)
  ""
  (/ (mjr_combe_! n) (mjr_combe_! (- n k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_comb-naive0 (n k)
  ""
  (cond ((= k 0) 1)
        ((= n 0) 0)
        ((< n k) 0) ;; 0 ways to select more objects than we have
        ('t      (/ (mjr_combe_! n) (* (mjr_combe_! k) (mjr_combe_! (- n k)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_comb-naive1 (n k)
  (cond ((= k 0) 1)
        ((= n 0) 0)
        ((< n k) 0) ;; 0 ways to select more objects than we have
        ('t      (/ (mjr_combe_perm n k) (mjr_combe_! k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_comb-naive2 (n k)
  "Only works for n<=31.

Makes sophisticated use of Pascal's triangle -- only stores one row at a time, requires on storage beyond the triangle, and only computes the parts of the
rows required (both sides of the symmetry are computed as that is cheaper than fancy indexing to avoid the single addition required for each cell beyond the
halfway point).  Requires only integer arithmetic.  One of the best options for small, fixed size integers. Great for C, slow for LISP."
  (if (< (- n k) k)
      (mjr_combe_comb-naive2 n (- n k))
      (cond ((= k 0) 1)
            ((= k 1) n)
            ((= n 0) 0)
            ((< n k) 0) ;; 0 ways to select more objects than we have
            ('t      (let ((b (make-array (max 4 (1+ k)) :element-type 'fixnum :initial-element 0)))
                       (setf (aref b 0) 1
                             (aref b 1) 3
                             (aref b 2) 3
                             (aref b 3) 1)
                       (loop for r from 4 upto n
                             finally (return (aref b k))
                             do (loop for c from (min k (1+ r)) downto (max 1 (- k (- n r)))
                                      do (incf (aref b c) (aref b (1- c))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_central-comb-naive (n)
  "central binomial coefficient for integer arguments"
  (mjr_combe_comb (* 2 n) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_multinomial-naive (n &rest k-list)
  ""
  (/ (mjr_combe_! n)
     (reduce #'* (mapcar #'mjr_combe_! k-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_stirling2nd-naive (n k)
  "Recursive implementation

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol I"
  (cond ((and (= n 0) (= k 0)) 1)
        ((= n 0)               0)
        ((= k 0)               0)
        ((> k n)               0)
        ('t                    (+ (* k (mjr_combe_stirling2nd-naive (1- n) k)) (mjr_combe_stirling2nd-naive (1- n) (1- k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_euler1st-naive (n k)
  "Slow, recursive implementation"
  (cond ((=  k 0)        1)
        ((=  k (1- n))   1)
        ((>= k n)        0)
        ((< (- n k 1) k) (mjr_combe_euler1st-naive n (- n k 1)))
        ('t              (+ (* (- n k) (mjr_combe_euler1st-naive (1- n) (1- k)))
                            (* (1+  k) (mjr_combe_euler1st-naive (1- n) k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_catalan-naive (n)
  "Compute the N'th Catalan number.

Interpretations:
  * ???"
  (/ (mjr_combe_central-comb n) (1+ n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_bell-naive (n)
  ;; MJR TODO NOTE <2011-11-11 13:54:55 CST> mjr_combe_bell-naive: CHECK THIS!!

  ""
  (mjr_combe_12way-lu-all n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_!
  ;; http://oeis.org/A000142
  (loop for i from 0
        for b in '(1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000
                   20922789888000 355687428096000 6402373705728000 121645100408832000 2432902008176640000)
        do (assert-equal b (mjr_combe_! i) i))
  ;; Errors
  (assert-error 'error     (mjr_combe_!  -1))
  (assert-error 'error     (mjr_combe_!  #C(1 1)))
  (assert-error 'error     (mjr_combe_!  't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_!!
  ;; http://oeis.org/A006882
  (loop for i from 0
        for b in '(1 1 2 3 8 15 48 105 384 945 3840 10395 46080 135135 645120 2027025 10321920 34459425 185794560 654729075
                   3715891200 13749310575 81749606400 316234143225 1961990553600 7905853580625 51011754393600)
        do (assert-equal b (mjr_combe_!! i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_perm
  ;; One line of Pascal's triangle
  (assert-equal 1                           (mjr_combe_perm   4   0))
  (assert-equal 4                           (mjr_combe_perm   4   1))
  (assert-equal 12                          (mjr_combe_perm   4   2))
  (assert-equal 24                          (mjr_combe_perm   4   3))
  (assert-equal 24                          (mjr_combe_perm   4   4))
  ;; Normal use
  (assert-equal 187333454629601280000       (mjr_combe_perm  22  19))
  (assert-equal 42597073035662745600000     (mjr_combe_perm  30  17))
  (assert-equal 6288139352883548160000      (mjr_combe_perm  31  16))
  (assert-equal 3497296636753920000         (mjr_combe_perm  28  14))
  (assert-equal 34                          (mjr_combe_perm  34   1))
  (assert-equal 1                           (mjr_combe_perm  34   0))
  (assert-equal 125318793600                (mjr_combe_perm  28   8))
  (assert-equal 62990928000                 (mjr_combe_perm  26   8))
  (assert-equal 8515157028618240000         (mjr_combe_perm  21  18))
  (assert-equal 225769355325930913136640000 (mjr_combe_perm  34  19))
  ;; Check against the naive version
  (dotimes (i 1000)
    (let* ((n (mjr_prng_int-co 1 1000))
           (r (mjr_prng_random n)))
      (assert-equal (mjr_combe_perm n r) (mjr_combe_perm-naive n r))))
  ;; Errors
  (assert-error 'error                      (mjr_combe_perm  -1        5))      ;; bad arg types
  (assert-error 'error                      (mjr_combe_perm  #C(1 1)   5))
  (assert-error 'error                      (mjr_combe_perm  't        5))
  (assert-error 'error                      (mjr_combe_perm  -1        5))
  (assert-error 'error                      (mjr_combe_perm  #C(1 1)   5))
  (assert-error 'error                      (mjr_combe_perm  't        5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_comb-with-replacement
  ;; Normal use with replacement
  (assert-equal 1              (mjr_combe_comb-with-replacement   4   0))
  (assert-equal 4              (mjr_combe_comb-with-replacement   4   1))
  (assert-equal 10             (mjr_combe_comb-with-replacement   4   2))
  (assert-equal 20             (mjr_combe_comb-with-replacement   4   3))
  (assert-equal 35             (mjr_combe_comb-with-replacement   4   4))
  ;; Normal use
  (assert-equal 131282408400   (mjr_combe_comb-with-replacement  22  19))
  (assert-equal 1749695026860  (mjr_combe_comb-with-replacement  30  17))
  (assert-equal 991493848554   (mjr_combe_comb-with-replacement  31  16))
  (assert-equal 35240152720    (mjr_combe_comb-with-replacement  28  14))
  (assert-equal 34             (mjr_combe_comb-with-replacement  34   1))
  (assert-equal 1              (mjr_combe_comb-with-replacement  34   0))
  (assert-equal 23535820       (mjr_combe_comb-with-replacement  28   8))
  (assert-equal 13884156       (mjr_combe_comb-with-replacement  26   8))
  (assert-equal 33578000610    (mjr_combe_comb-with-replacement  21  18))
  (assert-equal 76360380541900 (mjr_combe_comb-with-replacement  34  19))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_comb
  ;; Normal use
  (assert-equal 1540              (mjr_combe_comb  22  19))
  (assert-equal 119759850         (mjr_combe_comb  30  17))
  (assert-equal 300540195         (mjr_combe_comb  31  16))
  (assert-equal 40116600          (mjr_combe_comb  28  14))
  (assert-equal 34                (mjr_combe_comb  34   1))
  (assert-equal 1                 (mjr_combe_comb  34   0))
  (assert-equal 3108105           (mjr_combe_comb  28   8))
  (assert-equal 1562275           (mjr_combe_comb  26   8))
  (assert-equal 1330              (mjr_combe_comb  21  18))
  (assert-equal 1855967520        (mjr_combe_comb  34  19))
  ;; Make sure it is symmetric
  (loop for n from 10 upto 20
        do (loop for k from 0 upto n
                 do (assert-equal (mjr_combe_comb n k) (mjr_combe_comb n (- n k)))))
  ;; Check against the naive version
  (dotimes (i 1000)
    (let* ((n (mjr_prng_int-co 1 1000))
           (r (mjr_prng_random n))
           (c (mjr_combe_comb n r)))
      (assert-equal c (mjr_combe_comb-naive0 n r))
      (assert-equal c (mjr_combe_comb-naive1 n r))))
  ;; Check against the naive version for small arguments
  (dotimes (n 100)
    (dotimes (k n)
      (let ((c (mjr_combe_comb n k)))
        (assert-equal c (mjr_combe_comb-naive0 n k))
        (assert-equal c (mjr_combe_comb-naive1 n k))
        (if (< n 32)
            (assert-equal c (mjr_combe_comb-naive2 n k))))))
  ;; http://oeis.org/A007318
  (loop with kv = #2a((1 0  0  0   0   0   0   0   0   0  0  0)
                      (1 1  0  0   0   0   0   0   0   0  0  0)
                      (1 2  1  0   0   0   0   0   0   0  0  0)
                      (1 3  3  1   0   0   0   0   0   0  0  0)
                      (1 4  6  4   1   0   0   0   0   0  0  0)
                      (1 5  10 10  5   1   0   0   0   0  0  0)
                      (1 6  15 20  15  6   1   0   0   0  0  0)
                      (1 7  21 35  35  21  7   1   0   0  0  0)
                      (1 8  28 56  70  56  28  8   1   0  0  0)
                      (1 9  36 84  126 126 84  36  9   1  0  0)
                      (1 10 45 120 210 252 210 120 45  10 1  0)
                      (1 11 55 165 330 462 462 330 165 55 11 1))
        for n from 0 upto (1- (array-dimension kv 0))
        do (loop for k from 0 upto (1- (array-dimension kv 1))
                 do (assert-equal (aref kv n k) (mjr_combe_comb n k))))
  ;; Errors
  (assert-error 'error            (mjr_combe_comb  -1        5))      ;; bad arg types
  (assert-error 'error            (mjr_combe_comb  #C(1 1)   5))
  (assert-error 'error            (mjr_combe_comb  't        5))
  (assert-error 'error            (mjr_combe_comb  -1        5))
  (assert-error 'error            (mjr_combe_comb  #C(1 1)   5))
  (assert-error 'error            (mjr_combe_comb  't        5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_central-comb
  (dotimes (i 1000)
    (let* ((n (mjr_prng_int-co 1 500)))
      (assert-equal (mjr_combe_central-comb-naive n) (mjr_combe_central-comb n))))
    ;; http://oeis.org/A000984
    (loop for i from 0
          for b in '(1 2 6 20 70 252 924 3432 12870 48620 184756 705432 2704156 10400600 40116600 155117520 601080390
                     2333606220 9075135300 35345263800 137846528820 538257874440 2104098963720 8233430727600 32247603683100)
          do (assert-equal b (mjr_combe_central-comb i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_multinomial
  (assert-equal 3     (mjr_combe_multinomial  3 2 0 1))
  (assert-equal 6     (mjr_combe_multinomial  3 1 1 1))
  (assert-equal 34650 (mjr_combe_multinomial 11 1 4 4 2))
  ;; Random tests
  (dotimes (i 1000)
    (let* ((n (mjr_prng_int-co 1 400))
           (k (loop for j from 0 upto (mjr_prng_random 5)
                    collect (mjr_prng_random 200)))
           (s (reduce #'+ k)))
      (assert-equal (apply #'mjr_combe_multinomial n k)       (apply #'mjr_combe_multinomial n k) n k)
      (assert-equal (apply #'mjr_combe_multinomial (+ n s) k) (apply #'mjr_combe_multinomial (+ n s) k) (+ n s) k)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_stirling2nd
  (assert-equal 1     (mjr_combe_stirling2nd 0  0))
  (assert-equal 0     (mjr_combe_stirling2nd 6  0))
  (assert-equal 0     (mjr_combe_stirling2nd 0  2))
  ;; Check against the naive version for small arguments
  (dotimes (n 20)
    (dotimes (k (* 2 n))
      (assert-equal (mjr_combe_stirling2nd-naive n k) (mjr_combe_stirling2nd n k))))
  ;; http://oeis.org/A008277
  (loop with kv = #2a((1 0   0    0     0     0     0    0   0  0)
                      (1 1   0    0     0     0     0    0   0  0)
                      (1 3   1    0     0     0     0    0   0  0)
                      (1 7   6    1     0     0     0    0   0  0)
                      (1 15  25   10    1     0     0    0   0  0)
                      (1 31  90   65    15    1     0    0   0  0)
                      (1 63  301  350   140   21    1    0   0  0)
                      (1 127 966  1701  1050  266   28   1   0  0)
                      (1 255 3025 7770  6951  2646  462  36  1  0)
                      (1 511 9330 34105 42525 22827 5880 750 45 1))
        for n from 1 upto  (array-dimension kv 0)
        do (loop for k from 1 upto (array-dimension kv 1)
                 do (assert-equal (aref kv (1- n) (1- k)) (mjr_combe_stirling2nd n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_stirling1st-XXX
  ;; http://oeis.org/A008275
  (loop with kv = #2a((1     0       0      0      0     0     0   0   0  0)
                      (0     1       0      0      0     0     0   0   0  0)
                      (0    -1       1      0      0     0     0   0   0  0)
                      (0     2      -3      1      0     0     0   0   0  0)
                      (0    -6      11     -6      1     0     0   0   0  0)
                      (0    24     -50     35    -10     1     0   0   0  0)
                      (0  -120     274   -225     85   -15     1   0   0  0)
                      (0   720   -1764   1624   -735   175   -21   1   0  0)
                      (0 -5040   13068 -13132   6769 -1960   322 -28   1  0)
                      (0 40320 -109584 118124 -67284 22449 -4536 546 -36  1))
        for n from 0 upto (1- (array-dimension kv 0))
        do (loop for k from 0 upto (1- (array-dimension kv 1))
                 do (assert-equal (aref kv n k)       (mjr_combe_stirling1st n k))
                 do (assert-equal (abs (aref kv n k)) (mjr_combe_stirling1st-unsigned n k))))
  ;; Consistency between mjr_combe_stirling1st mjr_combe_stirling1st-unsigned
  (dotimes (i 150)
    (let ((n (mjr_combe_stirling1st-unsigned 0 20))
          (k (mjr_combe_stirling1st-unsigned 0 20)))
      (assert-equal (abs (mjr_combe_stirling1st n k)) (mjr_combe_stirling1st-unsigned n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_stirling1st-unsigned
  ;; See: mjr_combe_stirling1st-XXX
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_stirling1st
  ;; See: mjr_combe_stirling1st-XXX
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_euler1st
  (dotimes (n 25)
    (dotimes (k 25)
      (assert-equal (mjr_combe_euler1st n k) (mjr_combe_euler1st-naive n k))))
  ;; http://oeis.org/A008292
  (loop with kv = #2a((1 0   0     0     0      0     0     0   0 0)
                      (1 0   0     0     0      0     0     0   0 0)
                      (1 1   0     0     0      0     0     0   0 0)
                      (1 4   1     0     0      0     0     0   0 0)
                      (1 11  11    1     0      0     0     0   0 0)
                      (1 26  66    26    1      0     0     0   0 0)
                      (1 57  302   302   57     1     0     0   0 0)
                      (1 120 1191  2416  1191   120   1     0   0 0)
                      (1 247 4293  15619 15619  4293  247   1   0 0)
                      (1 502 14608 88234 156190 88234 14608 502 1 0))
        for n from 0 upto (1- (array-dimension kv 0))
        do (loop for k from 0 upto (1- (array-dimension kv 1))
                 do (assert-equal (aref kv n k) (mjr_combe_euler1st-naive n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_k-partitions
  ;; http://oeis.org/A008284
  (loop with kv = #2a((1 0 0  0  0  0  0  0 0 0 0 0 0)
                      (1 1 0  0  0  0  0  0 0 0 0 0 0)
                      (1 1 1  0  0  0  0  0 0 0 0 0 0)
                      (1 2 1  1  0  0  0  0 0 0 0 0 0)
                      (1 2 2  1  1  0  0  0 0 0 0 0 0)
                      (1 3 3  2  1  1  0  0 0 0 0 0 0)
                      (1 3 4  3  2  1  1  0 0 0 0 0 0)
                      (1 4 5  5  3  2  1  1 0 0 0 0 0)
                      (1 4 7  6  5  3  2  1 1 0 0 0 0)
                      (1 5 8  9  7  5  3  2 1 1 0 0 0)
                      (1 5 10 11 10 7  5  3 2 1 1 0 0)
                      (1 6 12 15 13 11 7  5 3 2 1 1 0)
                      (1 6 14 18 18 14 11 7 5 3 2 1 1))
        for n from 1 upto (array-dimension kv 0)
        do (loop for k from 1 upto (array-dimension kv 1)
                 do (assert-equal (aref kv (1- n) (1- k)) (mjr_combe_k-partitions n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_partition-into-parts-of-size-ge-k
  ;; http://oeis.org/A026807
  (loop with kv = #2a((1   0  0  0 0 0 0 0 0 0 0 0 0)
                      (2   1  0  0 0 0 0 0 0 0 0 0 0)
                      (3   1  1  0 0 0 0 0 0 0 0 0 0)
                      (5   2  1  1 0 0 0 0 0 0 0 0 0)
                      (7   2  1  1 1 0 0 0 0 0 0 0 0)
                      (11  4  2  1 1 1 0 0 0 0 0 0 0)
                      (15  4  2  1 1 1 1 0 0 0 0 0 0)
                      (22  7  3  2 1 1 1 1 0 0 0 0 0)
                      (30  8  4  2 1 1 1 1 1 0 0 0 0)
                      (42  12 5  3 2 1 1 1 1 1 0 0 0)
                      (56  14 6  3 2 1 1 1 1 1 1 0 0)
                      (77  21 9  5 3 2 1 1 1 1 1 1 0)
                      (101 24 10 5 3 2 1 1 1 1 1 1 1))
        for n from 1 upto (array-dimension kv 0)
        do (loop for k from 1 upto (array-dimension kv 1)
                 do (assert-equal (aref kv (1- n) (1- k)) (mjr_combe_partition-into-parts-of-size-ge-k n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_inversion-number
  (assert-equal 0  (mjr_combe_inversion-number '(0 1 2 3 4)))
  (assert-equal 0  (mjr_combe_inversion-number #(0 1 2 3 4)))
  (assert-equal 4  (mjr_combe_inversion-number #(1 2 3 4 0)))
  (assert-equal 10 (mjr_combe_inversion-number #(4 3 2 1 0)))
  (assert-equal 2  (mjr_combe_inversion-number #(1 0 2 4 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_descent-number
  (assert-equal 0 (mjr_combe_descent-number '(0 1 2 3 4)))
  (assert-equal 0 (mjr_combe_descent-number #(0 1 2 3 4)))
  (assert-equal 1 (mjr_combe_descent-number #(1 2 3 4 0)))
  (assert-equal 4 (mjr_combe_descent-number #(4 3 2 1 0)))
  (assert-equal 2 (mjr_combe_descent-number #(1 0 2 4 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_bell
  ;; http://oeis.org/A000110
  (loop for i from 0
        for b in '(1 1 2 5 15 52 203 877 4140 21147 115975 678570 4213597 27644437 190899322 1382958545 10480142147 82864869804
                   682076806159 5832742205057 51724158235372 474869816156751 4506715738447323 44152005855084346)
        do (assert-equal b (mjr_combe_bell  i)))
  ;; Check against naive version
  (loop for n from 0 upto 10
        do (assert-equal (mjr_combe_bell n) (mjr_combe_bell-naive n)))

  (assert-equal 35742549198872617291353508656626642567                 (mjr_combe_bell 42))
  (assert-equal 359334085968622831041960188598043661065388726959079837 (mjr_combe_bell 55))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_catalan
  ;; http://oeis.org/A000108
  (loop for i from 0
        for b in '(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845 35357670 129644790 477638700
                   1767263190 6564120420 24466267020 91482563640 343059613650 1289904147324 4861946401452 18367353072152
                   69533550916004 263747951750360 1002242216651368 3814986502092304)
        do (assert-equal b (mjr_combe_catalan i)))
  ;; Check against naive version
  (loop for n from 0 upto 300
        do (assert-equal (mjr_combe_catalan-naive n) (mjr_combe_catalan n)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_partitions
  ;; http://oeis.org/A000041
  (loop for i from 0
        for b in '(1 1 2 3 5 7 11 15 22 30 42 56 77 101 135 176 231 297 385 490 627 792 1002 1255 1575 1958 2436 3010 3718 4565
                   5604 6842 8349 10143 12310 14883 17977 21637 26015 31185 37338 44583 53174 63261 75175 89134)
        do (assert-equal b (mjr_combe_partitions       i)))
  (loop for i from 46 upto 55
        do (assert-equal (mjr_combe_partitions-naive i) (mjr_combe_partitions i)))
  (assert-equal 190569292 (mjr_combe_partitions 100))
; (assert-equal 3972999029388                    (mjr_combe_partitions 200))
; (assert-equal 24061467864032622473692149727991 (mjr_combe_partitions 1000))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_derangements
  ;; http://oeis.org/A000166
  (loop for i from 0
        for b in '(1 0 1 2 9 44 265 1854 14833 133496 1334961 14684570 176214841 2290792932 32071101049 481066515734
                   7697064251745 130850092279664 2355301661033953 44750731559645106 895014631192902121 18795307255050944540)
        do (assert-equal b (mjr_combe_derangements i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_rencontres
  ;; http://oeis.org/A008290
  (loop with kv = #2a((1      0      0      0      0      0      0      0      0      0)
                      (0      1      0      0      0      0      0      0      0      0)
                      (1      0      1      0      0      0      0      0      0      0)
                      (2      3      0      1      0      0      0      0      0      0)
                      (9      8      6      0      1      0      0      0      0      0)
                      (44     45     20     10     0      1      0      0      0      0)
                      (265    264    135    40     15     0      1      0      0      0)
                      (1854   1855   924    315    70     21     0      1      0      0)
                      (14833  14832  7420   2464   630    112    28     0      1      0)
                      (133496 133497 66744  22260  5544   1134   168    36     0      1))
        for n from 0 upto (1- (array-dimension kv 0))
        do (loop for k from 0 upto (1- (array-dimension kv 1))
                 do (assert-equal (aref kv n k) (mjr_combe_rencontres n k) (list n k))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_rencontres-mjr_combe_derangements
  (loop for n from 0 upto 100
        do (assert-equal (mjr_combe_derangements n) (mjr_combe_rencontres n 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combe_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 '(
   mjr_combe_comb
   )
 )
