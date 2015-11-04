;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      pre-rirrf.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Rational Approximations For Irrational Functions.@EOL
;; @std       Common Lisp
;; @see       
;; @copyright 
;;  @parblock
;;  Copyright (c) 1992, 1996, 2008, 2013, 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      unit-tests!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_RIRRF
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_CHK
        :MJR_POLY)
  (:DOCUMENTATION "Brief: Rational Approximations For Irrational Functions.;")
  (:EXPORT #:mjr_rirrf_help

           #:mjr_rirrf_sin-4c

           #:mjr_rirrf_sin-4
           #:mjr_rirrf_sin-2

           #:mjr_rirrf_cos-4
           #:mjr_rirrf_cos-2

           #:mjr_rirrf_tan-4
           #:mjr_rirrf_tan-2

           #:mjr_rirrf_exp-4
           #:mjr_rirrf_exp-2

           #:mjr_rirrf_sqrt-4
           #:mjr_rirrf_sqrt-2

           #:mjr_rirrf_log-4
           #:mjr_rirrf_log-2
           ))

(in-package :MJR_RIRRF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_help ()
  "Help for MJR_RIRRF: Rational Approximations For Irrational Functions.

This library implements completely deterministic, rational approximations for some irrational functions.  By 'deterministic' we mean that a finite sequence of
rational arithmetic operations are used to compute the approximation.  The results will be consistent across platforms so long as arbitrary precision integer
and rational arithmetic are correctly implemented in the LISP being used.

WARNING: Take care to note the ranges upon which the functions are defined.  Example: tan-4 is good on [-1,1], while tan-4 is valid for any value not near a
         singularity.

Some Use Cases:

  * Employ standard test cases involving irrational functions for unit tests of numerical algorithms.  Having a known, rational value that should result from
    a test case is much less difficult to test and better evidence of proper function.

  * Use well known functions to generate graphical results, but also render tabular results that can be checked via unit tests.

  * Avoid round off problems for some numerical schemes involving sin/cos."
  (documentation 'mjr_rirrf_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_sin-4 (x)
  "Rational approximation to sin function.

Uses a low degree polynomial hand fitted to sin and its derivative over 0..2*pi.  pi=355/113 Most accurate near 0, pi, and 2*pi.  Least accurate extremes near
about 0.5 & 5.7."
  (if (< x 0)
      (- (mjr_rirrf_sin-4 (- x)))
      (mjr_poly_eval #(-212675535432514568/96711681202978424765625
                       1882084384358536/30269696777145046875
                       -3755840961706636/6395006361368671875
                       8769991453468/6004700808796875
                       7011036523/1429407056250
                       18757661/3811752150
                       -412170551/2415899250
                       339/252050
                       1
                       0)
                     (multiple-value-bind (quot rem) (truncate x 710/113)
                       (declare (ignore quot))
                       rem))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_sin-4c (x)
  "Rational approximation to sin function on [-355/113, 355/113] good to 4 digits.

Uses a single polynomial hand fitted to sin and its derivative.  pi=355/113.  Zeros at -355/113, 0, and 355/113.  Value of -1 at -355/226, and +1 at 355/226.
Derivative is zero at -355/226 and 355/226.  Derivative is -1 at -355/113 and 355/113, and it is 1 at 0.  The approximation is most accurate near 0, pi, and
2*pi.  The approximation is an odd function."
  (if (or (< x -355/113) (> x 355/113))
      (warn "mjr_rirrf_sin-4c: X should be in [-355/113, 355/113]"))
  (mjr_poly_eval #(212675535432514568/96711681202978424765625
                   0
                   -1240843244554964/6395006361368671875
                   0
                   844422282619/101487900993750
                   0
                   -402619339/2415899250
                   0
                   1
                   0)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_sin-2 (x)
  "Rational approximation to sin function.  

Uses a low degree polynomial hand fitted to sin and its derivative over 0..2*pi.  pi=22/7.  Most accurate near 0, pi, and 2*pi."
  (if (< x 0)
      (- (mjr_rirrf_sin-2 (- x)))
      (mjr_poly_eval #(-5764801/2037266805024
                       823543/10289226288
                       -2235331/2806152624
                       117649/42517464
                       12005/46382688
                       9947/702768
                       -51695/287496
                       7/1452
                       1
                       0)
                     (multiple-value-bind (quot rem) (truncate x 44/7)
                       (declare (ignore quot))
                       rem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_cos-2 (x)
  "Rational approximation to cos function good to 2 digits.

Uses mjr_rirrf_sin-4 internally by shifting this function over 11/7 units -- i.e. pi=22/7"
  (mjr_rirrf_sin-2 (+ x 11/7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_cos-4 (x)
  "Rational approximation to cos function good to 4 digits.

Uses mjr_rirrf_sin-4 internally by shifting this function over 355/226 units -- i.e. pi=355/113"
  (mjr_rirrf_sin-4 (+ x 355/226)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_exp-2 (x)
  "Rational approximation to exp function between 0 and 1 good to 2 digits.

Uses a low degree polynomial hand fitted to exp and its derivative.  Most accurate near 0 and 1."
  (if (or (< x 0) (> x 1))
      (warn "mjr_rirrf_exp-2: X should be in [0, 1]"))
  (mjr_poly_eval #(282/1001
                   437/1001
                   1
                   1)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_exp-4 (x)
  "Rational approximation to exp function between 0 and 1 good to 5 digits.

Uses a low degree polynomial hand fitted to exp and its derivative.  Most accurate near 0, 1/2, and 1."
  (if (or (< x 0) (> x 1))
      (warn "mjr_rirrf_exp-4: X should be in [0, 1]"))
  (mjr_poly_eval #(77816/5596591
                   17852/508781
                   170/1001
                   2795271/5596591
                   1
                   1)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_sqrt-2 (x)
  "Rational approximation to sqrt function between 1/4 and 1 good to *about* 2 digits.

Uses a Chebyshev approximation with rationalized polynomial coefficients."
  (if (or (< x 1/4) (> x 1))
      (warn "mjr_rirrf_sqrt-2: X should be in [1/4, 1]"))
  (mjr_poly_eval #(-2/7 1244/1223 3/11) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_sqrt-4 (x)
  "Rational approximation to sqrt function between 1/4 and 1 good to 5 digits.

Uses a Chebyshev approximation with rationalized polynomial coefficients."
  (if (or (< x 1/4) (> x 1))
      (warn "mjr_rirrf_sqrt-4: X should be in [1/4, 1]"))
  (mjr_poly_eval #( 665/1028
                   -345415/104862
                   648694/89655
                   -101168/11215
                   19613/2754
                   -12199/3140
                   201041/98206
                   40/269)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_tan-4 (x)
  "Rational approximation to sqrt function between -1 and 1 good to 4 digits.

Uses a modified Chebyshev approximation with rationalized polynomial coefficients.  This is an odd function, zero at zero, with derivative of 1 at zero."
  (if (or (< x -1) (> x 1))
      (warn "mjr_rirrf_tan-4: X should be in [-1, 1]"))
  (mjr_poly_eval #(364/16103
                   0
                   -14/471
                   0
                   173/3079
                   0
                   1609/41746
                   0
                   3005/21968
                   0
                   7605/22838
                   0
                   1
                   0)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_tan-2 (x)
  "Rational approximation to tan function good to 2 digits when not near a singularity.

Uses mjr_rirrf_sin-2 and mjr_rirrf_cos-2 internally."
  (/ (mjr_rirrf_sin-2 x) (mjr_rirrf_cos-2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_log-4 (x)
  "Rational approximation to sqrt function between 1/2 and 3/2 good to 4 digits.

Uses a Chebyshev approximation with rationalized polynomial coefficients."
  (if (or (< x 1/2) (> x 3/2))
      (warn "mjr_rirrf_log-4: X should be in [1/2, 3/2]"))
  (mjr_poly_eval #(1307/5631
                   -1271/677
                   13143/2000
                   -130553/10000
                   24821/1526
                   -1192/89
                   4996/625
                   -1185/433)
                 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rirrf_log-2 (x)
  "Rational approximation to sqrt function between 1/2 and 3/2 good to 3 digits.

Uses a low degree polynomial hand fitted to sqrt and its derivative.  Most accurate near 1/2, 1, and 3/2 -- exact at 1"
  (if (or (< x 1/2) (> x 3/2))
      (warn "mjr_rirrf_log-2: X should be in [1/2, 3/2]"))
  (mjr_poly_eval #( 109472/364809
                   -226872/121603
                   1744268/364809
                   -806802/121603
                   2147921/364809
                   -300213/121603)
                 x))
