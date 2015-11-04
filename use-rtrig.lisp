;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-rtrig.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Rational Approximations For Irrational Functions.@EOL
;; @std       Common Lisp
;; @see       tst-rtrig.lisp
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
(defpackage :MJR_RTRIG
  (:USE :COMMON-LISP
        :MJR_POLY)
  (:DOCUMENTATION "Brief: Rational Approximations For Irrational Functions.;")
  (:EXPORT #:mjr_rtrig_help

           #:mjr_rtrig_sin-f
           #:mjr_rtrig_cos-f
           #:mjr_rtrig_tan-f

           #:mjr_rtrig_sin-q
           #:mjr_rtrig_cos-q
           #:mjr_rtrig_tan-q
           ))

(in-package :MJR_RTRIG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_help ()
  "Help for MJR_RTRIG: Rational Approximations For Trigonometric Functions.

This library implements completely deterministic, rational approximations for the trigonometric functions sin, cos, and tan.  By 'deterministic' we mean that
a finite sequence of rational arithmetic operations are used to compute the approximation resulting in consistent results across conforming platforms. By
rational we mean that rational inputs result in rational outputs, and the functions have a rational period.  For the sin approximations implemented here, this
means that the following are true ($\widetilde{\sin}$ is the approximation):

        $\widetilde{\sin}$ is $C^\infty$.

        $\widetilde{\sin}(\mathbb{Q})\subset\mathbb{Q}$.

        Mirror symmetry across any vertical line intersecting a local extrema -- i.e. 
        $\widetilde{\sin}\left(pn+\frac{p}{2}-y\right)=\widetilde{\sin}\left(pn+\frac{p}{2}+y\right)$ for all $n\in\mathbb{Z}$ and $y\in\mathbb{R}$.

        $\widetilde{\sin}$ is odd and periodic with period $2p\in\mathbb{Q}$ -- i.e. $-\widetilde{\sin}\left(x\right)=\widetilde{\sin}(-x)$,
        $\widetilde{\sin}\left(x\right)=\widetilde{\sin}(x+2p)$, and $p\in\mathbb{Q}$.

        $\widetilde{\sin}$ is bounded below by -1 and above by 1 -- i.e. $\widetilde{\sin}\left(\mathbb{R}\right)\subseteq[-1,1]$.

        $\widetilde{\sin}$ monotone up on $\left[0,\frac{p}{2}\right] \cup \left[\frac{3p}{2},p\right]$, and monotone down on
        $\left[\frac{p}{2},\frac{3p}{2}\right]$.

        $\widetilde{\sin}$ has zeros at the start and middle of each period -- i.e. $\widetilde{\sin}\left(np\right)=0$ for all $n\in\mathbb{Z}$.

        $\widetilde{\sin}$ has correct derivatives at the zeros -- i.e. $\widetilde{\sin}(pn)=(-1)^n$ for $n\in\mathbb{Z}$.

        $\widetilde{\sin}$ has local maxima at $2pn+\frac{p}{2}$ for all $n\in\mathbb{Z}$ -- i.e. $\widetilde{\sin}'\left(2pn+\frac{p}{2}\right)=0$ and
        $\widetilde{\sin}\left(2pn+\frac{p}{2}\right)=1$ for all $n\in\mathbb{Z}$.

        $\widetilde{\sin}$ has local minima at $2pn+\frac{3p}{2}$ for all $n\in\mathbb{Z}$ -- i.e. $\widetilde{\sin}'\left(2pn+\frac{3p}{2}\right)=0$ and
        $\widetilde{\sin}\left(2pn+\frac{3p}{2}\right)=1$ for all $n\in\mathbb{Z}$.

See the exp-ratTrig.lisp example code for some insight into how the polynomial approximations in this code were developed.

Some Use Cases:

  * Employ standard test cases involving trigonometric functions for unit tests of numerical algorithms.  Having a known, rational value that should result
    from a test case is much less difficult to test and better evidence of proper function.

  * Use well known functions to generate graphical results, but also render tabular results that can be checked via unit tests.

  * Avoid round off problems for some numerical schemes involving sin/cos.  A good example are some topological computations where only the topological
    properties of the manifold need be represented but numerical round-off error might destroy results."
  (documentation 'mjr_rtrig_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_sin-f (x &optional (rat 't))
  "Rational 9th degree polynomial approximation of the sin function with maximal absolute error of $0.00007$ on $\left[-2\pi, 2\pi\right]$.

When RAT is non-NIL we can think of this approximation as a rational, sin-like function with $\pi$ replaced by $p=\frac{355}{113}$.  For details regarding
what we mean by the term sin-like, see the function MJR_RTRIG_HELP.

WARNING: When RAT is non-NIL, $f$ is a quite poor numerical approximation of sin outside of $\left[-2\pi, 2\pi\right]$ as the absolute error will grow,
periodically, as high as 2!  Set RAT non-NIL, then the period is PI and this absolute error is held to $0.00007$ on the entire real line."
  (if (minusp x)
      (- (mjr_rtrig_sin-f (- x) rat))
      (let* ((f #(-212675535432514568/96711681202978424765625 1882084384358536/30269696777145046875 -3755840961706636/6395006361368671875
                  8769991453468/6004700808796875 7011036523/1429407056250 18757661/3811752150 -412170551/2415899250 339/252050 1 0))
             (p (if rat
                    355/113
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (mjr_poly_eval f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_cos-f (x &optional (rat 't))
  "Rational 8th degree polynomial approximation of the cos function with maximal absolute error of $0.0003$ on $\left[-2\pi, 2\pi\right]$.

When RAT is non-NIL we can think of this approximation as a rational, cos-like function with $\pi$ replaced by $p=\frac{355}{113}$.  For details regarding
what we mean by the term cos-like, see the function MJR_RTRIG_HELP.

WARNING: When RAT is non-NIL, $f$ is a quite poor numerical approximation of cos outside of $\left[-2\pi, 2\pi\right]$ as the absolute error will grow,
periodically, as high as 2!  Set RAT non-NIL, then the period is PI and this absolute error is held to $0.0002$ on the entire real line."
  (if (minusp x)
      (mjr_rtrig_cos-f (- x) rat)
      (let* ((f #(-127981738136380448/6810681774857635546875 9060654027354368/19185019084106015625 -208121477853728/54042307279171875
                  1116548328128/152231851490625 12434886346/428822116875 14199128/1207949625 -190744/378075 0 1))
             (p (if rat
                    355/113
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (mjr_poly_eval f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_tan-f (x &optional (rat 't))
  "Rational approximation of the tan function

Uses the polynomial approximations implemented in MJR_RTRIG_SIN-F and MJR_RTRIG_COS-F"
  (if (minusp x)
      (- (mjr_rtrig_tan-f (- x) rat))
      (let* ((sf #(-212675535432514568/96711681202978424765625 1882084384358536/30269696777145046875 -3755840961706636/6395006361368671875
                   8769991453468/6004700808796875 7011036523/1429407056250 18757661/3811752150 -412170551/2415899250 339/252050 1 0))
             (cf #(-127981738136380448/6810681774857635546875  9060654027354368/19185019084106015625 -208121477853728/54042307279171875
                   1116548328128/152231851490625 12434886346/428822116875 14199128/1207949625 -190744/378075 0 1))
             (p (if rat
                    355/113
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (/ (mjr_poly_eval sf x) (mjr_poly_eval cf x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_sin-q (x &optional (rat 't))
  "Rational 4th degree polynomial approximation of the sin function with maximal absolute error of $0.0007$ on $\left[\frac{\pi|{2}, \frac{\pi|{2}\right]$.

When RAT is non-NIL we can think of this approximation as a rational, sin-like function with $\pi$ replaced by $p=\frac{22}{7}$.  For details regarding what
we mean by the term sin-like, see the function MJR_RTRIG_HELP.  

WARNING: When RAT is non-NIL, $f$ is a quite poor numerical approximation of sin outside of $\left[\frac{\pi|{2}, \frac{\pi|{2}\right]$ as the absolute error
will grow, periodically, as high as 2!  Set RAT non-NIL, then the period is PI and this absolute error is held to $0.0007$$ on the entire real line."
  (if (minusp x)
      (- (mjr_rtrig_sin-q (- x) rat))
      (let* ((f #(40474/1449459 -26117/131769 133/11979 1 0))
             (p (if rat
                    22/7
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (cond ((<= x (* 1/2 p))    (mjr_poly_eval f x))
              ((<= x (* 1   p))    (mjr_poly_eval f (- p x)))
              ((<= x (* 3/2 p)) (- (mjr_poly_eval f (- x p))))
              ((<= x (* 2   p)) (- (mjr_poly_eval f (- (* 2 p) x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_cos-q (x &optional (rat 't))
  "Rational approximation of the cos function with maximal absolute error of $0.001$ on $\left[-\frac{\pi}{2}, \frac{\pi}{2}\right]$.

When RAT is non-NIL we can think of this approximation as a rational, cos-like function with $\pi$ replaced by $p=\frac{22}{7}$.  For details regarding what
we mean by the term cos-like, see the function MJR_RTRIG_HELP.  

WARNING: When RAT is non-NIL, $f$ is a quite poor numerical approximation of cos outside of $\left[\frac{\pi|{2}, \frac{\pi|{2}\right]$ as the absolute error
will grow, periodically, as high as 2!  Set RAT non-NIL, then the period is PI and this absolute error is held to $0.0007$$ on the entire real line."
  (if (minusp x)
      (mjr_rtrig_cos-q (- x) rat)
      (let* ((f #(40474/1449459 2989/131769 -6104/11979 0 1))
             (p (if rat
                    22/7
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (cond ((<= x (* 1/2 p))    (mjr_poly_eval f x))
              ((<= x (* 1   p)) (- (mjr_poly_eval f (- p x))))
              ((<= x (* 3/2 p)) (- (mjr_poly_eval f (- x p))))
              ((<= x (* 2   p))    (mjr_poly_eval f (- (* 2 p) x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rtrig_tan-q (x &optional (rat 't))
  "Rational approximation of the tan function

Uses the polynomial approximations implemented in MJR_RTRIG_SIN-Q and MJR_RTRIG_COS-Q"
  (if (minusp x)
      (- (mjr_rtrig_tan-q (- x) rat))
      (let* ((sf #(40474/1449459 -26117/131769   133/11979 1 0))
             (cf #(40474/1449459   2989/131769 -6104/11979 0 1))
             (p (if rat
                    22/7
                    pi))
             (x (if (> x (* 2 p))
                    (if (or (floatp p) (floatp x))
                        (rem (float x 1.0d0) (* 2.0d0 p))
                        (rem x               (* 2 p)))
                    x)))
        (cond ((<= x (* 1/2 p)) (/    (mjr_poly_eval sf x)                 (mjr_poly_eval cf x)))
              ((<= x (* 1   p)) (/    (mjr_poly_eval sf (- p x))        (- (mjr_poly_eval cf (- p x)))))
              ((<= x (* 3/2 p)) (/ (- (mjr_poly_eval sf (- x p)))       (- (mjr_poly_eval cf (- x p)))))
              ((<= x (* 2   p)) (/ (- (mjr_poly_eval sf (- (* 2 p) x)))    (mjr_poly_eval cf (- (* 2 p) x))))))))
