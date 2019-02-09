;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-numu.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Numerical utilities.@EOL
;; @std       Common Lisp
;; @see       tst-numu.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_NUMU
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_CHK
        :MJR_UTIL
        :MJR_VVEC)
  (:DOCUMENTATION "Brief: Numerical utilities.;")
  (:EXPORT #:mjr_numu_help

           #:mjr_numu_max-accuracy

           #:mjr_numu_abssqr

           #:mjr_numu_absdif
           #:mjr_numu_min-nil
           #:mjr_numu_max-nil
           #:mjr_numu_tuple-max-min

           #:mjr_numu_fnd-max-periodic-point #:mjr_numu_near-periodic-point?

           #:mjr_numu_code

           #:mjr_numu_csign #:mjr_numu_signum-pos

           #:mjr_numu_dabs

           #:mjr_numu_sqrt #:mjr_numu_cubert
           #:mjr_numu_hypot
           #:mjr_numu_argument

           #:mjr_numu_log!
           #:mjr_numu_gamma-lanczos #:mjr_numu_gamma-lanczos-15 #:mjr_numu_gamma-lanczos-9
           #:mjr_numu_gamma-spouge
           #:mjr_numu_gamma
           #:mjr_numu_log-gamma

           #:mjr_numu_log-binomial
           #:mjr_numu_binomial

           #:mjr_numu_iverson-bracket

           #:mjr_numu_sum
           #:mjr_numu_prod

           #:mjr_numu_abs-max
           #:mjr_numu_abs-min

           #:mjr_numu_complex-to-vector

           #:mjr_numu_2p-linear-interpolate
           ;; Not exported
           ;;#:mjr_numu_gamma-domain-check
           ))

(in-package :MJR_NUMU)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_help ()
  "Help for MJR_NUMU:  Numerical utilities."
  (documentation 'mjr_numu_help 'function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_abs-max   (&rest rest)
  "Returns the argument that is the largest in absolute value"
  (let ((maxx (car rest)))
    (dolist (x rest maxx)
      (if (< (abs maxx) (abs x))
          (setq maxx x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_abs-min   (&rest rest)
  "Returns the argument that is the smallest in absolute value"
  (let ((minx (car rest)))
    (dolist (x rest minx)
      (if (> (abs minx) (abs x))
          (setq minx x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_sum (&key start end step len seq-fun)
  "User friendly wrapper for MJR_VVEC_MAP-SUM that directly takes keyword arguments. nil values are ignored."
  (mjr_vvec_map-sum (mjr_util_strip-nil-val-kwarg (list :start start :end end :step step :len len :map-fun seq-fun))
                    :filter-fun (lambda (v f i) (declare (ignore f i)) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_prod (&key start end step len seq-fun)
  "User friendly wrapper for MJR_VVEC_MAP-PROD that directly takes keyword arguments. nil values are ignored."
  (mjr_vvec_map-prod (mjr_util_strip-nil-val-kwarg (list :start start :end end :step step :len len :map-fun seq-fun))
                     :filter-fun (lambda (v f i) (declare (ignore f i)) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_tuple-max-min (r g b)
  "Return values: max value, min value, max index, min index"
    (if (> r g)
        (if (> r b)
            (if (> g b)
                (values   r b 0 2)     ;; r>g r>b  g>b
                (values   r g 0 1))    ;; r>g r>b  b>=g
            (values       b g 2 1))    ;; r>g b>=r
        (if (> g b)
            (if (> r b)
                (values   g b 1 2)     ;; g>=r g>b r>b
                (values   g r 1 0))    ;; g>=r g>b b>=r
            (values       b r 2 0))))  ;; g>=r b>=g

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_min-nil (a b)
  "Return minimum of two numeric arguments, nil if both arguments are nil, and the non-nil value of only one is non-nil."
  (if (and a b) (min a b) (or a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_max-nil (a b)
  "Return maximum of two numeric arguments, nil if both arguments are nil, and the non-nil value of only one is non-nil."
  (if (and a b) (max a b) (or a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_absdif (a b)
  "Absolute difference between two numbers: (abs (- a b))"
  (abs (- a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_abssqr (a)
  "Square of the absolute value.  Computation is a bit more stable than (EXPT (ABS A) 2) for complex A."
  (typecase a
    (complex   (+ (expt (realpart a) 2) (expt (imagpart a) 2)))
    (otherwise (expt (abs a) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_dabs (x)
  "The derivative of abs(x) for real x -- -1 when x<0, +1 when x>0, and undef for x=0."
  (cond ((< x 0) -1)
        ((> x 0)  1)
        ('t       (error 'floating-point-invalid-operation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_csign (x)
  "The sign function for both real and complex X -- the 'lexicographic sign function'.
This is NOT the signum, sgn, or sign function."
  (if (complexp x)
      (let ((rs (signum (realpart x))))
        (if (> (abs rs) 1/2)
            rs
            (signum (imagpart x))))
      (signum x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_signum-pos (x)
  "Modified signum that returns 1 for X near zero.
This is NOT the signum, sgn, or sign, or csign function.

Normal use case for his function is when the signum is being used to maximize/minimize the result of an arbitrary sign choice for numerical stability.  In
most cases, the signum is multiplied by a value in this use case, and the production of a zero is undesirable in this application."
  (let ((snv (signum x)))
    (if (> (abs snv) 1/2)
        snv
        1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_fnd-max-periodic-point (seed period lower-bound upper-bound &optional eps)
  "Find the maximum number PP=seed+period*n such that n is an integer and lower-bound<=PP<=upper-bound

This is useful for finding special points (discontinuities, extrema, etc) for periodic functions.  max(sin) @ pi/2,2*pi; min(sin) @ 3*pi/2,2*pi, max(cos) @
0,2*pi, min(cos) @ pi,2*pi"
  (let* ((ymsdp  (/ (- upper-bound seed) period))
         (minPPI (floor (- ymsdp 1)))
         (lowPP  (loop
                   for i from minPPI
                   until (>= (+ seed (* i period)) upper-bound)
                   finally (return (+ seed (* i period)))))
         (maxPP  (if (mjr_cmp_<= lowPP upper-bound eps)
                     lowPP
                   (if (mjr_cmp_<= (- lowPP period) upper-bound eps)
                       (- lowPP period)))))
    (if maxPP
        (if (mjr_cmp_<= lower-bound maxPP eps)
            maxPP))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_near-periodic-point? (seed period the-number &optional (max-distance 100) (eps nil))
"Is the given point within epsilon of a number like PP=seed+period*n such that n is an integer?"
  (dotimes (i max-distance)
    (dotimes (j 2)
      (if (mjr_cmp_= (+ seed (* (1- (* 2 j)) i period)) the-number eps) (return-from mjr_numu_near-periodic-point? 't))))
  nil)

;;; Rename this function to mjr_eps_=o+p*n, and move it to eps package.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_code (the-number &key (lang :lang-matlab))
  "Return a string using the syntax of the selected programming language or computational environment."
  (if (not (numberp the-number))
      (error "mjr_numu_code: Argument must be a number!"))
  (labels ((fmtFloat     (x) (substitute #\e #\d (if (or (> (abs x) 1000000)
                                                        (and (not (zerop x)) (< (abs x) 1/1000000)))
                                                    (format nil "~e" x)
                                                    (format nil "~f" x))))
           (fmtFloatSgl  (x) (let* ((x   (float x 1.0e0))
                                    (y   (or (if (<= (abs x) 1.2d-38)  ;; Really make sure we have something in the IEEE single float range
                                                 0.0e0
                                                 (if (>= x 3.4e38)
                                                     3.4e38
                                                     (if (<= x -3.4e38)
                                                         -3.4e38)))
                                             x)))
                                    (if (or (> (abs y) 1000000)
                                      (and (not (zerop y)) (< (abs y) 1/1000000)))
                                  (format nil "~e" y)
                                  (format nil "~f" y))))
           (fmtRatFlt    (x) (fmtFloat (float x 1.0d0)))
           (fmtRatFltSgl (x) (fmtFloatSgl x))
           (fmtSgnInt32  (x) (if (> (abs x) 2147483647)
                                (mjr_numu_code (float the-number 1.0d0) :lang lang)
                                (format nil "~D" x)))
           (fmtSgnInt64  (x) (if (> (abs x) 9223372036854775808)
                                (mjr_numu_code (float the-number 1.0d0) :lang lang)
                                (format nil "~D" x)))
           (fmtSgnIntC   (x) (cond ((> (abs x) 9223372036854775808) (mjr_numu_code (float the-number 1.0d0) :lang lang))
                                  ((> (abs x) 2147483647)          (format nil "~DL" x))
                                  ('t                              (format nil "~D" x)))))
    (let* ((bams (case lang            ;;    rea           cplx                      int           rat
                   (:lang-vtk          (list #'fmtFloatSgl nil                       #'fmtSgnInt32 #'fmtFloatSgl))  ;; Some VTK readers won't read doubles!!
                   (:lang-gnuplot      (list #'fmtFloatSgl nil                       #'fmtSgnInt32 #'fmtFloatSgl))
                   (:lang-povray       (list #'fmtFloatSgl "<~a,~a>"                 #'fmtSgnInt64 #'fmtRatFltSgl))
                   ((:lang-matlab
                     :lang-octave
                     :lang-scilab
                     :lang-euler
                     :lang-r)          (list #'fmtFloat    "(~a+~ai)"                #'fmtSgnInt32 #'fmtRatFlt))
                   ((:lang-gap)        (list #'fmtFloat    "(~a+~ai)"                "~D"          "~S"))
                   (:lang-mathematica  (list #'fmtFloat    "(~a+I~a)"                "~D"          "~S"))
                   (:lang-maple        (list #'fmtFloat    "(~a+i*~a)"               "~D"          "~S"))
                   ((:lang-maxima
                     :lang-axiom
                     :lang-open-axiom) (list #'fmtFloat    "(~a+~a*%i)"              "~D"          "~S"))
                   (:lang-M2           (list #'fmtFloat    "(~a+~a*ii)"              "~D"          "~S"))
                   ((:lang-latex
                     :lang-pdflatex
                     :lang-amstex
                     :lang-tex)        (list #'fmtFloat    "(~a+~a i)"               "~D"          "\\frac{~a}{~a}"))
                   ((:lang-c99
                     :lang-c)          (list #'fmtFloat    "(~a+~aI)"                #'fmtSgnIntC  #'fmtRatFlt))
                   ((:lang-gp
                     :lang-pari
                     :lang-pari/gp)    (list #'fmtFloat    "(~a+~a*I)"               "~D"          #'fmtRatFlt))
                   (:lang-c++          (list #'fmtFloat    "complex<double>(~a,~a)"  #'fmtSgnIntC  #'fmtRatFlt))
                   (:lang-ruby         (list #'fmtFloat    "Complex(~a,~a)"          "~D"          "Rational(~a,~a)"))
                   (:lang-idl          (list #'fmtFloat    "complex(~a,~a)"          #'fmtSgnInt32 #'fmtRatFlt))
                   ((:lang-hp48
                     :lang-f77
                     :lang-r90
                     :lang-fortran)    (list #'fmtFloat    "(~a,~a)"                 "~D"          #'fmtRatFlt))
                   (:lang-python       (list #'fmtFloat    "(~a+~aj)"                "~D"          #'fmtRatFlt))
                   (:lang-csv          (list #'fmtFloat    "(~a+i~a)"                "~D"          #'fmtRatFlt))
                   (:lang-csvl         (list #'fmtFloat    "~s"                      "~D"          "~S"))
                   (:lang-lisp         (list #'fmtFloat    "~s"                      "~D"          "~S"))
                   ('t                 (error "mjr_numu_code: Language unsupported!")))))
      (typecase the-number
        (complex   (if (= 1 (count #\~ (nth 1 bams)))
                       (format nil (nth 1 bams) the-number)
                       (format nil (nth 1 bams) (mjr_numu_code (realpart the-number) :lang lang) (mjr_numu_code (imagpart the-number) :lang lang))))
        (integer   (let ((fmt (nth 2 bams)))
                     (if (stringp fmt)
                         (format nil fmt the-number)
                         (funcall fmt the-number))))
        (rational  (let ((fmt (nth 3 bams)))
                     (if (stringp fmt)
                         (if (= 1 (count #\~ fmt))
                             (format nil fmt the-number)
                             (format nil fmt (mjr_numu_code (numerator the-number) :lang lang) (mjr_numu_code (denominator the-number) :lang lang)))
                         (funcall fmt the-number))))
        (otherwise (let ((fmt (nth 0 bams)))
                     (if (stringp fmt)
                         (format nil fmt the-number)
                         (funcall fmt the-number))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_cubert (x &key (aggressive-conversion nil))
  "Compute the cube root of X.  Real roots are preferred over complex ones"
  (and aggressive-conversion)
  (if (realp x)
      (* (signum x) (expt (abs x) 1/3))
      (expt x 1/3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_sqrt (x &key (aggressive-conversion nil))
  "Compute the square root of X.  If possible, an integer or rational number is returned.  Otherwise a floating point or complex number will be returned.

The aggressiveness of the algorithm varies according the the value of :AGGRESSIVE-CONVERSION:
  * :NEAR-INT-CONVERT --  floating point X within *MJR_CMP_EPS* of an integer are converted to that integer.
  * :NEAR-RAT-CONVERT --  First try :NEAR-INT-CONVERT, then rationalize X if an integer conversion was not possible.
  * nil (the default) --  floating point numbers are used as provided"
  (if (and aggressive-conversion (floatp x))
      (if (mjr_cmp_= (round x) x)
          (mjr_numu_sqrt (round x))
          (if (eq aggressive-conversion :NEAR-RAT-CONVERT)
              (mjr_numu_sqrt (rationalize x))
              (mjr_numu_sqrt x)))
      (cond ((integerp x)  (let* ((absx   (abs x))
                                  (xisqrt (isqrt absx)))
                             (if (= absx (* xisqrt xisqrt))
                                 (if (< x 0)
                                     (complex 0 xisqrt)
                                     xisqrt)
                                 (sqrt x))))
            ((rationalp x) (let* ((numerator-sqrt   (mjr_numu_sqrt (numerator x)))
                                  (denominator-sqrt (mjr_numu_sqrt (abs (denominator x)))))
                             (if (and (integerp numerator-sqrt) (integerp denominator-sqrt))
                                 (/ numerator-sqrt denominator-sqrt)
                                 (sqrt x))))
            ((complexp x)  (let* ((sqrtx  (sqrt x))
                                  (sqrtxi (complex (round (realpart sqrtx)) (round (imagpart sqrtx)))))
                             (if (= (* sqrtxi sqrtxi) x)
                                 sqrtxi
                                 sqrtx)))
            ('t            (sqrt x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_hypot (x y)
  "Computes (sqrt (+ (expt (abs x) 2) (expt (abs y) 2))) in such a way as to avoid floating point overflow/underflow."
  (if (and (rationalp x) (rationalp y))
      (mjr_numu_sqrt (+ (* (abs x) (abs x)) (* (abs y) (abs y))))
      (let ((x (if (complexp x) (mjr_numu_hypot (realpart x) (imagpart x)) (abs x)))
            (y (if (complexp y) (mjr_numu_hypot (realpart y) (imagpart y)) (abs y))))
        (if (< x y)
            (rotatef x y))
        (if (mjr_chk_!=0 x)
            (* (abs x) (mjr_numu_sqrt (1+ (expt (/ y x) 2))))
            0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_argument (z)
  "The POSITIVE phase of the complex number z.  #C(1,0)=0, #C(0,1)=pi/2, #C(-1,0)=pi, #C(0,-1)=3*pi/2"
  (cond  ((not (numberp z)) (error "mjr_numu_argument: Input must be a number!")))
    (let ((p (phase z)))
      (if (minusp p)
          (+ (* 2 pi) p)
          p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-domain-check (z)
  "Check that z is in the domain of the gamma function.  Error if z is a pole, and warn if it is near a pole."
  (let ((x (realpart z))
        (y (imagpart z)))
    (if (zerop y)
        (cond ((zerop x)                              (error "mjr_numu_gamma-domain-check: Gamma undefied at 0"))
              ((and (< x 0) (integerp x))             (error "mjr_numu_gamma-domain-check: Gamma undefied on negative integers (~a)!" z))
              ((and (< x 0) (zerop (- x (round x))))  (error "mjr_numu_gamma-domain-check: Gamma undefied on negative integers (~a)!" z))))
    (if (mjr_cmp_zerop y)
        (cond ((mjr_cmp_zerop x)                  (warn "mjr_numu_gamma-domain-check: Gamma undefied at 0. Argument near zero: ~a" z))
              ((and (< x 0) (mjr_cmp_integerp x)) (warn "mjr_numu_gamma-domain-check: Gamma undefied on negative integers. Argument near integer: ~a" z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-lanczos-9 (z &optional ln)
  "Lanczos approximation with $g=7$ and $n=9.  Generally good to about 4 digits. Euler's reflection formula is used when $\\Re(z)<\\frac{1}{2}$.

Euler's reflection formula: $$\\Gamma(1-z)\\Gamma(z)=\\frac{\\pi}{\\sin{(\\pi z)}} \\,\\,\\, \\forall z \\notin \\mathbb{Z}$$

References:
  Cornelius Lanczos (1964); A Precision Approximation of the Gamma Function; SIAM Journal on Numerical Analysis series B; p86-96"
  (mjr_numu_gamma-domain-check z)
  (if (< (realpart z) 1/2)
      (if ln
          (- (log pi) (+ (log (sin (* pi z))) (mjr_numu_gamma-lanczos-9 (- 1 z) 't)))
          (/ pi (* (sin (* pi z)) (mjr_numu_gamma-lanczos-9 (- 1 z)))))
      (let* ((g  7)
             (p #( 0.999999999999809932276847004734780d+0
                   0.676520368121885098567009190444019d+3
                  -0.125913921672240287047156078755283d+4
                   0.771323428777653078848652825889400d+3
                  -0.176615029162140599065845513540000d+3
                   0.125073432786869048144589368530000d+2
                  -0.138571095265720116895547070000000d+0
                   0.998436957801957085956300000000000d-5
                   0.150563273514931155834000000000000d-6))
             (n  9) ;; (length p)
             (z  (- z 1))
             (x  (mjr_numu_sum :start 1 :end (1- n) :seq-fun (lambda (i) (/ (aref p i) (+ z i)))))
             (tm (+ z g 1/2)))
        (if ln
            (+ (* 1/2 (log (* 2 pi)))
               (* (+ z 1/2) (log tm))
               (- tm)
               (log (+ x (aref p 0))))
            (* (sqrt (* 2 pi))
               (expt tm (+ z 1/2))
               (exp (- tm))
               (+ x (aref p 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-lanczos-15 (z &optional ln)
  "Lanczos approximation with $g=4.7421875d0$ and $n=15.  Generally good to about 4 digits. Euler's reflection formula is used when $\\Re(z)<\\frac{1}{2}$.

Euler's reflection formula: $$\\Gamma(1-z)\\Gamma(z)=\\frac{\\pi}{\\sin{(\\pi z)}} \\,\\,\\, \\forall z \\notin \\mathbb{Z}$$

References:
  Cornelius Lanczos (1964); A Precision Approximation of the Gamma Function; SIAM Journal on Numerical Analysis series B; p86-96
  Paul Godfrey (2001); A note on the computation  of the convergent Lanczos complexa gamma approximation; http://my.fit.edu/~gabdo/gamma.txt"
  (mjr_numu_gamma-domain-check z)
  (if (< (realpart z) 1/2)
      (if ln
          (- (log pi) (+ (log (sin (* pi z))) (mjr_numu_gamma-lanczos-15 (- 1 z) 't)))
          (/ pi (* (sin (* pi z)) (mjr_numu_gamma-lanczos-15 (- 1 z)))))
      (let* ((g  607/128)
             (p #( 0.99999999999999709182d+0
                   0.57156235665862923517d+2
                  -0.59597960355475491248d+2
                   0.14136097974741747174d+2
                  -0.49191381609762019978d+0
                   0.33994649984811888699d-4
                   0.46523628927048575665d-4
                  -0.98374475304879564677d-4
                   0.15808870322491248884d-3
                  -0.21026444172410488319d-3
                   0.21743961811521264320d-3
                  -0.16431810653676389022d-3
                   0.84418223983852743293d-4
                  -0.26190838401581408670d-4
                  0.36899182659531622704d-5))
             (n 15) ;; (length p)
             (z  (- z 1))
             (x  (mjr_numu_sum :start 1 :end (1- n) :seq-fun (lambda (i) (/ (aref p i) (+ z i)))))
             (tm (+ z g 1/2)))
        (if ln
            (+ (* 1/2 (log (* 2 pi)))
               (* (+ z 1/2) (log tm))
               (- tm)
               (log (+ x (aref p 0))))
            (* (sqrt (* 2 pi))
               (expt tm (+ z 1/2))
               (exp (- tm))
               (+ x (aref p 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-lanczos (z &optional ln)
  "See: MJR_NUMU_GAMMA-LANCZOS-15"
  (mjr_numu_gamma-lanczos-15 z ln))

;; May get better accuracy when $x\in\mathbb{R}$ and $x$ is small:
;; $$\Gamma(x) = \Gamma(1+x)/x$$

;; Euler's reflection formula:
;; $$\\Gamma(1-z)\\Gamma(z)=\\frac{\\pi}{\\sin{(\\pi z)}} \\,\\,\\, \\forall z \\notin \\mathbb{Z}$$
;; Basic Gamma property
;; $$\\Gamma(z+1)=z\\Gamma(z)$$
;; Combine them:
;; $$z\\Gamma(z)\\Gamma(-z) = \\frac{-\\pi}{\\sin(\\pi z)}$$
;; Solve for $\\Gamma(-z)$
;; $$\\Gamma(-z) = \\frac{-\\pi}{z\\Gamma(z)\\sin(\\pi z)}$$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-spouge (z &optional a)
  "Spouge approximation (a=9 by default). Euler's reflection formula is used when $\\Re(z)<1/2$.

     $$\\Gamma(z+1) = (z+a)^{z+1/2} e^{-(z+a)} \\left[ c_0 + \\sum_{k=1}^{a-1} \\frac{c_k}{z+k} + \\varepsilon_a(z) \\right]$$

     where $a$ is an arbitrary positive integer, $\\varepsilon_a(z)$ is an unknown term, and the $c_k$ are given by:

     $$c_0 = \\sqrt{2 \\pi}$$
     $$c_k = \\frac{(-1)^{k-1}}{(k-1)!} (-k+a)^{k-1/2} e^{-k+a}$$

     If $Re(z)>0$ and $a>2$, then the error from removing the $\\varepsilon_a(z)$ term is bounded by:

     $$a^{-1/2} (2 \\pi)^{-(a+1/2)}$$

     Euler's reflection formula: $$\\Gamma(1-z)\\Gamma(z)=\\frac{\\pi}{\\sin{(\\pi z)}} \\,\\,\\, \\forall z \\notin \\mathbb{Z}$$

References:
  John L. Spouge (1994); Computation of the gamma, digamma, and trigamma functions; SIAM Journal on Numerical Analysis; p931-944"
  ;(cond ((<= (realpart z) 0) (error "mjr_numu_gamma-spouge: Re(z) must be positive!!")))
  (mjr_numu_gamma-domain-check z)
  (if (< (realpart z) 1/2)
      (/ pi (* (sin (* pi z)) (mjr_numu_gamma-spouge (- 1 z) a)))
  (let ((a (or a 9))
        (c #( 0.25066282746310002d+1
              0.84314224282776270d+4
             -0.20309930316682980d+5
              0.17787504448869255d+5
             -0.69137898439344360d+4
              0.11647605340404104d+4
             -0.70448070157729940d+2
              0.92886251984385430d+0
             -0.53934163263076290d-3))
        (z (1- z)))
    (* (expt (+ z a) (+ z 1/2))
       (exp (- (+ z a)))
       (+ (aref c 0)
          (if (= a 9)
              (mjr_numu_sum :start 1 :end (1- a) :seq-fun (lambda (k) (/ (aref c k) (+ z k))))
              (loop for k from 1 upto (1- a)
                    for kf = 1 then (* kf (1- k))
                    sum (/ (* (if (evenp k) -1 1)
                              (expt (- a k) (- k 1/2))
                              (exp (- a k)))
                           (* kf
                              (+ z k))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma (z)
  "Use MJR_NUMU_GAMMA-LANCZOS to approximate the gamma"
  (mjr_numu_gamma-lanczos z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_log-gamma (z)
  "Use MJR_NUMU_GAMMA-LANCZOS to approximate the logarithm of the gamma"
  (mjr_numu_gamma-lanczos z 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_log-binomial (n k)
  "Approximation the logarithm of the binomial coefficient.  Return is always a double-float."
  (if (and (integerp k) (< k 0))
      (error "binomial: INFINITY (log 0)!")
      (if (and (integerp n) (< n 0))
          (if (integerp k)
              (if (evenp k)
                 (mjr_numu_log-binomial (- k n 1) (- 0 n 1))  ;; n negative integer, k positive even integer
                 (error "binomial: UNDEF (log -1)!"))
              (error "binomial: INFINITY!"))
          (- (mjr_numu_log-gamma (+ n 1))
             (+ (mjr_numu_log-gamma (+ k 1))
                (mjr_numu_log-gamma (1+ (- n k))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_binomial (n k)
  "Approximation the binomial coefficient.  Return is always a double-float.

For exact answers when the arguments are integers and non-negative, use MJR_COMBE_COMB."
  (if (and (integerp k) (< k 0))
      0
      (if (and (integerp n) (< n 0))
          (if (integerp k)
              (* (if (evenp k) 1 -1)
                 (mjr_numu_binomial (- k n 1) (- 0 n 1)))
              (error "binomial: INFINITY!"))
          (exp (mjr_numu_log-binomial n k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_log! (n)
  "Compute the natural logarithm of the factorial.  Return is a DOUBLE-FLOAT"
  (mjr_numu_log-gamma (1+ n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_iverson-bracket (pred &rest rest)
  "Return 1 if PRED applied to the remaining arguments is true, and 0 otherwise."
  (if (apply pred rest)
      1
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_max-accuracy (val)
  "If val is a float, convert to a double float.  Otherwise return val.  Used to maximize numerical accuracy in computations."
  (if (floatp val)
      (float val 0.0d0)
      val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_complex-to-vector (z)
  "Convert a complex numberr to a two element, real vector"
  (vector (realpart z) (imagpart z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_2p-linear-interpolate (x x0 x1 y0 y1)
  "Linear interpolation between two points.

Computes the point on the line above x and containing the two points.  That is:
    $$y=\frac{y_1-y_0}{x_1-x_0}(x-x_0)+y_0$$"
  (+ y0
     (* (/ (- x x0)
           (- x1 x0))
        (- y1 y0))))

