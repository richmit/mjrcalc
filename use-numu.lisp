;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-numu.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Numerical utilities.@EOL
;; @std       Common Lisp
;; @see       tst-numu.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
        :MJR_VVEC)
  (:DOCUMENTATION "Brief: Numerical utilities.;")
  (:EXPORT #:mjr_numu_max-accuracy

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
           #:mjr_numu_gamma-lanczos
           #:mjr_numu_gamma-spouge
           #:mjr_numu_gamma
           #:mjr_numu_log-gamma

           #:mjr_numu_log-binomial
           #:mjr_numu_binomial

           #:mjr_numu_iverson-bracket

           #:mjr_numu_sum
           #:mjr_numu_prod

           #:mjr_numu_complex-to-vector
           ))

(in-package :MJR_NUMU)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_sum (&key start end step len seq-fun)
  "User friendly wrapper for MJR_VVEC_MAP-SUM that directly takes keyword arguments. nil values are ignored."
  (mjr_vvec_map-sum (list :start start :end end :step step :len len :map-fun seq-fun) :filter-fun (lambda (v f i) (declare (ignore f i)) v))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_prod (&key start end step len seq-fun)
  "User friendly wrapper for MJR_VVEC_MAP-PROD that directly takes keyword arguments. nil values are ignored."
  (mjr_vvec_map-prod (list :start start :end end :step step :len len :map-fun seq-fun) :filter-fun (lambda (v f i) (declare (ignore f i)) v))
  )

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
  (let* ((bams (case lang            ;;    rea            cplx                      int  rat
                 (:lang-vtk          (list "~,15,,,,,'eG" nil                       "~D" "~,15,,,,,'eG"))
                 (:lang-povray       (list "~,15,,,,,'eG" "<~a,~a>"                 "~D" "~,15,,,,,'eG"))
                 ((:lang-matlab
                   :lang-octave
                   :lang-scilab
                   :lang-euler
                   :lang-r
                   :lang-gap)        (list "~,15,,,,,'eG" "(~a+~ai)"                "~D" "~,15,,,,,'eG"))
                 (:lang-mathematica  (list "~,15,,,,,'eG" "(~a+I~a)"                "~D" "~S"))
                 (:lang-maple        (list "~,15,,,,,'eG" "(~a+i*~a)"               "~D" "~S"))
                 ((:lang-maxima
                   :lang-axiom
                   :lang-open-axiom) (list "~,15,,,,,'eG" "(~a+~a*%i)"              "~D" "~S"))
                 (:lang-M2           (list "~,15,,,,,'eG" "(~a+~a*ii)"              "~D" "~S"))
                 ((:lang-latex
                   :lang-pdflatex
                   :lang-amstex
                   :lang-tex)        (list "~,15,,,,,'eG" "(~a+~a i)"               "~D" "\\frac{~a}{~a}"))
                 ((:lang-c99
                   :lang-c)          (list "~,15,,,,,'eG" "(~a+~aI)"                "~D" "~,15,,,,,'eG"))
                 ((:lang-gp
                   :lang-pari
                   :lang-pari/gp)    (list "~,15,,,,,'eG" "(~a+~a*I)"               "~D" "~,15,,,,,'eG"))
                 (:lang-c++          (list "~,15,,,,,'eG" "complex<double>(~a,~a)"  "~D" "~,15,,,,,'eG"))
                 (:lang-ruby         (list "~,15,,,,,'eG" "Complex(~a,~a)"          "~D" "~,15,,,,,'eG"))
                 (:lang-idl          (list "~,15,,,,,'eG" "complex(~a,~a)"          "~D" "~,15,,,,,'eG"))
                 ((:lang-hp48
                   :lang-f77   
                   :lang-r90     
                   :lang-fortran)    (list "~,15,,,,,'eG" "(~a,~a)"                 "~D" "~,15,,,,,'eG"))
                 (:lang-python       (list "~,15,,,,,'eG" "(~a+~aj)"                "~D" "~,15,,,,,'eG"))
                 (:lang-csv          (list "~,15,,,,,'eG" "(~a+i~a)"                "~D" "~,15,,,,,'eG"))
                 (:lang-csvl         (list "~,15,,,,,'eG" "~s"                      "~D" "~S"))
                 (:lang-lisp         (list "~,15,,,,,'eG" "~s"                      "~D" "~S"))
                 ('t                 (error "mjr_numu_code: Language unsupported!")))))
    (typecase the-number
      (complex   (if (= 1 (count #\~ (nth 1 bams)))
                     (format nil (nth 1 bams) the-number)
                     (format nil (nth 1 bams) (mjr_numu_code (realpart the-number) :lang lang) (mjr_numu_code (imagpart the-number) :lang lang))))
      (integer   (format nil (nth 2 bams) the-number))
      (rational  (if (= 1 (count #\~ (nth 3 bams)))
                     (format nil (nth 3 bams) the-number)
                     (format nil (nth 3 bams) (mjr_numu_code (numerator the-number) :lang lang) (mjr_numu_code (denominator the-number) :lang lang))))
      (otherwise (format nil (nth 0 bams) the-number)))))

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
                                  (sqrtxi (complex (truncate (realpart sqrtx)) (truncate (imagpart sqrtx)))))
                             (if (= (* sqrtxi sqrtxi) x)
                                 sqrtxi
                                 sqrtx)))
            ('t            (sqrt x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_hypot (x y)
  "Computes (sqrt (+ (expt (abs x) 2) (expt (abs y))) in such a way as to avoid floating point overflow/underflow."
  (if (and (rationalp x) (rationalp y))
      (mjr_numu_sqrt (+ (* (abs x) (abs x)) (* (abs y) (abs y))))
      (let ((x (abs x))
            (y (abs y)))
        (if (< x y)
            (rotatef x y))
        (if (mjr_chk_!=0 x)
            (* (abs x) (mjr_numu_sqrt (1+ (expt (/ y x) 2))))
            0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_argument (z)
  "The phase of the complex number z.  #C(1,0)=0, #C(0,1)=pi/2, #C(-1,0)=pi, #C(0,-1)=3*pi/2"
  (cond  ((not (numberp z)) (error "mjr_numu_argument: Input must be a number!")))
    (let ((p (phase z)))
      (if (< p 0)
          (+ (* 2 pi) p)
          p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_numu_gamma-lanczos (z &optional ln)
  "Lanczos approximation with g=7.  Generally good to about 4 digits.

References:
  Cornelius Lanczos (1964); A Precision Approximation of the Gamma Function; SIAM Journal on Numerical Analysis series B; p86-96"
  (if (< (realpart z) 1/2)
      (if ln
          (- (log pi) (+ (log (sin (* pi z))) (mjr_numu_gamma-lanczos (- 1 z) 't)))
          (/ pi (* (sin (* pi z)) (mjr_numu_gamma-lanczos (- 1 z)))))
      (let* ((g  7)
             (p  #(0.99999999999980993d0 676.5203681218851d0    -1259.1392167224028d0 771.32342877765313d0 -176.61502916214059d0
                   12.507343278686905d0  -0.13857109526572012d0 9.9843695780195716d-6 1.5056327351493116d-7))
             (z  (- z 1))
             (x  (mjr_numu_sum :start 1 :end (1+ g) :seq-fun (lambda (i) (/ (aref p i) (+ z i)))))
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
(defun mjr_numu_gamma-spouge (z &optional a)
  "Spouge approximation (a=9 by default).

     $$\\Gamma(z+1) = (z+a)^{z+1/2} e^{-(z+a)} \\left[ c_0 + \\sum_{k=1}^{a-1} \\frac{c_k}{z+k} + \\varepsilon_a(z) \\right]$$
     
     where $a$ is an arbitrary positive integer, $\\varepsilon_a(z)$ is an unknown term, and the $c_k$ are given by:
     
     $$c_0 = \\sqrt{2 \\pi}$$
     $$c_k = \\frac{(-1)^{k-1}}{(k-1)!} (-k+a)^{k-1/2} e^{-k+a}$$
     
     If $Re(z)>0$ and $a>2$, then the error from removing the $\\varepsilon_a(z)$ term is bounded by:
     
     $$a^{-1/2} (2 \\pi)^{-(a+1/2)}$$

References:
  John L. Spouge (1994); Computation of the gamma, digamma, and trigamma functions; SIAM Journal on Numerical Analysis; p931-944"
  (let ((a (or a 9))
        (c #(2.5066282746310002d0 8431.422428277627d0 -20309.93031668298d0 17787.504448869255d0 -6913.789843934436d0
             1164.7605340404104d0 -70.44807015772994d0 0.9288625198438543d0 -5.393416326307629d-4))
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
                              (+ z k)))))))))

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
