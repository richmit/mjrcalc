;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-intg.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-intg.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_INTG-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CMP :MJR_INTG :MJR_PRNG :MJR_EPS))

(in-package :MJR_INTG-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test cases:
;; $$t_{1}  = \int_1^a\frac{1}{x} \, \mathrm{d}x = \log(x)\rvert_{x=a} \;\;\;\;\;\;\;\;\; a=2 => t_1 \approxeq 0.6931473$$
;; $$t_{2}  = \int_1^{e} e^x\log(x) \, \mathrm{d}x \approxeq 8.8376984024515853410383471394221771414495584594944254$$
;; $$t_{3}  = \int_{-1}^1 1-x^2 \, \mathrm{d}x = \frac{4}{3}$$
;; $$t_{4}  = \int_0^1\frac{2}{\sqrt{\pi}\cdot e^{x^2}} \, \mathrm{d}x \approxeq 0.842701$$
;; $$t_{5}  = \int_{-3}^4 \tan^{-1}(10 x) \, \mathrm{d}x = 4\tan^{-1}(40)+3\tan^{-1}(-30)-\frac{1}{20}\log\left(\frac{16}{9}\right) \approxeq 1.5420119327087798$$
;; $$t_{6}  = \int_{1}^a\frac{1}{x^2} \, \mathrm{d}x = 1-\frac{1}{x}\rvert_{x=a}$$
;; $$t_{7}  = \int_0^3x^3\log\left\vert (x^2-1)(x^2-2)\right\vert \, \mathrm{d}x$$
;; $$t_{8}  = \int_0^1 \frac{\log x}{\sqrt{x}} \, \mathrm{d}x = -4$$
;; $$t_{9}  = \int_0^1 \log(x) \sqrt{x} \, \mathrm{d}x = -\frac{4}{9}$$
;; $$t_{10} = \int_0^1 \log(x)\sin(10 \pi x) \, \mathrm{d}x$$
;; $$t_{11} = \int_0^1 \frac{\log x}{\left(1+\log^2x\right)^2} \, \mathrm{d}x$$
;; $$t_{12} = \int_0^1 \cos(100 \sin x) \, \mathrm{d}x$$


(defvar ti1 "1/x")                   (defun tf1  (x) (/ x))                                                      (defvar ta1  1)     (defvar tb1        2) (defvar tv1  0.69314730)   (defun ti1 (x) (log x))
(defvar ti2 "exp(x)*log(x)")         (defun tf2  (x) (* (exp x) (log x)))                                        (defvar ta2  1)     (defvar tb2  (exp 1)) (defvar tv2  8.8376984)
(defvar ti3 "1-x^2")                 (defun tf3  (x) (- 1 (* x x)))                                              (defvar ta3 -1)     (defvar tb3        1) (defvar tv3  4/3)
(defvar ti4 "2/(sqrt(pi)*exp(x^2))") (defun tf4  (x) (/ 2 (sqrt pi) (exp (* x x))))                              (defvar ta4  0)     (defvar tb4        1) (defvar tv4  0.842701)
(defvar ti5 "atan(10*x)")            (defun tf5  (x) (atan (* 10 x)))                                            (defvar ta5 -3)     (defvar tb5        4) (defvar tv5  1.5420119327)
(defvar ti6 "1/(x^2)")               (defun tf6  (x) (/ (* x x)))                                                (defvar ta6  1)     (defvar tb6        2) (defvar tv5  1.5420119327) (defun ti6 (x) (- 1 (/ x)))
;; HARD ONES
                                   (defun tf7  (x) (* (EXPT X 3) (LOG (ABS (* (- (* X X) 1) (- (* X X) 2)))))) (defvar ta7  0.0d0) (defvar tb7        3) (defvar tv7  52.7408) 
                                   (defun tf8  (x) (/ (LOG X) (SQRT X)))                                       (defvar ta8  0.0d0) (defvar tb8        1) (defvar tv8  -4)
                                   (defun tf8  (x) (* (LOG X) (SQRT X)))                                       (defvar ta8  0.0d0) (defvar tb8        1) (defvar tv8  -4/9)
                                   (defun tf10 (x) (* (LOG X) (SIN (* (* 10 PI) X))))                          (defvar ta9  0.0d0) (defvar tb9        1) (defvar tv9  -0.128137)
                                   (defun tf11 (x) (/ (LOG X) (EXPT (+ 1 (EXPT (LOG X) 2)) 2)))                (defvar ta10 0.0d0) (defvar tb10       1) (defvar tv10 -0.189275)
                                   (defun tf12 (x) (COS (* 100 (SIN X))))                                      (defvar ta11 0.0d0) (defvar tb11      pi) (defvar tv11 0.627874E-01)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_loc-adp-dnc-simpson-naive (fun &key start end min-width (the-err 1e-5))
  "Compute the definite integral of FUN between A and B using the adaptive Simpson's rule.
References:
  Kythe & Schaferkotter (2005); Handbook of Computational Methods for Integration; pp 89-94"
    (labels ((simpson-1 (f start end)           (* (/ (- end start) 6)
                                                   (+ (funcall f start)
                                                      (* 4 (funcall f (/ (+ start end) 2)))
                                                      (funcall f end))))
             (simpson-r (f start end miw me oa) (let* ((m  (/ (+ start end) 2))
                                                       (la (simpson-1 f start m))
                                                       (ra (simpson-1 f m end)))
                                                  (if (or (and miw (< (- end start) miw))
                                                          (< (abs (- oa la ra)) me))
                                                      (+ la ra)
                                                      (+ (simpson-r f start m miw (/ me 2) la)
                                                         (simpson-r f m end miw (/ me 2) ra))))))
      (simpson-r fun start end min-width the-err (simpson-1 fun start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_loc-adp-dnc-simpson
  ;; A few hand picked examples
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_loc-adp-dnc-simpson       #'tf1 :start ta1 :end tb1 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_loc-adp-dnc-simpson       #'tf2 :start ta2 :end tb2 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_loc-adp-dnc-simpson       #'tf3 :start ta3 :end tb3 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_loc-adp-dnc-simpson       #'tf4 :start ta4 :end tb4 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_loc-adp-dnc-simpson       #'tf5 :start ta5 :end tb5 :min-width 1/100))  
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_loc-adp-dnc-simpson         ti1 :start ta1 :end tb1 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_loc-adp-dnc-simpson         ti2 :start ta2 :end tb2 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_loc-adp-dnc-simpson         ti3 :start ta3 :end tb3 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_loc-adp-dnc-simpson         ti4 :start ta4 :end tb4 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_loc-adp-dnc-simpson         ti5 :start ta5 :end tb5 :min-width 1/100))
  ;; Naive verification
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_loc-adp-dnc-simpson-naive #'tf1 :start ta1 :end tb1 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_loc-adp-dnc-simpson-naive #'tf2 :start ta2 :end tb2 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_loc-adp-dnc-simpson-naive #'tf3 :start ta3 :end tb3 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_loc-adp-dnc-simpson-naive #'tf4 :start ta4 :end tb4 :min-width 1/100))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_loc-adp-dnc-simpson-naive #'tf5 :start ta5 :end tb5 :min-width 1/100))
  ;; Exact case
  (assert-equal 2988999421878269599/4312214059711498200 (mjr_intg_loc-adp-dnc-simpson       #'tf1 :start ta1  :end tb1   :min-width 1/100))
  (assert-equal 4/3                                     (mjr_intg_loc-adp-dnc-simpson       #'tf3 :start ta3  :end tb3   :min-width 1/100))
  (assert-equal 2988999421878269599/4312214059711498200 (mjr_intg_loc-adp-dnc-simpson         ti1 :start ta1  :end tb1   :min-width 1/100))
  (assert-equal 4/3                                     (mjr_intg_loc-adp-dnc-simpson         ti3 :start ta3  :end tb3   :min-width 1/100))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_composite-trap
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 20)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_composite-trap #'tf1 :start ta1 :end u :len 1000))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_composite-trap #'tf6 :start ta6 :end u :len 1000))))
  ;; A few hand picked examples
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_composite-trap     #'tf1 :start ta1 :end tb1 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_composite-trap     #'tf2 :start ta2 :end tb2 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_composite-trap     #'tf3 :start ta3 :end tb3 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_composite-trap     #'tf4 :start ta4 :end tb4 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_composite-trap     #'tf5 :start ta5 :end tb5 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_composite-trap       ti1 :start ta1 :end tb1 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_composite-trap       ti2 :start ta2 :end tb2 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_composite-trap       ti3 :start ta3 :end tb3 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_composite-trap       ti4 :start ta4 :end tb4 :len 100))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_composite-trap       ti5 :start ta5 :end tb5 :len 100))
  ;; Exact case
  (assert-equal 1753/2520   (mjr_intg_composite-trap #'tf1                                  :start ta1  :end tb1   :len 6))
  (assert-equal 39200/29403 (mjr_intg_composite-trap #'tf3                                  :start ta3  :end tb3     :len 100))
  (assert-equal 1753/2520   (mjr_intg_composite-trap   ti1                                  :start ta1  :end tb1   :len 6))
  (assert-equal 39200/29403 (mjr_intg_composite-trap   ti3                                  :start ta3  :end tb3     :len 100))
  ;; Errors
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 1   :end -1))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 1   :end -1      :len 100))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  1      :len 0))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  1      :len 1))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  1      :len -1))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  1      :len 't))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  1      :len #C(0 0)))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 't  :end  1      :len 100))
  (assert-error 'error      (mjr_intg_composite-trap #'identity                             :start 0   :end  't     :len 100))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_composite-simpson
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 10)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_composite-simpson #'tf1 :start ta1 :end u :len 501))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_composite-simpson #'tf6 :start ta6 :end u :len 501))))
  ;; Test case from http://tutorial.math.lamar.edu/Classes/CalcII/ApproximatingDefIntegrals.aspx
  (loop for n = 2 then (* n 2)
        for j from 1 upto 7
        for ia = (mjr_intg_composite-simpson (lambda (x) (exp (* x x))) :start 0 :end 2 :len (1+ n))
        for ic in '(22.157093000000000 17.353626000000000 16.538595000000000 16.458813000000000 16.453030000000000 16.452654000000000 16.452630000000000)
        do (assert-equality (mjr_eps_make-fixed= 0.000001d0) ic ia (1+ n)))
  ;; A few hand picked examples
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_composite-simpson     #'tf1 :start ta1 :end tb1 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_composite-simpson     #'tf2 :start ta2 :end tb2 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_composite-simpson     #'tf3 :start ta3 :end tb3 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_composite-simpson     #'tf4 :start ta4 :end tb4 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_composite-simpson     #'tf5 :start ta5 :end tb5 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_composite-simpson       ti1 :start ta1 :end tb1 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_composite-simpson       ti2 :start ta2 :end tb2 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_composite-simpson       ti3 :start ta3 :end tb3 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_composite-simpson       ti4 :start ta4 :end tb4 :len 101))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_composite-simpson       ti5 :start ta5 :end tb5 :len 101))
  ;; Exact case
  (assert-equal 14411/20790    (mjr_intg_composite-simpson #'tf1                       :start ta1  :end tb1   :len 7))
  (assert-equal 4/3            (mjr_intg_composite-simpson #'tf3                       :start ta3  :end tb3   :len 101))
  (assert-equal 14411/20790    (mjr_intg_composite-simpson   ti1                       :start ta1  :end tb1   :len 7))
  (assert-equal 4/3            (mjr_intg_composite-simpson   ti3                       :start ta3  :end tb3   :len 101))
  ;; Errors
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 1   :end -1))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 1   :end -1      :len 101))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1      :len 0))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1      :len 1))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1      :len -1))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1      :len 't))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1      :len #C(0 0)))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 't  :end  1      :len 101))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  't     :len 101))
  (assert-error 'error      (mjr_intg_composite-simpson #'identity                             :start 0   :end  1     :len 100))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_glb-adp-composite-trapezoidal
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 20)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_glb-adp-composite-trapezoidal #'tf1 :start ta1 :end u))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_glb-adp-composite-trapezoidal #'tf6 :start ta6 :end u))))
  ;; A few hand picked examples
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_glb-adp-composite-trapezoidal             #'tf1 :start ta1 :end tb1))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_glb-adp-composite-trapezoidal             #'tf2 :start ta2 :end tb2))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_glb-adp-composite-trapezoidal             #'tf3 :start ta3 :end tb3))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_glb-adp-composite-trapezoidal             #'tf4 :start ta4 :end tb4))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_glb-adp-composite-trapezoidal             #'tf5 :start ta5 :end tb5))

  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_glb-adp-composite-trapezoidal               ti1 :start ta1 :end tb1))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_glb-adp-composite-trapezoidal               ti2 :start ta2 :end tb2))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_glb-adp-composite-trapezoidal               ti3 :start ta3 :end tb3))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_glb-adp-composite-trapezoidal               ti4 :start ta4 :end tb4))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_glb-adp-composite-trapezoidal               ti5 :start ta5 :end tb5))
  ;; Exact case
  (assert-equal (values 1398101/1048576 2048) (mjr_intg_glb-adp-composite-trapezoidal #'tf3             :start ta3  :end tb3))
  (assert-equal (values 1398101/1048576 2048) (mjr_intg_glb-adp-composite-trapezoidal   ti3             :start ta3  :end tb3))
  ;; Errors                                                                                                   
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 1   :end -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 't  :end 1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :max-evals 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :max-evals 1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :max-evals -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :max-evals 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :max-evals #C(0 0)))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :good-err-stop 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :good-err-stop 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :good-err-stop #C(0 0)))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :the-err -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :the-err 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :the-err 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-trapezoidal #'identity                             :start 0   :end 1       :the-err #C(0 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-gauss-kronrod
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 20)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_simple-gauss-kronrod #'tf1 :start ta1 :end u))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_simple-gauss-kronrod #'tf6 :start ta6 :end u))))
  ;; A few hand picked examples
  (loop for ord in '(15 21 31 41 51 61)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv1 (mjr_intg_simple-gauss-kronrod             #'tf1 :start ta1 :end tb1 :order ord) 1 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv2 (mjr_intg_simple-gauss-kronrod             #'tf2 :start ta2 :end tb2 :order ord) 2 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv3 (mjr_intg_simple-gauss-kronrod             #'tf3 :start ta3 :end tb3 :order ord) 3 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv4 (mjr_intg_simple-gauss-kronrod             #'tf4 :start ta4 :end tb4 :order ord) 4 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv1 (mjr_intg_simple-gauss-kronrod               ti1 :start ta1 :end tb1 :order ord) 1 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv2 (mjr_intg_simple-gauss-kronrod               ti2 :start ta2 :end tb2 :order ord) 2 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv3 (mjr_intg_simple-gauss-kronrod               ti3 :start ta3 :end tb3 :order ord) 3 ord)
        do (assert-equality (mjr_eps_make-fixed= 1.0d-6) tv4 (mjr_intg_simple-gauss-kronrod               ti4 :start ta4 :end tb4 :order ord) 4 ord))
  ;; Errors                                                                                                   
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 1   :end -1))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 't  :end 1))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 't))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 0))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 1))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order -1))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 't))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order #C(0 0)))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 2))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 3))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 20))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 22))
  (assert-error 'error                   (mjr_intg_simple-gauss-kronrod #'identity                             :start 0   :end 1       :order 30))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_glb-adp-composite-romberg
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 20)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_glb-adp-composite-romberg #'tf1 :start ta1 :end u))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_glb-adp-composite-romberg #'tf6 :start ta6 :end u))))
  ;; A few hand picked examples
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_glb-adp-composite-romberg             #'tf1 :start ta1 :end tb1))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_glb-adp-composite-romberg             #'tf2 :start ta2 :end tb2))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_glb-adp-composite-romberg             #'tf3 :start ta3 :end tb3))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_glb-adp-composite-romberg             #'tf4 :start ta4 :end tb4))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_glb-adp-composite-romberg             #'tf5 :start ta5 :end tb5))
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_glb-adp-composite-romberg               ti1 :start ta1 :end tb1))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_glb-adp-composite-romberg               ti2 :start ta2 :end tb2))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_glb-adp-composite-romberg               ti3 :start ta3 :end tb3))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_glb-adp-composite-romberg               ti4 :start ta4 :end tb4))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_glb-adp-composite-romberg               ti5 :start ta5 :end tb5))
  ;; Exact case
  (assert-equal (values 4/3 16)          (mjr_intg_glb-adp-composite-romberg #'tf3                                  :start ta3  :end tb3))
  (assert-equal (values 4/3 16)          (mjr_intg_glb-adp-composite-romberg   ti3                                  :start ta3  :end tb3))
  ;; Errors                                                                                                   
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 1   :end -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 't  :end 1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :max-evals 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :max-evals 1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :max-evals -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :max-evals 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :max-evals #C(0 0)))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :good-err-stop 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :good-err-stop 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :good-err-stop #C(0 0)))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :the-err -1))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :the-err 0))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :the-err 't))
  (assert-error 'error                   (mjr_intg_glb-adp-composite-romberg #'identity                             :start 0   :end 1       :the-err #C(0 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_loc-adp-dnc-trapezoidal
  ;; A few random test cases
  (dotimes (i 100)
    (let ((u (mjr_prng_float-co 2 20)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_loc-adp-dnc-trapezoidal #'tf1 (list :start ta1 :end u)))
      (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_loc-adp-dnc-trapezoidal #'tf6 (list :start ta6 :end u)))))
  ;; A few hand picked examples                                                                               
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_loc-adp-dnc-trapezoidal             #'tf1 (list :start ta1 :end tb1)))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_loc-adp-dnc-trapezoidal             #'tf2 (list :start ta2 :end tb2)))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_loc-adp-dnc-trapezoidal             #'tf3 (list :start ta3 :end tb3)))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_loc-adp-dnc-trapezoidal             #'tf4 (list :start ta4 :end tb4)))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_loc-adp-dnc-trapezoidal             #'tf5 (list :start ta5 :end tb5)))
  (assert-equality (mjr_eps_make-fixed= .001) tv1 (mjr_intg_loc-adp-dnc-trapezoidal               ti1 (list :start ta1 :end tb1)))
  (assert-equality (mjr_eps_make-fixed= .001) tv2 (mjr_intg_loc-adp-dnc-trapezoidal               ti2 (list :start ta2 :end tb2)))
  (assert-equality (mjr_eps_make-fixed= .001) tv3 (mjr_intg_loc-adp-dnc-trapezoidal               ti3 (list :start ta3 :end tb3)))
  (assert-equality (mjr_eps_make-fixed= .001) tv4 (mjr_intg_loc-adp-dnc-trapezoidal               ti4 (list :start ta4 :end tb4)))
  (assert-equality (mjr_eps_make-fixed= .001) tv5 (mjr_intg_loc-adp-dnc-trapezoidal               ti5 (list :start ta5 :end tb5)))
  ;; Exact Case
  (assert-equal (values 349525/262144 1537) (mjr_intg_loc-adp-dnc-trapezoidal #'tf3             (list :start ta3  :end tb3)))
  (assert-equal (values 349525/262144 1537) (mjr_intg_loc-adp-dnc-trapezoidal   ti3             (list :start ta3  :end tb3)))
  ;; Errors                                                                                                   
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 't  :end 1)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 't)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :max-evals 0)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :max-evals 1)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :max-evals -1)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :max-evals 't)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :max-evals #C(0 0))))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :the-err -1)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :the-err 0)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :the-err 't)))
  (assert-error 'error                   (mjr_intg_loc-adp-dnc-trapezoidal #'identity                             (list :start 0   :end 1       :the-err #C(0 0))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-rect-left
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 1                             (mjr_intg_simple-rect-left    #'tf1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-rect-left    #'tf3 :start ta3  :end tb3))
  (assert-equal 1                             (mjr_intg_simple-rect-left      ti1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-rect-left      ti3 :start ta3  :end tb3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-rect-right
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 1/2                           (mjr_intg_simple-rect-right   #'tf1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-rect-right   #'tf3 :start ta3  :end tb3))
  (assert-equal 1/2                           (mjr_intg_simple-rect-right     ti1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-rect-right     ti3 :start ta3  :end tb3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-trap
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 3/4                           (mjr_intg_simple-trap         #'tf1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-trap         #'tf3 :start ta3  :end tb3))
  (assert-equal 3/4                           (mjr_intg_simple-trap           ti1 :start ta1  :end tb1))
  (assert-equal 0                             (mjr_intg_simple-trap           ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 3/4                           (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 1 :closed 't))
  (assert-equal 0                             (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 1 :closed 't))
  (assert-equal 3/4                           (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 1 :closed 't))
  (assert-equal 0                             (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 1 :closed 't))
  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 1 :closed 't) (mjr_intg_simple-trap #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 1 :closed 't) (mjr_intg_simple-trap #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-simpson
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 25/36                         (mjr_intg_simple-simpson      #'tf1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-simpson      #'tf3 :start ta3  :end tb3))
  (assert-equal 25/36                         (mjr_intg_simple-simpson        ti1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-simpson        ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 25/36                         (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 2 :closed 't))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 2 :closed 't))
  (assert-equal 25/36                         (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 2 :closed 't))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 2 :closed 't))

  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 2 :closed 't) (mjr_intg_simple-simpson #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 2 :closed 't) (mjr_intg_simple-simpson #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-simpson-3/8
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 111/160                       (mjr_intg_simple-simpson-3/8  #'tf1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-simpson-3/8  #'tf3 :start ta3  :end tb3))
  (assert-equal 111/160                       (mjr_intg_simple-simpson-3/8    ti1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-simpson-3/8    ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 111/160                       (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 3 :closed 't))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 3 :closed 't))
  (assert-equal 111/160                       (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 3 :closed 't))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 3 :closed 't))
  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 3 :closed 't) (mjr_intg_simple-simpson-3/8 #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 3 :closed 't) (mjr_intg_simple-simpson-3/8 #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-boole
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 4367/6300                   (mjr_intg_simple-boole  #'tf1 :start ta1  :end tb1))
  (assert-equal 4/3                         (mjr_intg_simple-boole  #'tf3 :start ta3  :end tb3))
  (assert-equal 4367/6300                   (mjr_intg_simple-boole    ti1 :start ta1  :end tb1))
  (assert-equal 4/3                         (mjr_intg_simple-boole    ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 4367/6300                   (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 4 :closed 't))
  (assert-equal 4/3                         (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 4 :closed 't))
  (assert-equal 4367/6300                   (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 4 :closed 't))
  (assert-equal 4/3                         (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 4 :closed 't))
  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 4 :closed 't) (mjr_intg_simple-boole #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 4 :closed 't) (mjr_intg_simple-boole #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-rect-mid
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 2/3                           (mjr_intg_simple-rect-mid     #'tf1 :start ta1  :end tb1))
  (assert-equal 2                             (mjr_intg_simple-rect-mid     #'tf3 :start ta3  :end tb3))
  (assert-equal 2/3                           (mjr_intg_simple-rect-mid       ti1 :start ta1  :end tb1))
  (assert-equal 2                             (mjr_intg_simple-rect-mid       ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 2/3                           (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 2 :closed nil))
  (assert-equal 2                             (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 2 :closed nil))
  (assert-equal 2/3                           (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 2 :closed nil))
  (assert-equal 2                             (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 2 :closed nil))
  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 2 :closed nil) (mjr_intg_simple-rect-mid #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 2 :closed nil) (mjr_intg_simple-rect-mid #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-milne
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't need many tests here.
  ;; Hand picked cases
  (assert-equal 218/315                       (mjr_intg_simple-milne  #'tf1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-milne  #'tf3 :start ta3  :end tb3))
  (assert-equal 218/315                       (mjr_intg_simple-milne    ti1 :start ta1  :end tb1))
  (assert-equal 4/3                           (mjr_intg_simple-milne    ti3 :start ta3  :end tb3))
  ;; Make sure hand picked cases match Newton-Cotes
  (assert-equal 218/315                       (mjr_intg_simple-newton-cotes #'tf1 :start ta1  :end tb1 :order 4 :closed nil))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes #'tf3 :start ta3  :end tb3 :order 4 :closed nil))
  (assert-equal 218/315                       (mjr_intg_simple-newton-cotes   ti1 :start ta1  :end tb1 :order 4 :closed nil))
  (assert-equal 4/3                           (mjr_intg_simple-newton-cotes   ti3 :start ta3  :end tb3 :order 4 :closed nil))
  ;; A few random test cases to make sure match Newton-Cotes
  (dotimes (i 100)
    (let ((u (rationalize (mjr_prng_float-co 2 20))))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf1 :start ta1 :end u :order 4 :closed nil) (mjr_intg_simple-milne #'tf1 :start ta1 :end u))
      (assert-equalp (mjr_intg_simple-newton-cotes #'tf6 :start ta6 :end u :order 4 :closed nil) (mjr_intg_simple-milne #'tf6 :start ta6 :end u))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-gauss-legendre
  ;; Note: The test cases for mjr_intg_divide-and-conquer make heavy use of this function, so we don't really need many tests here;
  ;; however, this is such an important function we include several thousand anyhow.
  ;; A few random test cases
  (dotimes (i 10000)
    (let ((u   (mjr_prng_float-co 2  20))
          (ord (mjr_prng_int-co   15 20)))
      (assert-equality (mjr_eps_make-fixed= .001)   (ti1 u) (mjr_intg_simple-gauss-legendre #'tf1 :start ta1 :end u :order ord) u ord)
      (assert-equality (mjr_eps_make-fixed= .001)   (ti6 u) (mjr_intg_simple-gauss-legendre #'tf6 :start ta6 :end u :order ord) u ord)
      (assert-equality (mjr_eps_make-fixed= .00001) (ti1 u) (mjr_intg_simple-gauss-legendre #'tf1 :start ta1 :end u :order 20)  u 20 )
      (assert-equality (mjr_eps_make-fixed= .00001) (ti6 u) (mjr_intg_simple-gauss-legendre #'tf6 :start ta6 :end u :order 20)  u 20 )))
  ;; Hand picked cases
  (loop for ord from 2 upto 2
        do (assert-equality (mjr_eps_make-fixed= .001)   0.693147180559945d0  (mjr_intg_simple-gauss-legendre  #'tf1 :start ta1  :end tb1 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .001)   1.3333333333333335d0 (mjr_intg_simple-gauss-legendre  #'tf3 :start ta3  :end tb3 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .001)   0.693147180559945d0  (mjr_intg_simple-gauss-legendre    ti1 :start ta1  :end tb1 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .001)   1.3333333333333335d0 (mjr_intg_simple-gauss-legendre    ti3 :start ta3  :end tb3 :order ord)))
  (loop for ord from 4 upto 20
        do (assert-equality (mjr_eps_make-fixed= .00001) 0.693147180559945d0  (mjr_intg_simple-gauss-legendre  #'tf1 :start ta1  :end tb1 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .00001) 1.3333333333333335d0 (mjr_intg_simple-gauss-legendre  #'tf3 :start ta3  :end tb3 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .00001) 0.693147180559945d0  (mjr_intg_simple-gauss-legendre    ti1 :start ta1  :end tb1 :order ord))
        do (assert-equality (mjr_eps_make-fixed= .00001) 1.3333333333333335d0 (mjr_intg_simple-gauss-legendre    ti3 :start ta3  :end tb3 :order ord)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_glb-adp-composite-trap-or-romberg
  ;; Note: mjr_intg_glb-adp-composite-trap-or-romberg is teh computational kernel for both mjr_intg_glb-adp-composite-trapezoidal and mjr_intg_glb-adp-composite-romberg, and is thus
  ;; well tested by the mjr_intg_glb-adp-composite-trapezoidal and mjr_intg_glb-adp-composite-romberg tests above.
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_divide-and-conquer
  ;; A few random test cases
  (dolist (intfun (list #'mjr_intg_simple-gauss-legendre))
    (loop for i from 1 upto 1000
          for u = (mjr_prng_float-co 2 20)
          for numi = 10
          do (assert-equality (mjr_eps_make-fixed= .0001) (ti1 u) (mjr_intg_divide-and-conquer #'tf1 intfun (list :start ta1 :end u :len 100)) 1 intfun numi)
          do (assert-equality (mjr_eps_make-fixed= .0001) (ti6 u) (mjr_intg_divide-and-conquer #'tf6 intfun (list :start ta6 :end u :len 100)) 6 intfun numi)))
  (dolist (intfun (list #'mjr_intg_simple-trap #'mjr_intg_simple-rect-left #'mjr_intg_simple-rect-right #'mjr_intg_simple-rect-mid))
    (loop for i from 1 upto 10
          for u = (mjr_prng_float-co 2 3)
          for numi = 5000
          do (assert-equality (mjr_eps_make-fixed= .01) (ti1 u) (mjr_intg_divide-and-conquer #'tf1 intfun (list :start ta1 :end u :len 100)) 1 intfun numi)
          do (assert-equality (mjr_eps_make-fixed= .01) (ti6 u) (mjr_intg_divide-and-conquer #'tf6 intfun (list :start ta6 :end u :len 100)) 6 intfun numi)))
  (dolist (intfun (list #'mjr_intg_simple-simpson #'mjr_intg_simple-simpson-3/8 #'mjr_intg_simple-boole #'mjr_intg_simple-milne))
    (loop for i from 1 upto 500
          for u = (mjr_prng_float-co 2 20)
          for numi = (+ 500 (truncate (* 50 u)))
          do (assert-equality (mjr_eps_make-fixed= .0001) (ti1 u) (mjr_intg_divide-and-conquer #'tf1 intfun (list :start ta1 :end u :len 100)) 1 intfun numi)
          do (assert-equality (mjr_eps_make-fixed= .0001) (ti6 u) (mjr_intg_divide-and-conquer #'tf6 intfun (list :start ta6 :end u :len 100)) 6 intfun numi)))
  (loop for o from 3 upto 20
        do (loop for i from 1 upto 10
                 for u = (mjr_prng_float-co 2 20)
                 for numi = (+ 500 (truncate (* 50 u)))
                 do (assert-equality (mjr_eps_make-fixed= .0001) (ti1 u) (mjr_intg_divide-and-conquer #'tf1
                                                                                                      (lambda (fun &key start end) 
                                                                                                        (mjr_intg_simple-newton-cotes fun :start start :end end :order o :closed 't))
                                                                                                      (list :start ta1 :end u :len 100)))
                 do (assert-equality (mjr_eps_make-fixed= .0001) (ti6 u) (mjr_intg_divide-and-conquer #'tf6
                                                                                                      (lambda (fun &key start end)
                                                                                                        (mjr_intg_simple-newton-cotes fun :start start :end end :order o :closed 't))
                                                                                                      (list :start ta6 :end u :len 100)))
                 when (> o 4)
                 do (assert-equality (mjr_eps_make-fixed= .001) (ti1 u) (mjr_intg_divide-and-conquer #'tf1
                                                                                                     (lambda (fun &key start end) 
                                                                                                       (mjr_intg_simple-newton-cotes fun :start start :end end :order o :closed nil))
                                                                                                     (list :start ta1 :end u :len 100)))
                 when (> o 4)
                 do (assert-equality (mjr_eps_make-fixed= .001) (ti6 u) (mjr_intg_divide-and-conquer #'tf6
                                                                                                     (lambda (fun &key start end)
                                                                                                       (mjr_intg_simple-newton-cotes fun :start start :end end :order o :closed 't))
                                                                                                     (list :start ta6 :end u :len 100)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_simple-newton-cotes

  ;; XREF: The test cases for mjr_intg_divide-and-conquer make heavy use of this function with high values for order.

  ;; XREF: The test cases for mjr_intg_simple-rect-mid, mjr_intg_simple-milne, mjr_intg_simple-trap, mjr_intg_simple-simpson,
  ;;       mjr_intg_simple-simpson-3/8, and mjr_intg_simple-bool extensively test this this function for low orders.

  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intg_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )

