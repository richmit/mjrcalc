;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-nleqm.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-nleqm.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1995-2010,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_NLEQM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_NLEQM :MJR_EPS))

(in-package :MJR_NLEQM-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tf1 (x)
  "Return f and df.
Roots (2):
     * root 1 ~ #(-0.6397934167947074d0 -0.19186949964473912d0)
     * root 2 ~ #(0.8493088032390027d0 0.9927594657716575d0)
Example Newton Iterations:
     * x0     #( 0.0000000000000000d0  0.00000000000000000d0)
     * x1     #(-1.0000000000000000d0 -0.66666666666666666d0)
     * x2     #(-0.8749999999999998d0 -0.10833333333333350d0)
     * x3     #(-0.6728107572941081d0 -0.18800647916161534d0)
     * x4     #(-0.6403653425200700d0 -0.19204044857330035d0)
     * x5     #(-0.6397935460210262d0 -0.19186963941596752d0)
     * x6     #(-0.6397934167947249d0 -0.19186949964474756d0)
     * x7     #(-0.6397934167947074d0 -0.19186949964473934d0)
     * x8     #(-0.6397934167947074d0 -0.19186949964473912d0)"
  (let* ((x1 (aref x 0))
         (x2 (aref x 1)))
    (values (vector (- (* x1 x1)   (* -2 x2 x2) (* x1 x2) x1            1)
                    (+ (* 3 x1 x1) (* 2 x2 x2)  (* x1 x2)    (* -3 x2) -2))
            (make-array '(2 2) :initial-contents
                        (list (list (- (* 2 x1) x2 1) (- (* 4 x2) x1))
                              (list (+ (* 6 x1) x2)   (+ (* 4 x2) x1 -3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tf2 (x1 x2)
  "Return f and df.
Roots(4):
     * root 1 = (vector (+ (sqrt (/ 1 3))) (+ (sqrt (/ 8 3)))) ~ #( 0.57735026  1.6329932)
     * root 2 = (vector (- (sqrt (/ 1 3))) (+ (sqrt (/ 8 3)))) ~ #(-0.57735026  1.6329932)
     * root 3 = (vector (+ (sqrt (/ 1 3))) (- (sqrt (/ 8 3)))) ~ #( 0.57735026 -1.6329932)
     * root 4 = (vector (- (sqrt (/ 1 3))) (- (sqrt (/ 8 3)))) ~ #(-0.57735026 -1.6329932)
Example Newton Iterations:
     * x0:   #(1.0000000000000000d0 1.0000000000000000d0)
     * x1:   #(0.6666666666666666d0 1.8333333333333335d0)
     * x2:   #(0.5833333333333334d0 1.6439393939393938d0)
     * x3:   #(0.5773809523809522d0 1.6330296048037984d0)
     * x4:   #(0.5773502700049092d0 1.6329931622620855d0)"
  (values (vector (+ (* x1 x1)    (* x2 x2)       -3)
                  (+ (* -2 x1 x1) (* -1/2 x2 x2)  2))
          (make-array '(2 2) :initial-contents
                      (list (list (* 2 x1)  (* 2 x2))
                            (list (* -4 x1) (* -1 x2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tf3 (x)
  "Return f and df.
Roots(2):
     * root 1 ~ #( 0.826031357654187d0  0.5636241621612585d0)
     * root 2 ~ #(-0.826031357654187d0 -0.5636241621612585d0)"
  (let* ((x1 (aref x 0))
         (x2 (aref x 1)))
    (values  (vector (- x2 (expt x1 3))
                     (+ (* x1 x1) (* x2 x2) -1))
             (make-array '(2 2) :initial-contents (list (list (* -3 x1 x1) 1)
                                                        (list (* 2 x1)     (* 2 x2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tf4i (x)
  "Return f and df^-1.
Roots(2):
     * root 1 = #(1 0)
     * root 2 = #(0 1)"
  (let* ((x1 (aref x 0))
         (x2 (aref x 1))
         (d  (- (* 2 x1) (* 2 x2))))
    (values (vector (+ (* x1 x1) (* x2 x2) -1)
                    (+ x1 x2 -1))
            (make-array '(2 2) :initial-contents (list (list (/ 1 d)  (/ (* -2 x2) d))
                                                       (list (/ -1 d) (/ (* 2 x1) d)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tf4 (x)
  "Return f and df.
Roots(2):
     * root 1 = #(1 0)
     * root 2 = #(0 1)"
  (let* ((x1 (aref x 0))
         (x2 (aref x 1)))
    (values (vector (+ (* x1 x1) (* x2 x2) -1)
                    (+ x1 x2 -1))
            (make-array '(2 2) :initial-contents (list (list (* 2 x1) (* 2 x2))
                                                       (list 1        1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleqm_root-newton
  (assert-equality #'mjr_eps_=
                   #(-0.6397934167947074d0 -0.19186949964473912d0)  ; Expected
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf1 #(0.0d0 0.0d0)   :max-itr 10 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector))))
  (assert-equality #'mjr_eps_=
                   #(0.8493088032390027d0 0.9927594657716575d0) ; Expected
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf1 #(2.0d0 2.0d0)   :max-itr 10 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector))))
  (assert-equality #'mjr_eps_=
                   (vector (+ (sqrt (/ 1 3))) (+ (sqrt (/ 8 3))))
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf2 #(1.0d0 1.0d0)   :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-number))))
  (assert-equality #'mjr_eps_=
                   (vector (- (sqrt (/ 1 3))) (+ (sqrt (/ 8 3))))
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf2 #(-1.0d0 1.0d0)  :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-number))))
  (assert-equality #'mjr_eps_=
                   (vector (+ (sqrt (/ 1 3))) (- (sqrt (/ 8 3))))
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf2 #(1.0d0 -1.0d0)  :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-number))))
  (assert-equality #'mjr_eps_=
                   (vector (- (sqrt (/ 1 3))) (- (sqrt (/ 8 3))))
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf2 #(-1.0d0 -1.0d0) :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-number))))
  (assert-equality #'mjr_eps_=
                   #( 0.826031357654187d0  0.5636241621612585d0)
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf3 #(1.0d0 1.0d0)   :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector))))
  (assert-equality #'mjr_eps_=
                   #(-0.826031357654187d0 -0.5636241621612585d0)
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf3 #(-1.0d0 -1.0d0) :max-itr 15 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector))))
  (assert-equality #'mjr_eps_=
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf4i #(1.0d0 -1.0d0) :max-itr 20 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector :df-is-inverse 't)))
                   (car (multiple-value-list (mjr_nleqm_root-newton #'tf4  #(1.0d0 -1.0d0) :max-itr 20 :yeps 1d-15 :xeps 1d-25 :arg-mode :arg-vector))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)


;; Maxima test code
;;
;;     load("linearalgebra");
;;     f1 : x^2+2*y^2-x*y-x-1$
;;     f2 : 3*x^2+2*y^2+x*y-3*y-2$
;;     NI :: (invert(jacobian([f1, f2], [x,y])) . (-[f1, f2]))$
;;
;;     NI, x=0.0, y=0.0;
;;     % + NI, x=%[1][1], y=%[2][1];
;;     ...DO ABOVE SEVERAL TIMES...
;;     sublis([x=%[1][1], y=%[2][1]], matrix([f1],[f2]));
;;
;;     xc : matrix([0.0], [0.0]);
;;     'xc :: xc+sublis([x=xc[1][1], y=xc[2][1]], NI);
;;     ...DO ABOVE SEVERAL TIMES...
;;     sublis([x=xc[1][1], y=xc[2][1]], matrix([f1],[f2]));
;;
;;     ev(xc, xc='xc+sublis([x=xc[1][1], y=xc[2][1]], NI));
