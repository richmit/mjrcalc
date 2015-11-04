;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-optm.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-optm.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_OPTM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_OPTM :MJR_EPS))

(in-package :MJR_OPTM-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rosenbrock-banana (X Y)
  "Rosenbrock's Banana Function.  Returns values for function, gradient, and hessian.
f(x,y) = 100*(y - x^2)^2 + (1 - x)^2
df/dx  = -400*x*(y-x^2)-2*(1-x)
df/dy  = 200*(y-x^2)
df/dxx = -80000*y^3+720000*x^2*y^2+400*y^2-1200000*x^4*y-4800*x^2*y+4800*x*y-800*y+560000*x^6+6000*x^4-8000*x^3+2412*x^2-24*x+12
df/dxy = -240000*x*y^2+480000*x^3*y+800*x*y-800*y-240000*x^5-1600*x^3+2400*x^2-800*x
df/dyx = -240000*x*y^2+480000*x^3*y+800*x*y-800*y-240000*x^5-1600*x^3+2400*x^2-800*x
df/dyy = 120000*y^2-240000*x^2*y+120000*x^4+400*x^2-800*x+400
Global Minimum: f(1,1)=0
For a nice plot see: exp-ClassicOptBanana.lisp"
  (values (+ (* 100 (EXPT (- Y (EXPT X 2)) 2)) (EXPT (- 1 X) 2))
          (make-array 2 :initial-contents (list
                                           (+ (- (* 400 X (- Y (EXPT X 2)))) (- (* 2 (- 1 X))))
                                           (* 200 (- Y (EXPT X 2)))))

          (make-array '(2 2) :initial-contents (list (list (+ (- (* 80000 (EXPT Y 3))) (* 720000 (EXPT X 2) (EXPT Y 2)) (* 400 (EXPT Y 2))
                                                              (- (* 1200000 (EXPT X 4) Y)) (- (* 4800 (EXPT X 2) Y)) (* 4800 X Y)
                                                              (- (* 800 Y)) (* 560000 (EXPT X 6)) (* 6000 (EXPT X 4))
                                                              (- (* 8000 (EXPT X 3))) (* 2412 (EXPT X 2)) (- (* 24 X)) 12)
                                                           (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X))))
                                                     (list (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X)))
                                                           (+ (- (* 120000 (EXPT Y 2)) (* 240000 (EXPT X 2) Y)) (* 120000 (EXPT X 4))
                                                              (* 400 (EXPT X 2)) (- (* 800 X)) 400))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun freudenstein-roth (X Y)
  "Freudenstein and Roth Function.  Returns values for function, gradient, and hessian.
f(x,y) =  (-13 + x + ((5 - y)*y - 2)*y)^2 + (-29 + x + ((y + 1)*y - 14)* y)^2
df/dx  = 12*y^2-32*y+4*x-84
df/dy  = 12*y^5-40*y^4+8*y^3-240*y^2+24*x*y+24*y-32*x+864
df/dxx = 4
df/dxy = 24*y-32
df/dyx = 24*y-32
df/dyy = 60*y^4-160*y^3+24*y^2-480*y+24*x+24
Global Minimum: f(5,4)=0
Local Minimum:  f(11.41, -0.8986)=48.9842
For a nice plot see: exp-ClassicOptFreudensteinRoth.lisp"
  (values (+ (EXPT (+ (- 13) X (* (- (* (- 5 Y) Y) 2) Y)) 2)
             (EXPT (+ (- 29) X (* (- (* (+ Y 1) Y) 14) Y)) 2))
          (make-array 2 :initial-contents (list
                                           (+ (- (* 12 (EXPT Y 2)) (* 32 Y)) (* 4 X) (- 84))
                                           (+ (- (* 12 (EXPT Y 5)) (* 40 (EXPT Y 4)))
                                              (* 8 (EXPT Y 3)) (- (* 240 (EXPT Y 2)))
                                              (* 24 X Y) (* 24 Y) (- (* 32 X)) 864)))
          (make-array '(2 2) :initial-contents (list (list 4
                                                           (- (* 24 y) 32))
                                                     (list (- (* 24 y) 32)
                                                           (+ (- (* 60 (EXPT Y 4)) (* 160 (EXPT Y 3)))
                                                              (* 24 (EXPT Y 2)) (- (* 480 Y)) (* 24 X) 24))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beale (X Y)
  "Beale's Function.  Returns values for function, gradient, and hessian.
f(x,y) = (3/5 - x*(1-y))^2 + (9/4 - x*(1-y^2))^2 + (21/8 - x*(1-y^3))^2
df/dx  = 2*x*y^6+2*x*y^4-4*x*y^3+21*y^3/4-2*x*y^2+9*y^2/2-4*x*y+6*y/5+6*x-219/20
df/dy  = 6*x^2*y^5+4*x^2*y^3-6*x^2*y^2+63*x*y^2/4-2*x^2*y+9*x*y-2*x^2+6*x/5
df/dxx = 2*y^6+2*y^4-4*y^3-2*y^2-4*y+6
df/dxy = 12*x*y^5+8*x*y^3-12*x*y^2+63*y^2/4-4*x*y+9*y-4*x+6/5
df/dyx = 12*x*y^5+8*x*y^3-12*x*y^2+63*y^2/4-4*x*y+9*y-4*x+6/5
df/dyy = 30*x^2*y^4+12*x^2*y^2-12*x^2*y+63*x*y/2-2*x^2+9*x
Starting Point: (1, 1)
Global Minimum: f(3,1/2)=0"
  (values (+ (EXPT (- 3/2  (* X (- 1 Y)))          2)
             (EXPT (- 9/4  (* X (- 1 (EXPT Y 2)))) 2)
             (EXPT (- 21/8 (* X (- 1 (EXPT Y 3)))) 2))
          (make-array 2 :initial-contents (list
                                           (+ (* 2 X (EXPT Y 6)) (* 2 X (EXPT Y 4)) (- (* 4 X (EXPT Y 3)))
                                              (/ (* 21 (EXPT Y 3)) 4) (- (* 2 X (EXPT Y 2))) (/ (* 9 (EXPT Y 2)) 2)
                                              (- (* 4 X Y)) (/ (* 6 Y) 5) (* 6 X) (- (/ 219 20)))
                                           (+ (* 6 (EXPT X 2) (EXPT Y 5)) (* 4 (EXPT X 2) (EXPT Y 3))
                                              (- (* 6 (EXPT X 2) (EXPT Y 2))) (/ (* 63 X (EXPT Y 2)) 4)
                                              (- (* 2 (EXPT X 2) Y)) (* 9 X Y) (- (* 2 (EXPT X 2))) (/ (* 6 X) 5))))
          (make-array '(2 2) :initial-contents (list (list
                                                      (+ (* 2 (EXPT Y 6)) (* 2 (EXPT Y 4)) (- (* 4 (EXPT Y 3))) (- (* 2 (EXPT Y 2)))
                                                         (- (* 4 Y)) 6)
                                                      (+ (* 12 X (EXPT Y 5)) (* 8 X (EXPT Y 3)) (- (* 12 X (EXPT Y 2)))
                                                         (/ (* 63 (EXPT Y 2)) 4) (- (* 4 X Y)) (* 9 Y) (- (* 4 X)) (/ 6 5)))
                                                     (list
                                                      (+ (* 12 X (EXPT Y 5)) (* 8 X (EXPT Y 3)) (- (* 12 X (EXPT Y 2)))
                                                         (/ (* 63 (EXPT Y 2)) 4) (- (* 4 X Y)) (* 9 Y) (- (* 4 X)) (/ 6 5))
                                                      (+ (* 30 (EXPT X 2) (EXPT Y 4)) (* 12 (EXPT X 2) (EXPT Y 2))
                                                         (- (* 12 (EXPT X 2) Y)) (/ (* 63 X Y) 2) (- (* 2 (EXPT X 2))) (* 9 X)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_optm_minimize-mjr-descent
  ;; Start at minimum
  (assert-equalp (values #(1 1)   0 34 "X-EPS") (mjr_optm_minimize-mjr-descent #'rosenbrock-banana #(1 1)))
;;  (assert-equalp (values #(3 1/2) 0 34 "X-EPS") (mjr_optm_minimize-mjr-descent #'beale #(3 1/2)))
;;  (assert-equalp (values #(5 4)   0 34 "X-EPS") (mjr_optm_minimize-mjr-descent #'freudenstein-roth #(5 4)))
  ;; Typical test
  ;; (multiple-value-bind (v f i r) (mjr_optm_minimize-mjr-descent #'rosenbrock-banana #(2.123456 2.654321))
  ;;   (declare (ignore i))
  ;;   (let ((x (aref v 0))
  ;;         (y (aref v 1)))
  ;;     (assert-equalp r "X-EPS")
  ;;     (assert-equality (mjr_eps_make-fixed= 0.00001) 1 x)
  ;;     (assert-equality (mjr_eps_make-fixed= 0.00001) 1 y)
  ;;     (assert-equality (mjr_eps_make-fixed= 0.00001) 0 f)))
  1

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_optm_minimize-random-delta
  ;; Start at minimum
  (assert-equalp (values #(1 1)   0 17 "X-EPS") (mjr_optm_minimize-random-delta #'rosenbrock-banana #(1 1)   :arg-mode :arg-number))
  (assert-equalp (values #(3 1/2) 0 17 "X-EPS") (mjr_optm_minimize-random-delta #'beale             #(3 1/2) :arg-mode :arg-number))
  (assert-equalp (values #(5 4)   0 17 "X-EPS") (mjr_optm_minimize-random-delta #'freudenstein-roth #(5 4)   :arg-mode :arg-number))
  ;; Typical test
  (multiple-value-bind (x f i r) (mjr_optm_minimize-random-delta #'rosenbrock-banana #(2.123456 2.654321) :max-itr 1000000 :arg-mode :arg-number)
    (declare (ignore i))
    (assert-equalp r "X-EPS")
    (assert-equality (mjr_eps_make-fixed= 0.1)   1 (aref x 0))
    (assert-equality (mjr_eps_make-fixed= 0.1)   1 (aref x 1))
    (assert-equality (mjr_eps_make-fixed= 0.001) 0 f))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_optm_minimize-hooke-jeeves
  ;; Start at minimum
  (assert-equalp (values #(1 1)   0 17 "X-EPS") (mjr_optm_minimize-hooke-jeeves #'rosenbrock-banana #(1 1)   :arg-mode :arg-number))
  (assert-equalp (values #(3 1/2) 0 17 "X-EPS") (mjr_optm_minimize-hooke-jeeves #'beale             #(3 1/2) :arg-mode :arg-number))
  (assert-equalp (values #(5 4)   0 17 "X-EPS") (mjr_optm_minimize-hooke-jeeves #'freudenstein-roth #(5 4)   :arg-mode :arg-number))
  ;; Typical test
  (multiple-value-bind (x f i r) (mjr_optm_minimize-hooke-jeeves #'rosenbrock-banana #(2 2) :arg-mode :arg-number)
    (declare (ignore i))
    (assert-equalp r "X-EPS")
    (assert-equality (mjr_eps_make-fixed= 0.00000001) 1 (aref x 0))
    (assert-equality (mjr_eps_make-fixed= 0.00000001) 1 (aref x 1))
    (assert-equality (mjr_eps_make-fixed= 0.00001)    0 f))
  (multiple-value-bind (x f i r) (mjr_optm_minimize-hooke-jeeves #'rosenbrock-banana #(2.123456 2.654321) :max-itr 2000 :arg-mode :arg-number)
    (declare (ignore i))
    (assert-equalp r "X-EPS")
    (assert-equality (mjr_eps_make-fixed= 0.01)  1 (aref x 0))
    (assert-equality (mjr_eps_make-fixed= 0.01)  1 (aref x 1))
    (assert-equality (mjr_eps_make-fixed= 0.001) 0 f))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_optm_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
