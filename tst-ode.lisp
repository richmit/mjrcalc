;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-ode.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-ode.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010, 2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_ODE-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_ODE :MJR_POLY :MJR_EPS :MJR_PRNG :MJR_DQUAD))

(in-package :MJR_ODE-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun restrictedThreeBody (time y)
  (declare (ignore time))
  (let* ((x1  (aref y 0))
         (x2  (aref y 1))
         (v1  (aref y 2))
         (v2  (aref y 3))
         (mu  20/1629)
         (s1  (+ x1 mu -1))
         (s2  (- 1 mu))
         (s3  (+ x1 mu))
         (x22 (expt x2 2))
         (s12 (expt s1 2))
         (s32 (expt s3 2))
         (bf1 (expt (+ x22 s12) 3/2))
         (bf2 (expt (+ x22 s32) 3/2)))
    (vector v1
            v2
            (- (* 2 v2) (- x1) (/ (* mu s1) bf1) (/ (* s2 s3) bf2))
            (- x2 (* 2 v1) (/ (* mu x2) bf1) (/ (* s2 x2) bf2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun polyd01 (x y) (or y) (mjr_poly_eval #(7 -168 1630 -8080 21075 -26000 10000) x))
;(defun polys01 (x)          (mjr_poly_eval #(1  -28  326 -2020  7025 -13000 10000 0)  x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun polyd01 (x y) (or y) (mjr_poly_eval #(6 -10 12 -12 10 -6) x))
(defun polys01 (x)          (mjr_poly_eval #(1 -2 3 -4 5 -6 7)  x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-kernel
  ;; We test mjr_ode_single-step-erk by comparing several "adapters" of the function to direct implementations
  (loop for (algo-dir algo-ker) in (list (list #'mjr_ode_erk-step-runge-kutta-4-direct #'mjr_ode_erk-step-runge-kutta-4)
                                         (list #'mjr_ode_erk-step-euler-1-direct       #'mjr_ode_erk-step-euler-1)
                                         (list #'mjr_ode_erk-step-heun-2-direct        #'mjr_ode_erk-step-heun-2))
        do (dotimes (i 150)
             (let ((ivx (mjr_prng_int-co 1 50))
                   (h   (/ (mjr_prng_random 50) 50.0)))
               (map 'vector
                    (lambda (x y) (assert-equality #'mjr_eps_= x y 0.0001))
                    (first (apply algo-dir (list #'restrictedThreeBody) ivx (list #(9/10 0 0 -2)) h nil nil nil nil nil))
                    (first (apply algo-ker (list #'restrictedThreeBody) ivx (list #(9/10 0 0 -2)) h nil nil nil nil nil)))))
        do (dotimes (i 150)
             (let ((ivx (mjr_prng_int-co 1 50))
                   (h   (/ (mjr_prng_random 50) 50.0))
                   (ivy (list (vector (mjr_prng_int-co 1 50)))))
               (assert-equality (mjr_eps_make-fixed= -.0001)
                                (first (apply algo-dir (list #'polyd01) ivx ivy h nil nil nil nil nil))
                                (first (apply algo-ker (list #'polyd01) ivx ivy h nil nil nil nil nil))))))
  ;; XREF: mjr_ode_erk-step-kernel is also tested heavily in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_slv-ivp-erk-interval
  (let* ((start  3.0d0)
         (ivy    (polys01 start)))
    (dotimes (i 10)
      (let ((end (mjr_prng_float-oo 3 3.5)))
        (dolist (algo (list
                       ;;#'mjr_ode_erk-step-euler-1-direct
                       #'mjr_ode_erk-step-heun-euler-1-2
                       #'mjr_ode_erk-step-mid-point-2
                       #'mjr_ode_erk-step-heun-2
                       ))
          (assert-equality (mjr_eps_make-fixed= -5)
                           (polys01 end)
                           (mjr_ode_slv-ivp-erk-interval #'polyd01 ivy start end :y-err-abs-max 1d-3 :x-delta-min 1d-10 :algorithm algo)
                           (list algo end))))))
  ;; Higher accuracy solvers
  (let* ((start  3.0d0)
         (ivy    (polys01 start)))
    (dotimes (i 20)
      (let ((end (mjr_prng_float-oo 3 4)))
        (dolist (algo (list
                       #'mjr_ode_erk-step-merson-4-5
                       #'mjr_ode_erk-step-zonneveld-4-3
                       #'mjr_ode_erk-step-fehlberg-4-5
                       #'mjr_ode_erk-step-fehlberg-7-8
                       #'mjr_ode_erk-step-bogackia-shampine-3-2
                       #'mjr_ode_erk-step-cash-karp-5-4
                       #'mjr_ode_erk-step-dormand-prince-5-4
                       #'mjr_ode_erk-step-verner-6-5
                       #'mjr_ode_erk-step-kutta-three-eight-4
                       #'mjr_ode_erk-step-runge-kutta-4
                       ))
          (assert-equality (mjr_eps_make-fixed= -5)
                           (polys01 end)
                           (mjr_ode_slv-ivp-erk-interval #'polyd01 ivy start end :y-err-abs-max 1d-3 :x-delta-min 1d-10 :algorithm algo)
                           (list algo end))))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_slv-ivp-erk-mesh
  (let* ((start  3.0d0)
         (ivy    (polys01 start)))
    (dotimes (i 20)
      (let ((end (mjr_prng_float-oo 3 4))
            (len (mjr_prng_int-co 5 15)))
        (dolist (algo (list
                       #'mjr_ode_erk-step-fehlberg-4-5
                       #'mjr_ode_erk-step-fehlberg-7-8
                       #'mjr_ode_erk-step-bogackia-shampine-3-2
                       #'mjr_ode_erk-step-cash-karp-5-4
                       #'mjr_ode_erk-step-dormand-prince-5-4
                       #'mjr_ode_erk-step-verner-6-5))
          (let* ((sol   (mjr_ode_slv-ivp-erk-mesh #'polyd01 ivy (list :start start :end end :len len) :y-err-abs-max 1d-6 :algorithm algo))
                 (x     (mjr_dquad_get-axis-vector sol "x"))
                 (y     (mjr_dquad_get-data-array sol "y"))
                 (x-len (length x)))
            (assert-equalp len x-len)
            (dotimes (i x-len)
              (assert-equality (mjr_eps_make-fixed= -5) (polys01 (aref x i)) (aref y i) (list algo end len i x y))))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-zonneveld-4-3
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-merson-4-5
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-heun-euler-1-2
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-fehlberg-4-5
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-fehlberg-7-8
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-bogackia-shampine-3-2
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-cash-karp-5-4
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-dormand-prince-5-4
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-verner-6-5
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval & mjr_ode_slv-ivp-erk-mesh
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-runge-kutta-4-direct
  ;; XREF: Tested in mjr_ode_erk-step-kernel
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-runge-kutta-4
  ;; XREF: Tested in mjr_ode_erk-step-kernel & mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-euler-1-direct
  ;; XREF: Tested in mjr_ode_erk-step-kernel
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-euler-1
  ;; XREF: Tested in mjr_ode_erk-step-kernel
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-heun-2-direct
  ;; XREF: Tested in mjr_ode_erk-step-kernel
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-heun-2
  ;; XREF: Tested in mjr_ode_erk-step-kernel & mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-kutta-three-eight-4
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ode_erk-step-mid-point-2
  ;; XREF: Tested in mjr_ode_slv-ivp-erk-interval
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
