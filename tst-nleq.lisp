;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-nleq.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for use-nleq.lisp.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_NLEQ-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_NLEQ :MJR_EPS))

(in-package :MJR_NLEQ-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  f1  (x) (values (+ (* 1 x x x x x) (* -4 x x x x) (* -5 x x x) (* 20 x x) (* 4 x) -16) ; -1 1 -2 2 4
                        (+ (* 5 x x x x) (* -16 x x x) (* -15 x x) (* 40 x) 4)
                        (+ (* 20 x x x) (* -48 x x) (* -30 x) 40)
                        (+ (* 60 x x) (* -96 x) -30)))
(defvar f1s "x^5-4*x^4-5*x^3+20*x^2+4*x-16")
(defun  f2  (x) (values (sin x)                                                                ; 0
                        (cos x)
                        (- (sin x))))
(defvar f2s "sin(x)")
(defun  f3  (x) (values x                                                                      ; 0
                        1
                        0))
(defvar f3s "x")
(defun  f4  (x) (values (* 3 x)                                                                ; 0
                        3
                        0))
(defvar f4s "3*x")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-bsect
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f3  -1 1))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f3  -1 1 :use-false-position 't))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f4  -1 2 :use-false-position 't))

  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f3s -1 1))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f3s -1 1 :use-false-position 't))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f4s -1 2 :use-false-position 't))
  ;; Close to a good answer..
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f2  -1.0  0.5))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f3  -1.0  1.5))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -10.0 -1.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -1.5  -0.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -1.5  -10  :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -0.5  -1.5 :xeps 1e-10))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f2s -1.0  0.5))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f3s -1.0  1.5))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -10.0 -1.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -1.5  -0.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -1.5  -10  :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -0.5  -1.5 :xeps 1e-10))
  ;; Now do it with :use-false-position..
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f2  -1.0  0.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f3  -1.0  1.5              :use-false-position 't))
; (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -10.0 -1.5 :xeps 1e-10 :use-false-position 't)) ; No workie
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -1.5  -0.5 :xeps 1e-10 :use-false-position 't))
; (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -1.5  -10  :xeps 1e-10 :use-false-position 't)) ; No workie
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -0.5  -1.5 :xeps 1e-10 :use-false-position 't))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f2s -1.0  0.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f3s -1.0  1.5              :use-false-position 't))
; (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -10.0 -1.5 :xeps 1e-10 :use-false-position 't)) ; No workie
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -1.5  -0.5 :xeps 1e-10 :use-false-position 't))
; (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -1.5  -10  :xeps 1e-10 :use-false-position 't)) ; No workie
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -0.5  -1.5 :xeps 1e-10 :use-false-position 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-newton
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-newton #'f2   1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-newton #'f1   1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-newton #'f1   2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-newton #'f1  -1.1))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-newton   f2s  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-newton   f1s  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-newton   f1s  2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-newton   f1s -1.1))

  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "log(x)*sin(x)" 3.5e0))
  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "sin(x)*sin(x)" 3.5e0))
  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "x^2*sin(x)"    3.5e0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-laguerre
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-laguerre #'f2  10 1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-laguerre #'f1  5  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-laguerre #'f1  5  2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-laguerre #'f1  5 -1.1))

;;  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-laguerre   f2s 10 1.1)) ;; BROKEY
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-laguerre   f1s 5  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-laguerre   f1s 5  2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-laguerre   f1s 5 -1.1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_fixed-point-itr
  (assert-equality (mjr_eps_make-fixed= .001) 1.1140665 (mjr_nleq_fixed-point-itr (lambda (x) (/ (sin x))) 1))

  (assert-equality (mjr_eps_make-fixed= .001) 1.1140665 (mjr_nleq_fixed-point-itr "1/sin(x)" 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
