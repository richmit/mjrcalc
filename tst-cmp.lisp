;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-cmp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_CMP.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CMP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CMP :MJR_PRNG))

(in-package :MJR_CMP-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_cmp_BASIC

  (let ((old-eps *mjr_cmp_eps*))
    (setq *mjr_cmp_eps* (float 1/10000))

    (assert-true   (mjr_cmp_> (+ 1 1/100000) 1))
    (assert-false  (mjr_cmp_> (- 1 1/100000) 1))
    (assert-false  (mjr_cmp_> (+ 1 1/100000) 1.0))
    (assert-false  (mjr_cmp_> (- 1 1/100000) 1.0))

    (assert-false  (mjr_cmp_< (+ 1 1/100000) 1))
    (assert-true   (mjr_cmp_< (- 1 1/100000) 1))
    (assert-false  (mjr_cmp_< (+ 1 1/100000) 1.0))
    (assert-false  (mjr_cmp_< (- 1 1/100000) 1.0))

    (assert-false  (mjr_cmp_= (+ 1 1/100000) 1))
    (assert-false  (mjr_cmp_= (- 1 1/100000) 1))
    (assert-true   (mjr_cmp_= (+ 1 1/100000) 1.0))
    (assert-true   (mjr_cmp_= (- 1 1/100000) 1.0))

    (setq *mjr_cmp_eps* old-eps)))

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_cmp_RAT

      (assert-true  (mjr_cmp_<  1  2))
      (assert-false (mjr_cmp_<  2  1))
      (assert-false (mjr_cmp_< -1 -2))
      (assert-true  (mjr_cmp_< -2 -1))
      (assert-true  (mjr_cmp_< -1  2))
      (assert-false (mjr_cmp_<  1 -2))
      (assert-false (mjr_cmp_<  1  1))
      (assert-false (mjr_cmp_<  0  0))
      (assert-false (mjr_cmp_< -1 -1))

      (assert-false (mjr_cmp_>  1  2))
      (assert-true  (mjr_cmp_>  2  1))
      (assert-true  (mjr_cmp_> -1 -2))
      (assert-false (mjr_cmp_> -2 -1))
      (assert-false (mjr_cmp_> -1  2))
      (assert-true  (mjr_cmp_>  1 -2))
      (assert-false (mjr_cmp_>  1  1))
      (assert-false (mjr_cmp_>  0  0))
      (assert-false (mjr_cmp_> -1 -1))
      (assert-true  (mjr_cmp_>  1 -1))
      (assert-false (mjr_cmp_> -1  1))

      (assert-false (mjr_cmp_=  1  2))
      (assert-false (mjr_cmp_=  2  1))
      (assert-false (mjr_cmp_= -1 -2))
      (assert-false (mjr_cmp_= -2 -1))
      (assert-false (mjr_cmp_= -1  2))
      (assert-false (mjr_cmp_=  1 -2))
      (assert-true  (mjr_cmp_=  1  1))
      (assert-true  (mjr_cmp_=  0  0))
      (assert-true  (mjr_cmp_= -1 -1))
      (assert-false (mjr_cmp_=  1 -1))
      (assert-false (mjr_cmp_= -1  1))

      (assert-true  (mjr_cmp_<=  1  2))
      (assert-false (mjr_cmp_<=  2  1))
      (assert-false (mjr_cmp_<= -1 -2))
      (assert-true  (mjr_cmp_<= -2 -1))
      (assert-true  (mjr_cmp_<= -1  2))
      (assert-false (mjr_cmp_<=  1 -2))
      (assert-true  (mjr_cmp_<=  1  1))
      (assert-true  (mjr_cmp_<=  0  0))
      (assert-true  (mjr_cmp_<= -1 -1))
      (assert-false (mjr_cmp_<=  1 -1))
      (assert-true  (mjr_cmp_<= -1  1))

      (assert-false (mjr_cmp_>=  1  2))
      (assert-true  (mjr_cmp_>=  2  1))
      (assert-true  (mjr_cmp_>= -1 -2))
      (assert-false (mjr_cmp_>= -2 -1))
      (assert-false (mjr_cmp_>= -1  2))
      (assert-true  (mjr_cmp_>=  1 -2))
      (assert-true  (mjr_cmp_>=  1  1))
      (assert-true  (mjr_cmp_>=  0  0))
      (assert-true  (mjr_cmp_>= -1 -1))
      (assert-true  (mjr_cmp_>=  1 -1))
      (assert-false (mjr_cmp_>= -1  1))

      (assert-true  (mjr_cmp_!=  1  2))
      (assert-true  (mjr_cmp_!=  2  1))
      (assert-true  (mjr_cmp_!= -1 -2))
      (assert-true  (mjr_cmp_!= -2 -1))
      (assert-true  (mjr_cmp_!= -1  2))
      (assert-true  (mjr_cmp_!=  1 -2))
      (assert-false (mjr_cmp_!=  1  1))
      (assert-false (mjr_cmp_!=  0  0))
      (assert-false (mjr_cmp_!= -1 -1))
      (assert-true  (mjr_cmp_!=  1 -1))
      (assert-true  (mjr_cmp_!= -1  1))

      (assert-false (mjr_cmp_=0  1))
      (assert-false (mjr_cmp_=0  -1))
      (assert-true  (mjr_cmp_=0  0))

      (assert-false (mjr_cmp_zerop  1))
      (assert-false (mjr_cmp_zerop  -1))
      (assert-true  (mjr_cmp_zerop  0))

      (assert-true  (mjr_cmp_!=0  1))
      (assert-true  (mjr_cmp_!=0  -1))
      (assert-false (mjr_cmp_!=0  0))

      (assert-true  (mjr_cmp_not-zerop  1))
      (assert-true  (mjr_cmp_not-zerop  -1))
      (assert-false (mjr_cmp_not-zerop  0))

      (assert-true  (mjr_cmp_abs<  1  2))
      (assert-false (mjr_cmp_abs<  2  1))
      (assert-true  (mjr_cmp_abs< -1 -2))
      (assert-false (mjr_cmp_abs< -2 -1))
      (assert-true  (mjr_cmp_abs< -1  2))
      (assert-true  (mjr_cmp_abs<  1 -2))
      (assert-false (mjr_cmp_abs<  1  1))
      (assert-false (mjr_cmp_abs<  0  0))
      (assert-false (mjr_cmp_abs< -1 -1))
      (assert-false (mjr_cmp_abs<  1 -1))
      (assert-false (mjr_cmp_abs< -1  1))

      (assert-false (mjr_cmp_abs>  1  2))
      (assert-true  (mjr_cmp_abs>  2  1))
      (assert-false (mjr_cmp_abs> -1 -2))
      (assert-true  (mjr_cmp_abs> -2 -1))
      (assert-false (mjr_cmp_abs> -1  2))
      (assert-false (mjr_cmp_abs>  1 -2))
      (assert-false (mjr_cmp_abs>  1  1))
      (assert-false (mjr_cmp_abs>  0  0))
      (assert-false (mjr_cmp_abs> -1 -1))
      (assert-false (mjr_cmp_abs>  1 -1))
      (assert-false (mjr_cmp_abs> -1  1))

      (assert-true  (mjr_cmp_abs<=  1  2))
      (assert-false (mjr_cmp_abs<=  2  1))
      (assert-true  (mjr_cmp_abs<= -1 -2))
      (assert-false (mjr_cmp_abs<= -2 -1))
      (assert-true  (mjr_cmp_abs<= -1  2))
      (assert-true  (mjr_cmp_abs<=  1 -2))
      (assert-true  (mjr_cmp_abs<=  1  1))
      (assert-true  (mjr_cmp_abs<=  0  0))
      (assert-true  (mjr_cmp_abs<= -1 -1))
      (assert-true  (mjr_cmp_abs<=  1 -1))
      (assert-true  (mjr_cmp_abs<= -1  1))

      (assert-false (mjr_cmp_abs>=  1  2))
      (assert-true  (mjr_cmp_abs>=  2  1))
      (assert-false (mjr_cmp_abs>= -1 -2))
      (assert-true  (mjr_cmp_abs>= -2 -1))
      (assert-false (mjr_cmp_abs>= -1  2))
      (assert-false (mjr_cmp_abs>=  1 -2))
      (assert-true  (mjr_cmp_abs>=  1  1))
      (assert-true  (mjr_cmp_abs>=  0  0))
      (assert-true  (mjr_cmp_abs>= -1 -1))
      (assert-true  (mjr_cmp_abs>=  1 -1))
      (assert-true  (mjr_cmp_abs>= -1  1))

      (assert-equal  1 (mjr_cmp_min  1  2))
      (assert-equal  1 (mjr_cmp_min  2  1))
      (assert-equal -2 (mjr_cmp_min -1 -2))
      (assert-equal -2 (mjr_cmp_min -2 -1))
      (assert-equal -1 (mjr_cmp_min -1  2))
      (assert-equal -2 (mjr_cmp_min  1 -2))
      (assert-equal  1 (mjr_cmp_min  1  1))
      (assert-equal  0 (mjr_cmp_min  0  0))
      (assert-equal -1 (mjr_cmp_min -1 -1))
      (assert-equal -1 (mjr_cmp_min  1 -1))
      (assert-equal -1 (mjr_cmp_min -1  1))

      (assert-equal  2 (mjr_cmp_max  1  2))
      (assert-equal  2 (mjr_cmp_max  2  1))
      (assert-equal -1 (mjr_cmp_max -1 -2))
      (assert-equal -1 (mjr_cmp_max -2 -1))
      (assert-equal  2 (mjr_cmp_max -1  2))
      (assert-equal  1 (mjr_cmp_max  1 -2))
      (assert-equal  1 (mjr_cmp_max  1  1))
      (assert-equal  0 (mjr_cmp_max  0  0))
      (assert-equal -1 (mjr_cmp_max -1 -1))
      (assert-equal  1 (mjr_cmp_max  1 -1))
      (assert-equal  1 (mjr_cmp_max -1  1))

      (assert-equal  1 (mjr_cmp_abs-min  1  2))
      (assert-equal  1 (mjr_cmp_abs-min  2  1))
      (assert-equal -1 (mjr_cmp_abs-min -1 -2))
      (assert-equal -1 (mjr_cmp_abs-min -2 -1))
      (assert-equal -1 (mjr_cmp_abs-min -1  2))
      (assert-equal  1 (mjr_cmp_abs-min  1 -2))
      (assert-equal  1 (mjr_cmp_abs-min  1  1))
      (assert-equal  0 (mjr_cmp_abs-min  0  0))
      (assert-equal -1 (mjr_cmp_abs-min -1 -1))
      (assert-equal  1 (mjr_cmp_abs-min  1 -1))
      (assert-equal -1 (mjr_cmp_abs-min -1  1))

      (assert-equal  2 (mjr_cmp_abs-max  1  2))
      (assert-equal  2 (mjr_cmp_abs-max  2  1))
      (assert-equal -2 (mjr_cmp_abs-max -1 -2))
      (assert-equal -2 (mjr_cmp_abs-max -2 -1))
      (assert-equal  2 (mjr_cmp_abs-max -1  2))
      (assert-equal -2 (mjr_cmp_abs-max  1 -2))
      (assert-equal  1 (mjr_cmp_abs-max  1  1))
      (assert-equal  0 (mjr_cmp_abs-max  0  0))
      (assert-equal -1 (mjr_cmp_abs-max -1 -1))
      (assert-equal  1 (mjr_cmp_abs-max  1 -1))
      (assert-equal -1 (mjr_cmp_abs-max -1  1))
      )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_cmp_signum
  ; Hand picked tests
  (assert-equalp (values  1 nil)    (mjr_cmp_signum +1))
  (assert-equalp (values -1 nil)    (mjr_cmp_signum -1))
  (assert-equalp (values  0 nil)    (mjr_cmp_signum 0))
  (assert-equalp (values  0   t)    (mjr_cmp_signum 1.0e-7))
  (assert-equalp (values  1 nil)    (mjr_cmp_signum 1.0e-3))
  (assert-equalp (values  1 nil)    (mjr_cmp_signum 1.0e-2))
  (assert-equalp (values  1 nil)    (mjr_cmp_signum 1.0e-1))
  ; Check on integers and rationals
  (dotimes (i 100)
    (let ((j (mjr_prng_int-cc 100 100))
          (k (mjr_prng_int-cc 2 100)))
      (assert-equal (signum j) (mjr_cmp_signum j))
      (assert-equal (signum (/ j k)) (mjr_cmp_signum (/ j k)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
