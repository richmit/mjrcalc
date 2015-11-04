;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-chk.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_chk.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CHK-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CHK :MJR_PRNG))

(in-package :MJR_CHK-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_chk_SMALL-EPS

  (let ((old-eps *mjr_chk_eps*))
    (setq *mjr_chk_eps* (float 1/10000))

    (assert-true   (mjr_chk_> (+ 1 1/100000) 1))
    (assert-false  (mjr_chk_> (- 1 1/100000) 1))
    (assert-false  (mjr_chk_> (+ 1 1/100000) 1.0))
    (assert-false  (mjr_chk_> (- 1 1/100000) 1.0))

    (assert-false  (mjr_chk_< (+ 1 1/100000) 1))
    (assert-true   (mjr_chk_< (- 1 1/100000) 1))
    (assert-false  (mjr_chk_< (+ 1 1/100000) 1.0))
    (assert-false  (mjr_chk_< (- 1 1/100000) 1.0))

    (assert-true   (mjr_chk_!= (+ 1 1/100000) 1))
    (assert-true   (mjr_chk_!= (- 1 1/100000) 1))
    (assert-false  (mjr_chk_!= (+ 1 1/100000) 1.0))
    (assert-false  (mjr_chk_!= (- 1 1/100000) 1.0))

    (assert-true   (mjr_chk_!=0 1/100000))
    (assert-true   (mjr_chk_!=0 1/100000))
    (assert-false  (mjr_chk_!=0 (float 1/100000)))
    (assert-false  (mjr_chk_!=0 (float 1/100000)))

    (setq *mjr_chk_eps* old-eps)))

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_chk_BASIC

      (assert-true  (mjr_chk_<  1  2))
      (assert-false (mjr_chk_<  2  1))
      (assert-false (mjr_chk_< -1 -2))
      (assert-true  (mjr_chk_< -2 -1))
      (assert-true  (mjr_chk_< -1  2))
      (assert-false (mjr_chk_<  1 -2))
      (assert-false (mjr_chk_<  1  1))
      (assert-false (mjr_chk_<  0  0))
      (assert-false (mjr_chk_< -1 -1))

      (assert-false (mjr_chk_>  1  2))
      (assert-true  (mjr_chk_>  2  1))
      (assert-true  (mjr_chk_> -1 -2))
      (assert-false (mjr_chk_> -2 -1))
      (assert-false (mjr_chk_> -1  2))
      (assert-true  (mjr_chk_>  1 -2))
      (assert-false (mjr_chk_>  1  1))
      (assert-false (mjr_chk_>  0  0))
      (assert-false (mjr_chk_> -1 -1))
      (assert-true  (mjr_chk_>  1 -1))
      (assert-false (mjr_chk_> -1  1))

      (assert-true  (mjr_chk_!=  1  2))
      (assert-true  (mjr_chk_!=  2  1))
      (assert-true  (mjr_chk_!= -1 -2))
      (assert-true  (mjr_chk_!= -2 -1))
      (assert-true  (mjr_chk_!= -1  2))
      (assert-true  (mjr_chk_!=  1 -2))
      (assert-false (mjr_chk_!=  1  1))
      (assert-false (mjr_chk_!=  0  0))
      (assert-false (mjr_chk_!= -1 -1))
      (assert-true  (mjr_chk_!=  1 -1))
      (assert-true  (mjr_chk_!= -1  1))

      (assert-true  (mjr_chk_positivep   1))
      (assert-false (mjr_chk_positivep  -1))
      (assert-false (mjr_chk_positivep   0))

      (assert-false (mjr_chk_negativep   1))
      (assert-true  (mjr_chk_negativep  -1))
      (assert-false (mjr_chk_negativep   0))

      (assert-true  (mjr_chk_!=0   1))
      (assert-true  (mjr_chk_!=0  -1))
      (assert-false (mjr_chk_!=0   0))

      )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
