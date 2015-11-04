;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-cas.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,2012,2013 by Mitch Richling.  All rights reserved.
;; @brief     Unit tests for :mjr_cas.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CAS-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CAS :MJR_MXP))

(in-package :MJR_CAS-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_cas_diff
  (assert-equal '0                      (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "1") "x")))
  (assert-equal '1                      (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x") "x")))
  (assert-equal '("*" 2 "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x^2") "x")))
  (assert-equal '("*" 2 "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x*x") "x")))
  (assert-equal '("cos" "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "sin(x)") "x")))
  (assert-equal '("*" -1 ("sin" "x"))   (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "cos(x)") "x")))
)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_cas_canonize
  (assert-equal '("*" 2 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+x")))
  (assert-equal '("*" 5 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+x+3*x")))
  (assert-equal '("*" 5 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+(x+3*x)")))
  (assert-equal '("*" 6 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "2*x+x+3*x")))
  (assert-equal '("+" ("*" 2 "y") ("*" 4 "x")) (mjr_cas_canonize (mjr_mxp_infix-to-tree "2*y+x+3*x")))
  (assert-equal '("+" ("*" 2 "y") ("*" 4 "x")) (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+2*y+3*x")))
)


;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)


