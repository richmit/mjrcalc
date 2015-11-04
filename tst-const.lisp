;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-const.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_const.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CONST-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CONST))

(in-package :MJR_CONST-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_const_search

  ;; Exact searches with a matching symbol
  (assert-equal (values -9.28476377d-24 2.3d-31 "J/T" "m_e: electron magnetic momentum")                               (mjr_const_search "m_e:"))
  (assert-equal (values 299792458 NIL "m/s" "c: speed of light in vacuum")                                             (mjr_const_search "c:"))

  ;; Exact searches with a matching symbol and unit conversion
  (assert-equal (values '(* 2.9979245e8 "m" (/ "s")) NIL '(* 1.0) "c: speed of light in vacuum")                       (mjr_const_search "c:" :units 1.0))
  (assert-equal (values '(* 936851431250/1397 "mph") NIL '(* "mph") "c: speed of light in vacuum")                     (mjr_const_search "c:" :units "mph"))
  (assert-equal (values '(* 5396264244/5 "kph") NIL '(* "kph") "c: speed of light in vacuum")                          (mjr_const_search "c:" :units "kph"))

  ;; Exact searches with a matching symbol and intervals
  (assert-equal (values 299792458 NIL "m/s" "c: speed of light in vacuum")                                             (mjr_const_search "c:" :return-as-interval 't))

  ;; Return as interval
  (assert-equal (values '(:AE -9.28476377d-24 2.3000000031792465d-31) 2.3d-31 "J/T" "m_e: electron magnetic momentum") (mjr_const_search "m_e:" :return-as-interval :ae))
  (assert-equal (values '(:AE -9.28476377d-24 2.3000000031792465d-31) 2.3d-31 "J/T" "m_e: electron magnetic momentum") (mjr_const_search "m_e:" :return-as-interval 't))
  (assert-equal (values '(-9.284764d-24 -9.28476354d-24) 2.3d-31 "J/T" "m_e: electron magnetic momentum")              (mjr_const_search "m_e:" :return-as-interval :s))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)



