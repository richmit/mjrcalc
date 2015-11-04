;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-units.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2011,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_UNITS.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_UNITS-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_UNITS))

(in-package :MJR_UNITS-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_units_to-si-fundamental
  (assert-equal '(* 13081/609638400000000 "m" (/ "s") (/ "s")) (mjr_units_to-si-fundamental "103*ft/fortnight^2"))
  (assert-equal '(* 3.179064 "m" (/ "s") (/ "s"))              (mjr_units_to-si-fundamental "10.43*ft/s^2"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_units_convert
  (assert-equal '(* 50/127 "in_us")                                     (mjr_units_convert 1        "cm"       "in"))
  (assert-equal '(* 127/50 "cm")                                        (mjr_units_convert 1        "in"       "cm"))
  (assert-equal '(* 12 "in_us")                                         (mjr_units_convert 1        "ft"       "in"))
  (assert-equal '(* 127/5000 "m")                                       (mjr_units_convert 1        "in"       'm))
  (assert-equal '(* 127/5000 "m")                                       (mjr_units_convert 1        "in"       "m"))

  (assert-equal '(* 127/50 "cm")                                        (mjr_units_convert 1        "in"       "cm"))
  (assert-equal '(* 50/127 "in_us")                                     (mjr_units_convert 1        "cm"       "in"))
  (assert-equal '(* 1/12 "ft_us")                                       (mjr_units_convert 1        "in"       "ft"))
  (assert-equal '(* 5000/127 "in_us")                                   (mjr_units_convert 1        'm         "in"))
  (assert-equal '(* 5000/127 "in_us")                                   (mjr_units_convert 1        "m"        "in"))

  (assert-equal '(* 3125/3429 "ft_us" (/ "s") (/ "s"))                  (mjr_units_convert 1        "km/min^2" "ft/s^2"))
  (assert-equal '(* 22395937500/283591 "mi_us" (/ "hour") (/ "hour"))   (mjr_units_convert 7963/812 "m/s^2"    "mi/hour^2"))
  (assert-equal '(* 3600 (/ "hour"))                                    (mjr_units_convert 1        "1/s"      "1/hour"))
  (assert-equal '(* 3600 (/ "hour"))                                    (mjr_units_convert 1        "/s"       "/hour"))
  (assert-equal '(* 1/3600 "hour")                                      (mjr_units_convert 1        "s"        "hour"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_units_compatible
  (assert-true   (mjr_units_compatible "g"         "g"))
  (assert-true   (mjr_units_compatible "kg"        "kg"))
  (assert-true   (mjr_units_compatible "kg"        "g"))
  (assert-true   (mjr_units_compatible "cm"        "m"))
  (assert-true   (mjr_units_compatible "cm"        "in"))
  (assert-true   (mjr_units_compatible "in"        "cm"))
  (assert-true   (mjr_units_compatible "ft"        "in"))
  (assert-true   (mjr_units_compatible "in"        'm))
  (assert-true   (mjr_units_compatible "km/min^2"  "ft/s^2"))
  (assert-true   (mjr_units_compatible "m/s^2"     "mi/hour^2"))
  (assert-true   (mjr_units_compatible "1/s"       "1/hour"))
  (assert-true   (mjr_units_compatible "/s"        "/hour"))
  (assert-true   (mjr_units_compatible "s"         "hour"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
