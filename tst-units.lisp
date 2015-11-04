;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-units.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-units.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
