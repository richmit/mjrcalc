;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-const.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Tests for :mjr_const.@EOL
;; @std       Common Lisp
;; @see       use-const.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CONST-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CONST))

(in-package :MJR_CONST-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)



