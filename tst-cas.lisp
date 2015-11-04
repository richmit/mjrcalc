;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-cas.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests for :mjr_cas.@EOL
;; @std       Common Lisp
;; @see       use-cas.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1996,2012,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CAS-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CAS :MJR_MXP))

(in-package :MJR_CAS-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_cas_diff
  (assert-equal '0                      (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "1") "x")))
  (assert-equal '1                      (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x") "x")))
  (assert-equal '("*" 2 "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x^2") "x")))
  (assert-equal '("*" 2 "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "x*x") "x")))
  (assert-equal '("cos" "x")            (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "sin(x)") "x")))
  (assert-equal '("*" -1 ("sin" "x"))   (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "cos(x)") "x")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_cas_canonize
  (assert-equal '("*" 2 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+x")))
  (assert-equal '("*" 5 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+x+3*x")))
  (assert-equal '("*" 5 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+(x+3*x)")))
  (assert-equal '("*" 6 "x")                   (mjr_cas_canonize (mjr_mxp_infix-to-tree "2*x+x+3*x")))
  (assert-equal '("+" ("*" 2 "y") ("*" 4 "x")) (mjr_cas_canonize (mjr_mxp_infix-to-tree "2*y+x+3*x")))
  (assert-equal '("+" ("*" 2 "y") ("*" 4 "x")) (mjr_cas_canonize (mjr_mxp_infix-to-tree "x+2*y+3*x")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
