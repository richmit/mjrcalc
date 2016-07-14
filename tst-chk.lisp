;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-chk.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-chk.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CHK-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CHK))

(in-package :MJR_CHK-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

      (assert-true  (mjr_chk_plusp   1))
      (assert-false (mjr_chk_plusp  -1))
      (assert-false (mjr_chk_plusp   0))

      (assert-false (mjr_chk_minusp   1))
      (assert-true  (mjr_chk_minusp  -1))
      (assert-false (mjr_chk_minusp   0))

      (assert-true  (mjr_chk_!=0   1))
      (assert-true  (mjr_chk_!=0  -1))
      (assert-false (mjr_chk_!=0   0))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
