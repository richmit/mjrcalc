;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-cmp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Tests for :MJR_CMP.@EOL
;; @std       Common Lisp
;; @see       use-cmp.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CMP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CMP :MJR_PRNG))

(in-package :MJR_CMP-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
