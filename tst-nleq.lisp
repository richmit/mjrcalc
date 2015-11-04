;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-nleq.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-nleq.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2013,2014,,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_NLEQ-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_NLEQ :MJR_EPS))

(in-package :MJR_NLEQ-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  f1  (x) (values (+ (* 1 x x x x x) (* -4 x x x x) (* -5 x x x) (* 20 x x) (* 4 x) -16) ; -1 1 -2 2 4
                        (+ (* 5 x x x x) (* -16 x x x) (* -15 x x) (* 40 x) 4)
                        (+ (* 20 x x x) (* -48 x x) (* -30 x) 40)
                        (+ (* 60 x x) (* -96 x) -30)))
(defvar f1s "x^5-4*x^4-5*x^3+20*x^2+4*x-16")
(defun  f2  (x) (values (sin x)                                                                ; 0
                        (cos x)
                        (- (sin x))))
(defvar f2s "sin(x)")
(defun  f3  (x) (values x                                                                      ; 0
                        1
                        0))
(defvar f3s "x")
(defun  f4  (x) (values (* 3 x)                                                                ; 0
                        3
                        0))
(defvar f4s "3*x")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-bsect
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f3  -1 1))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f3  -1 1 :use-false-position 't))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect #'f4  -1 2 :use-false-position 't))

  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f3s -1 1))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f3s -1 1 :use-false-position 't))
  (assert-equalp  (values 0 0 nil) (mjr_nleq_root-bsect   f4s -1 2 :use-false-position 't))
  ;; Close to a good answer..
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f2  -1.0  0.5))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f3  -1.0  1.5))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -10.0 -1.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -1.5  -0.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -1.5  -10  :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -0.5  -1.5 :xeps 1e-10))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f2s -1.0  0.5))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f3s -1.0  1.5))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -10.0 -1.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -1.5  -0.5 :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -1.5  -10  :xeps 1e-10))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -0.5  -1.5 :xeps 1e-10))
  ;; Now do it with :use-false-position..
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f2  -1.0  0.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect #'f3  -1.0  1.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -10.0 -1.5 :xeps 1e-10 :use-false-position 't :yeps 0 :max-itr 3000)) ; Pathological case for false position
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -1.5  -0.5 :xeps 1e-10 :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect #'f1  -1.5  -10  :xeps 1e-10 :use-false-position 't :yeps 0 :max-itr 3000)) ; Pathological case for false position
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect #'f1  -0.5  -1.5 :xeps 1e-10 :use-false-position 't))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f2s -1.0  0.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-bsect   f3s -1.0  1.5              :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -10.0 -1.5 :xeps 1e-10 :use-false-position 't :yeps 0 :max-itr 3000)) ; Pathological case for false position
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -1.5  -0.5 :xeps 1e-10 :use-false-position 't))
  (assert-equality (mjr_eps_make-fixed= .001) -2 (mjr_nleq_root-bsect   f1s -1.5  -10  :xeps 1e-10 :use-false-position 't :yeps 0 :max-itr 3000)) ; Pathological case for false position
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-bsect   f1s -0.5  -1.5 :xeps 1e-10 :use-false-position 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-newton
  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-newton #'f2   1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-newton #'f1   1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-newton #'f1   2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-newton #'f1  -1.1))

  (assert-equality (mjr_eps_make-fixed= .001)  0 (mjr_nleq_root-newton   f2s  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  1 (mjr_nleq_root-newton   f1s  1.1))
  (assert-equality (mjr_eps_make-fixed= .001)  2 (mjr_nleq_root-newton   f1s  2.1))
  (assert-equality (mjr_eps_make-fixed= .001) -1 (mjr_nleq_root-newton   f1s -1.1))

  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "log(x)*sin(x)" 3.5e0))
  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "sin(x)*sin(x)" 3.5e0))
  (assert-equality (mjr_eps_make-fixed=  .01) pi (mjr_nleq_root-newton "x^2*sin(x)"    3.5e0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_root-laguerre
  (assert-equality (mjr_eps_make-fixed= .0001)  0 (mjr_nleq_root-laguerre #'f2  10 1.1))
  (assert-equality (mjr_eps_make-fixed= .0001)  1 (mjr_nleq_root-laguerre #'f1  5  1.1))
  (assert-equality (mjr_eps_make-fixed= .0001)  2 (mjr_nleq_root-laguerre #'f1  5  2.1))
  (assert-equality (mjr_eps_make-fixed= .0001) -1 (mjr_nleq_root-laguerre #'f1  5 -1.1))

  (assert-equality (mjr_eps_make-fixed= .0001)  0 (mjr_nleq_root-laguerre   f2s 10 1.1))
  (assert-equality (mjr_eps_make-fixed= .0001)  1 (mjr_nleq_root-laguerre   f1s 5  1.1))
  (assert-equality (mjr_eps_make-fixed= .0001)  2 (mjr_nleq_root-laguerre   f1s 5  2.1))
  (assert-equality (mjr_eps_make-fixed= .0001) -1 (mjr_nleq_root-laguerre   f1s 5 -1.1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_nleq_fixed-point-itr
  (assert-equality (mjr_eps_make-fixed= .001) 1.1140665 (mjr_nleq_fixed-point-itr (lambda (x) (/ (sin x))) 1))

  (assert-equality (mjr_eps_make-fixed= .001) 1.1140665 (mjr_nleq_fixed-point-itr "1/sin(x)" 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
