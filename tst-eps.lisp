;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-eps.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-eps.lisp
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
(defpackage :MJR_EPS-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_EPS))

(in-package :MJR_EPS-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_BASIC

  (let* ((new-eps   (float 1/1000))
         (new-eps/2 (/ new-eps 2))
         (new-eps*2 (* new-eps 2))
         (old-eps   *mjr_eps_eps*))
    (setq *mjr_eps_eps* new-eps)

    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1))
    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1.0))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1.0))

    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1))
    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1.0))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1.0))

    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1    new-eps))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1    new-eps))
    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1.0  new-eps))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1.0  new-eps))

    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1    new-eps))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1    new-eps))
    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1.0  new-eps))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1.0  new-eps))

    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))

    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))

    (setq *mjr_eps_eps* old-eps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_integerp

(assert-true  (mjr_eps_integerp  0))
(assert-true  (mjr_eps_integerp  1))
(assert-true  (mjr_eps_integerp -1))

(assert-true  (mjr_eps_integerp  0.0))
(assert-true  (mjr_eps_integerp  1.0))
(assert-true  (mjr_eps_integerp -1.0))

(assert-false (mjr_eps_integerp  1/2))
(assert-false (mjr_eps_integerp -1/2))
(assert-false (mjr_eps_integerp  3/2))
(assert-false (mjr_eps_integerp -3/2))

(assert-false (mjr_eps_integerp  0.5))
(assert-false (mjr_eps_integerp -0.5))
(assert-false (mjr_eps_integerp  1.5))
(assert-false (mjr_eps_integerp -1.5))

  (loop for i from -500 upto 500
        do (assert-true  (mjr_eps_integerp i))
        do (assert-true  (mjr_eps_integerp (float i)))
        do (assert-false (mjr_eps_integerp (+ 1/2 i)))
        do (assert-false (mjr_eps_integerp (+ 0.5 i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_integerp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_integerp (- i (/ *mjr_eps_eps* j))))
                 do (assert-false (mjr_eps_integerp (+ i (* *mjr_eps_eps* j))))
                 do (assert-false (mjr_eps_integerp (- i (* *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_evenp
  (assert-false (mjr_eps_evenp  1))
  (assert-false (mjr_eps_evenp -1))
  (assert-true  (mjr_eps_evenp  2))
  (assert-true  (mjr_eps_evenp -2))

  (assert-false (mjr_eps_evenp  1.0))
  (assert-false (mjr_eps_evenp -1.0))
  (assert-true  (mjr_eps_evenp  2.0))
  (assert-true  (mjr_eps_evenp -2.0))

  (loop for i from -500 upto 500 by 2
        do (assert-true  (mjr_eps_evenp i))
        do (assert-true  (mjr_eps_evenp (float i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_evenp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_evenp (- i (/ *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_oddp
  (assert-true  (mjr_eps_oddp  1))
  (assert-true  (mjr_eps_oddp -1))
  (assert-false (mjr_eps_oddp  2))
  (assert-false (mjr_eps_oddp -2))

  (assert-true  (mjr_eps_oddp  1.0))
  (assert-true  (mjr_eps_oddp -1.0))
  (assert-false (mjr_eps_oddp  2.0))
  (assert-false (mjr_eps_oddp -2.0))

  (loop for i from -501 upto 500 by 2
        do (assert-true  (mjr_eps_oddp i))
        do (assert-true  (mjr_eps_oddp (float i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_oddp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_oddp (- i (/ *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_zap
  (assert-equalp #(1 2 3 4)                                   (mjr_eps_zap #(1 2 3 4)))
  (assert-equalp #(1 2 3 0)                                   (mjr_eps_zap #(1 2 3 1d-15)))
  ;; Specify eps
  (assert-equalp #(0 2 3 4)                                   (mjr_eps_zap #(1 2 3 4) 2))
  (assert-equalp #(1 2 3 0)                                   (mjr_eps_zap #(1 2 3 1d-15) .1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
