;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-qmci.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       tst-qmci.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 2015,2016, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_QMCI-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PRNG :MJR_EPS :MJR_QMCI))

(in-package :MJR_QMCI-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_qmci_b-adic-radical-inverse-function-rat
  (assert-equalp 1/4 (mjr_qmci_b-adic-radical-inverse-function-rat 2 2))
  (assert-equalp 1/8 (mjr_qmci_b-adic-radical-inverse-function-rat 4 2))
  (assert-equalp 2/3 (mjr_qmci_b-adic-radical-inverse-function-rat 2 3))
  (assert-equalp 4/9 (mjr_qmci_b-adic-radical-inverse-function-rat 4 3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_qmci_2-adic-radical-inverse-function-rat
  (loop for i from 0 upto 100
        do (assert-equalp
            (mjr_qmci_b-adic-radical-inverse-function-rat i 2)
            (mjr_qmci_2-adic-radical-inverse-function-rat i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_qmci_b-adic-radical-inverse-function-real
  (loop with eqtest = (mjr_eps_make-fixed= 1e-3)
        for b from 2 upto 10
        do (loop for i from 0 upto 100
                 do (assert-equality eqtest
                                     (mjr_qmci_b-adic-radical-inverse-function-real i b)
                                     (mjr_qmci_b-adic-radical-inverse-function-rat  i b))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_qmci_2-adic-radical-inverse-function-real
  (loop for i from 0 upto 100
        do (assert-equalp
            (mjr_qmci_b-adic-radical-inverse-function-real i 2)
            (mjr_qmci_2-adic-radical-inverse-function-real i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_qmci_simple-quasi-monte-carlo

  (loop with eqtest = (mjr_eps_make-fixed= 1e-3)
        for i from 1 upto 1000
        for s = (mjr_prng_int-cc 0 10)
        for e = (+ s (mjr_prng_int-cc 1 10))
        for o = (mjr_prng_int-cc 100 1000)
        for v1 = (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start s :end e :order o :qsf #'mjr_qmci_2-adic-radical-inverse-function-rat )
        for v2 = (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start s :end e :order o :qsf #'mjr_qmci_2-adic-radical-inverse-function-real)
        do (assert-equality eqtest v1 v2)
        do (assert-equalp v1 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start s :end e :order o :qsf #'mjr_qmci_b-adic-radical-inverse-function-rat  :qsffa '(2)))
        do (assert-equalp v2 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start s :end e :order o :qsf #'mjr_qmci_b-adic-radical-inverse-function-real :qsffa '(2))))

  (assert-equalp 11179096173/33554432000 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order 10000 :qsf #'mjr_qmci_2-adic-radical-inverse-function-rat            ))
  (assert-equalp 11179096173/33554432000 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order 10000 :qsf #'mjr_qmci_b-adic-radical-inverse-function-rat :qsffa '(2)))
  (assert-equalp                       0 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     1 :qsf #'mjr_qmci_2-adic-radical-inverse-function-rat            ))
  (assert-equalp                       0 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     1 :qsf #'mjr_qmci_b-adic-radical-inverse-function-rat :qsffa '(2)))
  (assert-equalp                     1/8 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     2 :qsf #'mjr_qmci_2-adic-radical-inverse-function-rat            ))
  (assert-equalp                     1/8 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     2 :qsf #'mjr_qmci_b-adic-radical-inverse-function-rat :qsffa '(2)))
  (assert-equalp                    5/48 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     3 :qsf #'mjr_qmci_2-adic-radical-inverse-function-rat            ))
  (assert-equalp                    5/48 (mjr_qmci_simple-quasi-monte-carlo (lambda (x) (* x x)) :start 0 :end 1 :order     3 :qsf #'mjr_qmci_b-adic-radical-inverse-function-rat :qsffa '(2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
