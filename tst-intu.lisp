;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-intu.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-intu.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_INTU-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_INTU :MJR_PRNG))

(in-package :MJR_INTU-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_quadratic-residue?-naive (a p)
  ""
  (loop for x from 0 upto (floor p 2)
        when (= (mod (* x x) p) (mod a p))
        return 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_mod-expt-naive (x y n)
  "Compute the modular exponent (x^y mod n) using the repeated multiplication algorithm."
  (if (= y 0)
      1
      (loop with po = 1
            for i from 1 upto y
            do (setf po (mod (* po x) n))
            finally (return po))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_extended-gcd-naive (a b)
  "Return a, b, and g such that $a*x+b*y=g=\gcd(a,b)$"
  (let ((lastx 1)
        (lasty 0))
    (loop
       with x = 0
       with y = 1
         with at = a
         with bt = b
       while (not (zerop bt))
       for (quo rem) = (multiple-value-list (truncate at bt))
       do (psetq at bt
                 bt rem)
       do (psetq x     (- lastx (* quo x))
                 lastx x)
       do (psetq y     (- lasty (* quo y))
                 lasty y))
    (values lastx lasty (+ (* a lastx) (* b lasty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intu_pd
  (assert-equal "0"               (mjr_intu_pd 0))
  (assert-equal "1"               (mjr_intu_pd 1))
  (assert-equal "10"              (mjr_intu_pd 10))
  (assert-equal "100"             (mjr_intu_pd 100))
  (assert-equal "1,000"           (mjr_intu_pd 1000))
  (assert-equal "10,000"          (mjr_intu_pd 10000))
  (assert-equal "100,000"         (mjr_intu_pd 100000))
  (assert-equal "1,000,000"       (mjr_intu_pd 1000000))
  (assert-equal "10,000,000"      (mjr_intu_pd 10000000))
  (assert-equal "100,000,000"     (mjr_intu_pd 100000000))
  (assert-equal "1,000,000,000"   (mjr_intu_pd 1000000000))
  (assert-equal "10,000,000,000"  (mjr_intu_pd 10000000000))
  (assert-equal "-1"              (mjr_intu_pd -1))
  (assert-equal "-10"             (mjr_intu_pd -10))
  (assert-equal "-100"            (mjr_intu_pd -100))
  (assert-equal "-1,000"          (mjr_intu_pd -1000))
  (assert-equal "-10,000"         (mjr_intu_pd -10000))
  (assert-equal "-100,000"        (mjr_intu_pd -100000))
  (assert-equal "-1,000,000"      (mjr_intu_pd -1000000))
  (assert-equal "-10,000,000"     (mjr_intu_pd -10000000))
  (assert-equal "-100,000,000"    (mjr_intu_pd -100000000))
  (assert-equal "-1,000,000,000"  (mjr_intu_pd -1000000000))
  (assert-equal "-10,000,000,000" (mjr_intu_pd -10000000000))
  (assert-equal "10,000,000,000"  (mjr_intu_pd 10000000000.00000001))
  (assert-equal "-10,000,000,000" (mjr_intu_pd -10000000000.00000001))
  ;; Errors
  (assert-error 'error            (mjr_intu_pd 't))
  (assert-error 'error            (mjr_intu_pd #C(1 1)))
  (assert-error 'error            (mjr_intu_pd 1/2))
  (assert-error 'error            (mjr_intu_pd 1.4))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intu_px
  (assert-equal "0"                  (mjr_intu_px 0))
  (assert-equal "1"                  (mjr_intu_px 1))
  (assert-equal "A"                  (mjr_intu_px 10))
  (assert-equal "C"                  (mjr_intu_px 12))
  (assert-equal "E"                  (mjr_intu_px 14))
  (assert-equal "10"                 (mjr_intu_px 16))
  (assert-equal "12"                 (mjr_intu_px 18))
  (assert-equal "AB CDEF"            (mjr_intu_px #xabcdef))
  (assert-equal "123 4567 89AB CDEF" (mjr_intu_px #x123456789abcdef))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intu_log
  (assert-equal 3  (mjr_intu_log 8    2))
  (assert-equal 4  (mjr_intu_log 16   2))
  (assert-equal 10 (mjr_intu_log 1024 2))
  (assert-equal 3  (mjr_intu_log 27   3))
;  ;; Errors
  (assert-error 'error (mjr_intu_log #C(1 1) 2))
  (assert-error 'error (mjr_intu_log 't      2))
  (assert-error 'error (mjr_intu_log -2      2))
  (assert-error 'error (mjr_intu_log 0       2))
  (assert-error 'error (mjr_intu_log 1.3     2))
  (assert-error 'error (mjr_intu_log 8       #C(1 1)))
  (assert-error 'error (mjr_intu_log 8       't))
  (assert-error 'error (mjr_intu_log 8       -2))
  (assert-error 'error (mjr_intu_log 8       0))
  (assert-error 'error (mjr_intu_log 8       1.3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_intu_mod-expt
  ;; Zero exponent
  (assert-equal 1      (mjr_intu_mod-expt -2 0  10))
  (assert-equal 1      (mjr_intu_mod-expt 2  0  10))
  (assert-equal 1      (mjr_intu_mod-expt 0  0  10))
  ;; Normal cases
  (assert-equal 8      (mjr_intu_mod-expt 2  3  10))
  (assert-equal 2      (mjr_intu_mod-expt -2 3  10))
  (assert-equal 2      (mjr_intu_mod-expt 2  3  3))
  (assert-equal 1      (mjr_intu_mod-expt -2 3  3))
  (assert-equal 0      (mjr_intu_mod-expt 2  3  4))
  (assert-equal 0      (mjr_intu_mod-expt -2 3  4))
  (dotimes (i 50)
    (let ((x (mjr_prng_int-oo -100 100))
          (y (mjr_prng_int-co 0 20))
          (m (mjr_prng_int-co 0 10000)))
      (assert-equal (mjr_intu_mod-expt-naive x y m) (mjr_intu_mod-expt x y m))))
  ;; Errors
  (assert-error 'error (mjr_intu_mod-expt 11     5        't))
  (assert-error 'error (mjr_intu_mod-expt 11     't       16))
  (assert-error 'error (mjr_intu_mod-expt 't     5        16))
  (assert-error 'error (mjr_intu_mod-expt #C(1 1) 5       16))
  (assert-error 'error (mjr_intu_mod-expt 11      #C(1 1) 16))
  (assert-error 'error (mjr_intu_mod-expt 11      5       #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
