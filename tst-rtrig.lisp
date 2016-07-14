;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-rtrig.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-rtrig.lisp
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
(defpackage :MJR_RTRIG-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_RTRIG :MJR_EPS :MJR_PRNG))

(in-package :MJR_RTRIG-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_sin-f
  ;; Do some random cases
  (let ((p 355/113))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100)))
            (v (mjr_rtrig_sin-f r)))
        (assert-true (rationalp v))                                               ;; rational
        (assert-true (<= -1 v))                                                   ;; bounded below
        (assert-true (>= 1 v))                                                    ;; bounded above
        (assert-true (= v (- (mjr_rtrig_sin-f (- r)))))                           ;; odd
        (assert-true (= v (mjr_rtrig_sin-f (+ r (* 2 p)))))))                     ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_sin-f (* n p)))                          ;; zeros
          do (assert-equal  1 (mjr_rtrig_sin-f (+ (/ p 2) (* 2 n p))))            ;; max
          do (assert-equal -1 (mjr_rtrig_sin-f (+ (* 3 (/ p 2)) (* 2 n p)))))     ;; min
    )
  (dotimes (i 5000)
    (let* ((r (mjr_prng_float-cc -100d0 100d0)))
      (assert-equality (mjr_eps_make-fixed= 0.00007) (mjr_rtrig_sin-f r nil) (sin r))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_tan-f
  ;; Do some random cases
  (let ((p 355/113))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100))))
        (if (not (integerp (/ (- r (/ p 2)) p)))
            (let ((v (mjr_rtrig_tan-f r)))
              (assert-true (rationalp v))                                         ;; rational
              (assert-true (= v (- (mjr_rtrig_tan-f (- r)))))                     ;; odd
              (assert-true (= v (mjr_rtrig_tan-f (+ r (* 2 p)))))))))             ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_tan-f (* n p))))                         ;; zeros
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_cos-f
  ;; Do some random cases
  (let ((p 355/113))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100)))
            (v (mjr_rtrig_cos-f r)))
        (assert-true (rationalp v))                                               ;; rational
        (assert-true (<= -1 v))                                                   ;; bounded below
        (assert-true (>= 1 v))                                                    ;; bounded above
        (assert-true (= v (mjr_rtrig_cos-f (- r))))                               ;; even
        (assert-true (= v (mjr_rtrig_cos-f (+ r (* 2 p)))))))                     ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_cos-f (+ (/ p 2) (* n p))))              ;; zeros
          do (assert-equal  1 (mjr_rtrig_cos-f (* 2 n p)))                        ;; max
          do (assert-equal -1 (mjr_rtrig_cos-f (+ p (* 2 n p)))))                 ;; min
    )
  (dotimes (i 5000)
    (let* ((r (mjr_prng_float-cc -100d0 100d0)))
      (assert-equality (mjr_eps_make-fixed= 0.0003) (mjr_rtrig_cos-f r nil) (cos r))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_sin-q
  ;; Do some random cases
  (let ((p 22/7))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100)))
            (v (mjr_rtrig_sin-q r)))
        (assert-true (rationalp v))                                               ;; rational
        (assert-true (<= -1 v))                                                   ;; bounded below
        (assert-true (>= 1 v))                                                    ;; bounded above
        (assert-true (= v (- (mjr_rtrig_sin-q (- r)))))                           ;; odd
        (assert-true (= v (mjr_rtrig_sin-q (+ r (* 2 p)))))))                     ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_sin-q (* n p)))                          ;; zeros
          do (assert-equal  1 (mjr_rtrig_sin-q (+ (/ p 2) (* 2 n p))))            ;; max
          do (assert-equal -1 (mjr_rtrig_sin-q (+ (* 3 (/ p 2)) (* 2 n p)))))     ;; min
          )
  (dotimes (i 5000)
    (let* ((r (mjr_prng_float-cc -100d0 100d0)))
      (assert-equality (mjr_eps_make-fixed= 0.0007) (mjr_rtrig_sin-q r nil) (sin r))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_cos-q
  ;; Do some random cases
  (let ((p 22/7))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100)))
            (v (mjr_rtrig_cos-q r)))
        (assert-true (rationalp v))                                               ;; rational
        (assert-true (<= -1 v))                                                   ;; bounded below
        (assert-true (>= 1 v))                                                    ;; bounded above
        (assert-true (= v (mjr_rtrig_cos-q (- r))))                               ;; even
        (assert-true (= v (mjr_rtrig_cos-q (+ r (* 2 p)))))))                     ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_cos-q (+ (/ p 2) (* n p))))              ;; zeros
          do (assert-equal  1 (mjr_rtrig_cos-q (* 2 n p)))                        ;; max
          do (assert-equal -1 (mjr_rtrig_cos-q (+ p (* 2 n p)))))                 ;; min
    )
  (dotimes (i 5000)
    (let* ((r (mjr_prng_float-cc -100d0 100d0)))
      (assert-equality (mjr_eps_make-fixed= 0.001) (mjr_rtrig_cos-q r nil) (cos r))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rtrig_tan-q
  ;; Do some random cases
  (let ((p 22/7))
    (dotimes (i 2000)
      (let* ((r (/ (mjr_prng_int-cc -100 100) (mjr_prng_int-co 1 100))))
        (if (not (integerp (/ (- r (/ p 2)) p)))
            (let ((v (mjr_rtrig_tan-q r)))
              (assert-true (rationalp v))                                         ;; rational
              (assert-true (= v (- (mjr_rtrig_tan-q (- r)))))                     ;; odd
              (assert-true (= v (mjr_rtrig_tan-q (+ r (* 2 p)))))))))             ;; periodic
    (loop for n from -1000 to 1000
          do (assert-equal  0 (mjr_rtrig_tan-q (* n p))))                         ;; zeros
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
