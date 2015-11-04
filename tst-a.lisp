;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-a.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Tests for :MJR_A.@EOL
;; @std       Common Lisp
;; @see       use-a.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2006,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_A-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_A :MJR_CMP :MJR_PRNG :MJR_NUMU))

(in-package :MJR_A-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_d2dms-naive (df) 
  "Return degrees, minutes, and seconds from fractional degrees.  
If the input is rational, then the outputs will be too.
If the input is a float, then the fractional outputs (seconds) will be DOUBLE-FLOAT."
  (cond  ((complexp df)      (error "mjr_a_dms2d: Input must not be complex!"))
         ((not (numberp df)) (error "mjr_a_dms2d: Input must be numbers!")))
  (let* ((df (mjr_numu_max-accuracy df))
         (d  (truncate (abs df)))
         (t1 (- (abs df) d))
         (m  (truncate (* 60 t1)))
         (s  (* 3600 (- t1 (/ m 60)))))
    (values (if (< df 0) (- d) d) m s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_normalize
  ;; Make sure angle stays the same after whole trips around the circle
  (dotimes (i 1000)
    (let* ((a (mjr_prng_random 360.0d0))
           (n (mjr_prng_int-cc -100 100))
           (ta (+ a (* 360 n))))
      (if (not (zerop n))
          (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) a (mjr_a_normalize ta) ta))))
  ;; Make sure default is :angular-measure & smallest-abs have the documented defaults
  (dotimes (i 1000)
    (let* ((a (mjr_prng_float-co -100000 100000)))
      (assert-equal (mjr_a_normalize a) (mjr_a_normalize a :smallest-abs    nil))
      (assert-equal (mjr_a_normalize a) (mjr_a_normalize a :angular-measure :am-degrees))))
  ;; Make sure default is :angular-measure & smallest-abs have the documented defaults
  (dotimes (i 1000)
    (let* ((a (mjr_prng_float-co -100000 100000)))
      (assert-equal (mjr_a_normalize a) (mjr_a_normalize a :smallest-abs    nil))
      (assert-equal (mjr_a_normalize a) (mjr_a_normalize a :angular-measure :am-degrees))))
  ;; Make sure normalized angle is within parameters
  (dotimes (i 1000)
    (let* ((a (mjr_prng_float-co -100000 100000)))
      (dotimes (j 3)
        (setq a (cond ((= j 0) a)
                      ((= j 1) (rationalize a))
                      ((= j 2) (setq a (truncate a)))))
        ;; integer multiple of full circle away
        (assert-true (>=  0.001 (second (multiple-value-list (truncate (abs (/ (- a (mjr_a_normalize a                 )) 360)))))))
        (assert-true (>=  0.001 (second (multiple-value-list (truncate (abs (/ (- a (mjr_a_normalize a :smallest-abs 't)) 360)))))))
        ;; basic size OK
        (assert-true (<= 0    (mjr_a_normalize a)))
        (assert-true (>= 360  (mjr_a_normalize a)))
        (assert-true (<= -180 (mjr_a_normalize a :smallest-abs 't)))
        (assert-true (>= 180  (mjr_a_normalize a :smallest-abs 't))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_dms2d
  (assert-equal 3661/3600 (mjr_a_dms2d 1       1       1))
  (assert-equal 1         (mjr_a_dms2d 1       0       0))
  (assert-equal 1/60      (mjr_a_dms2d 0       1       0))
  (assert-equal 1/3600    (mjr_a_dms2d 0       0       1))
  (assert-equal 0         (mjr_a_dms2d 0       0       0))
  (assert-equal 3599/3600 (mjr_a_dms2d 1       0       -1))
  (assert-equal 59/60     (mjr_a_dms2d 1       -1      0))
  ;; Errors
  (assert-error 'error    (mjr_a_dms2d 't      1       1))
  (assert-error 'error    (mjr_a_dms2d 1       't      1))
  (assert-error 'error    (mjr_a_dms2d 1       1       't))
  (assert-error 'error    (mjr_a_dms2d #C(1 1) 1       1))
  (assert-error 'error    (mjr_a_dms2d 1       #C(1 1) 1))
  (assert-error 'error    (mjr_a_dms2d 1       1       #C(1 1)))
  (assert-error 'error    (mjr_a_dms2d nil))
  (assert-error 'error    (mjr_a_dms2d ))
  ;; Make sure missing args are assumed to be zero
  (dotimes (i 500)
    (let ((d (mjr_prng_random 10000))
          (m (mjr_prng_random 10000)))
      (assert-equal (mjr_a_dms2d d 0 0) (mjr_a_dms2d d))
      (assert-equal (mjr_a_dms2d d m 0) (mjr_a_dms2d d m))))
  ;; Test the string parsing functionality
  (dotimes (i 500)
    (let ((d (mjr_prng_random 10000))
          (m (mjr_prng_random 60))
          (s (mjr_prng_random 60)))
      (assert-equalp (mjr_a_dms2d d m s) (mjr_a_dms2d (format nil "~d:~d:~d" d m s)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_d2dms
  (assert-equal (values 1 1  1)  (mjr_a_d2dms 3661/3600))
  (assert-equal (values 1 0  0)  (mjr_a_d2dms 1))
  (assert-equal (values 0 1  0)  (mjr_a_d2dms 1/60))
  (assert-equal (values 0 0  1)  (mjr_a_d2dms 1/3600))
  (assert-equal (values 0 0  0)  (mjr_a_d2dms 0))
  (assert-equal (values 0 59 59) (mjr_a_d2dms 3599/3600))
  (assert-equal (values 0 59 0)  (mjr_a_d2dms 59/60))
  ;; Test naive version
  (dotimes (i 1000)
    (let ((d (mjr_prng_random 10000))
          (r (/ (mjr_prng_random 10000) (mjr_prng_int-cc 1 10000)))
          (f (mjr_prng_random 10000.0)))
      (assert-equality (lambda (x y) (every (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) x y))
                       (multiple-value-list (mjr_a_d2dms d))
                       (multiple-value-list (mjr_a_d2dms-naive d))
                       d)
      (assert-equality (lambda (x y) (every (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) x y))
                       (multiple-value-list (mjr_a_d2dms r))
                       (multiple-value-list (mjr_a_d2dms-naive r))
                       r)
      (assert-equality (lambda (x y) (every (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) x y))
                       (multiple-value-list (mjr_a_d2dms f))
                       (multiple-value-list (mjr_a_d2dms-naive f))
                       f)))
  ;; Errors
  (assert-error 'error    (mjr_a_d2dms 't))
  (assert-error 'error    (mjr_a_d2dms #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_r2d
  (assert-true (> 0.0001 (abs (- 180.0d0 (mjr_a_r2d pi)))))
  (assert-true (> 0.0001 (abs (- 360.0d0 (mjr_a_r2d (* 2 pi))))))
  (assert-true (> 0.0001 (abs (- 57.29577951308232d0 (mjr_a_r2d 1)))))
  ;; Errors
  (assert-error 'error (mjr_a_r2d 't))
  (assert-error 'error (mjr_a_r2d #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_d2r
  (assert-true (> 0.0001 (abs (- pi       (mjr_a_d2r 180.0d0)))))
  (assert-true (> 0.0001 (abs (- (* 2 pi) (mjr_a_d2r 360.0d0)))))
  (assert-true (> 0.0001 (abs (- 1.0d0    (mjr_a_d2r 57.29577951308232d0)))))
  ;; Errors
  (assert-error 'error (mjr_a_d2r 't))
  (assert-error 'error (mjr_a_d2r #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_xxx_d2dms+dms2d
  ;; Make sure (d2dms (dms2d x)) is idempotent for normalized, integer d, m, & s
  (dotimes (i 1000)
    (let ((d (mjr_prng_random 10000))
          (m (mjr_prng_random 60))
          (s (mjr_prng_random 60)))
      (assert-equal (list d m s) (multiple-value-list (mjr_a_d2dms (mjr_a_dms2d d m s))))))
  ;; make sure (dms2d (d2dms x)) is idempotent for positive x
  (dotimes (i 1000)
    (let ((d (mjr_prng_random 10000))
          (r (/ (mjr_prng_random 10000) (mjr_prng_int-cc 1 10000)))
          (f (mjr_prng_random 10000.0)))
      (assert-equal                                                   d (multiple-value-call #'mjr_a_dms2d (mjr_a_d2dms d)))
      (assert-equal                                                   r (multiple-value-call #'mjr_a_dms2d (mjr_a_d2dms r)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) f (multiple-value-call #'mjr_a_dms2d (mjr_a_d2dms f)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_a_xxx_r2d+d2r
  ;; make sure (r2d (d2r x)) and  (d2r (r2d x)) are idempotent 
  (dotimes (i 1000)
    (let ((d (mjr_prng_int-cc -10000 10000))
          (r (/ (mjr_prng_int-cc -10000 10000) (mjr_prng_int-cc 1 10000)))
          (f (mjr_prng_float-co -10000.0 10000.0)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) d (mjr_a_d2r (mjr_a_r2d d)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) r (mjr_a_d2r (mjr_a_r2d r)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) f (mjr_a_d2r (mjr_a_r2d f)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) d (mjr_a_r2d (mjr_a_d2r d)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) r (mjr_a_r2d (mjr_a_d2r r)))
      (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b 0.001))) f (mjr_a_r2d (mjr_a_d2r f)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
