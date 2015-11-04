;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-prng.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_pring.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_PRNG-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PRNG))

(in-package :MJR_PRNG-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_random
  1
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_int-oo
  (dotimes (i 1000)
    (let* ((a (- (random 10) (random 10)))
           (b (+ a 2 (random 10)))
           (r (mjr_prng_int-oo a b)))
      (assert-true (< a r)      a b r)
      (assert-true (< r b)      a b r)
      (assert-true (integerp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_int-cc
  (dotimes (i 1000)
    (let* ((a (- (random 10) (random 10)))
           (b (+ a (random 10)))
           (r (mjr_prng_int-cc a b)))
      (assert-true (<= a r)     a b r)
      (assert-true (<= r b)     a b r)
      (assert-true (integerp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_int-co
  (dotimes (i 1000)
    (let* ((a (- (random 10) (random 10)))
           (b (+ a 1 (random 10)))
           (r (mjr_prng_int-co a b)))
      (assert-true (<= a r)     a b r)
      (assert-true (<  r b)     a b r)
      (assert-true (integerp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_float-oo
  (dotimes (i 1000)
    (let* ((a (- (random 10.0) (random 10.0)))
           (b (+ a 2 (random 10.0)))
           (r (mjr_prng_float-oo a b)))
      (assert-true (< a r)      a b r)
      (assert-true (< r b)      a b r)
      (assert-true (floatp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_float-cc
  (dotimes (i 1000)
    (let* ((a (- (random 10.0) (random 10.0)))
           (b (+ a (random 10.0)))
           (r (mjr_prng_float-cc a b)))
      (assert-true (<= a r)     a b r)
      (assert-true (<= r b)     a b r)
      (assert-true (floatp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_float-co
  (dotimes (i 1000)
    (let* ((a (- (random 10.0) (random 10.0)))
           (b (+ a 1 (random 10.0)))
           (r (mjr_prng_float-co a b)))
      (assert-true (<= a r)     a b r)
      (assert-true (<  r b)     a b r)
      (assert-true (floatp r) a b r)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_prng_vector
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 0 20)))
      (assert-equal len (length (mjr_prng_vector len #'mjr_prng_float-co -1 1)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests
 )



