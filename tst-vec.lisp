;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-vec.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests..@EOL
;; @std       Common Lisp
;; @see       use-vec.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008-2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_VEC-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_EPS :MJR_VEC :MJR_PRNG))

(in-package :MJR_VEC-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_make-const
  (assert-equalp #(0 0 0 0 0 0) (mjr_vec_make-const 6))
  (assert-equalp #(1 1 1 1 1 1) (mjr_vec_make-const 6  1))
  (assert-equalp #(0 0 0 0 0)   (mjr_vec_make-const 5))
  (assert-equalp #(1 1 1 1 1)   (mjr_vec_make-const 5  1))
  (assert-equalp #(2 2 2 2 2)   (mjr_vec_make-const 5  2))
  ;; Errors
  (assert-error 'error          (mjr_vec_make-const 0))
  (assert-error 'error          (mjr_vec_make-const -1))
  (assert-error 'error          (mjr_vec_make-const 1.2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_make-from-func
  (assert-equalp #(0 1 2 3) (mjr_vec_make-from-func #'identity :start 0 :end 3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_rationalize
  ;; rationalize routines seem to come in two flavors: 1) Samrt and 2) Naive combined with IEEE floats. :)
                          (assert-equalp #(1 0 0)                                                (mjr_vec_rationalize #(1   0 0)))
#+(not (or ECL ABCL))     (assert-equalp #(11/10 0 0)                                            (mjr_vec_rationalize #(1.1 0 0)))
#+(or ECL ABCL)           (assert-equalp #(9227469/8388608 0 0)                                  (mjr_vec_rationalize #(1.1 0 0)))
                          (assert-equalp #(11/10 0 0)                                            (mjr_vec_rationalize #(11/10 0 0)))
                          (assert-equalp #(1 2 3)                                                (mjr_vec_rationalize #(1   2 3)))
#+(not (or ECL ABCL))     (assert-equalp #(11/10 54/10 23/10)                                    (mjr_vec_rationalize #(1.1 5.4 2.3)))
#+(or ECL ABCL)           (assert-equalp #(9227469/8388608 11324621/2097152 9646899/4194304)     (mjr_vec_rationalize #(1.1 5.4 2.3)))
                          (assert-equalp #(11/10 3/4 9/3)                                        (mjr_vec_rationalize #(11/10 3/4 9/3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_make-e
  (assert-equalp #(1 0 0 0 0)                 (mjr_vec_make-e 0 :len 5))
  (assert-equalp #(0 1 0 0 0)                 (mjr_vec_make-e 1 :len 5))
  (assert-equalp #(0 0 1 0 0)                 (mjr_vec_make-e 2 :len 5))
  (assert-equalp #(0 0 0 1 0)                 (mjr_vec_make-e 3 :len 5))
  (assert-equalp #(0 0 0 0 1)                 (mjr_vec_make-e 4 :len 5))
  (assert-equalp #(2 0 0 0 0)                 (mjr_vec_make-e 0 :len 5 :one-value 2))
  (assert-equalp #(0 2 0 0 0)                 (mjr_vec_make-e 1 :len 5 :one-value 2))
  (assert-equalp #(0 0 2 0 0)                 (mjr_vec_make-e 2 :len 5 :one-value 2))
  (assert-equalp #(0 0 0 2 0)                 (mjr_vec_make-e 3 :len 5 :one-value 2))
  (assert-equalp #(0 0 0 0 2)                 (mjr_vec_make-e 4 :len 5 :one-value 2))
  (assert-equalp #(1 2 2 2 2)                 (mjr_vec_make-e 0 :len 5 :zero-value 2))
  (assert-equalp #(2 1 2 2 2)                 (mjr_vec_make-e 1 :len 5 :zero-value 2))
  (assert-equalp #(2 2 1 2 2)                 (mjr_vec_make-e 2 :len 5 :zero-value 2))
  (assert-equalp #(2 2 2 1 2)                 (mjr_vec_make-e 3 :len 5 :zero-value 2))
  (assert-equalp #(2 2 2 2 1)                 (mjr_vec_make-e 4 :len 5 :zero-value 2))
  (assert-equalp #(3 2 2 2 2)                 (mjr_vec_make-e 0 :len 5 :one-value 3 :zero-value 2))
  (assert-equalp #(2 3 2 2 2)                 (mjr_vec_make-e 1 :len 5 :one-value 3 :zero-value 2))
  (assert-equalp #(2 2 3 2 2)                 (mjr_vec_make-e 2 :len 5 :one-value 3 :zero-value 2))
  (assert-equalp #(2 2 2 3 2)                 (mjr_vec_make-e 3 :len 5 :one-value 3 :zero-value 2))
  (assert-equalp #(2 2 2 2 3)                 (mjr_vec_make-e 4 :len 5 :one-value 3 :zero-value 2))
  ;; Typical polynomial case:
  (assert-equalp #(2)                         (mjr_vec_make-e 0 :len  1 :one-value 2))
  (assert-equalp #(2 0)                       (mjr_vec_make-e 0 :len  2 :one-value 2))
  (assert-equalp #(2 0 0)                     (mjr_vec_make-e 0 :len  3 :one-value 2))
  (assert-equalp #(2 0 0 0)                   (mjr_vec_make-e 0 :len  4 :one-value 2))
  (assert-equalp #(2 0 0 0 0)                 (mjr_vec_make-e 0 :len  5 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0)               (mjr_vec_make-e 0 :len  6 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0)             (mjr_vec_make-e 0 :len  7 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0)           (mjr_vec_make-e 0 :len  8 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0 0)         (mjr_vec_make-e 0 :len  9 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0 0 0)       (mjr_vec_make-e 0 :len 10 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0 0 0 0)     (mjr_vec_make-e 0 :len 11 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0 0 0 0 0)   (mjr_vec_make-e 0 :len 12 :one-value 2))
  (assert-equalp #(2 0 0 0 0 0 0 0 0 0 0 0 0) (mjr_vec_make-e 0 :len 13 :one-value 2))
  ;; Errors
  (assert-error 'error                        (mjr_vec_make-e 2 :len 0))
  (assert-error 'error                        (mjr_vec_make-e 2 :len -1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_norm-two-squared
  (dotimes (i 200)
    (let* ((len (mjr_prng_int-co 1 20))
           (p1  (mjr_prng_vector len #'mjr_prng_float-co -1 1)))
      (assert-equalp (mjr_vec_norm-two-squared p1) (reduce #'+ (map 'list (lambda (x) (* x x)) p1)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_norm-two
  (dotimes (i 200)
    (let* ((len (mjr_prng_int-co 1 20))
           (p1  (mjr_prng_vector len #'mjr_prng_float-co -1 1)))
      (assert-equality (mjr_eps_make-fixed= 0.00001) (mjr_vec_norm-two p1) (sqrt (reduce #'+ (map 'list (lambda (x) (* x x)) p1))))))

  (assert-equal 3.7416573 (mjr_vec_norm-two #(1 2 3)))
  (assert-equal 3.1622777 (mjr_vec_norm-two #(1 3)))
  (assert-equal 3         (mjr_vec_norm-two #(3)))
  (assert-equal 3         (mjr_vec_norm-two 3))

  (assert-equal 3.7416573 (mjr_vec_norm-two #(1 2 -3)))
  (assert-equal 3.1622777 (mjr_vec_norm-two #(1 -3)))
  (assert-equal 3         (mjr_vec_norm-two #(-3)))
  (assert-equal 3         (mjr_vec_norm-two -3))

  ;; Error cases
  (assert-error 'error    (mjr_vec_norm-two #()))
  ;; Bad type for input
  (assert-error 'error    (mjr_vec_norm-two 't))
  (assert-error 'error    (mjr_vec_norm-two '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_norm-two-squared
  (dotimes (i 200)
    (let* ((len (mjr_prng_int-co 1 20))
           (p1  (mjr_prng_vector len #'mjr_prng_float-co -1 1)))
      (assert-equality (mjr_eps_make-fixed= 0.00001) (mjr_vec_norm-two-squared p1) (reduce #'+ (map 'list (lambda (x) (* x x)) p1)))))

  (assert-equal 14 (mjr_vec_norm-two-squared #(1 2 3)))
  (assert-equal 10 (mjr_vec_norm-two-squared #(1 3)))
  (assert-equal 9  (mjr_vec_norm-two-squared #(3)))
  (assert-equal 9  (mjr_vec_norm-two-squared 3))

  (assert-equal 14 (mjr_vec_norm-two-squared #(1 2 -3)))
  (assert-equal 10 (mjr_vec_norm-two-squared #(1 -3)))
  (assert-equal 9  (mjr_vec_norm-two-squared #(-3)))
  (assert-equal 9  (mjr_vec_norm-two-squared -3))

  ;; Error cases
  (assert-error 'error    (mjr_vec_norm-two-squared #()))
  ;; Bad type for input
  (assert-error 'error    (mjr_vec_norm-two-squared 't))
  (assert-error 'error    (mjr_vec_norm-two-squared '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_norm-infinity
  (dotimes (i 200)
    (let* ((len (mjr_prng_int-co 1 20))
           (p1  (mjr_prng_vector len #'mjr_prng_float-co -1 1)))
      (assert-equality (mjr_eps_make-fixed= 0.00001) (mjr_vec_norm-infinity p1) (reduce #'max (map 'list (lambda (x) (abs x)) p1)))))

  (assert-equal 3 (mjr_vec_norm-infinity #(1 2 3)))
  (assert-equal 3 (mjr_vec_norm-infinity #(1 3)))
  (assert-equal 3 (mjr_vec_norm-infinity #(3)))
  (assert-equal 3 (mjr_vec_norm-infinity 3))

  (assert-equal 3 (mjr_vec_norm-infinity #(1 2 -3)))
  (assert-equal 3 (mjr_vec_norm-infinity #(1 -3)))
  (assert-equal 3 (mjr_vec_norm-infinity #(-3)))
  (assert-equal 3 (mjr_vec_norm-infinity -3))

  ;; Error cases
  (assert-error 'error    (mjr_vec_norm-infinity #()))
  ;; Bad type for input
  (assert-error 'error    (mjr_vec_norm-infinity 't))
  (assert-error 'error    (mjr_vec_norm-infinity '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_norm-one
  (dotimes (i 200)
    (let* ((len (mjr_prng_int-co 1 20))
           (p1  (mjr_prng_vector len #'mjr_prng_float-co -1 1)))
      (assert-equality (mjr_eps_make-fixed= 0.00001) (mjr_vec_norm-one p1) (reduce #'+ (map 'list (lambda (x) (abs x)) p1)))))

  (assert-equal 6 (mjr_vec_norm-one #(1 2 3)))
  (assert-equal 4 (mjr_vec_norm-one #(1 3)))
  (assert-equal 3 (mjr_vec_norm-one #(3)))
  (assert-equal 3 (mjr_vec_norm-one 3))

  (assert-equal 6 (mjr_vec_norm-one #(1 2 -3)))
  (assert-equal 4 (mjr_vec_norm-one #(1 -3)))
  (assert-equal 3 (mjr_vec_norm-one #(-3)))
  (assert-equal 3 (mjr_vec_norm-one -3))

  ;; Error cases
  (assert-error 'error    (mjr_vec_norm-one #()))
  ;; Bad type for input
  (assert-error 'error    (mjr_vec_norm-one 't))
  (assert-error 'error    (mjr_vec_norm-one '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_normalize
  ;; Test cases
  (assert-equalp #(1)       (mjr_vec_normalize #(1)))
  (assert-equalp #(3/5 4/5) (mjr_vec_normalize #(3 4)))
  (assert-equalp 1          (mjr_vec_normalize 1))
  ;; Some random tests
  (loop for i from 1 upto 50
        for r = (mjr_prng_int-co 1 3)
        for v1 = (mjr_vec_+ (mjr_vec_make-e (mjr_prng_random r)) (mjr_prng_vector r #'mjr_prng_float-co -.5 .5))
        for v2 = (mjr_vec_make-e (mjr_prng_random r) :len r)
        ;; en is already normalized
        do (assert-equalp v2 (mjr_vec_normalize v2))
        ;; Default norm function is mjr_vec_norm-two
        do (assert-equality (mjr_eps_make-fixed= 0.00001) 1 (mjr_vec_norm-two (mjr_vec_normalize v1)))
        ;; Explicit norm function
        do (assert-equality (mjr_eps_make-fixed= 0.00001) 1 (mjr_vec_norm-two      (mjr_vec_normalize v1 #'mjr_vec_norm-two)))
        do (assert-equality (mjr_eps_make-fixed= 0.00001) 1 (mjr_vec_norm-infinity (mjr_vec_normalize v1 #'mjr_vec_norm-infinity)))
        do (assert-equality (mjr_eps_make-fixed= 0.00001) 1 (mjr_vec_norm-one      (mjr_vec_normalize v1 #'mjr_vec_norm-one))))
  ;; Error cases
  (assert-error 'error    (mjr_vec_normalize #()))
  (assert-error 'error    (mjr_vec_normalize #(0)))
  (assert-error 'error    (mjr_vec_normalize #(0 0)))
  (assert-error 'error    (mjr_vec_normalize #(0 0 0)))
  ;; Bad type for input
  (assert-error 'error    (mjr_vec_normalize 't))
  (assert-error 'error    (mjr_vec_normalize '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_<
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 3)))
  (assert-equalp 't  (mjr_vec_< #(1 2 3) #(1 2 4)))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 2)))
  (assert-equalp 't  (mjr_vec_< #(1 2 3) #(2 1 2)))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 3) :order :lex))
  (assert-equalp 't  (mjr_vec_< #(1 2 3) #(1 2 4) :order :lex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 2) :order :lex))
  (assert-equalp 't  (mjr_vec_< #(1 2 3) #(2 1 2) :order :lex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 3) :order :revlex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 4) :order :revlex))
  (assert-equalp 't  (mjr_vec_< #(1 2 3) #(1 2 2) :order :revlex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(2 1 2) :order :revlex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 3) :order :grlex))
  (assert-equalp nil (mjr_vec_< #(3 2 2) #(4 1 1) :order :grlex))
  (assert-equalp 't  (mjr_vec_< #(4 1 1) #(3 2 2) :order :grlex))
  (assert-equalp nil (mjr_vec_< #(3 2 2) #(2 2 3) :order :grlex))
  (assert-equalp 't  (mjr_vec_< #(2 2 3) #(3 2 2) :order :grlex))
  (assert-equalp nil (mjr_vec_< #(1 2 3) #(1 2 3) :order :grevlex))
  (assert-equalp 't  (mjr_vec_< #(3 2 2) #(4 1 1) :order :grevlex))
  (assert-equalp nil (mjr_vec_< #(4 1 1) #(3 2 2) :order :grevlex))
  (assert-equalp 't  (mjr_vec_< #(3 2 2) #(2 2 3) :order :grevlex))
  (assert-equalp nil (mjr_vec_< #(2 2 3) #(3 2 2) :order :grevlex))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_*
  (assert-equalp #(3 4 3)   (mjr_vec_* #(1 2 3) #(3 2 1)))
  (assert-equalp #(1 4 9)   (mjr_vec_* #(1 2 3) #(1 2 3)))
  (assert-equalp #(1 4 12)  (mjr_vec_* #(1 2 3) #(1 2 4)))
  (assert-equalp #(1 4 6)   (mjr_vec_* #(1 2 3) #(1 2 2)))
  (assert-equalp #(2 2 6)   (mjr_vec_* #(1 2 3) #(2 1 2)))
  (assert-equalp #(2 4 6)   (mjr_vec_* 2 #(1 2 3)))
  (assert-equalp #(6 4 2)   (mjr_vec_* 2 #(3 2 1)))
  (assert-equalp #(2 4 6)   (mjr_vec_* #(1 2 3) 2))
  (assert-equalp #(6 4 2)   (mjr_vec_* #(3 2 1) 2))
  (assert-equalp #(4  8 12) (mjr_vec_* 2 2 #(1 2 3)))
  (assert-equalp #(12 8 4)  (mjr_vec_* 2 2 #(3 2 1)))
  (assert-equalp #(4  8 12) (mjr_vec_* 2 #(1 2 3) 2))
  (assert-equalp #(12 8 4)  (mjr_vec_* 2 #(3 2 1) 2))
  (assert-equalp #(4  8 12) (mjr_vec_* #(1 2 3) 2 2))
  (assert-equalp #(12 8 4)  (mjr_vec_* #(3 2 1) 2 2))
  (assert-equalp #(6 12 18) (mjr_vec_* 2 3 #(1 2 3)))
  (assert-equalp #(18 12 6) (mjr_vec_* 2 3 #(3 2 1)))
  (assert-equalp #(6 12 18) (mjr_vec_* 2 #(1 2 3) 3))
  (assert-equalp #(18 12 6) (mjr_vec_* 2 #(3 2 1) 3))
  (assert-equalp #(6 12 18) (mjr_vec_* #(1 2 3) 2 3))
  (assert-equalp #(18 12 6) (mjr_vec_* #(3 2 1) 2 3))
  (assert-equalp #(6 12 18) (mjr_vec_* 3 2 #(1 2 3)))
  (assert-equalp #(18 12 6) (mjr_vec_* 3 2 #(3 2 1)))
  (assert-equalp #(6 12 18) (mjr_vec_* 3 #(1 2 3) 2))
  (assert-equalp #(18 12 6) (mjr_vec_* 3 #(3 2 1) 2))
  (assert-equalp #(6 12 18) (mjr_vec_* #(1 2 3) 3 2))
  (assert-equalp #(18 12 6) (mjr_vec_* #(3 2 1) 3 2))
  (assert-equalp 6          (mjr_vec_* 3 2))
  (assert-equalp 6          (mjr_vec_* 3 2))
  (loop for i from 1 upto 100
        for len = (mjr_prng_int-cc 1 100)
        for v1  = (mjr_prng_vector len #'mjr_prng_int-co 1 100)
        for c   = (mjr_prng_int-cc -100 100)
        for v2  = (mjr_vec_make-const len c)
        do (assert-equalp (mjr_vec_* v1 v2) (mjr_vec_* c  v1))
        do (assert-equalp (mjr_vec_* v1 v2) (mjr_vec_* v1 c)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_cross
  (assert-equalp #(-3 6 -3)                 (mjr_vec_cross #(1 2 3) #(4 5 6)))
  (assert-equalp #(7 -14 7)                 (mjr_vec_cross #(3 2 1) #(4 5 6)))
  (loop for i from 1 upto 500
        for v1 = (mjr_prng_vector 3 #'mjr_prng_int-co 1 100)
        for v2 = (mjr_prng_vector 3 #'mjr_prng_int-co 1 100)
        do (assert-equalp (mjr_vec_- (mjr_vec_cross v1 v2)) (mjr_vec_cross v2 v1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_dot
  (assert-equalp 32                 (mjr_vec_dot #(1 2 3) #(4 5 6)))
  (assert-equalp 28                 (mjr_vec_dot #(3 2 1) #(4 5 6)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_+
  (assert-equalp #(4 4 4) (mjr_vec_+ #(1 2 3) #(3 2 1)))
  (assert-equalp #(2 4 6) (mjr_vec_+ #(1 2 3) #(1 2 3)))
  (assert-equalp #(2 4 7) (mjr_vec_+ #(1 2 3) #(1 2 4)))
  (assert-equalp #(2 4 5) (mjr_vec_+ #(1 2 3) #(1 2 2)))
  (assert-equalp #(3 3 5) (mjr_vec_+ #(1 2 3) #(2 1 2)))
  (assert-equalp #(3 4 5) (mjr_vec_+ 2 #(1 2 3)))
  (assert-equalp #(5 4 3) (mjr_vec_+ 2 #(3 2 1)))
  (assert-equalp #(3 4 5) (mjr_vec_+ #(1 2 3) 2))
  (assert-equalp #(5 4 3) (mjr_vec_+ #(3 2 1) 2))
  (assert-equalp #(5 6 7) (mjr_vec_+ 2 2 #(1 2 3)))
  (assert-equalp #(7 6 5) (mjr_vec_+ 2 2 #(3 2 1)))
  (assert-equalp #(5 6 7) (mjr_vec_+ 2 #(1 2 3) 2))
  (assert-equalp #(7 6 5) (mjr_vec_+ 2 #(3 2 1) 2))
  (assert-equalp #(5 6 7) (mjr_vec_+ #(1 2 3) 2 2))
  (assert-equalp #(7 6 5) (mjr_vec_+ #(3 2 1) 2 2))
  (assert-equalp #(6 7 8) (mjr_vec_+ 2 3 #(1 2 3)))
  (assert-equalp #(8 7 6) (mjr_vec_+ 2 3 #(3 2 1)))
  (assert-equalp #(6 7 8) (mjr_vec_+ 2 #(1 2 3) 3))
  (assert-equalp #(8 7 6) (mjr_vec_+ 2 #(3 2 1) 3))
  (assert-equalp #(6 7 8) (mjr_vec_+ #(1 2 3) 2 3))
  (assert-equalp #(8 7 6) (mjr_vec_+ #(3 2 1) 2 3))
  (assert-equalp #(6 7 8) (mjr_vec_+ 3 2 #(1 2 3)))
  (assert-equalp #(8 7 6) (mjr_vec_+ 3 2 #(3 2 1)))
  (assert-equalp #(6 7 8) (mjr_vec_+ 3 #(1 2 3) 2))
  (assert-equalp #(8 7 6) (mjr_vec_+ 3 #(3 2 1) 2))
  (assert-equalp #(6 7 8) (mjr_vec_+ #(1 2 3) 3 2))
  (assert-equalp #(8 7 6) (mjr_vec_+ #(3 2 1) 3 2))
  (assert-equalp 5        (mjr_vec_+ 3 2))
  (assert-equalp 5        (mjr_vec_+ 2 3))
  (loop for i from 1 upto 100
        for len = (mjr_prng_int-cc 1 100)
        for v1  = (mjr_prng_vector len #'mjr_prng_int-co 1 100)
        for c   = (mjr_prng_int-cc -100 100)
        for v2  = (mjr_vec_make-const len c)
        do (assert-equalp (mjr_vec_+ v1 v2) (mjr_vec_+ c  v1))
        do (assert-equalp (mjr_vec_+ v1 v2) (mjr_vec_+ v1 c)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_vec_-
  (assert-equalp #(-2 0 2)   (mjr_vec_- #(1 2 3) #(3 2 1)))
  (assert-equalp #(0 0 0)    (mjr_vec_- #(1 2 3) #(1 2 3)))
  (assert-equalp #(0 0 -1)   (mjr_vec_- #(1 2 3) #(1 2 4)))
  (assert-equalp #(0 0 1)    (mjr_vec_- #(1 2 3) #(1 2 2)))
  (assert-equalp #(-1 1 1)   (mjr_vec_- #(1 2 3) #(2 1 2)))
  (assert-equalp #(1 0 -1)   (mjr_vec_- 2 #(1 2 3)))
  (assert-equalp #(-1 0 1)   (mjr_vec_- 2 #(3 2 1)))
  (assert-equalp #(-1 0 1)   (mjr_vec_- #(1 2 3) 2))
  (assert-equalp #(1 0 -1)   (mjr_vec_- #(3 2 1) 2))
  (assert-equalp #(-1 -2 -3) (mjr_vec_- 2 2 #(1 2 3)))
  (assert-equalp #(-3 -2 -1) (mjr_vec_- 2 2 #(3 2 1)))
  (assert-equalp #(-1 -2 -3) (mjr_vec_- 2 #(1 2 3) 2))
  (assert-equalp #(-3 -2 -1) (mjr_vec_- 2 #(3 2 1) 2))
  (assert-equalp #(-3 -2 -1) (mjr_vec_- #(1 2 3) 2 2))
  (assert-equalp #(-1 -2 -3) (mjr_vec_- #(3 2 1) 2 2))
  (assert-equalp #(-2 -3 -4) (mjr_vec_- 2 3 #(1 2 3)))
  (assert-equalp #(-4 -3 -2) (mjr_vec_- 2 3 #(3 2 1)))
  (assert-equalp #(-2 -3 -4) (mjr_vec_- 2 #(1 2 3) 3))
  (assert-equalp #(-4 -3 -2) (mjr_vec_- 2 #(3 2 1) 3))
  (assert-equalp #(-4 -3 -2) (mjr_vec_- #(1 2 3) 2 3))
  (assert-equalp #(-2 -3 -4) (mjr_vec_- #(3 2 1) 2 3))
  (assert-equalp #(0 -1 -2)  (mjr_vec_- 3 2 #(1 2 3)))
  (assert-equalp #(-2 -1 0)  (mjr_vec_- 3 2 #(3 2 1)))
  (assert-equalp #(0 -1 -2)  (mjr_vec_- 3 #(1 2 3) 2))
  (assert-equalp #(-2 -1 0)  (mjr_vec_- 3 #(3 2 1) 2))
  (assert-equalp #(-4 -3 -2) (mjr_vec_- #(1 2 3) 3 2))
  (assert-equalp #(-2 -3 -4) (mjr_vec_- #(3 2 1) 3 2))
  (assert-equalp 1           (mjr_vec_- 3 2))
  (assert-equalp -1          (mjr_vec_- 2 3))
  (loop for i from 1 upto 500
        for v1 = (mjr_prng_vector 3 #'mjr_prng_int-co 1 100)
        for v2 = (mjr_prng_vector 3 #'mjr_prng_int-co 1 100)
        do (assert-equalp (mjr_vec_+ v1 (mjr_vec_- v2)) (mjr_vec_- v1 v2))
        do (assert-equalp (mjr_vec_- (mjr_vec_- v1 v2)) (mjr_vec_- v2 v1)))
  (loop for i from 1 upto 100
        for len = (mjr_prng_int-cc 1 100)
        for v1  = (mjr_prng_vector len #'mjr_prng_int-co 1 100)
        for c   = (mjr_prng_int-cc -100 100)
        for v2  = (mjr_vec_make-const len c)
        do (assert-equalp (mjr_vec_- v1 v2) (mjr_vec_- v1 c))
        do (assert-equalp (mjr_vec_- v2 v1) (mjr_vec_- c v1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
