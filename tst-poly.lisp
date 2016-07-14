;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-poly.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       tst-poly.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2003,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_POLY-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_POLY :MJR_VEC :MJR_EPS :MJR_PRNG :MJR_PRIME :MJR_COMBE))

(in-package :MJR_POLY-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the small prime list..

(if (null mjr_prime::*mjr_prime_small-list*)
    (mjr_prime_init-small-prime-list))


; Some irreducible polys over Q: #(1 -7 14 -4), #(28 -11 24), #(2 3 -21 -6), #(1 0 -3 -1 -4 14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_iexpt-naive0 (poly n)
  "Compute poly^n"
  (if (zerop n)
      #(1)
      (let ((pp (mjr_poly_simplify (copy-seq poly))))
        (dotimes (i (1- n) pp)
          (setf pp (mjr_poly_* pp poly))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_iexpt-naive1 (poly n)
  "Compute poly^n"
  (if (zerop n)
      #(1)
      (apply #'mjr_poly_* (loop for i from 1 upto n
                                collect poly))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_shift-naive0 (b poly)
  "Shift POLY right B units (or left if B is negative).

Based on a simple implementation using MJR_POLY_SUBST. This function is about 5.5x slower than MJR_POLY_SHIFT-NAIVE1."
  (cond ((not (vectorp poly))  (error "mjr_poly_shift: POLY is not a vector!"))
        ((not (numberp b))     (error "mjr_poly_shift: B is not a number!"))
        ((< (length poly) 1)   (error "mjr_poly_shift: POLY is empty!")))
  (mjr_poly_subst (vector 1 (- b)) poly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_shift-naive1 (b poly)
  "Shift POLY right B units (or left if B is negative).

This function performs the computation term-wise by computing poly[j]*(x-b)^j from j=n..0 and adding the results in place.  This requires O(n) space (for the
row of pascal's triangle, the new polynomial, and the powers of b), but is an efficient O(n^3) in time complexity.  This function is about 3.2x slower than
MJR_POLY_SHIFT-NAIVE2."
  (cond ((not (vectorp poly))  (error "mjr_poly_shift: POLY is not a vector!"))
        ((not (numberp b))     (error "mjr_poly_shift: B is not a number!"))
        ((< (length poly) 1)   (error "mjr_poly_shift: POLY is empty!")))
  (let* ((poly-len   (length poly))
         (poly-len-1 (1- poly-len))
         (b          (- b))
         (new-poly   (make-array poly-len :initial-element 0)))
    (loop initially (setf (aref new-poly poly-len-1) (aref poly poly-len-1))             ;; First product
          with pascal-row = (make-array poly-len :initial-element 1)                     ;; Pascal Triangle
          with b-pow      = (make-array poly-len :initial-element 1)                     ;; Powers of b
          for master-idx from 1 upto poly-len-1
          do (setf (aref b-pow master-idx) (* (aref b-pow (1- master-idx)) b))           ;; Update next power of b
          do (loop with tmp-pascal-elt = 0                                               ;; Compute next row of pascal triangle
                   for idx from 0 upto master-idx
                   do (psetf (aref pascal-row idx) (if (or (zerop idx) (= master-idx idx))
                                                       1
                                                       (+ (aref pascal-row idx) tmp-pascal-elt))
                             tmp-pascal-elt        (aref pascal-row idx)))
          do (loop for idx from 0 upto master-idx                                        ;; Add new poly to sum
                   do (incf (aref new-poly (- poly-len-1 idx))
                            (* (aref b-pow (- master-idx idx))
                               (aref pascal-row idx)
                               (aref poly (- poly-len-1 master-idx)))))
          finally (return new-poly))))                                                   ;; Finally... Return the new polynomial

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_eval-poly-and-first-n-derivatives-naive (poly x &optional (order 1))
  "Return value of poly at x as well as the value of the first ORDER derivatives at x."
  (values-list (loop for i from 0 upto order
                     collect (mjr_poly_eval (mjr_poly_diff poly i) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_shift-naive2 (b poly)
  "Shift POLY right B units (or left if B is negative).

This function performs the computation by using the Taylor expansion at -B.  Slightly slower than MJR_POLY_SHIFT."
  (let ((plen-1 (1- (length poly))))
    (reverse (map 'vector #'/
                  (multiple-value-list (mjr_poly_eval-poly-and-first-n-derivatives poly (- b) plen-1))
                  (loop for i from 0 upto plen-1
                        for f = 1 then (* f i)
                        collect f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_make-legendre-naive (n)
  "Compute the Nth legendre polynomial using $$P_n(x)=\\frac{(2n-1)\\cdot x\\cdot P_{n-1}(x) - (n-1)\\cdot P_{n-2}(x)}{n}$$"
  (let ((polys (vector #(1) #(1 0)))
        (order 0)
        (p-2   0)
        (p-1   1))
    (loop for i from 2 upto n
          do (setf (aref polys (abs (- p-2 order)))
                   (mjr_poly_* (mjr_poly_- (mjr_poly_* (vector (- (* 2 i) 1)) #(1 0) (aref polys (abs (- p-1 order))))
                                           (mjr_poly_* (vector (- i 1)) (aref polys (abs (- p-2 order)))))
                               (vector (/ i))))
          do (setf order (abs (- order 1))))
    (if (< n 2)
        (aref polys n)
        (aref polys (abs (- p-1 order))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_make-laguerre-naive (pow)
  "Use direct formula for laguerre"
  (concatenate 'vector (loop for i from pow downto 0
                             collect (* (mjr_combe_comb pow i) (/ (if (oddp i) -1 1) (mjr_combe_! i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_scount-fourier-naive (p l h)
  "Construct the fourier sequence, evaluate the sequences, count sign changes, and suptract -- direct implimentation of the thm."
  (let ((fs (mjr_poly_seq-make-fourier p)))
    (- (mjr_poly_count-sign-changes (mjr_poly_seq-eval fs l))
       (mjr_poly_count-sign-changes (mjr_poly_seq-eval fs h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_make-primative-from-roots-naive (&rest the-roots)
  "Return the primative polynomial with the given roots.

The leading coefficient will be the product of the denominators of the rational roots provided. This implies that if none of the roots are non-integer
rationals, the the polynomial will be monic.  If all roots are of rational or integer type, then the polynomial will have INTEGER coefficients.

The roots may be given as individual arguments, or as a single list."
  (let ((the-roots (if (and (car the-roots) (listp (car the-roots))) (car the-roots) the-roots)))
    (apply #'mjr_poly_* (mapcar (lambda (x) (if (rationalp x)
                                                (vector (abs (denominator x)) (* (- (signum x)) (abs (numerator x))))
                                                (vector 1                     (- x))))
                                the-roots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_poly_make-from-roots-naive (&rest the-roots)
  (let ((the-roots (if (and (car the-roots) (listp (car the-roots))) (car the-roots) the-roots)))
    (apply #'mjr_poly_* (mapcar (lambda (x) (vector 1 (- x))) the-roots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_onep
  (assert-false (mjr_poly_onep  #(0 0 2)))
  (assert-false (mjr_poly_onep  #(2)))
  (assert-false (mjr_poly_onep  2))
  (assert-false (mjr_poly_onep  #(0 0 2.0)))
  (assert-false (mjr_poly_onep  #(2.0)))
  (assert-false (mjr_poly_onep  2.0))

  (assert-false (mjr_poly_onep  #(0 2 1)))
  (assert-false (mjr_poly_onep  #(0 2 1.0)))

  (assert-false (mjr_poly_onep  #(1 0 0)))
  (assert-false (mjr_poly_onep  #(0 1 0)))
  (assert-false (mjr_poly_onep  #(1.0 0 0)))
  (assert-false (mjr_poly_onep  #(0 1.0 0)))

  (assert-false (mjr_poly_onep  #(1 1 0)))
  (assert-false (mjr_poly_onep  #(1 0 1)))
  (assert-false (mjr_poly_onep  #(1 1 1)))

  (assert-true  (mjr_poly_onep  #(0 0 1)))
  (assert-true  (mjr_poly_onep  #(1)))
  (assert-true  (mjr_poly_onep  1))
  (assert-true  (mjr_poly_onep  #(0 0 1.0)))
  (assert-true  (mjr_poly_onep  #(1.0)))
  (assert-true  (mjr_poly_onep  1.0))

  (assert-false (mjr_poly_onep  #(0 0 0)))
  (assert-false (mjr_poly_onep  #(0)))
  (assert-false (mjr_poly_onep  0))
  (assert-false (mjr_poly_onep  #(0 0.0 0.0)))
  (assert-false (mjr_poly_onep  #(0.0)))
  (assert-false (mjr_poly_onep  0.0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_zerop
  (assert-false (mjr_poly_zerop  #(0 0 1)))
  (assert-false (mjr_poly_zerop  #(    1)))
  (assert-false (mjr_poly_zerop        1))
  (assert-false (mjr_poly_zerop  #(0 0 1.0)))
  (assert-false (mjr_poly_zerop  #(    1.0)))
  (assert-false (mjr_poly_zerop        1.0))

  (assert-false (mjr_poly_zerop  #(0   1   0)))
  (assert-false (mjr_poly_zerop  #(1   0   0)))
  (assert-false (mjr_poly_zerop  #(0   1.0 0)))
  (assert-false (mjr_poly_zerop  #(1.0 0   0)))

  (assert-true  (mjr_poly_zerop  #(0   0   0)))
  (assert-true  (mjr_poly_zerop  #(        0)))
  (assert-true  (mjr_poly_zerop            0))
  (assert-true  (mjr_poly_zerop  #(0   0.0 0.0)))
  (assert-true  (mjr_poly_zerop  #(        0.0)))
  (assert-true  (mjr_poly_zerop            0.0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_print
  (with-output-to-string (*standard-output* nil)
    (assert-prints "2*x^3 + 3*x^2 + 4*x + 5"   (mjr_poly_print #(2 3 4 5)))
    (assert-prints "2*x^3 + 3*x^2 + 4*x + 5"   (mjr_poly_print #(0 2 3 4 5)))
    (assert-prints "0"                         (mjr_poly_print #(0)))
    ;; Should always return what is input
    (dotimes (i 30)
      (let* ((p (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10)))
        (assert-equalp p (mjr_poly_print p))))
    (assert-equalp 't       (mjr_poly_print 't))
    (assert-equalp '(1 2 3) (mjr_poly_print '(1 2 3)))
    (assert-equalp #C(1 2)  (mjr_poly_print #C(1 2)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_diff
  (assert-equalp #(6 6 4)  (mjr_poly_diff #(2 3 4 5) 1))
  (assert-equalp #(12 6)   (mjr_poly_diff #(2 3 4 5) 2))
  (assert-equalp #(12)     (mjr_poly_diff #(2 3 4 5) 3))
  (assert-equalp #(0)      (mjr_poly_diff #(2 3 4 5) 4))
  (assert-equalp #(0)      (mjr_poly_diff #(2 3 4 5) 5))
  (assert-equalp #(0)      (mjr_poly_diff #(2 3 4 5) 10))
  (dotimes (i 30)
    (let* ((p1  (vector (mjr_prng_int-co   0 100)))
           (p2  (vector (mjr_prng_float-co 0 100))))
      (assert-equalp p1 (mjr_poly_diff p1 0))
      (assert-equalp p2 (mjr_poly_diff p2 0))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_intg
  (assert-equalp 25/3 (mjr_poly_intg #(1 2 3)  1 2))
  (assert-equalp 11   (mjr_poly_intg #(3 2 1)  1 2))
  (assert-equalp 7/3  (mjr_poly_intg #(1 0 0)  1 2))
  (assert-equalp 9/2  (mjr_poly_intg #(0 3 0)  1 2))
  (assert-equalp 9/2  (mjr_poly_intg #(0 3 0) -1 2))
  (assert-equalp 15   (mjr_poly_intg #(1 2 3) -1 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_-
  ;; Single arguments case
  (assert-equalp #(      -1) (mjr_poly_-       1))
  (assert-equalp #(      -1) (mjr_poly_- #(    1)))
  (assert-equalp #(-1 -2 -3) (mjr_poly_- #(1 2 3)))
  (assert-equalp #(-1  0 -1) (mjr_poly_- #(1 0 1)))
  ;; Make sure the types are as expected
  (assert-equalp #(-1) (mjr_poly_- 2 3))
  (assert-equalp #(-5) (mjr_poly_- 2 3 4))
  (assert-equalp #(-1) (mjr_poly_- 2    #(3)))
  (assert-equalp #(-1) (mjr_poly_- #(2) #(3)))
  (assert-equalp #(-5) (mjr_poly_- 2 3 #(4)))
  (dotimes (i 30)
    (let* ((p   (mjr_poly_simplify (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10))))
      ;; Should always return zero
      (assert-equalp #(0) (mjr_poly_simplify (mjr_poly_- p p)))
      ;; Subtract zero, and get the same thing
      (assert-equalp p (mjr_poly_- p #(0)))
      ;; Subtract from zero and we get the negative
      (assert-equalp (mjr_poly_- p) (mjr_poly_- #(0) p))))
  ;; ERRORS
  (assert-error 'error    (mjr_poly_- 't))
  (assert-error 'error    (mjr_poly_- '(1 2 3)))
  (assert-error 'error    (mjr_poly_- nil))
  (assert-error 'error    (mjr_poly_- 't        3))
  (assert-error 'error    (mjr_poly_- '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_- nil       3))
  (assert-error 'error    (mjr_poly_- 3 't        3))
  (assert-error 'error    (mjr_poly_- 3 '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_- 3 nil       3))
  (assert-error 'error    (mjr_poly_- #(2 3)  't        3))
  (assert-error 'error    (mjr_poly_- #(2 3)  '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_- #(2 3)  nil       3))
  ;; Zero arguments case (ERROR for -)
  (assert-error 'error    (mjr_poly_-))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_+
  ;; Zero arguments case
  (assert-equalp #(0) (mjr_poly_+))
  ;; Single arguments case
  (assert-equalp #(1) (mjr_poly_+ 1))
  (assert-equalp #(1) (mjr_poly_+ #(1)))
  ;; Make sure the types are as expected
  (assert-equalp #(5) (mjr_poly_+ 2 3))
  (assert-equalp #(9) (mjr_poly_+ 2 3 4))
  (assert-equalp #(5) (mjr_poly_+ 2    #(3)))
  (assert-equalp #(5) (mjr_poly_+ #(2) #(3)))
  (assert-equalp #(9) (mjr_poly_+ 2 3 #(4)))
  ;; Add zero, and get the same thing
  (dotimes (i 30)
    (let* ((p   (mjr_poly_simplify (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10))))
      (assert-equalp p (mjr_poly_+ 0 p)    p)
      (assert-equalp p (mjr_poly_+ p 0)    p)
      (assert-equalp p (mjr_poly_+ #(0) p) p)
      (assert-equalp p (mjr_poly_+ p #(0)) p)))
  ;; ERRORS
  (assert-error 'error    (mjr_poly_+ 't))
  (assert-error 'error    (mjr_poly_+ '(1 2 3)))
  (assert-error 'error    (mjr_poly_+ nil))
  (assert-error 'error    (mjr_poly_+ 't        3))
  (assert-error 'error    (mjr_poly_+ '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_+ nil       3))
  (assert-error 'error    (mjr_poly_+ 3 't        3))
  (assert-error 'error    (mjr_poly_+ 3 '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_+ 3 nil       3))
  (assert-error 'error    (mjr_poly_+ #(2 3)  't        3))
  (assert-error 'error    (mjr_poly_+ #(2 3)  '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_+ #(2 3)  nil       3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_+-
  ;; Add the negative is the same as subtracting
  (dotimes (i 200)
    (let* ((p   (mjr_poly_simplify (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10)))
           (q   (mjr_poly_simplify (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10))))
      (assert-equalp (mjr_poly_- p q) (mjr_poly_+ p (mjr_poly_- q)))
      (assert-equalp (mjr_poly_- q p) (mjr_poly_+ q (mjr_poly_- p)))
      (assert-equalp #(0)             (mjr_poly_+ p (mjr_poly_- p)))
      (assert-equalp #(0)             (mjr_poly_+ q (mjr_poly_- q)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_*
  ;; Zero arguments case
  (assert-equalp #(1) (mjr_poly_*))
  ;; Single arguments case
  (assert-equalp #(1) (mjr_poly_* 1))
  (assert-equalp #(1) (mjr_poly_* #(1)))
  ;; Make sure the types are as expected
  (assert-equalp #(6) (mjr_poly_* 2 3))
  (assert-equalp #(24) (mjr_poly_* 2 3 4))
  (assert-equalp #(6) (mjr_poly_* 2    #(3)))
  (assert-equalp #(6) (mjr_poly_* #(2) #(3)))
  (assert-equalp #(24) (mjr_poly_* 2 3 #(4)))
  ;; Multiply 1, and get the same thing
  (dotimes (i 30)
    (let* ((p (mjr_poly_simplify (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10))))
      (assert-equalp p (mjr_poly_* p #(1))   p)
      (assert-equalp p (mjr_poly_* #(1) p)   p)
      (assert-equalp p (mjr_poly_* p 1)      p)
      (assert-equalp p (mjr_poly_* 1 p)      p)
      (assert-equalp p (mjr_poly_* p)        p)))
  ;; ERRORS
  (assert-error 'error    (mjr_poly_* 't))
  (assert-error 'error    (mjr_poly_* '(1 2 3)))
  (assert-error 'error    (mjr_poly_* nil))
  (assert-error 'error    (mjr_poly_* 't        3))
  (assert-error 'error    (mjr_poly_* '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_* nil       3))
  (assert-error 'error    (mjr_poly_* 3 't        3))
  (assert-error 'error    (mjr_poly_* 3 '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_* 3 nil       3))
  (assert-error 'error    (mjr_poly_* #(2 3)  't        3))
  (assert-error 'error    (mjr_poly_* #(2 3)  '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_* #(2 3)  nil       3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_+-and-mjr_poly_*
  ;; Repeated adds should be the same as scalar multiplication
  (dotimes (i 100)
    (let* ((p (mjr_prng_vector (mjr_prng_int-cc 1 3) #'mjr_prng_int-cc 0 10)))
      (assert-equalp (mjr_poly_+ p p)         (mjr_poly_* 2 p))
      (assert-equalp (mjr_poly_+ p p p)       (mjr_poly_* 3 p))
      (assert-equalp (mjr_poly_+ p p p p)     (mjr_poly_* 4 p))
      (assert-equalp (mjr_poly_+ p p p p p)   (mjr_poly_* 5 p))
      (assert-equalp (mjr_poly_+ p p p p p p) (mjr_poly_* 6 p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_reflect
  (dotimes (i 50)
    (let* ((poly (loop for pp = (mjr_prng_vector (mjr_prng_int-cc 2 5) #'mjr_prng_int-cc -10 10)
                       when (not (zerop (aref pp 0)))
                       return pp)))
      (assert-equality #'mjr_eps_= (mjr_poly_subst #(-1 0) poly) (mjr_poly_reflect poly) (list poly)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_shift
  ;; Make sure the -naive implementation matches the optimized version
  (dotimes (i 20)
    (let ((poly (loop for pp = (mjr_prng_vector (mjr_prng_int-cc 2 5) #'mjr_prng_int-cc -10 10)
                      when (not (zerop (aref pp 0)))
                      return pp)))
      (dotimes (j 20)
        (let* ((s  (mjr_prng_int-co 0 20))
               (ps (mjr_poly_shift s poly)))
          (assert-equality #'mjr_eps_= (mjr_poly_subst (vector 1 (- s))   poly) ps (list "SU" s poly))
          (assert-equality #'mjr_eps_= (mjr_poly_shift-naive0 s poly) ps (list "N0" s poly))
          (assert-equality #'mjr_eps_= (mjr_poly_shift-naive1 s poly) ps (list "N1" s poly))
          (assert-equality #'mjr_eps_= (mjr_poly_shift-naive2 s poly) ps (list "N2" s poly)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_deflate
  (assert-equalp (values #(1 1) 0) (mjr_poly_deflate #(1 0 -1) 1))
  ;; Repeatedly deflate
  (assert-equalp (values #(2 -8 6) 0)  (mjr_poly_deflate #(2 -9 10 -3) 1/2))
  (assert-equalp (values #(2 -6)   0)  (mjr_poly_deflate #(2 -8 6)     1))
  (assert-equalp (values #(2)      0)  (mjr_poly_deflate #(2 -6)       3))
  ;; A non-zero remainder
  (assert-equalp (values #(2 -5 0) -3) (mjr_poly_deflate #(2 -9 10 -3) 2))
  ;; Errors
  (assert-error 'error                 (mjr_poly_deflate '(2 -9 10 -3) 2))
  (assert-error 'error                 (mjr_poly_deflate #(2 -9 10 -3) 't))
  ;; Some random tests
  (loop for i from 1 upto 1000
        for r1  = (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10)
        for r2  = (mjr_prng_int-cc -10 10)
        for p1 = (apply #'mjr_poly_make-from-roots r1)
        for p2 = (apply #'mjr_poly_make-from-roots r2 r1)
        do (assert-equalp p1 (mjr_poly_deflate p2 r2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-structure
  (assert-equalp '(#(4 1 6 0) #(2 1 6 2))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2 -2                 ) 't))
  (assert-equalp '(#(3 1 6 0))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2                    ) 't))
  (assert-equalp '(#(2 1 6 0))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0                       ) 't))
  (assert-equalp '(#(2 0 6 0))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2                         ) 't))
  (assert-equalp '(#(4 1 8 0) #(4 1 6 2) #(2 1 8 2) #(2 1 6 4))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2 -2 #C(1 2) #C(1 -2)) 't))
  (assert-equalp '(#(3 1 8 0) #(3 1 6 2))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2    #C(1 2) #C(1 -2)) 't))
  (assert-equalp '(#(2 1 8 0) #(2 1 6 2))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0       #C(1 2) #C(1 -2)) 't))
  (assert-equalp '(#(2 0 8 0) #(2 0 6 2))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2         #C(1 2) #C(1 -2)) 't))
  (assert-equalp '(#(4 1 6 0) #(4 1 4 2) #(2 1 6 2) #(4 1 2 4) #(2 1 4 4) #(0 1 6 4) #(4 1 0 6) #(2 1 2 6) #(0 1 4 6) #(2 1 0 8) #(0 1 2 8) #(0 1 0 10))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2 -2                 ) nil))
  (assert-equalp '(#(3 1 6 0) #(3 1 4 2) #(1 1 6 2) #(3 1 2 4) #(1 1 4 4) #(3 1 0 6) #(1 1 2 6) #(1 1 0 8))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2                    ) nil))
  (assert-equalp '(#(2 1 6 0) #(2 1 4 2) #(0 1 6 2) #(2 1 2 4) #(0 1 4 4) #(2 1 0 6) #(0 1 2 6) #(0 1 0 8))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0                       ) nil))
  (assert-equalp '(#(2 0 6 0) #(2 0 4 2) #(0 0 6 2) #(2 0 2 4) #(0 0 4 4) #(2 0 0 6) #(0 0 2 6) #(0 0 0 8))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2                         ) nil))
  (assert-equalp '(#(4 1 8 0) #(4 1 6 2) #(2 1 8 2) #(4 1 4 4) #(2 1 6 4) #(0 1 8 4) #(4 1 2 6) #(2 1 4 6) #(0 1 6 6) #(4 1 0 8) #(2 1 2 8) #(0 1 4 8) #(2 1 0 10) #(0 1 2 10) #(0 1 0 12))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2 -2 #C(1 2) #C(1 -2)) nil))
  (assert-equalp '(#(3 1 8 0) #(3 1 6 2) #(1 1 8 2) #(3 1 4 4) #(1 1 6 4) #(3 1 2 6) #(1 1 4 6) #(3 1 0 8) #(1 1 2 8) #(1 1 0 10))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0 -2    #C(1 2) #C(1 -2)) nil))
  (assert-equalp '(#(2 1 8 0) #(2 1 6 2) #(0 1 8 2) #(2 1 4 4) #(0 1 6 4) #(2 1 2 6) #(0 1 4 6) #(2 1 0 8) #(0 1 2 8) #(0 1 0 10))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2 0       #C(1 2) #C(1 -2)) nil))
  (assert-equalp '(#(2 0 8 0) #(2 0 6 2) #(0 0 8 2) #(2 0 4 4) #(0 0 6 4) #(2 0 2 6) #(0 0 4 6) #(2 0 0 8) #(0 0 2 8) #(0 0 0 10))
                 (mjr_poly_root-structure (mjr_poly_make-from-roots 1 2 3 4 5 6 -1 -2         #C(1 2) #C(1 -2)) nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_scount-descartes
  (assert-equalp (values 0 0 5 nil) (mjr_poly_scount-descartes #(1 2 3 4 5 6)))
  (assert-equalp (values 5 0 0 nil) (mjr_poly_scount-descartes #(1 -2 3 -4 5 -6)))
  (assert-equalp (values 6 0 1 nil) (mjr_poly_scount-descartes #(5 -2 3 -4 1 -4 0 1)))
  (assert-equalp (values 5 2 0 nil) (mjr_poly_scount-descartes #(5 -2 3 -4 1 -4 0 0)))
  (assert-equalp (values 6 1 0 nil) (mjr_poly_scount-descartes #(5 -2 3 -4 1 -4 1 0)))
  (assert-equalp (values 4 0 3 nil) (mjr_poly_scount-descartes #(5  2 3 -4 1 -4 0 1)))
  (assert-equalp (values 0 0 5 nil) (mjr_poly_scount-descartes #(0 1 2 3 4 5 6)))
  (assert-equalp (values 5 0 0 nil) (mjr_poly_scount-descartes #(0 1 -2 3 -4 5 -6)))
  (assert-equalp (values 6 0 1 nil) (mjr_poly_scount-descartes #(0 5 -2 3 -4 1 -4 0 1)))
  (assert-equalp (values 5 2 0 nil) (mjr_poly_scount-descartes #(0 5 -2 3 -4 1 -4 0 0)))
  (assert-equalp (values 6 1 0 nil) (mjr_poly_scount-descartes #(0 5 -2 3 -4 1 -4 1 0)))
  (assert-equalp (values 4 0 3 nil) (mjr_poly_scount-descartes #(0 5  2 3 -4 1 -4 0 1)))
  (assert-equalp (values 0 0 5 nil) (mjr_poly_scount-descartes #(0 0 1 2 3 4 5 6)))
  (assert-equalp (values 5 0 0 nil) (mjr_poly_scount-descartes #(0 0 1 -2 3 -4 5 -6)))
  (assert-equalp (values 6 0 1 nil) (mjr_poly_scount-descartes #(0 0 5 -2 3 -4 1 -4 0 1)))
  (assert-equalp (values 5 2 0 nil) (mjr_poly_scount-descartes #(0 0 5 -2 3 -4 1 -4 0 0)))
  (assert-equalp (values 6 1 0 nil) (mjr_poly_scount-descartes #(0 0 5 -2 3 -4 1 -4 1 0)))
  (assert-equalp (values 4 0 3 nil) (mjr_poly_scount-descartes #(0 0 5  2 3 -4 1 -4 0 1)))
  (assert-equalp (values 0 0 1 nil) (mjr_poly_scount-descartes #(0 1 2)))

  (assert-error  'error             (mjr_poly_scount-descartes #(0 0 0)))
  (assert-error  'error             (mjr_poly_scount-descartes #(0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_truncate
  (loop for lenb from 1 upto 10
        do (loop for lent from lenb upto (* 3 lenb)
                 do (loop for lenr from 1 upto (1- lenb)
                          with polyb = (mjr_vec_make-from-func (lambda (i) (or i) (mjr_prng_int-cc 1 10)) :len lenb)
                          with polyt = (mjr_vec_make-from-func (lambda (i) (or i) (mjr_prng_int-cc 1 10)) :len lent)
                          with polyr = (mjr_vec_make-from-func (lambda (i) (or i) (mjr_prng_int-cc 1 10)) :len lenr)
                          with poly1 = (mjr_poly_simplify (mjr_poly_+ (mjr_poly_* polyt polyb) polyr ))
                          do (multiple-value-bind (fq fr) (mjr_poly_truncate poly1 polyb)
                               (assert-equalp (list polyt polyr) (list fq fr))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_simplify
  (assert-equalp #(2 3 4)   (mjr_poly_simplify #(2 3 4)))
  (assert-equalp #(2 3 4)   (mjr_poly_simplify #(0 2 3 4)))
  (assert-equalp #(2 3 4)   (mjr_poly_simplify #(0 0 2 3 4)))
  (assert-equalp #(2 0 3 4) (mjr_poly_simplify #(0 2 0 3 4)))
  (assert-equalp #(2 3 4 0) (mjr_poly_simplify #(2 3 4 0)))
  (assert-equalp #(1)        (mjr_poly_simplify 1))
  (assert-equalp #(0)        (mjr_poly_simplify 0))
  ;; ERRORS
  (assert-error 'error    (mjr_poly_simplify 't))
  (assert-error 'error    (mjr_poly_simplify '(1 2 3)))
  (assert-error 'error    (mjr_poly_simplify nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_eval
  (assert-equalp 3  (mjr_poly_eval #(2 3 4) -1))
  (assert-equalp 4  (mjr_poly_eval #(2 3 4) 0))
  (assert-equalp 9  (mjr_poly_eval #(2 3 4) 1))
  (assert-equalp 18 (mjr_poly_eval #(2 3 4) 2))
  (assert-equalp 31 (mjr_poly_eval #(2 3 4) 3))
  ;; ERRORS
  (assert-error 'error    (mjr_poly_eval #(2 3 4) 't))
  (assert-error 'error    (mjr_poly_eval #(2 3 4) '(1 2 3)))
  (assert-error 'error    (mjr_poly_eval #(2 3 4) nil))
  (assert-error 'error    (mjr_poly_eval 't        3))
  (assert-error 'error    (mjr_poly_eval '(1 2 3)  3))
  (assert-error 'error    (mjr_poly_eval nil       3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_eval-poly-and-first-n-derivatives
  (assert-equalp (values 3  -1)       (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) -1  ))
  (assert-equalp (values 4   3)       (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 0   ))
  (assert-equalp (values 9   7)       (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 1   ))
  (assert-equalp (values 18 11)       (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 2   ))
  (assert-equalp (values 31 15)       (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 3   ))
  (assert-equalp (values 3 )          (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) -1 0))
  (assert-equalp (values 4 )          (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 0  0))
  (assert-equalp (values 9 )          (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 1  0))
  (assert-equalp (values 18)          (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 2  0))
  (assert-equalp (values 31)          (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 3  0))
  (assert-equalp (values 3  -1 4)     (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) -1 2))
  (assert-equalp (values 4   3 4)     (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 0  2))
  (assert-equalp (values 9   7 4)     (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 1  2))
  (assert-equalp (values 18 11 4)     (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 2  2))
  (assert-equalp (values 31 15 4)     (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 3  2))
  (assert-equalp (values 3  -1 4 0)   (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) -1 3))
  (assert-equalp (values 4   3 4 0)   (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 0  3))
  (assert-equalp (values 9   7 4 0)   (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 1  3))
  (assert-equalp (values 18 11 4 0)   (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 2  3))
  (assert-equalp (values 31 15 4 0)   (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 3  3))
  (assert-equalp (values 3  -1 4 0 0) (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) -1 4))
  (assert-equalp (values 4   3 4 0 0) (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 0  4))
  (assert-equalp (values 9   7 4 0 0) (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 1  4))
  (assert-equalp (values 18 11 4 0 0) (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 2  4))
  (assert-equalp (values 31 15 4 0 0) (mjr_poly_eval-poly-and-first-n-derivatives #(2 3 4) 3  4))

  (dotimes (i 20)
    (let* ((len (mjr_prng_int-cc 1 10))
           (p   (mjr_prng_vector len #'mjr_prng_int-cc 0 15)))
      (dotimes (j 20)
        (let ((x   (mjr_prng_int-co 0 100))
              (n   (mjr_prng_int-co 0 (+ 2 len))))
          ;; N defaults to 1
          (assert-equalp (mjr_poly_eval-poly-and-first-n-derivatives p x) (mjr_poly_eval-poly-and-first-n-derivatives p x 1))
          ;; Make sure the -naive implementation matches the optimized version
          (assert-equalp (mjr_poly_eval-poly-and-first-n-derivatives-naive p x n) (mjr_poly_eval-poly-and-first-n-derivatives p x n))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-solve-rational
  (assert-equalp '(1/2 3/5 7/11 2/3 5/7)        (sort (mjr_poly_root-solve-rational #(2310 -7201 8949 -5541 1709 -210)) #'<))
  (assert-equalp '(-3 -2 1/3 1 3/2 2)           (sort (mjr_poly_root-solve-rational #(1 1/6 -61/6 35/6 139/6 -26 6))    #'<))
  (assert-equalp '(0 1 2 3 4 5)                 (sort (mjr_poly_root-solve-rational  #(1 -15 85 -225 274 -120 0))       #'<))
  ;; Random Integer polynomial with integer roots
  (dotimes (i 1000)
    (let* ((r (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10))
           (p (apply #'mjr_poly_make-from-roots r)))
      (assert-false (set-difference r  (mjr_poly_root-solve-rational p) :test (mjr_eps_make-fixed= 1e-2)) r)))
  ;; Errors  set-equal
  (assert-error 'error (mjr_poly_root-solve-rational #()))
  (assert-error 'error (mjr_poly_root-solve-rational nil))
  (assert-error 'error (mjr_poly_root-solve-rational (list 1 2 3)))
  (assert-error 'error (mjr_poly_root-solve-rational #(#C(1 2) #C(3 4))))
  (assert-error 'error (mjr_poly_root-solve-rational 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_2primitive
  ;; Hand picked cases
  (assert-equalp #(1155 1386 1540 1650 1470)    (mjr_poly_2primitive #(1/2 3/5 2/3 5/7 7/11)))
  (assert-equalp #(1155 1386 1540 1650 1470 0)  (mjr_poly_2primitive #(1/2 3/5 2/3 5/7 7/11 0)))
  (assert-equalp #(385 462 0 550 490)           (mjr_poly_2primitive #(1/2 3/5 0 5/7 7/11)))
  (assert-equalp #(1 2 3)                       (mjr_poly_2primitive #(2 4 6)))
  (assert-equalp #(1)                           (mjr_poly_2primitive #(3)))
  (assert-equalp #(1)                           (mjr_poly_2primitive 3))
  ;; Zero poly is NOT primative and can not be converted to one
  (assert-error 'error  (mjr_poly_2primitive #(0 0 0 0 0)))
  (assert-error 'error  (mjr_poly_2primitive 0))
  ;; Some random tests
  (loop for i from 1 upto 1000
        for p  = (mjr_prng_vector (mjr_prng_int-cc 2 10) #'mjr_prng_int-cc -10 10)
        for d  = (mjr_prng_int-cc 1 10)
        for q  = (mjr_poly_scale p (/ d))
        when (not (every #'zerop p))
        do (let ((pp (mjr_poly_2primitive p))
                 (qq (mjr_poly_2primitive q)))
             (assert-equalp 1 (apply #'gcd (concatenate 'list (delete 0 pp))) p)
             (assert-true   (every #'integerp pp))
             (assert-equalp 1 (apply #'gcd (concatenate 'list (delete 0 qq))) qq)
             (assert-true   (every #'integerp qq))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-search-bsect
  (assert-true (> 0.001 (abs (- 1/2   (mjr_poly_root-search-bsect #(2310 -7201 8949 -5541 1709 -210) 0.3 0.6)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-search-newton
  (assert-true (> 0.001 (abs (- 1/2   (mjr_poly_root-search-newton #(2310 -7201 8949 -5541 1709 -210) 0.51)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-search-laguerre
  (assert-true (> 0.001 (abs (- 5/7   (mjr_poly_root-search-laguerre #(2310 -7201 8949 -5541 1709 -210) 100.55)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-from-roots
  (assert-equalp #(1 -7201/2310 2983/770 -1847/770 1709/2310 -1/11)
                 (mjr_poly_make-from-roots 1/2 3/5 2/3 5/7 7/11))
  (assert-equalp #(1 -7201/1155 93198781/5336100 -25747153/889350 2663201/84700 -31189009/1334025 7146087/592900 -3782953/889350 5247901/5336100 -1709/12705 1/121)
                 (mjr_poly_make-from-roots 1/2 3/5 2/3 5/7 7/11 1/2 3/5 2/3 5/7 7/11))
  (assert-equalp #(1 -277859/43010 357954/21505 -459791/21505 294449/21505 -150423/43010)
                 (mjr_poly_make-from-roots 3/2 7/5 13/11 19/17 29/23))
  (assert-equalp #(1 -5/2 5/2 -5/4 5/16 -1/32)
                 (mjr_poly_make-from-roots 1/2 1/2 1/2 1/2 1/2))
  (assert-equalp #(1 0 -10510021/5336100 0 2726233/1778700 0 -1046473/1778700 0 53951/485100 0 -1/121)
                 (mjr_poly_make-from-roots -5/7 -2/3 -7/11 -3/5 -1/2 1/2 3/5 7/11 2/3 5/7))
  (assert-equalp #(1 -129 7122 -220628 4224150 -51893310 411154568 -2057205252 6166988769 -9920878441 6469693230)
                 (mjr_poly_make-from-roots 2 3 5 7 11 13 17 19 23 29))
  (assert-equalp #(1 -10 45 -120 210 -252 210 -120 45 -10 1)
                 (mjr_poly_make-from-roots 1 1 1 1 1 1 1 1 1 1))
  ;; Wilkinson's polynomial
  (assert-equalp #(1 -210 20615 -1256850 53327946 -1672280820 40171771630 -756111184500 11310276995381 -135585182899530
                   1307535010540395 -10142299865511450 63030812099294896 -311333643161390640 1206647803780373360 -3599979517947607200
                   8037811822645051776 -12870931245150988800 13803759753640704000 -8752948036761600000 2432902008176640000)
                 (mjr_poly_make-from-roots 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
  ;; Make sure constructed poly matches -naive version and that they are zero on the root list
  (dotimes (i 500)
    (let* ((nr   (mjr_prng_int-cc 1 30))
           (r    (mjr_prng_list nr #'mjr_prng_int-cc -30 30))
           (p1    (apply #'mjr_poly_make-from-roots       r))
           (p2    (apply #'mjr_poly_make-from-roots-naive r)))
      (loop for x in r
            do (assert-equal 0 (mjr_poly_eval p1 x)))
      (assert-equalp p1 p2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-solve-low-degree
  (flet ((cpl-lex< (x y) (or (< (realpart x) (realpart y)) (and (= (realpart x) (realpart y)) (< (imagpart x) (imagpart y)))))
         (int-it (alis) (mapcar (lambda (x) (if (complexp x) (complex (round (realpart x)) (round (imagpart x))) (round x))) alis)))
    (assert-equalp '(1 1)              (sort (mjr_poly_root-solve-low-degree #(1 -2        1))        #'cpl-lex<))  ;; real, multiple
    (assert-equalp '(1 2)              (sort (mjr_poly_root-solve-low-degree #(1 -3        2))        #'cpl-lex<))  ;; real, distinct
    (assert-equalp '(0 1)              (sort (mjr_poly_root-solve-low-degree #(1 -1        0))        #'cpl-lex<))  ;; real, one zero
    (assert-equalp '(0 0)              (sort (mjr_poly_root-solve-low-degree #(1  0        0))        #'cpl-lex<))  ;; real, both zero
    (assert-equalp '(1)                (sort (mjr_poly_root-solve-low-degree #(0 -1        1))        #'cpl-lex<))  ;; linear
    (assert-equalp '(1)                (sort (mjr_poly_root-solve-low-degree #(  -1        1))        #'cpl-lex<))  ;; linear
    (assert-equalp nil                 (sort (mjr_poly_root-solve-low-degree #(            1))        #'cpl-lex<))  ;; constant 1
    (assert-equalp nil                 (sort (mjr_poly_root-solve-low-degree #(            #C(1 1)))  #'cpl-lex<))  ;; constant #C(1 1)
    (assert-equalp '(#C(1 -1) #C(1 1)) (sort (mjr_poly_root-solve-low-degree #(1 -2        2))        #'cpl-lex<))  ;; complex, a conjugate pair
    (assert-equalp '(#C(1 1)  #C(1 1)) (sort (mjr_poly_root-solve-low-degree #(1 #C(-2 -2) #C(0 2)))  #'cpl-lex<))  ;; complex, multiple
    (assert-equalp '(#C(1 1)  #C(1 2)) (sort (mjr_poly_root-solve-low-degree #(1 #C(-2 -3) #C(-1 3))) #'cpl-lex<))  ;; complex, distinct, not a conjugate pair
    (assert-equalp '(1        #C(1 1)) (sort (mjr_poly_root-solve-low-degree #(1 #C(-2 -1) #C(1 1)))  #'cpl-lex<))  ;; one complex, one real
    ;; Compute 500 linear eq with known roots, and make sure mjr_poly_root-solve-low-degree solves them correctly.
    (loop for i from 1 upto 500
          for rr1  = (mjr_prng_int-cc -10 10)
          for cr1  = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10))
          ;;                                                                                                                                                          ;; poly  #real #complex
          do (assert-false (set-difference (list rr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1)) ;; real  1     0
          do (assert-false (set-difference (list cr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1))  :test (mjr_eps_make-fixed= 1e-3)) (list cr1)) ;; real  0     1
          )
    ;; Compute 2000 quadratics with known roots, and make sure mjr_poly_root-solve-low-degree solves them correctly.
    (loop for i from 1 upto 2000
          for rr1  = (mjr_prng_int-cc -10 10)
          for cr1  = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10))
          for cr1c = (conjugate cr1)
          for rr2  = (loop for r = (mjr_prng_int-cc -10 10)                                    while (= rr1 r)                finally (return r))
          for cr2  = (loop for c = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10)) while (= cr1 c)                finally (return c))
          ;;                                                                                                                                                                       ;; poly  #real #complex
          do (assert-false (set-difference (list rr1 rr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr2))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1 rr2) ) ;; real  1+1   0
          do (assert-false (set-difference (list rr1 rr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr1))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1 rr1) ) ;; real  2     0
          do (assert-false (set-difference (list cr1 cr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr2))  :test (mjr_eps_make-fixed= 1e-3)) (list cr1 cr2) ) ;; real  0     1+1
          do (assert-false (set-difference (list cr1 cr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr1))  :test (mjr_eps_make-fixed= 1e-3)) (list cr1 cr1) ) ;; real  0     2
          do (assert-false (set-difference (list cr1 cr1c) (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr1c)) :test (mjr_eps_make-fixed= 1e-3)) (list cr1 cr1c)) ;; real  0     1+~1
          )
    ;; Compute 1000 cubics with known roots, and make sure mjr_poly_root-solve-low-degree solves them correctly.
    (loop for i from 1 upto 1000
          for rr1  = (mjr_prng_int-cc -10 10)
          for cr1  = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10))
          for cr1c = (conjugate cr1)
          for rr2  = (loop for r = (mjr_prng_int-cc -10 10)                                    while (= rr1 r)                finally (return r))
          for rr3  = (loop for r = (mjr_prng_int-cc -10 10)                                    while (or (= rr1 r) (= rr2 r)) finally (return r))
          for cr2  = (loop for c = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10)) while (= cr1 c)                finally (return c))
          for cr3  = (loop for c = (complex (mjr_prng_int-cc -10 10) (mjr_prng_int-cc -10 10)) while (or (= cr1 c) (= cr2 c)) finally (return c))
          ;;                                                                                                                                                                                   ;; poly  #real #complex
          do (assert-false (set-difference (list rr1 rr1 rr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr1 rr1))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1 rr1 rr1) ) ;; real  3     0
          do (assert-false (set-difference (list rr1 rr1 rr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr1 rr2))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1 rr1 rr2) ) ;; real  1+2   0
          do (assert-false (set-difference (list rr1 rr2 rr3)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr2 rr3))  :test (mjr_eps_make-fixed= 1e-3)) (list rr1 rr2 rr3) ) ;; real  1+1+1 0
          do (assert-false (set-difference (list rr1 cr1 cr1c) (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 cr1 cr1c)) :test (mjr_eps_make-fixed= 1e-3)) (list rr1 cr1 cr1c)) ;; real  1     1+~1
          do (assert-false (set-difference (list rr1 rr2 cr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr2 cr2))  :test (mjr_eps_make-fixed= 1e-1)) (list rr1 rr2 cr2) ) ;; cplx  1+1   1
          do (assert-false (set-difference (list rr1 rr1 cr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 rr1 cr1))  :test (mjr_eps_make-fixed= 1e-1)) (list rr1 rr1 cr1) ) ;; cplx  2     1
          do (assert-false (set-difference (list rr1 cr1 cr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots rr1 cr1 cr2))  :test (mjr_eps_make-fixed= 1e-1)) (list rr1 cr1 cr2) ) ;; cplx  1     1+1
          do (assert-false (set-difference (list cr1 cr2 cr3)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr2 cr3))  :test (mjr_eps_make-fixed= 1e-1)) (list cr1 cr2 cr3) ) ;; cplx  0     1+1+1
          do (assert-false (set-difference (list cr1 cr1 cr2)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr1 cr2))  :test (mjr_eps_make-fixed= 1e-1)) (list cr1 cr1 cr2) ) ;; cplx  0     2+1
          do (assert-false (set-difference (list cr1 cr1 cr1)  (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr1 cr1))  :test (mjr_eps_make-fixed= 1e-1)) (list cr1 cr1 cr1) ) ;; cplx  0     3
          do (assert-false (set-difference (list cr1 cr1c cr2) (mjr_poly_root-solve-low-degree (mjr_poly_make-from-roots cr1 cr1c cr2)) :test (mjr_eps_make-fixed= 1e-1)) (list cr1 cr1c cr2)) ;; cplx  0     1+1+~1
          ))
  ;; Errors
  (assert-error 'error                       (mjr_poly_root-solve-low-degree #(0)))
  (assert-error 'error                       (mjr_poly_root-solve-low-degree #(1 2 3 4 5)))
  (assert-error 'error                       (mjr_poly_root-solve-low-degree #()))
  (assert-error 'error                       (mjr_poly_root-solve-low-degree (list 1 2 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_constant-coeff
  (assert-equalp 3 (mjr_poly_constant-coeff #(1 2 3)))
  (assert-equalp 3 (mjr_poly_constant-coeff #(0 1 2 3)))
  (assert-equalp 1 (mjr_poly_constant-coeff #(0 0 1)))
  (assert-equalp 0 (mjr_poly_constant-coeff #(0 0 0)))
  (assert-equalp 1 (mjr_poly_constant-coeff #(0 1)))
  (assert-equalp 1 (mjr_poly_constant-coeff #(1)))
  (assert-equalp 0 (mjr_poly_constant-coeff #(0)))
  (assert-equalp 1 (mjr_poly_constant-coeff 1))
  (assert-equalp 0 (mjr_poly_constant-coeff 0))
  ;; Errors
;;(assert-error 'error (mjr_poly_constant-coeff #())) ;; Not an error now, and not sure it is worth checking
  (assert-error 'error (mjr_poly_constant-coeff nil))
  (assert-error 'error (mjr_poly_constant-coeff (list 1 2 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_leading-coeff
  (assert-equalp 1 (mjr_poly_leading-coeff #(1 2 3)))
  (assert-equalp 1 (mjr_poly_leading-coeff #(0 1 2 3)))
  (assert-equalp 1 (mjr_poly_leading-coeff #(0 0 1)))
  (assert-equalp 0 (mjr_poly_leading-coeff #(0 0 0)))
  (assert-equalp 1 (mjr_poly_leading-coeff #(0 1)))
  (assert-equalp 1 (mjr_poly_leading-coeff #(1)))
  (assert-equalp 0 (mjr_poly_leading-coeff #(0)))
  (assert-equalp 1 (mjr_poly_leading-coeff 1))
  (assert-equalp 0 (mjr_poly_leading-coeff 0))
  ;; Errors
  ;;(assert-error 'error (mjr_poly_leading-coeff #())) ;; Not an error now, and not sure it is worth checking
  (assert-error 'error (mjr_poly_leading-coeff nil))
  (assert-error 'error (mjr_poly_leading-coeff (list 1 2 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-lagrange
  (assert-equalp #(1/2 -1/2 0) (mjr_poly_make-lagrange '(-1 0 1) 0))
  (assert-equalp #(-1 0 1)     (mjr_poly_make-lagrange '(-1 0 1) 1))
  (assert-equalp #(1/2 1/2 0)  (mjr_poly_make-lagrange '(-1 0 1) 2))
  ;; Check that output poly has the correct values on the input points
  (dotimes (i 20)
    (let* ((pt    (remove-duplicates (concatenate 'list (mjr_prng_vector (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc 0 10))))
           (pl   (length pt)))
      (if (> pl 1)
          (dotimes (j pl)
            (loop for k from 0
                  for p in pt
                  do (if (= k j)
                         (assert-equal 1 (mjr_poly_eval (mjr_poly_make-lagrange pt j) p))
                         (assert-equal 0 (mjr_poly_eval (mjr_poly_make-lagrange pt j) p))
                         ))))))
  ;; Errors
  (assert-error 'error (mjr_poly_make-lagrange '(1 2 3) 3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_seq-make-chebyshev
  (assert-equalp #(#(1))                                            (mjr_poly_seq-make-chebyshev 1))
  (assert-equalp #(#(1) #(1 0))                                     (mjr_poly_seq-make-chebyshev 2))
  (assert-equalp #(#(1) #(1 0) #(2 0 -1))                           (mjr_poly_seq-make-chebyshev 3))
  (assert-equalp #(#(1) #(1 0) #(2 0 -1) #(4 0 -3 0))               (mjr_poly_seq-make-chebyshev 4))
  (assert-equalp #(#(1) #(1 0) #(2 0 -1) #(4 0 -3 0) #(8 0 -8 0 1)) (mjr_poly_seq-make-chebyshev 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-chebyshev
  ;; Normal cases for kind=1
  (assert-equalp #(1)             (mjr_poly_make-chebyshev 0))
  (assert-equalp #(1 0)           (mjr_poly_make-chebyshev 1))
  (assert-equalp #(2 0 -1)        (mjr_poly_make-chebyshev 2))
  (assert-equalp #(4 0 -3 0)      (mjr_poly_make-chebyshev 3))
  (assert-equalp #(8 0 -8 0 1)    (mjr_poly_make-chebyshev 4))
  ;; Normal cases for kind=2
  (assert-equalp #(1)             (mjr_poly_make-chebyshev 0 2))
  (assert-equalp #(2 0)           (mjr_poly_make-chebyshev 1 2))
  (assert-equalp #(4 0 -1)        (mjr_poly_make-chebyshev 2 2))
  (assert-equalp #(8 0 -4 0)      (mjr_poly_make-chebyshev 3 2))
  (assert-equalp #(16 0 -12 0 1)  (mjr_poly_make-chebyshev 4 2))
  ;; The default value for the second argument is 1
  (assert-equalp (mjr_poly_make-chebyshev 0)   (mjr_poly_make-chebyshev 0 1))
  (assert-equalp (mjr_poly_make-chebyshev 1)   (mjr_poly_make-chebyshev 1 1))
  (assert-equalp (mjr_poly_make-chebyshev 2)   (mjr_poly_make-chebyshev 2 1))
  (assert-equalp (mjr_poly_make-chebyshev 3)   (mjr_poly_make-chebyshev 3 1))
  (assert-equalp (mjr_poly_make-chebyshev 4)   (mjr_poly_make-chebyshev 4 1))
  ;; Make sure mjr_poly_make-chebyshev & mjr_poly_seq-make-chebyshev are consistent
  (loop for i from 1 upto 10
        do (assert-equalp (mjr_poly_seq-make-chebyshev i 2)
                          (map 'vector (lambda (n) (mjr_poly_make-chebyshev n 2))
                               (loop for j from 0 upto (1- i)
                                     collect j)))
        do (assert-equalp (mjr_poly_seq-make-chebyshev i)
                          (map 'vector (lambda (n) (mjr_poly_make-chebyshev n))
                               (loop for j from 0 upto (1- i)
                                     collect j))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-legendre
  ;; (%i39) ev(1/(2^n*n!)*diff((x^2-1)^n,x,n), diff, n=5, expand);
  ;; (%o39) 63*x^5/8-35*x^3/4+15*x/8
  (assert-equalp #(63/8 0 -35/4 0 15/8 0)                                                                (mjr_poly_make-legendre 5))

  ;; (%i40) ev(1/(2^n*n!)*diff((x^2-1)^n,x,n), diff, n=10, expand);
  ;; (%o40) 46189*x^10/256-109395*x^8/256+45045*x^6/128-15015*x^4/128+3465*x^2/256-63/256
  (assert-equalp #(46189/256 0 -109395/256 0 45045/128 0 -15015/128 0 3465/256 0 -63/256)                (mjr_poly_make-legendre 10))

  ;; (%i41) ev(1/(2^n*n!)*diff((x^2-1)^n,x,n), diff, n=21, expand);
  ;; (%o41) 67282234305*x^21/262144-172308161025*x^19/131072
  ;;                               +755505013725*x^17/262144
  ;;                               -115707975075*x^15/32768
  ;;                               +347123925225*x^13/131072-82047473235*x^11/65536
  ;;                               +48522699225*x^9/131072-2151252675*x^7/32768
  ;;                               +1673196525*x^5/262144-37182145*x^3/131072
  ;;                               +969969*x/262144
  (assert-equalp #(67282234305/262144 0 -172308161025/131072 0 755505013725/262144 0
                   -115707975075/32768 0 347123925225/131072 0 -82047473235/65536 0 48522699225/131072
                   0 -2151252675/32768 0 1673196525/262144 0 -37182145/131072 0 969969/262144 0)         (mjr_poly_make-legendre 21))

  ;; Check naive
  (dotimes (i 30)
    (assert-equalp (mjr_poly_make-legendre i) (mjr_poly_make-legendre-naive i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_iexpt
  ;; Check naive
  (dotimes (i 100)
    (let* ((p (mjr_prng_vector (mjr_prng_int-cc 1 20) #'mjr_prng_int-cc 0 15))
           (n (mjr_prng_int-co 0 20))
           (m (mjr_poly_iexpt p n)))
      (assert-equalp m (mjr_poly_iexpt-naive0 p n))
      (assert-equalp m (mjr_poly_iexpt-naive1 p n))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_factor-over-integers

  ;; Van der Waerden(1970); Transactions on Algebra; Blum and Schulenberger; Frederick Ungar; Sections 5.4 and 5.6
  (assert-equalp '(#(1 1 1) #(1 0 -1 2))  (mjr_poly_factor-over-integers #(1 1 0 1 1 2)))

  (assert-equalp  '(#(1 1) #(1 3) #(1 4) #(1 4) #(1 5) #(2 1) #(3 1) #(1 3 4) #(2 1 4))
                  (sort (mjr_poly_factor-over-integers (mjr_poly_* #(1 3 4) #(1 5 4) #(6 5 1) #(2 1 4) #(1 3) #(1 4) #(1 5)))
                        (lambda (a b) (let ((la (length a))
                                            (lb (length b)))
                                        (if (= la lb)
                                            (mjr_vec_< a b)
                                            (< (length a) (length b)))))))

  (loop for n in '(3 5 7 11 13) ;; to slow to go on.
        for v = (mjr_vec_make-const n 1)
        do (assert-equalp (list v) (mjr_poly_factor-over-integers v)))

  ;; Error now, should not be...
  ;;(assert-equalp '(#(1))             (mjr_poly_factor-over-integers #(1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_degree
  (assert-equalp 3 (mjr_poly_degree #(1 2 3 4)))
  (assert-equalp 2 (mjr_poly_degree #(0 2 3 4)))
  (assert-equalp 1 (mjr_poly_degree #(0 0 3 4)))
  (assert-equalp 0 (mjr_poly_degree #(0 0 0 4)))
  (assert-equalp 0 (mjr_poly_degree #(0 0 0 0)))
  (assert-equalp 0 (mjr_poly_degree #(0)))
  (assert-equalp 0 (mjr_poly_degree 0))
  ;; Errors
  (assert-error 'error (mjr_poly_degree #()))
  (assert-error 'error (mjr_poly_degree nil))
  (assert-error 'error (mjr_poly_degree (list 1 2 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_index
  ;; "Sum of the exponents of the non-zero terms."
  (assert-equalp 6 (mjr_poly_index #(1 2 3 4)))
  (assert-equalp 6 (mjr_poly_index #(1 2 3 0)))
  (assert-equalp 5 (mjr_poly_index #(1 2 0 4)))
  (assert-equalp 5 (mjr_poly_index #(1 2 0 0)))
  (assert-equalp 3 (mjr_poly_index #(1 0 0 4)))
  (assert-equalp 3 (mjr_poly_index #(1 0 0 0)))
  (assert-equalp 0 (mjr_poly_index #(0 0 0 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_density
    ;; "The number of non-zero terms."
  (assert-equalp 4 (mjr_poly_density #(1 2 3 4)))
  (assert-equalp 3 (mjr_poly_density #(1 2 3 0)))
  (assert-equalp 3 (mjr_poly_density #(1 2 0 4)))
  (assert-equalp 2 (mjr_poly_density #(1 2 0 0)))
  (assert-equalp 1 (mjr_poly_density #(0 2 0 0)))
  (assert-equalp 2 (mjr_poly_density #(1 0 0 4)))
  (assert-equalp 1 (mjr_poly_density #(1 0 0 0)))
  (assert-equalp 0 (mjr_poly_density #(0 0 0 0)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-search-largest-real

  ;; Faliure when we look for our first interval midpoint
  (assert-equality #'mjr_eps_=  4 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots 0 1 2 -1 -2 3 -3 4 -4)                  :max-itr 50 :xeps 1e-10))

  ;; all roots complex
  (assert-equalp nil              (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots #C(1 2) #C(1 -2) #C(3 4) #C(3 -4))      :max-itr 50 :xeps 1e-10))

  ;; positive and negative roots
  (assert-equality #'mjr_eps_=  4 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots 1 2 -1 -2 3 -3 4 -4)                    :max-itr 50 :xeps 1e-10))

  ;; mixed real and complex roots
  (assert-equality #'mjr_eps_=  4 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots 0 1 #C(1 2) #C(1 -2) 2 -1 -2 3 -3 4 -4) :max-itr 50 :xeps 1e-10))

  ;; all roots posiive
  (assert-equality #'mjr_eps_=  4 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots 1 2 3 4)                                :max-itr 50 :xeps 1e-10))

  ;; No positive
  (assert-equality #'mjr_eps_=  0 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots 0 -1 -2 -3 -4)                          :max-itr 50 :xeps 1e-10))

  ;; all roots negative
  (assert-equality #'mjr_eps_= -1 (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots -1 -2 -3 -4)                            :max-itr 50 :xeps 1e-10))

  ;; Some random tests
  (loop for i from 1 upto 50
        for lenb  = (mjr_prng_int-cc 2 10)
        for roots = (mjr_prng_list lenb #'mjr_prng_int-cc -100 100)
        do (assert-equality #'mjr_eps_= (apply #'max roots) (mjr_poly_root-search-largest-real (mjr_poly_make-from-roots roots) :max-itr 50 :xeps 1e-10)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-count-distinct-interval

  (assert-equalp 0 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1)   -2   0))
  (assert-equalp 0 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots -1)   0   2))

  (assert-equalp 0 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots -1)  -3   -2))
  (assert-equalp 0 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1)    2   3))

  (assert-equalp 7 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -2   6))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -2   5))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -2   4.5))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -1   6))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -0.5 6))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -1   5))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0)  -0.5 4.5))

  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -2   6))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -2   5))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -2   4.5))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -1   6))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -0.5 6))
  (assert-equalp 4 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -1   5))
  (assert-equalp 4 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1)  -0.5 4.5))

  (assert-equalp 7 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -2   6))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -2   5))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -2   4.5))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -1   6))
  (assert-equalp 6 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -0.5 6))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -1   5))
  (assert-equalp 5 (mjr_poly_root-count-distinct-interval (mjr_poly_make-from-roots 1 2 3 4 5 -1 0 #C(1 2) #C(1 -2))  -0.5 4.5))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_seq-make-sturm-canonical
  (assert-equalp '(#(1 1 0 -1 -1) #(4 3 0 -1) #(3/16 3/4 15/16) #(-32 -64) #(-3/16))    (mjr_poly_seq-make-sturm-canonical #(1 1 0 -1 -1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_seq-make-fourier
  (assert-equalp '(#(1 0 -7 7) #(3 0 -7) #(6 0) #(6))  (mjr_poly_seq-make-fourier #(1 0 -7 7)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_scount-sturm

  (assert-equalp 1 (mjr_poly_scount-sturm (mjr_poly_make-from-roots  #C(2 1) #C(2 -1) 2)     1 3))
  (assert-equalp 1 (mjr_poly_scount-sturm (mjr_poly_make-from-roots  0 2 4)                  1 3))
  (assert-equalp 2 (mjr_poly_scount-sturm (mjr_poly_make-from-roots  2 4)                    1 5))
  (assert-equalp 2 (mjr_poly_scount-sturm (mjr_poly_make-from-roots  2 4 #C(2 1) #C(2 -1))   1 5))
  (assert-equalp 2 (mjr_poly_scount-sturm (mjr_poly_make-from-roots  2 2 4 #C(2 1) #C(2 -1)) 1 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_scount-budan
  (assert-equalp 3 (mjr_poly_scount-budan (mjr_poly_make-from-roots  #C(2 1) #C(2 -1) 2)     1 3))
  (assert-equalp 1 (mjr_poly_scount-budan (mjr_poly_make-from-roots  0 2 4)                  1 3))
  (assert-equalp 2 (mjr_poly_scount-budan (mjr_poly_make-from-roots  2 4)                    1 5))
  (assert-equalp 4 (mjr_poly_scount-budan (mjr_poly_make-from-roots  2 4 #C(2 1) #C(2 -1))   1 5))
  (assert-equalp 5 (mjr_poly_scount-budan (mjr_poly_make-from-roots  2 2 4 #C(2 1) #C(2 -1)) 1 5))

  ;; Some random tests
  (loop for i from 1 upto 300
        for p = (mjr_prng_vector (mjr_prng_int-cc 2 10) #'mjr_prng_int-cc -100 100)
        for l = (mjr_prng_int-cc -10 10)
        for u = (+ l (mjr_prng_int-cc 1 10))
        do (assert-equalp (mjr_poly_scount-fourier p l u) (mjr_poly_scount-budan p l u)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_scount-fourier
  (assert-equalp 3 (mjr_poly_scount-fourier (mjr_poly_make-from-roots  #C(2 1) #C(2 -1) 2)     1 3))
  (assert-equalp 1 (mjr_poly_scount-fourier (mjr_poly_make-from-roots  0 2 4)                  1 3))
  (assert-equalp 2 (mjr_poly_scount-fourier (mjr_poly_make-from-roots  2 4)                    1 5))
  (assert-equalp 4 (mjr_poly_scount-fourier (mjr_poly_make-from-roots  2 4 #C(2 1) #C(2 -1))   1 5))
  (assert-equalp 5 (mjr_poly_scount-fourier (mjr_poly_make-from-roots  2 2 4 #C(2 1) #C(2 -1)) 1 5))
  (assert-equalp 2 (mjr_poly_scount-fourier #(1 0 -7 7)                                        0 2))
  ;; Some random tests
  (loop for i from 1 upto 300
        for p = (mjr_prng_vector (mjr_prng_int-cc 2 10) #'mjr_prng_int-cc -100 100)
        for l = (mjr_prng_int-cc -10 10)
        for u = (+ l (mjr_prng_int-cc 1 10))
        do (assert-equalp (mjr_poly_scount-fourier p l u) (mjr_poly_scount-fourier-naive p l u)))

  ;; XREF: This function is heavily tested in mjr_poly_scount-budan
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-laguerre
  (assert-equalp #(1)                (mjr_poly_make-laguerre 0))
  (assert-equalp #(-1 1)             (mjr_poly_make-laguerre 1))
  (assert-equalp #(1/2 -2 1)         (mjr_poly_make-laguerre 2))
  (assert-equalp #(-1/6 3/2 -3 1)    (mjr_poly_make-laguerre 3))
  (assert-equalp #(1/24 -2/3 3 -4 1) (mjr_poly_make-laguerre 4))
  (dotimes (i 40)
    (assert-equalp (mjr_poly_make-laguerre-naive i) (mjr_poly_make-laguerre i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_seq-make-laguerre
  (assert-equalp (concatenate 'vector (loop for pow from 0 upto 30
                                            collect (mjr_poly_make-laguerre pow))) (mjr_poly_seq-make-laguerre 31))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_make-hermite
  (assert-equalp #(1)              (mjr_poly_make-hermite 0))
  (assert-equalp #(1 0)            (mjr_poly_make-hermite 1))
  (assert-equalp #(1 0 -1)         (mjr_poly_make-hermite 2))
  (assert-equalp #(1 0 -3 0)       (mjr_poly_make-hermite 3))
  (assert-equalp #(1 0 -6 0 3)     (mjr_poly_make-hermite 4))
  (assert-equalp #(1 0 -10 0 15 0) (mjr_poly_make-hermite 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_seq-make-hermite
  (assert-equalp (concatenate 'vector (loop for pow from 0 upto 30
                                            collect (mjr_poly_make-hermite pow))) (mjr_poly_seq-make-hermite 31))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_root-solve-search-deflate

  ;; Integer polynomial with integer roots
  (dotimes (i 1000)
    (let* ((r (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10))
           (p (apply #'mjr_poly_make-from-roots r)))
      (assert-false (set-difference r  (mjr_poly_root-solve-search-deflate p) :test (mjr_eps_make-fixed= 1e-2)) r)))
  ;;Gausian Integer polynomial with Gausian integer roots
  (dotimes (i 1000)
    (let* ((r (mapcar #'complex (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10) (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10)))
           (p (apply #'mjr_poly_make-from-roots r)))
      (assert-false (set-difference r  (mjr_poly_root-solve-search-deflate p) :test (mjr_eps_make-fixed= 1e-2)) r)))
  ;; Integer polynomial with Gausian integer roots
  (dotimes (i 1000)
    (let* ((ra (mapcar #'complex (mjr_prng_list (mjr_prng_int-cc 1 5) #'mjr_prng_int-cc -10 10) (mjr_prng_list (mjr_prng_int-cc 1 10) #'mjr_prng_int-cc -10 10)))
           (rb (mapcar #'conjugate ra))
           (r (concatenate 'list ra rb))
           (p (apply #'mjr_poly_make-from-roots r)))
      (assert-false (set-difference r  (mjr_poly_root-solve-search-deflate p) :test (mjr_eps_make-fixed= 1e-2)) r)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_cubic-depress
    (assert-equalp #(1 0 6 -20) (mjr_poly_cubic-depress #(2 -30 162 -350)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_tschirnhaus-3-2
    (assert-equalp #(2 0 12 -40) (mjr_poly_tschirnhaus-3-2 #(2 -30 162 -350)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_discriminant-low-degree
  (loop for d from 2 upto 5
        do (dotimes (i 1000)
             (let* ((p (mjr_prng_vector 4 #'mjr_prng_int-cc -10 10)))
               (if (zerop (aref p 0))
                   (setf (aref p 0) (mjr_prng_int-cc 1 10)))
               (assert-equalp (mjr_poly_discriminant-high-degree p) (mjr_poly_discriminant-low-degree p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_discriminant-high-degree
  ;; A few hand picked test cases -- verfied with maple:
  ;;  > dlst := proc (p) discrim(add(t, t in [seq(x^i, i=(numelems(p)-1)..0, -1)] *~ p), x); end proc;
  ;;  > dlst([1,2,3,4,5,6,7]);
  ;;  -157351936
  (assert-equalp -157351936              (mjr_poly_discriminant-high-degree #(1 2 3 4 5 6 7)))
  (assert-equalp 4782969                 (mjr_poly_discriminant-high-degree #(1 1 1 1 1 1 1 1 1)))
  (assert-equalp -34828517376            (mjr_poly_discriminant-high-degree #(1 -2 3 -4 5 -6 7 -8)))
  (assert-equalp 4200118229/103723200000 (mjr_poly_discriminant-high-degree #(1/2 1/3 1/4 1/5 1/6 1/7)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_resultant
  (assert-equalp 16 (mjr_poly_resultant #(2 -3 4 -5) #(3 -4 5 -6)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_poly_mahler-measure
  ;; Some integer polynomials with small Mahler measure from http://www.cecm.sfu.ca/~mjm/Lehmer/search/
  ;;
  ;;    Measure         P
  ;;    1.1762808182    x^10+x^9-x^7-x^6-x^5-x^4-x^3+x+1
  ;;    1.1883681475    x^18+x^17+x^16+x^15-x^12-x^11-x^10-x^9-x^8-x^7-x^6+x^3+x^2+x+1
  ;;    1.2163916611    x^10-x^6+x^5-x^4+1
  ;;    1.2277855586    x^12+x^11+x^10-x^8-x^7-x^6-x^5-x^4+x^2+x+1
  ;;    1.2303914344    x^10+x^7+x^5+x^3+1
  ;;    1.2612309611    x^10-x^8+x^5-x^2+1
  ;;    1.2672338594    x^10+x^8+x^7+x^5+x^3+x^2+1
  ;;    1.2835823606    x^10+x^9-x^5+x+1
  ;;    1.2934859531    x^10-x^8+x^7-x^5+x^3-x^2+1
  ;;    1.2806381562    x^8+x^5-x^4+x^3+1
  ;;
  ;;    Lehmer's polynomial is the first on the list.  It is from his 1933 paper:
  ;;    Lehmer, D.H. (1933). "Factorization of certain cyclotomic functions". Ann. Math. (2) 34: 461-479. doi:10.2307/1968172. ISSN 0003-486X. Zbl 0007.19904.
  ;;
  ;;    More info about Lehmer's Conjecture: https://en.wikipedia.org/wiki/Lehmer%27s_conjecture
  ;;
  ;; A little bit of maxima to create lisp vectors:
  ;;
  ;;   (i) poly2coeff(p):=makelist(coeff(p,x,j),j,reverse(makelist(i,i,0,hipow(p,x))));
  ;;   (i) makelist(poly2coeff(pi), pi, [x^10+x^9-x^7-x^6-x^5-x^4-x^3+x+1,
  ;;                                     x^18+x^17+x^16+x^15-x^12-x^11-x^10-x^9-x^8-x^7-x^6+x^3+x^2+x+1 ,
  ;;                                     x^10-x^6+x^5-x^4+1,
  ;;                                     x^12+x^11+x^10-x^8-x^7-x^6-x^5-x^4+x^2+x+1,
  ;;                                     x^10+x^7+x^5+x^3+1,
  ;;                                     x^10-x^8+x^5-x^2+1,
  ;;                                     x^10+x^8+x^7+x^5+x^3+x^2+1,
  ;;                                     x^10+x^9-x^5+x+1,
  ;;                                     x^10-x^8+x^7-x^5+x^3-x^2+1,
  ;;                                     x^8+x^5-x^4+x^3+1]);
  ;;   (o) [[1,1,0,-1,-1,-1,-1,-1,0,1,1],
  ;;        [1,1,1,1,0,0,-1,-1,-1,-1,-1,-1,-1,0,0,1,1,1,1],
  ;;        [1,0,0,0,-1,1,-1,0,0,0,1],
  ;;        [1,1,1,0,-1,-1,-1,-1,-1,0,1,1,1],
  ;;        [1,0,0,1,0,1,0,1,0,0,1],
  ;;        [1,0,-1,0,0,1,0,0,-1,0,1],
  ;;        [1,0,1,1,0,1,0,1,1,0,1],
  ;;        [1,1,0,0,0,-1,0,0,0,1,1],
  ;;        [1,0,-1,1,0,-1,0,1,-1,0,1],
  ;;        [1,0,0,1,-1,1,0,0,1]]
  ;;
  ;; Some test cases
  ;;
  ;;         |----------------+----------------------------------------------------------------+-------------------------------------------------|
  ;;         | Mahler Measure | Polynomial                                                     | Coefficient Vector                              |
  ;;         |----------------+----------------------------------------------------------------+-------------------------------------------------|
  ;;         |   1.1762808182 | x^10+x^9-x^7-x^6-x^5-x^4-x^3+x+1                               | #(1 1 0 -1 -1 -1 -1 -1 0 1 1)                   |
  ;;         |   1.1883681475 | x^18+x^17+x^16+x^15-x^12-x^11-x^10-x^9-x^8-x^7-x^6+x^3+x^2+x+1 | #(1 1 1 1 0 0 -1 -1 -1 -1 -1 -1 -1 0 0 1 1 1 1) |
  ;;         |   1.2163916611 | x^10-x^6+x^5-x^4+1                                             | #(1 0 0 0 -1 1 -1 0 0 0 1)                      |
  ;;         |   1.2277855586 | x^12+x^11+x^10-x^8-x^7-x^6-x^5-x^4+x^2+x+1                     | #(1 1 1 0 -1 -1 -1 -1 -1 0 1 1 1)               |
  ;;         |   1.2303914344 | x^10+x^7+x^5+x^3+1                                             | #(1 0 0 1 0 1 0 1 0 0 1)                        |
  ;;         |   1.2612309611 | x^10-x^8+x^5-x^2+1                                             | #(1 0 -1 0 0 1 0 0 -1 0 1)                      |
  ;;         |   1.2672338594 | x^10+x^8+x^7+x^5+x^3+x^2+1                                     | #(1 0 1 1 0 1 0 1 1 0 1)                        |
  ;;         |   1.2806381562 | x^8+x^5-x^4+x^3+1                                              | #(1 0 0 1 -1 1 0 0 1)                           |
  ;;         |   1.2835823606 | x^10+x^9-x^5+x+1                                               | #(1 1 0 0 0 -1 0 0 0 1 1)                       |
  ;;         |   1.2934859531 | x^10-x^8+x^7-x^5+x^3-x^2+1                                     | #(1 0 -1 1 0 -1 0 1 -1 0 1)                     |
  ;;         |----------------+----------------------------------------------------------------+-------------------------------------------------|
  ;;
  ;; For Lehmer's polynomial:
  ;;  (i) expand(xreduce("*", map(lambda([y],max(1,rhs(abs(y)))), allroots(x^10+x^9-x^7-x^6-x^5-x^4-x^3+x+1=0))));
  ;;  (o) 1.176280818259936

  (loop for m in '(1.1762808182 1.1883681475 1.2163916611 1.2277855586 1.2303914344
                   1.2612309611 1.2672338594 1.2806381562 1.2835823606
                   1.2934859531)
        for p in '(#(1 1 0 -1 -1 -1 -1 -1 0 1 1) #(1 1 1 1 0 0 -1 -1 -1 -1 -1 -1 -1 0 0 1 1 1 1) #(1 0 0 0 -1 1 -1 0 0 0 1)
                   #(1 1 1 0 -1 -1 -1 -1 -1 0 1 1 1) #(1 0 0 1 0 1 0 1 0 0 1) #(1 0 -1 0 0 1 0 0 -1 0 1)
                   #(1 0 1 1 0 1 0 1 1 0 1) #(1 0 0 1 -1 1 0 0 1) #(1 1 0 0 0 -1 0 0 0 1 1) #(1 0 -1 1 0 -1 0 1 -1 0 1))
        for cm = (loop for i from 1 upto 3
                       for ccm = (mjr_poly_mahler-measure p :solve #'mjr_poly_root-solve-search-deflate)
                       when ccm
                       collect ccm)
        do (if cm
               (loop for ccm in cm
                     do (assert-equality (mjr_eps_make-fixed= 1.0d-3) ccm m p))
               (assert-true nil (format nil "Failed for ~a" p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
;; '(mjr_poly_discriminant-high-degree)
 )
