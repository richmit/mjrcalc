;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-perm.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-perm.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PERM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PERM :MJR_PRNG))

(in-package :MJR_PERM-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_oddp-naive (perm)
  "Return non-nil if the permutation is odd, or nil if it is not (or is invalid).
See MJR_PERM_CHECK-PERM for more information about invalid permutations."
  (let ((num-inversions (mjr_perm_swapping-number perm)))
    (and num-inversions (oddp num-inversions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_evenp-naive (perm)
  "Return non-nil if the permutation is even, or nil if it is not (or is invalid).
See MJR_PERM_CHECK-PERM for more information about invalid permutations."
  (let ((num-inversions (mjr_perm_swapping-number perm)))
    (and num-inversions (evenp num-inversions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_sgn-naive (perm)
  "Return the signature (or sign) of the permutation (-1 if odd, 1 if even, nil if invalid).
See MJR_PERM_CHECK-PERM for more information about invalid permutations."
  (let ((num-inversions (mjr_perm_swapping-number perm)))
    (if num-inversions
        (if (evenp num-inversions) 1 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_order-naive (perm)
  "Return the order (group theory wise) of the perm.  No error checking is performed."
  (loop for order from 1
        for nperm = (mjr_perm_* perm perm) then (mjr_perm_* nperm perm)
        when (equalp perm nperm)
        do (return order)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_swapping-number-naive (perm)
  "Return the number of inversions, or nil if the permutation is invalid.

See MJR_PERM_CHECK-PERM for more information about invalid permutations."
  (if (vectorp perm)
      (let* ((len     (length perm))
             (idx-cnt (make-array len :initial-element 0))
             (p-cpy   (make-array len :initial-element 0))
             (p-inv   (make-array len :initial-element 0)))
        (if (loop for i from 0 ; Check the perm, and compute inverse
                  for j across perm
                  when (not (integerp j))              do (return nil)
                  when (or (< j 0) (> j (1- len)))     do (return nil)
                  do (incf (aref idx-cnt j))
                  when (< 1 (mjr_perm_eval idx-cnt j)) do (return nil)
                  do (setf (aref p-inv j) i
                           (aref p-cpy i) j)
                  finally (return 't))
            (let ((the-count 0))
              (loop for i from 0 upto (1- len)
                    for j = (mjr_perm_eval p-cpy i)
                    for k = (mjr_perm_eval p-inv i)
                    when (not (= i j))
                    do (progn (incf the-count)
                              (rotatef (aref p-cpy i) (aref p-cpy k))
                              (rotatef (aref p-inv i) (aref p-inv j)))
                    finally (return the-count)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_cycle-structure-lengths
  (assert-equal '(1 1 1) (mjr_perm_cycle-structure-lengths #(0 1 2)))
  (assert-equal '(1 2)   (mjr_perm_cycle-structure-lengths #(0 2 1)))
  (assert-equal '(2 1)   (mjr_perm_cycle-structure-lengths #(1 0 2)))
  (assert-equal '(3)     (mjr_perm_cycle-structure-lengths #(1 2 0)))
  (assert-equal '(3)     (mjr_perm_cycle-structure-lengths #(2 0 1)))
  (assert-equal '(2 1)   (mjr_perm_cycle-structure-lengths #(2 1 0)))
  (assert-equal '(2 3)   (mjr_perm_cycle-structure-lengths (map 'vector #'1- #(3 4 1 5 2))))
  (assert-equal '(2 2)   (mjr_perm_cycle-structure-lengths (map 'vector #'1- #(3 4 1 2))))
  (assert-equal '(3 1)   (mjr_perm_cycle-structure-lengths #(1 2 0 3)))
  ;; The sum of the lengths should be the the same as the length
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equalp len (apply #'+ (mjr_perm_cycle-structure-lengths p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_cycle-structure-list
  (assert-equal '((0)(1)(2))    (mjr_perm_cycle-structure-list #(0 1 2)))
  (assert-equal '((0)(1 2))     (mjr_perm_cycle-structure-list #(0 2 1)))
  (assert-equal '((0 1)(2))     (mjr_perm_cycle-structure-list #(1 0 2)))
  (assert-equal '((0 1 2))      (mjr_perm_cycle-structure-list #(1 2 0)))
  (assert-equal '((0 2 1))      (mjr_perm_cycle-structure-list #(2 0 1)))
  (assert-equal '((0 2)(1))     (mjr_perm_cycle-structure-list #(2 1 0)))
  (assert-equal '((0 2)(1 3 4)) (mjr_perm_cycle-structure-list (map 'vector #'1- #(3 4 1 5 2))))
  (assert-equal '((0 2)(1 3))   (mjr_perm_cycle-structure-list (map 'vector #'1- #(3 4 1 2))))
  (assert-equal '((0 1 2)(3))   (mjr_perm_cycle-structure-list #(1 2 0 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_make-identity
      (assert-equalp #(0)         (mjr_perm_make-identity 1))
      (assert-equalp #(0 1)       (mjr_perm_make-identity 2))
      (assert-equalp #(0 1 2)     (mjr_perm_make-identity 3))
      (assert-equalp #(0 1 2 3)   (mjr_perm_make-identity 4))
      (assert-equalp #(0 1 2 3 4) (mjr_perm_make-identity 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_make-random
    (dotimes (i 20)
      (let* ((len (mjr_prng_int-co 1 20))
             (p   (mjr_perm_make-random len)))
        (assert-true (vectorp p))
        (assert-true (= len (length p)))
        (assert-true (mjr_perm_check-perm p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_swapping-number
  ;; Make sure the -naive version matches the production one.
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (mjr_perm_swapping-number p) (mjr_perm_swapping-number-naive p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_evenp
  ;; Make sure the -naive version matches the production one.
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (mjr_perm_evenp p) (mjr_perm_evenp-naive p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_oddp
  ;; Make sure the -naive version matches the production one.
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (mjr_perm_oddp p) (mjr_perm_oddp-naive p) p)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_xxx_oddp+evenp
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (not (mjr_perm_oddp p)) (mjr_perm_evenp p))
      (assert-equal (not (mjr_perm_oddp-naive p)) (mjr_perm_evenp-naive p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_sgn
  ;; Make sure the -naive version matches the production one.
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (mjr_perm_sgn p) (mjr_perm_sgn-naive p) p)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_order
  ;; Make sure the -naive version matches the production one.
  (dotimes (i 20)
    (let* ((len (mjr_prng_int-co 1 20))
           (p   (mjr_perm_make-random len)))
      (assert-equal (mjr_perm_order p) (mjr_perm_order-naive p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_check-perm
  ;; Normal perms
  (assert-true  (mjr_perm_check-perm #(2 3 0 1)))
  (assert-true  (mjr_perm_check-perm #(1 2 0 3)))
  (assert-true  (mjr_perm_check-perm #(1 0 2)))
  (assert-true  (mjr_perm_check-perm #(0)))
  ;; Value used twice
  (assert-false (mjr_perm_check-perm #(2 2 0 1)))
  ;; Value too large
  (assert-false (mjr_perm_check-perm #(1 2 4 3)))
  (assert-false (mjr_perm_check-perm #(1)))
  ;; Value too small
  (assert-false (mjr_perm_check-perm #(0 1 2 -1)))
  ;; Empty
  (assert-false (mjr_perm_check-perm #()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_*
  ;; Order one example
  (assert-equalp #(0 1 2 3) (mjr_perm_* #(2 3 0 1)                   #(2 3 0 1)))
  ;; Multiply by the inverse and get the identity
  (assert-equalp #(0 1 2 3) (mjr_perm_* #(2 0 1 3)                   #(1 2 0 3)))
  (assert-equalp #(0 1 2 3) (mjr_perm_* #(1 2 0 3)                   #(2 0 1 3)))
  ;; Typical example
  (assert-equalp #(3 0 2 1) (mjr_perm_* #(2 3 0 1)                   #(1 2 0 3)))
  (assert-equalp #(0 3 1 2) (mjr_perm_* #(1 2 0 3)                   #(2 3 0 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_/
  (assert-equalp #(2 3 0 1) (mjr_perm_/ #(2 3 0 1)))
  (assert-equalp #(2 0 1 3) (mjr_perm_/ #(1 2 0 3)))
  (assert-equalp #(1 0 2)   (mjr_perm_/ #(1 0 2)))
  (assert-equalp #(0)       (mjr_perm_/ #(0)))
  (assert-equalp #(0)       (mjr_perm_/ #(0) #(0)))
  ;; Make sure we always get the identity
  (dotimes (i 20)
    (let* ((len  (mjr_prng_int-co 1 20))
           (p    (mjr_perm_make-random len)))
      (assert-equalp (mjr_perm_make-identity len) (mjr_perm_/ p p))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_eval
  (assert-equal 2      (mjr_perm_eval #(2 3 0 1)                    0))
  (assert-equal 3      (mjr_perm_eval (map 'vector #'1+ #(2 3 0 1)) 1 :index-base 1))
  ;; Errors
  ;; No error test cases as this function has no error checking!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_apply-to-array2d
  (assert-equalp #2a((4 5 6)(1 2 3)(7 8 9))  (mjr_perm_apply-to-array2d #(1 0 2) #2a((1 2 3)(4 5 6)(7 8 9)) ))
  (assert-equalp #2a((4 5 6)(1 2 3))         (mjr_perm_apply-to-array2d #(1 0)   #2a((1 2 3)(4 5 6))        ))
  (assert-equalp #2a((4 5)(1 2)(7 8))        (mjr_perm_apply-to-array2d #(1 0 2) #2a((1 2)(4 5)(7 8))       ))
  (assert-equalp #2a((1 2 3))                (mjr_perm_apply-to-array2d #(0)     #2a((1 2 3))               ))
  (assert-equalp #2a((2)(1)(3))              (mjr_perm_apply-to-array2d #(1 0 2) #2a((1)(2)(3))             ))
  ;; Perm cols
  (assert-equalp #2a((2 1 3)(5 4 6)(8 7 9))  (mjr_perm_apply-to-array2d #(1 0 2) #2a((1 2 3)(4 5 6)(7 8 9)) 't))
  (assert-equalp #2a((2 1 3)(5 4 6))         (mjr_perm_apply-to-array2d #(1 0 2) #2a((1 2 3)(4 5 6))        't))
  (assert-equalp #2a((2 1)(5 4)(8 7))        (mjr_perm_apply-to-array2d #(1 0)   #2a((1 2)(4 5)(7 8))       't))
  (assert-equalp #2a((2 1 3))                (mjr_perm_apply-to-array2d #(1 0 2) #2a((1 2 3))               't))
  (assert-equalp #2a((1)(2)(3))              (mjr_perm_apply-to-array2d #(0)     #2a((1)(2)(3))             't))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_perm_apply-to-sequence
  (assert-equalp #(0 3 1 2) (mjr_perm_apply-to-sequence #(2 3 0 1)                   #(1 2 0 3)))
  (assert-equalp #(3 0 2 1) (mjr_perm_apply-to-sequence #(1 2 0 3)                   #(2 3 0 1)))
  ;; Multiplication (in reverse is the same as applying a perm to a vector.
  (dotimes (i 20)
    (let* ((len  (mjr_prng_int-co 1 20))
           (a    (mjr_perm_make-random len))
           (b    (mjr_perm_make-random len)))
      (assert-equalp (mjr_perm_* b a) (mjr_perm_apply-to-sequence a b))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 '(
   ;; mjr_perm_cycle-structure-lengths
   ;; mjr_perm_cycle-structure-list
   ;; mjr_perm_make-identity
   ;; mjr_perm_make-random
   ;; mjr_perm_swapping-number
   ;; mjr_perm_evenp
   ;; mjr_perm_oddp
   ;; mjr_perm_xxx_oddp+evenp
   ;; mjr_perm_sgn
   ;; mjr_perm_order
   ;; mjr_perm_check-perm
   ;; mjr_perm_*
   ;; mjr_perm_/
   mjr_perm_eval
   ;; mjr_perm_apply-to-array2d
   ;; mjr_perm_apply-to-sequence
   )
)
