;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-matt.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-matt.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2004,2010,2012,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_MATT-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_MAT :MJR_EPS :MJR_VEC :MJR_PRNG :MJR_MATT))

(in-package :MJR_MATT-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_matt_make-test
  ;; Make sure the example in Burkardt's test matrix library matches
  (assert-equalp #2A((3 1 1 1 1)(1 3 1 1 1)(1 1 3 1 1)(1 1 1 3 1)(1 1 1 1 3))                                                     (mjr_matt_make-test :mp-pei         :n 5 :b 2))
  (assert-equalp #2A((1/9 1/7 1/5 1/3 1)(1/7 1/5 1/3 1 -1)(1/5 1/3 1 -1 -1/3)(1/3 1 -1 -1/3 -1/5)(1 -1 -1/3 -1/5 -1/7))           (mjr_matt_make-test :mp-ris         :n 5))
  (assert-equalp #2A((1 1/2 1/3 1/4 1/5)(1/2 1 2/3 1/2 2/5)(1/3 2/3 1 3/4 3/5)(1/4 1/2 3/4 1 4/5)(1/5 2/5 3/5 4/5 1))             (mjr_matt_make-test :mp-westlake    :n 5))
  (assert-equalp #2A((1 1 1 1 1)(1 2 3 4 5)(1 3 6 10 15)(1 4 10 20 35)(1 5 15 35 70))                                             (mjr_matt_make-test :mp-pascal      :n 5))
  (assert-equalp #2A((1/2 1/3 1/4 1/5 1/6)(1/3 1/4 1/5 1/6 1/7)(1/4 1/5 1/6 1/7 1/8)(1/5 1/6 1/7 1/8 1/9)(1/6 1/7 1/8 1/9 1/10))  (mjr_matt_make-test :mp-cauchy      :n 5))
  (assert-equalp #2A((1 1 1 1 1) (1 2 4 8 16) (1 3 9 27 81) (1 4 16 64 256) (1 5 25 125 625))                                     (mjr_matt_make-test :mp-vandermonde :n 5))
  (assert-equalp #2A((1 1 1 1 1) (1 2 2 2 2) (1 2 3 3 3) (1 2 3 4 4) (1 2 3 4 5))                                                 (mjr_matt_make-test :mp-minij       :n 5))
  (assert-equalp #2A((2 -2 -2/3 -2/5 -2/7)(2/3 2 -2 -2/3 -2/5)(2/5 2/3 2 -2 -2/3)(2/7 2/5 2/3 2 -2)(2/9 2/7 2/5 2/3 2))           (mjr_matt_make-test :mp-parter      :n 5))
  (assert-equalp #2A((1/1 1/2 1/3 1/4 1/5)(1/2 1/3 1/4 1/5 1/6)(1/3 1/4 1/5 1/6 1/7)(1/4 1/5 1/6 1/7 1/8)(1/5 1/6 1/7 1/8 1/9))   (mjr_matt_make-test :mp-hilbert     :n 5))
  (assert-equalp #2A((1 2 3 4 5) (2 3 4 5 0) (3 4 5 0 0) (4 5 0 0 0) (5 0 0 0 0))                                                 (mjr_matt_make-test :mp-hankel      :n 5))
  (assert-equalp #2A((1 2 3 4 5) (2 1 2 3 4) (3 2 1 2 3) (4 3 2 1 2) (5 4 3 2 1))                                                 (mjr_matt_make-test :mp-toeplitz    :n 5))
  ;; Several tests in this loop
  (loop for n from 2 upto 20
        for x = (mjr_vec_make-seq :start n :end 1 :step -1)
        for y = (mjr_vec_make-seq :start n :end 1 :step -1)
        for b = (/ (mjr_prng_int-co 1 10) (mjr_prng_int-co 1 10))
        ;; Make sure the *-i versions match what we get from mjr_mat_inv.
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-cauchy      :n n))      (mjr_matt_make-test :mp-cauchy-i      :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-hilbert     :n n))      (mjr_matt_make-test :mp-hilbert-i     :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-minij       :n n))      (mjr_matt_make-test :mp-minij-i       :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-parter      :n n))      (mjr_matt_make-test :mp-parter-i      :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-ris         :n n))      (mjr_matt_make-test :mp-ris-i         :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-westlake    :n n))      (mjr_matt_make-test :mp-westlake-i    :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-vandermonde :n n))      (mjr_matt_make-test :mp-vandermonde-i :n n))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-vandermonde :x x))      (mjr_matt_make-test :mp-vandermonde-i :x x))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-cauchy      :x x))      (mjr_matt_make-test :mp-cauchy-i      :x x))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-cauchy      :x x :y y)) (mjr_matt_make-test :mp-cauchy-i      :x x :y y))
        do (assert-equalp (mjr_mat_inv (mjr_matt_make-test :mp-pei         :n n :b b)) (mjr_matt_make-test :mp-pei-i         :n n :b b))
        ;; Make sure the *-i versions are really inverses. :)
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-cauchy      :n n)      (mjr_matt_make-test :mp-cauchy-i      :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-hilbert     :n n)      (mjr_matt_make-test :mp-hilbert-i     :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-minij       :n n)      (mjr_matt_make-test :mp-minij-i       :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-parter      :n n)      (mjr_matt_make-test :mp-parter-i      :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-ris         :n n)      (mjr_matt_make-test :mp-ris-i         :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-westlake    :n n)      (mjr_matt_make-test :mp-westlake-i    :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-vandermonde :n n)      (mjr_matt_make-test :mp-vandermonde-i :n n))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-vandermonde :x x)      (mjr_matt_make-test :mp-vandermonde-i :x x))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-cauchy      :x x)      (mjr_matt_make-test :mp-cauchy-i      :x x))      :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-cauchy      :x x :y y) (mjr_matt_make-test :mp-cauchy-i      :x x :y y)) :mp-identity))
        do (assert-true (mjr_mat_test-property-struct (mjr_mat_* (mjr_matt_make-test :mp-pei         :n n :b b) (mjr_matt_make-test :mp-pei-i         :n n :b b)) :mp-identity))
        ;; Make sure various matrices have the properties we think they have
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-pascal    :n n)      :mp-symmetric :mp-integer))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-hankel    :n n)      :mp-hankel))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-hilbert   :n n)      :mp-hankel :mp-symmetric))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-minij     :n n)      :mp-integer :mp-symmetric))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-parter    :n n)      :mp-persymmetric :mp-toeplitz))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-ris       :n n)      :mp-hankel :mp-symmetric))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-westlake  :n n)      :mp-symmetric))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-toeplitz  :n n)      :mp-toeplitz))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-hankel    :x x)      :mp-hankel))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-toeplitz  :x x)      :mp-toeplitz))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-hankel    :x x :y y) :mp-hankel))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-toeplitz  :x x :y y) :mp-toeplitz))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-pei       :n n :b b) :mp-persymmetric :mp-symmetric))
        do (assert-true (mjr_mat_test-property-struct (mjr_matt_make-test :mp-clement   :n n)      :mp-diag-zero :mp-tri-diagonal))
        ;; Make sure det matches
        do (assert-equalp (mjr_matt_det-test :mp-cauchy        :n n)      (mjr_mat_det (mjr_matt_make-test :mp-cauchy      :n n)))       ;; Busted for n=1
        do (assert-equalp (mjr_matt_det-test :mp-hankel        :n n)      (mjr_mat_det (mjr_matt_make-test :mp-hankel      :n n)))
        do (assert-equalp (mjr_matt_det-test :mp-pascal        :n n)      (mjr_mat_det (mjr_matt_make-test :mp-pascal      :n n)))
        do (assert-equalp (mjr_matt_det-test :mp-hilbert       :n n)      (mjr_mat_det (mjr_matt_make-test :mp-hilbert     :n n)))
        do (assert-equalp (mjr_matt_det-test :mp-vandermonde   :n n)      (mjr_mat_det (mjr_matt_make-test :mp-vandermonde :n n)))       ;; Busted for n=1
        do (assert-equalp (mjr_matt_det-test :mp-cauchy        :x x)      (mjr_mat_det (mjr_matt_make-test :mp-cauchy      :x x)))       ;; Busted for n=1
        do (assert-equalp (mjr_matt_det-test :mp-vandermonde   :x x)      (mjr_mat_det (mjr_matt_make-test :mp-vandermonde :x x)))       ;; Busted for n=1
        do (assert-equalp (mjr_matt_det-test :mp-cauchy        :x x :y y) (mjr_mat_det (mjr_matt_make-test :mp-cauchy      :x x :y y)))  ;; Busted for n=1
        do (assert-equalp (mjr_matt_det-test :mp-pei           :n n :b b) (mjr_mat_det (mjr_matt_make-test :mp-pei         :n n :b b)))
        do (assert-equalp (mjr_matt_det-test :mp-clement       :n n)      (mjr_mat_det (mjr_matt_make-test :mp-clement     :n n)))

        ;; Make sure special cases of Cauchy are reproducible with :mp-cauchy
        do (assert-equalp (mjr_matt_make-test :mp-parter :n n)
                          (mjr_matt_make-test :mp-cauchy
                                                :x (mjr_vec_+ 1/2 (mjr_vec_make-seq :start 1 :end n))
                                                :y (mjr_vec_- (mjr_vec_make-seq :start 1 :end n))))
        do (assert-equalp (mjr_matt_make-test :mp-hilbert :n n)
                          (mjr_matt_make-test :mp-cauchy
                                                :x (mjr_vec_- (mjr_vec_make-seq :start 1 :end n) 1)
                                                :y (mjr_vec_make-seq :start 1 :end n)))

        do (assert-equalp (mjr_matt_make-test :mp-ris :n n)
                          (mjr_matt_make-test :mp-cauchy
                                                :x (mjr_vec_+ (+ 3 (* 2 n)) (mjr_vec_* -2 (mjr_vec_make-seq :start 1 :end n)))
                                                :y (mjr_vec_* -2 (mjr_vec_make-seq :start 1 :end n))))
        )

  ; Transformation matrices have some special properties...
  (dotimes (i 100)
    (let* ((lx (mjr_prng_int-co 3 20))
           (x  (mjr_vec_ewbo (mjr_prng_vector lx #'mjr_prng_int-co 0 10)
                             (mjr_prng_vector lx #'mjr_prng_int-co 0 (if (evenp i) 1 10))
                             #'complex))
           (n  (mjr_prng_random lx))
           (m  (loop for r = (mjr_prng_random lx)
                     when (not (= n r))
                     do (return r)))
           (hm  (mjr_matt_make-test :mp-householder   :x x :n n))
           (h2  (mjr_matt_make-test :mp-householder-2 :x x :n n :m m))
           (g2  (mjr_matt_make-test :mp-givens-2      :x x :n n :m m))
           (k   (+ (max m n) (mjr_prng_int-co 1 4)))
           (e1  (mjr_matt_make-test :mp-gauss-1            :n n :m m :k k))
           (f   (if (evenp i) (mjr_prng_int-co 1 10) (complex (mjr_prng_int-co 1 10) (mjr_prng_int-co 1 10))))
           (e2  (mjr_matt_make-test :mp-gauss-2            :n n      :k k :a f))
           (e3  (mjr_matt_make-test :mp-gauss-3            :n n :m m :k k :a f))
           )
      (assert-true (mjr_mat_test-property-struct hm :mp-hermitian))        ;; Householder are hermitian
      (assert-true (mjr_mat_test-property-math hm :mp-unitary))            ;; Householder are unitary
      (if (evenp i)
          (progn
            (assert-true (mjr_mat_test-property-struct hm :mp-symmetric))  ;; Householder are hermitian
            (assert-true (mjr_mat_test-property-math hm :mp-orthogonal)))) ;; Householder are orthogonal
      (assert-equality (mjr_eps_make-fixed= 0.00001) -1 (mjr_mat_det hm))  ;; Householder are det=-1
      (assert-equality (mjr_eps_make-fixed= 0.00001)  1 (mjr_mat_det g2))  ;; Givens are det=1
      (assert-equality (mjr_eps_make-fixed= 0.00001) -1 (mjr_mat_det h2))  ;; Householder are det=-1
      (assert-equality (mjr_eps_make-fixed= 0.00001) -1 (mjr_mat_det e1))  ;; Gauss type 1 are det=-1
      (assert-equality (mjr_eps_make-fixed= 0.00001)  f (mjr_mat_det e2))  ;; Gauss type 2 are det=f
      (assert-equality (mjr_eps_make-fixed= 0.00001)  1 (mjr_mat_det e3))  ;; Gauss type 3 are det=1
      ))
  )

(define-test mjr_matt_make-random
  ;; Verify matrix size and structure for 50 random cases
  (dotimes (i 50)
    (let* ((r (mjr_prng_int-co 1 20))
           (c (mjr_prng_int-co 1 20)))
      (dolist (the-mp '(
                        :mp-real
                        :mp-diagonal
                        :mp-l-triangular  
                        :mp-u-triangular  
                        :mp-complex       
                        :mp-rational      
                        ))
        (let ((m (mjr_matt_make-random the-mp :m r :n c :a 1 :b 10)))
          (assert-true (mjr_mat_test-property-struct m the-mp) (list the-mp r c m))
          (assert-equal r (mjr_mat_rows m) (list the-mp r c m))
          (assert-equal c (mjr_mat_cols m) (list the-mp r c m))))))

  ;; Verify matrix size and structure for 50 random cases
  (dotimes (i 50)
    (let* ((r (mjr_prng_int-co 1 20))
           (c (mjr_prng_int-co 1 20))
           (s (max r c)))
      (dolist (the-mp '(:mp-symmetric :mp-hermitian))
        (let ((m (mjr_matt_make-random the-mp :m r :n c :a 1 :b 10)))
          (assert-true (mjr_mat_test-property-struct m the-mp) (list the-mp r c m))
          (assert-equal s (mjr_mat_rows m) (list the-mp r c m))
          (assert-equal s (mjr_mat_cols m) (list the-mp r c m))))))
  
  ;; Verify matrix size and structure for 50 random cases
  (dotimes (i 50)
    (let* ((r (mjr_prng_int-co 1 20))
           (c (mjr_prng_int-co 1 20))
           (m (mjr_matt_make-random :mp-m-diagonal :m r :n c :a 1 :b 10 :k 1)))
          (assert-true (mjr_mat_test-property-struct m :mp-tri-diagonal) (list :mp-m-diagonal r c m))
          (assert-equal r (mjr_mat_rows m) (list :mp-m-diagonal r c m))
          (assert-equal c (mjr_mat_cols m) (list :mp-m-diagonal r c m))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
;; '(mjr_matt_make-random)
 )
