;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-mat.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-mat.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1995-2010,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_MAT-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_MAT :MJR_VEC :MJR_PRNG :MJR_MATT :MJR_ARR :MJR_EPS))

(in-package :MJR_MAT-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det-ge-naive1 (matrix &key show-progress pivot-row)
  "Return the determinant of a matrix or NIL if the matrix is not square or empty.
This implementation is pretty, but slow.  It is a good test for MJR_MAT_ELIMINATE. :)"
   (if (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
       (multiple-value-bind (newmat row-perm am tmm deti) (mjr_mat_eliminate matrix
                                                                            :full-elim 't :unitize-pivot 't :sort-echelon :pivot-diag
                                                                            :pivot-row pivot-row :show-progress show-progress)
         (declare (ignore row-perm am tmm))
         (if (mjr_mat_test-property-struct newmat :mp-identity)
             (/ deti)
             0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det-ge-naive2 (matrix &key show-progress pivot-row elim-method)
  "Return the determinant of a matrix or NIL if the matrix is not square or empty.
This implementation is pretty, but slow.  It is a good test for MJR_MAT_ELIMINATE. :)"
   (if (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
       (multiple-value-bind (newmat row-perm am tmm deti) (mjr_mat_eliminate matrix
                                                                            :full-elim nil :unitize-pivot nil :sort-echelon :pivot-diag
                                                                            :pivot-row pivot-row :show-progress show-progress
                                                                            :elim-method elim-method)
         (declare (ignore row-perm am tmm))
         (if (mjr_mat_test-property-struct newmat :mp-u-triangular)
             (* deti (mjr_mat_diag-prod newmat)) ; deti will be 1 or -1 in this case
             0))))

(defvar m1i ) (setq m1i  #2a((2)))                                                             ; integer, invertible, square, 1x1
(defvar m1s ) (setq m1s  #2a((0)))                                                             ; integer, invertible, square, 1x1
(defvar m5  ) (setq m5   #2a((1 2 3 4 4)(4 5 6 7 3)(7 8 9 8 2)(9 8 7 6 1)(3 9 1 3 9)))         ; integer, invertible, square, 5x5, det: -700, cpoly: #(1 -30 69 846 -806 700)
(defvar m4  ) (setq m4   #2a((1 2 3 4)(4 5 6 7)(7 8 9 8)(9 8 6 5)))                            ; integer, invertible, square, 4x4, det: 6
(defvar m   ) (setq m    #2a((1 2 3)(4 5 6)(7 8 9)))                                           ; integer, singular, square, 3x3. cpoly: #(1 -15 -18 0)
(defvar mf  ) (setq mf   #2a((1.0 2.0 3.0)(4.0 5.0 6.0)(7.0 8.0 9.0)))                         ; floating point, singular, square, 3x3
(defvar mi  ) (setq mi   #2a((1 2 3)(4 5 6)(7 8 8)))                                           ; integer, invertible, square, 3x3. cpoly: #(1 -14 -24 -3)
(defvar em  ) (setq em   #2a())                                                                ; empty matrix
(defvar wm  ) (setq wm   #2a((1 2 3)(4 5 6)))                                                  ; integer, wide matrix, 2x3
(defvar tm  ) (setq tm   #2a((1 2)(4 5)(7 8)))                                                 ; integer, tall matrix, 3x2
(defvar mtm ) (setq mtm  #2A((1 2)(3 4)(5 6)(7 8)(9 0)))                                       ; integer, medium tall matrix, 4x2
(defvar vwm ) (setq vwm  #2a((1 2 3 4 5 6 7 8 9)))                                             ; integer, very wide matrix, row-vector, 1x9
(defvar vtm ) (setq vtm  #2a((1)(2)(3)(4)(5)(6)(7)(8)(9)))                                     ; integer, very tall matrix, row-vector, 9x1
(defvar v3  ) (setq v3   #(1 2 3))                                                             ; integer, 3-vector
(defvar mv3 ) (setq mv3  #2a((1)(2)(3)))                                                       ; integer, 3-vector
(defvar u3  ) (setq u3   #(4 5 6))                                                             ; integer, 3-vector
(defvar v2  ) (setq v2   #(1 2))                                                               ; integer, 2-vector
(defvar c23 ) (setq c23  #2A((#C(0 0) #C(0 1) #C(0 2)) (#C(1 0) #C(1 1) #C(1 2))))             ; integer Complex, 2x3
(defvar c33 ) (setq c33  #2A((#C(0 0) #C(0 1) #C(0 2)) (#C(1 0) #C(1 1) #C(1 2))               ; integer Complex, 3x3
                             (#C(2 0) #C(2 1) #C(2 2))))
(defvar cf23) (setq cf23 #2A((#C(0.0 0.0) #C(0.0 1.0) #C(0.0 2.0))                             ; Floating point, Complex 2x3
                             (#C(1.0 0.0) #C(1.0 1.0) #C(1.0 2.0))))
(defvar v9  ) (setq v9   #(1 2 3 4 5 6 7 8 9))                                                 ; integer, vector, 9 elements
(defvar L9  ) (setq L9   '(1 2 3 4 5 6 7 8 9))                                                 ; list, 9 elements
(defvar tpm ) (setq tpm  #2A((1 2 3) (4 5 6) (7 8 9) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0))) ; integer, Tall padded matrix 9x3

(defvar ncp3) (setq ncp3 #2a((2 -1 1)(-1 2 1)(1 -1 2)))                                        ; cpoly: #(1 -6 11 -6)
(defvar ncp4) (setq ncp4 #2a((8 -1 3 -1)(-1 6 2 0)(3 2 9 1)(-1 0 1 7)))                        ; cpoly: #(1 -30 319 -1410 2138)
(defvar ncp5) (setq ncp5 #2a((2 1 0 0 0)(1 2 1 0 0)(0 1 2 1 0)(0 0 1 2 1)(0 0 0 1 2)))         ; cpoly: #(1 -10 36 -56 35 -6)

; Robert Gregory, David Karney (1969); A Collection of Matrices for Testing Computational Algorithms.
(defvar aCoMfTCA3-2)   (setq aCoMfTCA3-2   #2a((1 -2 3 1)(-2 1 -2 -1)(3 -2 1 5)(1 -1 5 3)))
(defvar aCoMfTCA3-1a)  (setq aCoMfTCA3-1a  #2A((-29/3 -8/3 -32) (8 5/2 51/2) (8/3 2/3 9)))
(defvar aCoMfTCA3-1b)  (setq aCoMfTCA3-1b  #2a((33 16 72)(-24 -10 -57)(-8 -4 -17))) ; det: 6
(defvar aCoMfTCA3-3a)  (setq aCoMfTCA3-3a  #2A((#C(10 1) #C(-2 6) #C(-3 -2))(#C(9 -3) #C(0 8) #C(-3 -2))(#C(-2 2) #C(-1 -2) #C(1 0)))) ; det:1
(defvar aCoMfTCA3-3b)  (setq aCoMfTCA3-3b  #2a((#C(1 0) #C(1 2) #C(2 10))(#c(1 1) #C(0 3) #C(-5 14))(#C(1 1) #C(0 5) #C(-8 20))))
(defvar aCoMfTCA3-5a)  (setq aCoMfTCA3-5a  #2A((68 -41 -17 10)(-41 25 10 -6)(-17 10 5 -3)(10 -6 -3 2))) ; det=1
(defvar aCoMfTCA3-5b)  (setq aCoMfTCA3-5b  #2a((5 7 6 5)(7 10 8 7)(6 8 10 9)(5 7 9 10)))
(defvar aCoMfTCA3-21)  (setq aCoMfTCA3-21  #2a((0 -1 1 -1)(1 -1 1 -1)(1 -1 0 0)(1 0 0 -1)))  ; Note that element (0,0) needs to be updated...
(defvar aCoMfTCA3-22A) (setq aCoMfTCA3-22A #2a((-73 78       24)(92       66 25)(-80 37       10)))
(defvar aCoMfTCA3-22B) (setq aCoMfTCA3-22B #2a((-73 78       24)(92       66 25)(-80 37 1001/100)))
(defvar aCoMfTCA3-22C) (setq aCoMfTCA3-22C #2a((-73 78       24)(9201/100 66 25)(-80 37       10)))
(defvar aCoMfTCA3-22D) (setq aCoMfTCA3-22D #2a((-73 7801/100 24)(92       66 25)(-80 37       10)))
(defvar aCoMfTCA3-4a)  (setq aCoMfTCA3-4a  #2A((1 0 0 0 0 1)(1 1 0 0 0 -1)(-1 1 1 0 0 1)(1 -1 1 1 0 -1)(-1 1 -1 1 1 1)(1 -1 1 -1 1 -1)))
(defvar aCoMfTCA3-4b)  (setq aCoMfTCA3-4b  #2A((1/2 1/4 -1/8 1/16 -1/32 1/32)
                                               (0 1/2 1/4 -1/8 1/16 -1/16)
                                               (0 0 1/2 1/4 -1/8 1/8)
                                               (0 0 0 1/2 1/4 -1/4)
                                               (0 0 0 0 1/2 1/2)
                                               (1/2 -1/4 1/8 -1/16 1/32 -1/32)))
(defvar aCoMfTCA5-1)  (setq aCoMfTCA5-1  #2a((33 16 72)(-24 -10 -57)(-8 -4 -17)))       ; eigenvalues: (1 2 3) -- same as aCoMfTCA3-1b
(defvar aCoMfTCA5-2)  (setq aCoMfTCA5-2  #2a((4 1 1)(2 4 1)(0 1 4)))                    ; eigenvalues: (3 3 6)
(defvar aCoMfTCA5-5)  (setq aCoMfTCA5-5  #2a((-2 2 2 2)(-3 3 2 2)(-2 0 4 2)(-1 0 0 5))) ; eigenvalues: (1 2 3 4)
(defvar aCoMfTCA5-25) (setq aCoMfTCA5-25 #2a((-1/3 1/6 0 9/2 -3 -1)
                                             (-4/3 2/3 0 9 -6 -2)
                                             (-4/3 -5/6 1 27/2 -9 -3)
                                             (-4/3 -5/6 -1 39/2 -12 -4)
                                             (-4/3 -5/6 -1 43/2 -13 -5)
                                             (-4/3 -5/6 -1 43/2 -10 -8)))               ; eigenvalues: (-3 -2 2 3/2 1 1/3), det: 6
(defvar rosser)       (setq rosser (mjr_mat_make-and-fill 8 8 '(611   196  -192   407    -8   -52   -49    29
                                                                196   899   113  -192   -71   -43    -8   -44
                                                               -192   113   899   196    61    49     8    52
                                                                407  -192   196   611     8    44    59   -23
                                                                 -8   -71    61     8   411  -599   208   208
                                                                -52   -43    49    44  -599   411   208   208
                                                                -49    -8     8    59   208   208    99  -911
                                                                 29   -44    52   -23   208   208  -911    99))) ; Rosser matrix (Example 4.10)

; William Knight, 1970; Remark on algorithm 343: Eigenvalues and eigenvectors of a real general matrix; Communications of the ACM
(defvar wk70-a #2a((-50 53 52 51)(-52 1 53 52)(-53 0 1 53)(-51 53 52 52))) ;; all 1
(defvar wk70-b #2a((-41 55 4 3 2 51)(-2 10 55 4 3 2)(-3 0 10 55 4 3)(-4 0 0 10 55 4)(-55 0 0 0 10 55)(-51 55 4 3 2 61))) ;; all 10

; J. Grad & M. A. Brebner 1968; Algorithm 343: Eigenvalues and Eigenvectors of a Real General Matrix; Communications of the ACM
(defvar gb68-b #2a((-2 1 1 1)(-7 -5 -2 -4)(0 -1 -3 -2)(-1 0 -1 0))) ; -4+i*2, -4-i*2, -1+sqrt(2), -1-sqrt(2)

; Richard L. Burden & J. Douglas Faires (2000); Numerical Analysis, ISBN: 0534382169
(defvar baf-a  #2a((4 1 -2 2)(1 2 0 1)(-2 0 3 -2)(2 1 -2 -1)))                           ; (tri-diag via householder example)
(defvar baf-q1 #2a((1 0 0 0)(0 -1/3 2/3 -2/3)(0 2/3 2/3 1/3)(0 -2/3 1/3 2/3)))
(defvar baf-q2 #2a((1 0 0 0)(0 1 0 0)(0 0 -3/5 -4/5)(0 0 -4/5 3/5)))
(defvar baf-t  #2A((4.0 -3 0 0)(-3 10/3 -5/3 0)(0 -5/3 -33/25 68/75)(0 0 68/75 149/75))) ; t=q2*q1*a*q1*q2

; wikipedia 2010; QR_decomposition page; A rational arithmetic QR example
(defvar qrt1-a)  (setf qrt1-a  #2A((12  -51    4)(      6     167  -68)(    -4     24   -41)))
(defvar qrt1-q)  (setf qrt1-q  #2A((6/7 3/7 -2/7)(-69/175 158/175 6/35)(58/175 -6/175 33/35)))
(defvar qrt1-qa) (setf qrt1-qa #2A((14   21  -14)(      0     175  -70)( 0          0   -35)))
; (assert-true (mjr_mat_test-property-struct (mjr_mat_- qrt1-qa (mjr_mat_* qrt1-q qrt1-a)) :mp-zero))
; (assert-true (mjr_mat_test-property-math qrt1-q :mp-orthogonal))
; (assert-true (mjr_mat_test-property-struct qrt1-qa :mp-u-triangular))
; (assert-equalp 1 (mjr_mat_det qrt1-q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rows
  (assert-equal 3 (mjr_mat_rows m))
  (assert-equal 2 (mjr_mat_rows wm))
  (assert-equal 3 (mjr_mat_rows tm))
  (assert-equal 1 (mjr_mat_rows vwm))
  (assert-equal 9 (mjr_mat_rows vtm))
  ;; Vectors are column matrices for this function
  (assert-equal 3 (mjr_mat_rows v3))
  (assert-equal 2 (mjr_mat_rows v2))
  ;; Empty case
  (assert-equal 0 (mjr_mat_rows em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cols
  (assert-equal 3 (mjr_mat_cols m))
  (assert-equal 3 (mjr_mat_cols wm))
  (assert-equal 2 (mjr_mat_cols tm))
  (assert-equal 9 (mjr_mat_cols vwm))
  (assert-equal 1 (mjr_mat_cols vtm))
  ;; Vectors are column matrices for this function
  (assert-equal 1 (mjr_mat_cols v3))
  (assert-equal 1 (mjr_mat_cols v2))
  ;; Empty case
  (assert-equal 0 (mjr_mat_cols em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_matp
  (assert-true  (mjr_mat_matp m))
  (assert-false (mjr_mat_matp v3))
  ;; MJR TODO NOTE mjr_mat_matp: Need to think about this next one.  Perhaps the empty array should not be a matrix...
  (assert-true  (mjr_mat_matp em))
  ;; With "vectors are a matrix" turned on.
  (assert-true  (mjr_mat_matp m  't))
  (assert-true  (mjr_mat_matp v3 't))
  ;; Non-arrays
  (assert-false (mjr_mat_matp 0))
  (assert-false (mjr_mat_matp nil))
  (assert-false (mjr_mat_matp (list 1 2)))
  (assert-false (mjr_mat_matp #C(1 2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-const
  (assert-equalp #2a((0 0)(0 0)(0 0))   (mjr_mat_make-const 3 2))
  (assert-equalp #2a((1 1 1)(1 1 1))    (mjr_mat_make-const 2 3 1))
  (assert-equalp em                     (mjr_mat_make-const 0 0 1))
  (assert-error 'error                  (mjr_mat_make-const -1 -1 1))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-zero
  (assert-equalp #2a((0 0)(0 0)(0 0))   (mjr_mat_make-zero 3 2))
  (assert-equalp #2a((0 0 0)(0 0 0))    (mjr_mat_make-zero 2 3))
  (assert-equalp em                     (mjr_mat_make-zero 0 0))
  (assert-error 'error                  (mjr_mat_make-zero -1 -1))
  ;; Matrix first arg
  (assert-equalp #2a((0 0)(0 0)(0 0))   (mjr_mat_make-zero tm))
  (assert-equalp #2a((0 0 0)(0 0 0))    (mjr_mat_make-zero wm))
  (assert-equalp #2a((0)(0)(0))         (mjr_mat_make-zero v3))
  ;; Matrix first arg and second arg (should ignore second arg)
  (assert-equalp #2a((0 0)(0 0)(0 0))   (mjr_mat_make-zero tm 100))
  (assert-equalp #2a((0 0 0)(0 0 0))    (mjr_mat_make-zero wm 100))
  (assert-equalp #2a((0)(0)(0))         (mjr_mat_make-zero v3 100))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cv2m
  (assert-equalp m               (mjr_mat_cv2m m))
  (assert-equalp wm              (mjr_mat_cv2m wm))
  (assert-equalp 1               (mjr_mat_cv2m 1))
  (assert-equalp (list 1 2 3)    (mjr_mat_cv2m (list 1 2 3)))
  (assert-equalp 't              (mjr_mat_cv2m 't))
  (assert-equalp #2a((1)(2)(3))  (mjr_mat_cv2m v3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_m2cv
  (assert-equalp #(1 2 3 4 5 6 7 8 9)  (mjr_mat_m2cv vtm))
  ;; No conversion cases
  (assert-equalp m                     (mjr_mat_m2cv m))
  (assert-equalp wm                    (mjr_mat_m2cv wm))
  (assert-equalp 1                     (mjr_mat_m2cv 1))
  (assert-equalp (list 1 2 3)          (mjr_mat_m2cv (list 1 2 3)))
  (assert-equalp 't                    (mjr_mat_m2cv 't))
  (assert-equalp v3                    (mjr_mat_m2cv v3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-from-func
  (assert-equalp c23                                                                     (mjr_mat_make-from-func #'complex :rows 2 :cols 3))
  (assert-equalp c33                                                                     (mjr_mat_make-from-func #'complex :rows 3))
  (assert-equalp c33                                                                     (mjr_mat_make-from-func #'complex :cols 3))
  (assert-equalp #2A((#C(10 10) #C(10 25) #C(10 40)) (#C(40 10) #C(40 25) #C(40 40)))    (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :start 10 :end 40))
  (assert-equalp #2A((#C(40 40) #C(40 25) #C(40 10)) (#C(10 40) #C(10 25) #C(10 10)))    (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :start 40 :end 10))
  (assert-equalp #2A((#C(0 10) #C(0 25) #C(0 40)) (#C(1 10) #C(1 25) #C(1 40)))          (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :cstart 10 :cend 40))
  (assert-equalp #2A((10 #C(10 1) #C(10 2)) (40 #C(40 1) #C(40 2)))                      (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :rstart 10 :rend 40))
  (assert-equalp #2A((#C(10 20) #C(10 35) #C(10 50)) (#C(40 20) #C(40 35) #C(40 50)))    (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :rstart 10 :rend 40 :start 20 :end 50))
  (assert-equalp #2A((0 #C(0 1) #C(0 2)) (-1 #C(-1 1) #C(-1 2)))                         (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :rfunc #'-))
  (assert-equalp #2A((0 #C(0 -1) #C(0 -2)) (1 #C(1 -1) #C(1 -2)))                        (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :cfunc #'-))
  (assert-equalp #2A((0 #C(0 -1) #C(0 -2)) (-1 #C(-1 -1) #C(-1 -2)))                     (mjr_mat_make-from-func #'complex :rows 2 :cols 3 :rfunc #'- :cfunc #'-))
  (assert-equalp #2A((#C(1 2) #C(1 4) #C(1 6)) (#C(2 2) #C(2 4) #C(2 6)))                (mjr_mat_make-from-func #'complex :rpoints #(1 2) :cpoints #(2 4 6)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-and-fill
  ;; Just right
  (assert-equalp m                                              (mjr_mat_make-and-fill m v9))
  (assert-equalp m                                              (mjr_mat_make-and-fill 3 v9))
  (assert-equalp m                                              (mjr_mat_make-and-fill 3 3 v9))
  (assert-equalp m                                              (mjr_mat_make-and-fill m L9))
  (assert-equalp m                                              (mjr_mat_make-and-fill 3 L9))
  (assert-equalp m                                              (mjr_mat_make-and-fill 3 3 L9))
  ;; Matrix too big for data
  (assert-equalp tpm                                            (mjr_mat_make-and-fill tpm v9))
  (assert-equalp tpm                                            (mjr_mat_make-and-fill 8 3 v9))
  (assert-equalp tpm                                            (mjr_mat_make-and-fill tpm L9))
  (assert-equalp tpm                                            (mjr_mat_make-and-fill 8 3 L9))
  (assert-equalp #2A((1 2 3 4) (5 6 7 8) (9 0 0 0) (0 0 0 0))   (mjr_mat_make-and-fill 4 L9))
  (assert-equalp #2a((1 2 3)(4 5 6)(7 8 9)(0 0 0))              (mjr_mat_make-and-fill 4 3 L9))
  ;; Too small
  (assert-equalp #2a((1 2)(3 4))                                (mjr_mat_make-and-fill 2 L9))
  (assert-equalp wm                                             (mjr_mat_make-and-fill 2 3 L9))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-diag
  (assert-equalp #2a((1 0 0)(0 2 0)(0 0 3))                      (mjr_mat_make-diag #(1 2 3)))
  (assert-equalp #2A((0 1 0 0) (0 0 2 0) (0 0 0 3) (0 0 0 0))    (mjr_mat_make-diag #(1 2 3) 1))
  (assert-equalp #2A((0 0 0 0) (1 0 0 0) (0 2 0 0) (0 0 3 0))    (mjr_mat_make-diag #(1 2 3) -1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-poly-companion
  (assert-equalp #2A((0 -5/3) (1 -4/3))              (mjr_mat_make-poly-companion #(3 4 5)))
  (assert-equalp #2A((-4/3 -5/3) (1 0))              (mjr_mat_make-poly-companion #(3 4 5) :form :matlab))
  (assert-equalp #2A((-4/3 1) (-5/3 0))              (mjr_mat_make-poly-companion #(3 4 5) :form :mattran))
  (assert-equalp #2A((0 -5/3) (1 -4/3))              (mjr_mat_make-poly-companion #(3 4 5) :form :standard))
  (assert-equalp #2A((0 1) (-5/3 -4/3))              (mjr_mat_make-poly-companion #(3 4 5) :form :transpose))
  (assert-equalp #2A((0 0 -5/2) (1 0 -2) (0 1 -3/2)) (mjr_mat_make-poly-companion #(2 3 4 5)))
  (assert-equalp #2A((-3/2 -2 -5/2) (1 0 0) (0 1 0)) (mjr_mat_make-poly-companion #(2 3 4 5) :form :matlab))
  (assert-equalp #2A((-3/2 1 0) (-2 0 1) (-5/2 0 0)) (mjr_mat_make-poly-companion #(2 3 4 5) :form :mattran))
  (assert-equalp #2A((0 0 -5/2) (1 0 -2) (0 1 -3/2)) (mjr_mat_make-poly-companion #(2 3 4 5) :form :standard))
  (assert-equalp #2A((0 1 0) (0 0 1) (-5/2 -2 -3/2)) (mjr_mat_make-poly-companion #(2 3 4 5) :form :transpose))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-identity
  (assert-equalp #2a((1 0 0)(0 1 0)(0 0 1))                  (mjr_mat_make-identity 3))
  (assert-equalp #2a((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))   (mjr_mat_make-identity 4))
  (assert-equalp #2a((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1))   (mjr_mat_make-identity 4.0))
  (assert-equalp #2a((1 0 0)(0 1 0)(0 0 1))                  (mjr_mat_make-identity m))
  (assert-equalp #2a((1 0 0)(0 1 0)(0 0 1))                  (mjr_mat_make-identity v3))
  (assert-equalp #2a((2 0 0)(0 2 0)(0 0 2))                  (mjr_mat_make-identity 3  2))
  (assert-equalp #2a((2 0 0 0)(0 2 0 0)(0 0 2 0)(0 0 0 2))   (mjr_mat_make-identity 4  2))
  (assert-equalp #2a((2 0 0)(0 2 0)(0 0 2))                  (mjr_mat_make-identity m  2))
  (assert-equalp #2a((2 0 0)(0 2 0)(0 0 2))                  (mjr_mat_make-identity v3 2))
  (dotimes (i 20)
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-identity))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diagonal))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diagonal-fr))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diag-unit))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diag-abs-unit))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diag-non-zero))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diag-const))
    (assert-true (mjr_mat_test-property-struct (mjr_mat_make-identity (mjr_prng_int-co 1 20)) :mp-diag-pos)))
  ;; Test errors
  (assert-error 'error                                       (mjr_mat_make-identity -1))
  (assert-error 'error                                       (mjr_mat_make-identity 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-table
  (assert-equalp #2A((0 0) (1 1) (2 2))          (mjr_mat_make-table (lambda (x) (+ x))                    :len 3 :n 1 :arg-mode :arg-number))
  (assert-equalp #2A((0 0 0) (0 1 1) (0 2 2)
                     (1 0 1) (1 1 2) (1 2 3)
                     (2 0 4) (2 1 5) (2 2 6))    (mjr_mat_make-table (lambda (x y) (+ (* x x) y))          :len 3 :n 2 :arg-mode :arg-number))
  (assert-equalp #2A((0 0) (1 1) (2 4))          (mjr_mat_make-table (lambda (x) (* x x))                  :len 3 :n 1 :arg-mode :arg-number))
  (assert-equalp #2A((0 0 0) (1 1 2) (2 4 4))    (mjr_mat_make-table (lambda (x) (vector (* x x) (* 2 x))) :len 3 :n 1 :arg-mode :arg-number))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_code
  ;; Test defaults, alternate spec of language, etc...
  (assert-equal "[1,2,3;4,5,6;7,8,9]"                                  (mjr_mat_code m))
  (assert-equal (mjr_mat_code m)                                       (mjr_mat_code m :lang :lang-matlab))
  (assert-equal (mjr_mat_code m :lang :lang-matlab)                    (mjr_mat_code m :lang :lang-matlab))
  (assert-equal (mjr_mat_code m :lang :lang-lisp)                      (mjr_mat_code m :lang :lang-lisp))
  ;; Test errors
  (assert-error 'error                                                 (mjr_mat_code em))
  (assert-error 'error                                                 (mjr_mat_code m :lang "ZOG"))
  (assert-error 'error                                                 (mjr_mat_code #((1 2 3)))) ; array of lists
  (assert-error 'error                                                 (mjr_mat_code 1))
  ;; Test a vector
  (assert-equal "[1;2;3]"                                              (mjr_mat_code v3))
  (assert-equal (mjr_mat_code mv3)                                     (mjr_mat_code v3))
  ;; Test not square
  (assert-equal "#2a((1 2 3)(4 5 6))"                                  (mjr_mat_code wm  :lang :lang-lisp))
  (assert-equal "#2a((1 2)(4 5)(7 8))"                                 (mjr_mat_code tm  :lang :lang-lisp))
  (assert-equal "#2a((1 2)(3 4)(5 6)(7 8)(9 0))"                       (mjr_mat_code mtm :lang :lang-lisp))
  (assert-equal "#2a((1 2 3 4 5 6 7 8 9))"                             (mjr_mat_code vwm :lang :lang-lisp))
  (assert-equal "#2a((1)(2)(3)(4)(5)(6)(7)(8)(9))"                     (mjr_mat_code vtm :lang :lang-lisp))
  ;; Test each language
  (assert-equal "[1,2,3;4,5,6;7,8,9]"                                  (mjr_mat_code m :lang :lang-matlab))
  (assert-equal "[1,2,3;4,5,6;7,8,9]"                                  (mjr_mat_code m :lang :lang-octave))
  (assert-equal "{{1,2,3},{4,5,6},{7,8,9}};"                           (mjr_mat_code m :lang :lang-mathematica))
  (assert-equal "[[1,2,3],[4,5,6],[7,8,9]];"                           (mjr_mat_code m :lang :lang-idl))
  (assert-equal "[[1,2,3][4,5,6][7,8,9]]"                              (mjr_mat_code m :lang :lang-hp48))
  (assert-equal "matrix([[1,2,3],[4,5,6],[7,8,9]]);"                   (mjr_mat_code m :lang :lang-maple))
  (assert-equal "rbind(c(1,2,3),c(4,5,6),c(7,8,9))"                    (mjr_mat_code m :lang :lang-r))
  (assert-equal "#2a((1 2 3)(4 5 6)(7 8 9))"                           (mjr_mat_code m :lang :lang-lisp))
  (assert-equal "1,2,3
4,5,6
7,8,9
"                                                                      (mjr_mat_code m :lang :lang-csv))
  (assert-equal "1,2,3
4,5,6
7,8,9
"                                                                      (mjr_mat_code m :lang :lang-csvl))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_transpose
  (assert-equalp   #2A((1 4 7) (2 5 8) (3 6 9))                     (mjr_mat_transpose m))
  (assert-equalp #2A((1 4 7) (2 5 8))                               (mjr_mat_transpose tm))
  (assert-equalp #2A((0 1) (#C(0 1) #C(1 1)) (#C(0 2) #C(1 2)))     (mjr_mat_transpose c23 nil))
  (assert-equalp #2A((0 1) (#C(0 -1) #C(1 -1)) (#C(0 -2) #C(1 -2))) (mjr_mat_transpose c23))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_*
  (assert-equalp #2A((30 36 42) (66 81 96) (102 126 150))             (mjr_mat_* m m))
  (assert-equalp #2A((30 36 42) (66 81 96))                           (mjr_mat_* wm m))
  (assert-equalp #2A((14) (32) (50))                                  (mjr_mat_* m v3))
  (assert-equalp #2A((468 576 684) (1062 1305 1548) (1656 2034 2412)) (mjr_mat_* m m m))
  (assert-equalp em                                                   (mjr_mat_* em em))
  ;; Check the element wise case
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 18))                    (mjr_mat_* m 2))
  (assert-equalp em                                                   (mjr_mat_* em 2))
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 18))                    (mjr_mat_* 2 m))
  (assert-equalp em                                                   (mjr_mat_* 2 em))
  ;; Number case
  (assert-equalp (* 2 3 4)                                            (mjr_mat_* 2 3 4))
  ;; sizes don't match.
  (assert-error 'error                                                (mjr_mat_* m wm))
  (assert-error 'error                                                (mjr_mat_* m em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_+
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 18))                    (mjr_mat_+ m m))
  (assert-equalp #2A((3 6 9) (12 15 18) (21 24 27))                   (mjr_mat_+ m m m))
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 17))                    (mjr_mat_+ m mi))
  (assert-equalp em                                                   (mjr_mat_+ em em))
  ;; Check the element wise case
  (assert-equalp #2A((3 4 5) (6 7 8) (9 10 11))                       (mjr_mat_+ m 2))
  (assert-equalp em                                                   (mjr_mat_+ em 2))
  (assert-equalp #2A((3 4 5) (6 7 8) (9 10 11))                       (mjr_mat_+ 2 m))
  (assert-equalp em                                                   (mjr_mat_+ 2 em))
  ;; Number case
  (assert-equalp (+ 2 3 4)                                            (mjr_mat_+ 2 3 4))
  ;; sizes don't match.
  (assert-error 'error                                                (mjr_mat_+ wm m))
  (assert-error 'error                                                (mjr_mat_+ m em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_expt
  (assert-equalp m                                                    (mjr_mat_expt m 1))
  (assert-equalp #2A((30 36 42) (66 81 96) (102 126 150))             (mjr_mat_expt m 2))
  (assert-equalp #2A((468 576 684) (1062 1305 1548) (1656 2034 2412)) (mjr_mat_expt m 3))
  ;; Can't do the computation cases.
  (assert-equalp nil                                                  (mjr_mat_expt m nil))
  (assert-equalp nil                                                  (mjr_mat_expt m -1))
  (assert-equalp nil                                                  (mjr_mat_expt m -2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_ew*
  (assert-equalp #2A((1 4 9) (16 25 36) (49 64 81))                   (mjr_mat_ew* m m))
  (assert-equalp #2A((1 8 27) (64 125 216) (343 512 729))             (mjr_mat_ew* m m m))
  (assert-equalp #2A((1 4 9) (16 25 36) (49 64 72))                   (mjr_mat_ew* m mi))
  (assert-equalp em                                                   (mjr_mat_ew* em em))
  ;; All errors -- all args must be matrices
  (assert-error 'error                                                (mjr_mat_ew* m 2))
  (assert-error 'error                                                (mjr_mat_ew* em 2))
  (assert-error 'error                                                (mjr_mat_ew* 2 m))
  (assert-error 'error                                                (mjr_mat_ew* 2 em))
  ;; More errors
  (assert-error 'error                                                (mjr_mat_ew* 2 3))
  (assert-error 'error                                                (mjr_mat_ew* 2 3 4))
  ;; sizes don't match.
  (assert-error 'error                                                (mjr_mat_ew* wm m))
  (assert-error 'error                                                (mjr_mat_ew* m em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_ew/
  (assert-equalp #2A((1 1 1) (1 1 1) (1 1 1))                         (mjr_mat_ew/ m m))
  (assert-equalp #2A((1 1/2 1/3) (1/4 1/5 1/6) (1/7 1/8 1/9))         (mjr_mat_ew/ m m m))
  (assert-equalp #2A((1 1 1) (1 1 1) (1 1 9/8))                       (mjr_mat_ew/ m mi))
  (assert-equalp em                                                   (mjr_mat_ew/ em em))
  ;; All errors -- all args must be matrices
  (assert-error 'error                                                (mjr_mat_ew/ m 2))
  (assert-error 'error                                                (mjr_mat_ew/ em 2))
  (assert-error 'error                                                (mjr_mat_ew/ 2 m))
  (assert-error 'error                                                (mjr_mat_ew/ 2 em))
  ;; More errors
  (assert-error 'error                                                (mjr_mat_ew/ 2 3))
  (assert-error 'error                                                (mjr_mat_ew/ 2 3 4))
  ;; sizes don't match.
  (assert-error 'error                                                (mjr_mat_ew/ wm m))
  (assert-error 'error                                                (mjr_mat_ew/ m em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_-
  (assert-equalp #2A((-1 -2 -3) (-4 -5 -6) (-7 -8 -9))                (mjr_mat_- m))
  (assert-equalp #2A((0 0 0) (0 0 0) (0 0 0))                         (mjr_mat_- m m))
  (assert-equalp #2A((-1 -2 -3) (-4 -5 -6) (-7 -8 -9))                (mjr_mat_- m m m))
  (assert-equalp #2A((0 0 0) (0 0 0) (0 0 1))                         (mjr_mat_- m mi))
  (assert-equalp em                                                   (mjr_mat_- em em))
  ;; Check the element wise case
  (assert-equalp #2A((-1 0 1) (2 3 4) (5 6 7))                        (mjr_mat_- m 2))
  (assert-equalp em                                                   (mjr_mat_- em 2))
  (assert-equalp #2A((1 0 -1) (-2 -3 -4) (-5 -6 -7))                  (mjr_mat_- 2 m))
  (assert-equalp em                                                   (mjr_mat_- 2 em))
  ;; Number case
  (assert-equalp (- 2 3)                                              (mjr_mat_- 2 3))
  (assert-equalp (- 2 3 4)                                            (mjr_mat_- 2 3 4))
  ;; sizes don't match.
  (assert-error 'error                                                (mjr_mat_- wm m))
  (assert-error 'error                                                (mjr_mat_- m em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_s/
  (assert-equalp #2A((1/2 1 3/2) (2 5/2 3) (7/2 4 9/2))               (mjr_mat_s/ m 2))
  (assert-equalp em                                                   (mjr_mat_s/ em 2))
  (assert-equalp #2A((1/2 1 3/2) (2 5/2 3) (7/2 4 9/2))               (mjr_mat_s/ 2 m))
  (assert-equalp em                                                   (mjr_mat_s/ 2 em))
  (assert-equalp #2A((1/2) (1) (3/2))                                 (mjr_mat_s/ 2 v3))
  (assert-equalp #2A((1/2) (1) (3/2))                                 (mjr_mat_s/ v3 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_float
  (assert-equalp mf                     (mjr_mat_float m))
  (assert-equalp cf23                   (mjr_mat_float c23))
  (assert-equalp #2A((1.0)(2.0)(3.0))   (mjr_mat_float v3))
  (dotimes (i 20)
    (let* ((r (mjr_prng_int-co 1 10))
           (c (mjr_prng_int-co 1 10))
           (m (mjr_mat_float (mjr_matt_make-random :mp-real :m r :n c :a 10))))
      (assert-equalp (* r c) (loop for ri from 0 upto (1- r)
                                   sum (loop for ci from 0 upto (1- c)
                                             when (floatp (aref m ri ci))
                                             sum 1)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rationalize
  (assert-equalp m                                      (mjr_mat_rationalize m))
  (assert-equalp m                                      (mjr_mat_rationalize mf))
  (assert-equalp c23                                    (mjr_mat_rationalize cf23))
  (assert-equalp #2a((1)(2)(3))                         (mjr_mat_rationalize #(1.0 2 3.0)))
  (assert-equalp #2a((1)(2)(3))                         (mjr_mat_rationalize v3))
  (dotimes (i 20)
    (let* ((r (mjr_prng_int-co 1 10))
           (c (mjr_prng_int-co 1 10))
           (m (mjr_mat_rationalize (mjr_matt_make-random :mp-real :m r :n c :a 10))))
      (assert-equalp (* r c) (loop for ri from 0 upto (1- r)
                                   sum (loop for ci from 0 upto (1- c)
                                             when (rationalp (aref m ri ci))
                                             sum 1)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_diag
  (assert-equalp #(1 5 9) (mjr_mat_diag m))
  (assert-equalp #(1 5)   (mjr_mat_diag wm))
  (assert-equalp #(1 5)   (mjr_mat_diag tm))
  (assert-equalp #(1)     (mjr_mat_diag vtm))
  (assert-equalp #(1)     (mjr_mat_diag vwm))
  (assert-equalp #(1)     (mjr_mat_diag v3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_every-idx
  ;;(pred rows-or-matrix &optional cols-or-missing)
  (assert-true  (mjr_mat_every-idx (lambda (i j) (integerp (aref m i j)))  m))
  (assert-false (mjr_mat_every-idx (lambda (i j) (integerp (aref mf i j))) mf))
  (assert-true  (mjr_mat_every-idx (lambda (i j) (< (aref m i j) 10))      m))
  (assert-false (mjr_mat_every-idx (lambda (i j) (< (aref m i j) 5))       m))
  ;; Just sizes
  (assert-true  (mjr_mat_every-idx (lambda (i j) (and (< i 3) (< j 3)))   3 3))
  (assert-false (mjr_mat_every-idx (lambda (i j) (and (< i 3) (< j 3)))   3 4))
  (assert-true  (mjr_mat_every-idx (lambda (i j) (and (< i 3) (< j 3)))   3 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_test-property-struct
  (assert-true (mjr_mat_test-property-struct #2a((11 12 13 14 15)
                                                 (21 22 23 24 14)
                                                 (31 32 33 23 13)
                                                 (41 42 32 22 12)
                                                 (51 41 31 21 11)) :mp-persymmetric))
  (assert-false (mjr_mat_test-property-struct #2a((11 12 13 14 15)
                                                  (21 22 23 24 14)
                                                  (31 32 33 23 13)
                                                  (41 42 32 22 12)
                                                  (51 41 32 21 11)) :mp-persymmetric))
  (assert-true (mjr_mat_test-property-struct #2a((5 6 7 8 9)
                                                 (4 5 6 7 8)
                                                 (3 4 5 6 7)
                                                 (2 3 4 5 6)
                                                 (1 2 3 4 5)) :mp-toeplitz))
  (assert-false (mjr_mat_test-property-struct #2a((5 6 7 8 9)
                                                  (4 5 6 7 8)
                                                  (3 4 5 6 7)
                                                  (2 3 4 5 6)
                                                  (1 2 3 5 5)) :mp-toeplitz))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_trace
  (assert-equalp 15  (mjr_mat_trace m))
  (assert-equalp 6   (mjr_mat_trace wm))
  (assert-equalp 6   (mjr_mat_trace tm))
  (assert-equalp 1   (mjr_mat_trace vtm))
  (assert-equalp 1   (mjr_mat_trace vwm))
  (assert-equalp 1   (mjr_mat_trace v3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_diag-prod
  (assert-equalp 45  (mjr_mat_diag-prod m))
  (assert-equalp 5   (mjr_mat_diag-prod wm))
  (assert-equalp 5   (mjr_mat_diag-prod tm))
  (assert-equalp 1   (mjr_mat_diag-prod vtm))
  (assert-equalp 1   (mjr_mat_diag-prod vwm))
  (assert-equalp 1   (mjr_mat_diag-prod v3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_print
  ;; This is one way to temporally redirect standard output...
  (with-output-to-string (*standard-output* nil)
    ;; Make sure it always returns MATRIX
    (dotimes (i 20)
      (let* ((mr (mjr_matt_make-random :mp-real :m (mjr_prng_int-co 1 10) :n (mjr_prng_int-co 1 10) :a 10)))
        (assert-equalp mr                 (mjr_mat_print mr))
        (assert-equalp mr                 (mjr_mat_print mr :fmt-str "~f"))
        (assert-equalp mr                 (mjr_mat_print mr :fmt-type "~f"))
        (assert-equalp mr                 (mjr_mat_print mr :fmt-type "~f" :ele-pad 3))
        (assert-equalp mr                 (mjr_mat_print mr :filter-func #'identity))
        (assert-equalp mr                 (mjr_mat_print mr :filter-func :pnp))
        (assert-equalp mr                 (mjr_mat_print mr :filter-func :zp))
        (assert-equalp mr                 (mjr_mat_print mr :filter-func :zbp))))
    (assert-equalp em                (mjr_mat_print em))
    (assert-equalp v3                (mjr_mat_print v3))
    (assert-equalp 1                 (mjr_mat_print 1))
    (assert-equalp :foo              (mjr_mat_print :foo))
    (assert-equalp #'+               (mjr_mat_print #'+))
    (assert-equalp m                 (mjr_mat_print m))
    (assert-equalp m                 (mjr_mat_print m :fmt-str "~f"))
    (assert-equalp m                 (mjr_mat_print m :fmt-type "~f"))
    (assert-equalp m                 (mjr_mat_print m :fmt-type "~f" :ele-pad 3))
    (assert-equalp m                 (mjr_mat_print m :filter-func #'identity))
    (assert-equalp m                 (mjr_mat_print m :filter-func :pnp))
    (assert-equalp m                 (mjr_mat_print m :filter-func :zp))
    (assert-equalp m                 (mjr_mat_print m :filter-func :zbp))
    (assert-equalp wm                (mjr_mat_print wm))
    (assert-equalp wm                (mjr_mat_print wm :fmt-str "~f"))
    (assert-equalp wm                (mjr_mat_print wm :fmt-type "~f"))
    (assert-equalp wm                (mjr_mat_print wm :fmt-type "~f" :ele-pad 3))
    (assert-equalp wm                (mjr_mat_print wm :filter-func #'identity))
    (assert-equalp wm                (mjr_mat_print wm :filter-func :pnp))
    (assert-equalp wm                (mjr_mat_print wm :filter-func :zp))
    (assert-equalp wm                (mjr_mat_print wm :filter-func :zbp))
    (assert-equalp tm                (mjr_mat_print tm))
    (assert-equalp tm                (mjr_mat_print tm :fmt-str "~f"))
    (assert-equalp tm                (mjr_mat_print tm :fmt-type "~f"))
    (assert-equalp tm                (mjr_mat_print tm :fmt-type "~f" :ele-pad 3))
    (assert-equalp tm                (mjr_mat_print tm :filter-func #'identity))
    (assert-equalp tm                (mjr_mat_print tm :filter-func :pnp))
    (assert-equalp tm                (mjr_mat_print tm :filter-func :zp))
    (assert-equalp tm                (mjr_mat_print tm :filter-func :zbp))
    ;; Error cases
    (assert-error 'error             (mjr_mat_print m :filter-func 1))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rowop-swap
  ;; square
  (assert-equalp #2A((4 5 6) (1 2 3) (7 8 9))   (mjr_mat_rowop-swap m    0 1))
  (assert-equalp #2A((7 8 9) (4 5 6) (1 2 3))   (mjr_mat_rowop-swap m    0 2))
  (assert-equalp #2A((1 2 3) (7 8 9) (4 5 6))   (mjr_mat_rowop-swap m    1 2))
  ;; tall row=0
  (assert-equalp #2A((3 4)(1 2)(5 6)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  0 1))
  (assert-equalp #2A((5 6)(3 4)(1 2)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  0 2))
  (assert-equalp #2A((7 8)(3 4)(5 6)(1 2)(9 0)) (mjr_mat_rowop-swap mtm  0 3))
  (assert-equalp #2A((9 0)(3 4)(5 6)(7 8)(1 2)) (mjr_mat_rowop-swap mtm  0 4))
  ;; tall row=1
  (assert-equalp #2A((1 2)(5 6)(3 4)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  1 2))
  (assert-equalp #2A((1 2)(7 8)(5 6)(3 4)(9 0)) (mjr_mat_rowop-swap mtm  1 3))
  ;; wide row=0
  (assert-equalp #2a((4 5 6)(1 2 3))            (mjr_mat_rowop-swap wm   0 1))
  ;; square
  (assert-equalp #2A((4 5 6) (1 2 3) (7 8 9))   (mjr_mat_rowop-swap m    1 0))
  (assert-equalp #2A((7 8 9) (4 5 6) (1 2 3))   (mjr_mat_rowop-swap m    2 0))
  (assert-equalp #2A((1 2 3) (7 8 9) (4 5 6))   (mjr_mat_rowop-swap m    2 1))
  ;; tall row=0
  (assert-equalp #2A((3 4)(1 2)(5 6)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  1 0))
  (assert-equalp #2A((5 6)(3 4)(1 2)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  2 0))
  (assert-equalp #2A((7 8)(3 4)(5 6)(1 2)(9 0)) (mjr_mat_rowop-swap mtm  3 0))
  (assert-equalp #2A((9 0)(3 4)(5 6)(7 8)(1 2)) (mjr_mat_rowop-swap mtm  4 0))
  ;; tall row=1
  (assert-equalp #2A((1 2)(5 6)(3 4)(7 8)(9 0)) (mjr_mat_rowop-swap mtm  2 1))
  (assert-equalp #2A((1 2)(7 8)(5 6)(3 4)(9 0)) (mjr_mat_rowop-swap mtm  3 1))
  ;; wide row=0
  (assert-equalp #2a((4 5 6)(1 2 3))            (mjr_mat_rowop-swap wm   1 0))
  ;; Error cases
  (assert-error 'error                          (mjr_mat_rowop-swap m    0 0))
  (assert-error 'error                          (mjr_mat_rowop-swap m    1 1))
  (assert-error 'error                          (mjr_mat_rowop-swap em   0 0))
  (assert-error 'error                          (mjr_mat_rowop-swap m   -1 2))
  (assert-error 'error                          (mjr_mat_rowop-swap m    0 3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rowop-mult
  (assert-equalp #2A((-1 -2 -3)(4 5 6)(7 8 9))  (mjr_mat_rowop-mult m    0 -1))
  (assert-equalp #2A((1 2 3)(-4 -5 -6)(7 8 9))  (mjr_mat_rowop-mult m    1 -1))
  ;; wide matrix
  (assert-equalp #2a((-1 -2 -3)(4 5 6))         (mjr_mat_rowop-mult wm   0 -1))
  ;; Testing automatic factor feature
  (assert-equalp #2a((1 1 3/2)(0 5 6)(7 0 9))   (mjr_mat_rowop-mult #2a((2 2 3)(0 5 6)(7 0 9))  0))
  (assert-equalp #2a((2 2 3)(0 1 6/5)(7 0 9))   (mjr_mat_rowop-mult #2a((2 2 3)(0 5 6)(7 0 9))  1))
  (assert-equalp #2a((2 2 3)(0 5 6)(1 0 9/7))   (mjr_mat_rowop-mult #2a((2 2 3)(0 5 6)(7 0 9))  2))
  ;; Error cases
  (assert-error 'error                          (mjr_mat_rowop-mult em   0 1))
  (assert-error 'error                          (mjr_mat_rowop-mult m   -1 1))
  (assert-error 'error                          (mjr_mat_rowop-mult m    3 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rowop-div
  (assert-equalp #2A((1/2 1 3/2)(4 5 6)(7 8 9)) (mjr_mat_rowop-div m    0 2))
  (assert-equalp #2A((1 2 3)(2 5/2 3)(7 8 9))   (mjr_mat_rowop-div m    1 2))
  ;; wide matrix
  (assert-equalp #2a((-1 -2 -3)(4 5 6))         (mjr_mat_rowop-div wm   0 -1))
  ;; Testing automatic factor feature
  (assert-equalp #2a((1 1 3/2)(0 5 6)(7 0 9))   (mjr_mat_rowop-div #2a((2 2 3)(0 5 6)(7 0 9))  0))
  (assert-equalp #2a((2 2 3)(0 1 6/5)(7 0 9))   (mjr_mat_rowop-div #2a((2 2 3)(0 5 6)(7 0 9))  1))
  (assert-equalp #2a((2 2 3)(0 5 6)(1 0 9/7))   (mjr_mat_rowop-div #2a((2 2 3)(0 5 6)(7 0 9))  2))
  ;; Error cases
  (assert-error 'error                                      (mjr_mat_rowop-div m    0 0))
  (assert-error 'error                                      (mjr_mat_rowop-div em   0 1))
  (assert-error 'error                                      (mjr_mat_rowop-div m   -1 1))
  (assert-error 'error                                      (mjr_mat_rowop-div m    3 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rowop-mult-add
  ;; square
  (assert-equalp #2A((1 2 3) (0 -3 -6) (7 8 9))             (mjr_mat_rowop-mult-add m    0 1))
  (assert-equalp #2A((1 2 3) (4 5 6) (0 -6 -12))            (mjr_mat_rowop-mult-add m    0 2))
  (assert-equalp #2A((1 2 3) (4 5 6) (0 -3/4 -3/2))         (mjr_mat_rowop-mult-add m    1 2))
  ;; square swap row argument order
  (assert-equalp #2A((0 3/4 3/2) (4 5 6) (7 8 9))           (mjr_mat_rowop-mult-add m    1 0))
  (assert-equalp #2A((0 6/7 12/7) (4 5 6) (7 8 9))          (mjr_mat_rowop-mult-add m    2 0))
  (assert-equalp #2A((1 2 3) (0 3/7 6/7) (7 8 9))           (mjr_mat_rowop-mult-add m    2 1))
  ;; tall row=0
  (assert-equalp #2A((1 2)(0 -2)(5 6)(7 8)(9 0))            (mjr_mat_rowop-mult-add mtm  0 1))
  (assert-equalp #2A((1 2)(3 4)(0 -4)(7 8)(9 0))            (mjr_mat_rowop-mult-add mtm  0 2))
  (assert-equalp #2A((1 2)(3 4)(5 6)(0 -6)(9 0))            (mjr_mat_rowop-mult-add mtm  0 3))
  (assert-equalp #2A((1 2)(3 4)(5 6)(7 8)(0 -18))           (mjr_mat_rowop-mult-add mtm  0 4))
  ;; tall row=1
  (assert-equalp #2A((1 2) (3 4) (0 -2/3) (7 8) (9 0))      (mjr_mat_rowop-mult-add mtm  1 2))
  (assert-equalp #2A((1 2) (3 4) (5 6) (0 -4/3) (9 0))      (mjr_mat_rowop-mult-add mtm  1 3))
  ;; wide row=0
  (assert-equalp #2A((1 2 3) (0 -3 -6))                     (mjr_mat_rowop-mult-add wm   0 1))
  ;; Explicit factor
  (assert-equalp #2A((1 2 3) (6 9 12) (7 8 9))              (mjr_mat_rowop-mult-add m    0 1 2))
  (assert-equalp #2A((1 2 3) (4 5 6) (9 12 15))             (mjr_mat_rowop-mult-add m    0 2 2))
  (assert-equalp #2A((1 2 3) (4 5 6) (15 18 21))            (mjr_mat_rowop-mult-add m    1 2 2))
  ;; Error cases
  (assert-error 'error                                      (mjr_mat_rowop-mult-add m    0 0))
  (assert-error 'error                                      (mjr_mat_rowop-mult-add m    1 1))
  (assert-error 'error                                      (mjr_mat_rowop-mult-add em   0 0))
  (assert-error 'error                                      (mjr_mat_rowop-mult-add m   -1 2))
  (assert-error 'error                                      (mjr_mat_rowop-mult-add m    0 3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_norm
  (assert-equal 24  (mjr_mat_norm m))
  (assert-equal 24  (mjr_mat_norm m  :norm :infinity))
  (assert-equal 18  (mjr_mat_norm m  :norm :one))
  (assert-equal 18  (mjr_mat_norm m  :norm 1))
  (assert-true  (> 0.001 (abs (- 16.881943 (mjr_mat_norm m  :norm :frobenius)))))
  (assert-equal 285 (mjr_mat_norm m  :norm :frobenius-sq))
  (dotimes (i 20)
    (let* ((m (mjr_mat_float (mjr_matt_make-random :mp-real :m (mjr_prng_int-co 1 10) :n (mjr_prng_int-co 1 10) :a 10))))
      (assert-equalp (mjr_mat_norm m :norm :one) (mjr_mat_norm m :norm 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_minor
  ;; Normal cases
  (assert-equalp #2A((1 3 4 4) (7 9 8 2) (9 7 6 1) (3 1 3 9))  (mjr_mat_minor m5 1   1))
  (assert-equalp #2A((1 2 4 4) (7 8 8 2) (9 8 6 1) (3 9 3 9))  (mjr_mat_minor m5 1   2))
  (assert-equalp m                                             (mjr_mat_minor m4 3   3))
  (assert-equalp #2A((5 6 7) (8 9 8) (8 6 5))                  (mjr_mat_minor m4 0   0))
  ;; Normal, but not square
  (assert-equalp #2a((4 6))                                    (mjr_mat_minor wm 0   1))
  (assert-equalp #2a((4)(7))                                   (mjr_mat_minor tm 0   1))
  (assert-equalp #2a((2 3))                                    (mjr_mat_minor wm 1   0))
  (assert-equalp #2a((2)(8))                                   (mjr_mat_minor tm 1   0))
  ;; Return copy with row=nil and col=nil
  (assert-equalp m4                                            (mjr_mat_minor m4 nil nil))
  (assert-equalp wm                                            (mjr_mat_minor wm nil nil))
  (assert-equalp tm                                            (mjr_mat_minor tm nil nil))
  (assert-equalp mv3                                           (mjr_mat_minor v3 nil nil))
  ;; Remove just row
  (assert-equalp wm                                            (mjr_mat_minor m  2   nil))
  (assert-equalp #2a((4 5 6))                                  (mjr_mat_minor wm 0   nil))
  (assert-equalp #2a((4 5)(7 8))                               (mjr_mat_minor tm 0   nil))
  (assert-equalp #2a((1 2 3))                                  (mjr_mat_minor wm 1   nil))
  (assert-equalp #2a((1 2)(7 8))                               (mjr_mat_minor tm 1   nil))
  (assert-equalp #2a((1)(3))                                   (mjr_mat_minor v3 1   nil))
  ;; Remove just col
  (assert-equalp #2a((2 3)(5 6)(8 9))                          (mjr_mat_minor m  nil 0))
  (assert-equalp #2a((1 3)(4 6)(7 9))                          (mjr_mat_minor m  nil 1))
  (assert-equalp #2a((1 2)(4 5)(7 8))                          (mjr_mat_minor m  nil 2))
  (assert-equalp #2a((2 3)(5 6))                               (mjr_mat_minor wm nil 0))
  (assert-equalp #2a((2)(5)(8))                                (mjr_mat_minor tm nil 0))
  (assert-equalp #2a((1 3)(4 6))                               (mjr_mat_minor wm nil 1))
  (assert-equalp #2a((1)(4)(7))                                (mjr_mat_minor tm nil 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_det-small
  ;; Normal cases
  (assert-equalp 0        (mjr_mat_det-small m))
  (assert-equalp 3        (mjr_mat_det-small mi))
  (assert-equalp 6        (mjr_mat_det-small m4))
  (assert-equalp 1        (mjr_mat_det-small aCoMfTCA3-22A))
  (assert-equalp -5947/50 (mjr_mat_det-small aCoMfTCA3-22B))
  (assert-equalp 52/25    (mjr_mat_det-small aCoMfTCA3-22C))
  (assert-equalp -141/5   (mjr_mat_det-small aCoMfTCA3-22D))
  ;; Bad sizes
  (assert-equalp nil  (mjr_mat_det-small em))
  (assert-equalp nil  (mjr_mat_det-small em))
  (assert-equalp nil  (mjr_mat_det-small tm))
  (assert-equalp nil  (mjr_mat_det-small tm))
  (assert-equalp nil  (mjr_mat_det-small wm))
  (assert-equalp nil  (mjr_mat_det-small wm))
  (assert-equalp nil  (mjr_mat_det-small m5)) ;; too big
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_det-minor-expand
  ;; Normal cases
  (assert-equalp 0        (mjr_mat_det-minor-expand m))
  (assert-equalp 3        (mjr_mat_det-minor-expand mi))
  (assert-equalp 6        (mjr_mat_det-minor-expand m4))
  (assert-equalp -700     (mjr_mat_det-minor-expand m5))
  (assert-equalp 1        (mjr_mat_det-minor-expand aCoMfTCA3-22A))
  (assert-equalp -5947/50 (mjr_mat_det-minor-expand aCoMfTCA3-22B))
  (assert-equalp 52/25    (mjr_mat_det-minor-expand aCoMfTCA3-22C))
  (assert-equalp -141/5   (mjr_mat_det-minor-expand aCoMfTCA3-22D))
  (assert-equalp 0        (mjr_mat_det-minor-expand rosser))
  ;; Bad sizes
  (assert-equalp nil  (mjr_mat_det-minor-expand em))
  (assert-equalp nil  (mjr_mat_det-minor-expand em))
  (assert-equalp nil  (mjr_mat_det-minor-expand tm))
  (assert-equalp nil  (mjr_mat_det-minor-expand tm))
  (assert-equalp nil  (mjr_mat_det-minor-expand wm))
  (assert-equalp nil  (mjr_mat_det-minor-expand wm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_factor-qdr
  (assert-equalp (values #2A((1 9/11 -1/6) (4 3/11 1/3) (7 -3/11 -1/6))
                         #2A((1/66 0 0) (0 11/9 0) (0 0 6))
                         #2A((66 78 83) (0 9/11 21/11) (0 0 1/6)))
                 (mjr_mat_factor-qdr mi))
  (assert-equalp (values #2A((1 59/75 165/269) (4 11/75 318/269) (7 38/75 -222/269) (3 -41/25 39/269))
                         #2A((1/75 0 0) (0 75/269 0) (0 0 269/666))
                         #2A((75 91 86) (0 269/75 424/75) (0 0 666/269)))
                 (mjr_mat_factor-qdr #2a((1 2 3)(4 5 6)(7 9 8)(3 2 1))))
  (assert-error 'error (mjr_mat_factor-qdr m))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_eliminate
  (assert-equalp (values #2A((0 6/7 12/7) (0 0 0) (7 0 -7)) #(2 0 NIL))    (mjr_mat_eliminate m  :full-elim 't))
  (assert-equalp (values #2A((0 6/7 12/7) (0 0 0) (7 0 -7)) #(2 0 NIL))    (mjr_mat_eliminate m  :full-elim 't :pivot-row :max-non-zero))
  (assert-equalp (values #2A((1 0 -1) (0 -3 -6) (0 0 0))    #(0 1 NIL))    (mjr_mat_eliminate m  :full-elim 't :pivot-row :first-non-zero))
  (assert-equalp (values #2A((1 0 -1) (0 -3 -6) (0 0 0))    #(0 1 NIL))    (mjr_mat_eliminate m  :full-elim 't :pivot-row :min-non-zero-top))
  (assert-equalp (values #2A((0 6/7 0) (0 0 1/2) (7 0 0))   #(2 0 1))      (mjr_mat_eliminate mi :full-elim 't))
  (assert-equalp (values #2A((0 6/7 0) (0 0 1/2) (7 0 0))   #(2 0 1))      (mjr_mat_eliminate mi :full-elim 't :pivot-row :max-non-zero))
  (assert-equalp (values #2A((1 0 0) (0 -3 0) (0 0 -1))     #(0 1 2))      (mjr_mat_eliminate mi :full-elim 't :pivot-row :first-non-zero))
  (assert-equalp (values #2A((1 0 0) (0 -3 0) (0 0 -1))     #(0 1 2))      (mjr_mat_eliminate mi :full-elim 't :pivot-row :min-non-zero-top))
  (dotimes (i 50)
    (let* ((mr (mjr_matt_make-random :mp-real :m (mjr_prng_int-co 1 20) :n (mjr_prng_int-co 1 20) :a 100)))
      (assert-equalp (mjr_mat_eliminate mr) (mjr_mat_eliminate mr :pivot-row :max-non-zero))))
  ;; XREF: This function is heavily tested in mjr_mat_det-ge, mjr_mat_solve-sys-sge, mjr_mat_inv-sge, and mjr_mat_rank-sge .
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_det-ge
  ;; Normal cases
  (assert-equalp 0        (mjr_mat_det-ge m))
  (assert-equalp 3        (mjr_mat_det-ge mi))
  (assert-equalp 6        (mjr_mat_det-ge m4))
  (assert-equalp -700     (mjr_mat_det-ge m5))
  (assert-equalp 1        (mjr_mat_det-ge aCoMfTCA3-22A))
  (assert-equalp -5947/50 (mjr_mat_det-ge aCoMfTCA3-22B))
  (assert-equalp 52/25    (mjr_mat_det-ge aCoMfTCA3-22C))
  (assert-equalp -141/5   (mjr_mat_det-ge aCoMfTCA3-22D))
  (assert-equalp 0        (mjr_mat_det-ge rosser))
  ;; Bad sizes
  (assert-equalp nil  (mjr_mat_det-ge em))
  (assert-equalp nil  (mjr_mat_det-ge em))
  (assert-equalp nil  (mjr_mat_det-ge tm))
  (assert-equalp nil  (mjr_mat_det-ge tm))
  (assert-equalp nil  (mjr_mat_det-ge wm))
  (assert-equalp nil  (mjr_mat_det-ge wm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_det
  ;; Normal cases
  (assert-equalp 0        (mjr_mat_det m))
  (assert-equalp 3        (mjr_mat_det mi))
  (assert-equalp 6        (mjr_mat_det m4))
  (assert-equalp -700     (mjr_mat_det m5))
  (assert-equalp 1        (mjr_mat_det aCoMfTCA3-22A))
  (assert-equalp -5947/50 (mjr_mat_det aCoMfTCA3-22B))
  (assert-equalp 52/25    (mjr_mat_det aCoMfTCA3-22C))
  (assert-equalp -141/5   (mjr_mat_det aCoMfTCA3-22D))
  (assert-equalp 0        (mjr_mat_det rosser))
  ;; Bad sizes
  (assert-equalp nil  (mjr_mat_det em))
  (assert-equalp nil  (mjr_mat_det em))
  (assert-equalp nil  (mjr_mat_det tm))
  (assert-equalp nil  (mjr_mat_det tm))
  (assert-equalp nil  (mjr_mat_det wm))
  (assert-equalp nil  (mjr_mat_det wm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_solve-sys-sub
  (assert-equalp #(2 3)                         (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #(3 2 0)))
  (assert-equalp #2A((2) (3))                   (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #2A((3) (2) (0))))
  ; All ans cases should be the same as above
  (assert-equalp #(2 3)                         (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #(3 2 0) :return-partial-results 't))
  (assert-equalp #2A((2) (3))                   (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #2A((3) (2) (0)) :return-partial-results 't))
  ; Inconsistent answer
  (assert-equalp nil                            (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #2A((3) (2) (2))))
  (assert-equalp (values #(2 3) #(1 0 -1) NIL)  (mjr_mat_solve-sys-sub #2A((0 1) (1 0) (0 0)) #(3 2 2) :return-partial-results 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_solve-sys-sge
  (assert-equalp #(-1/3 2/3 0)                                (mjr_mat_solve-sys-sge mi v3))
  (assert-equalp #2A((-1/3) (2/3) (0))                        (mjr_mat_solve-sys-sge mi mv3))
  (assert-equalp #(0 0 0)                                     (mjr_mat_solve-sys-sge mi #(0 0 0)))
  (assert-equalp #2A((1 0 -1)(0 1 2)(0 0 0))                  (mjr_mat_solve-sys-sge mi m))
  (assert-equalp #2A((-8/3 8/3 -1) (10/3 -13/3 2) (-1 2 -1))  (mjr_mat_solve-sys-sge mi #2a((1 0 0)(0 1 0)(0 0 1))))
  (assert-equalp #(1 -2 -5)                                   (mjr_mat_solve-sys-sge aCoMfTCA3-1b #(-359 281 85))) ;;  NOTE: Book has a bug. :)
  (assert-equalp #(1 1 1 1)                                   (mjr_mat_solve-sys-sge aCoMfTCA3-2 #(3 -4 7 8)))
  (assert-equalp #(1 1 1 1)                                   (mjr_mat_solve-sys-sge aCoMfTCA3-5b #(23 32 33 31)))
  (loop for n from 1 upto 5
        do (progn (setf (aref aCoMfTCA3-21 0 0) (- 1 (expt 10 (- n))))
                  (assert-equalp
                   (mjr_vec_make-from-func (lambda (i) (+ (expt 10 n) i)) :len 4)
                   (mjr_mat_solve-sys-sge aCoMfTCA3-21 #(-3 -2 -1 -3)))))                                        ;; aCoMfTCA#3.5
  ;; No solution cases
  (assert-equalp nil                                          (mjr_mat_solve-sys-sge m v3))
  (assert-equalp nil                                          (mjr_mat_solve-sys-sge wm v2))
  (assert-equalp #(-1/3 2/3)                                  (mjr_mat_solve-sys-sge tm v3))
  ;; Error cases
  (assert-error 'error                                        (mjr_mat_solve-sys-sge em #()))
  (assert-error 'error                                        (mjr_mat_solve-sys-sge wm v3))
  (assert-error 'error                                        (mjr_mat_solve-sys-sge tm v2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_inv
  ;; Good matrix
  (assert-equalp #2A((1/2))                                                          (mjr_mat_inv m1i))
  (assert-equalp #2A((-8/3 8/3 -1) (10/3 -13/3 2) (-1 2 -1))                         (mjr_mat_inv mi))
  (assert-equalp aCoMfTCA3-3a                                                        (mjr_mat_inv aCoMfTCA3-3b))
  (assert-equalp aCoMfTCA3-3b                                                        (mjr_mat_inv aCoMfTCA3-3a))
  (assert-equalp aCoMfTCA3-1a                                                        (mjr_mat_inv aCoMfTCA3-1b))
  (assert-equalp aCoMfTCA3-1b                                                        (mjr_mat_inv aCoMfTCA3-1a))
  (assert-equalp aCoMfTCA3-4a                                                        (mjr_mat_inv aCoMfTCA3-4b))
  (assert-equalp aCoMfTCA3-4b                                                        (mjr_mat_inv aCoMfTCA3-4a))
  (assert-equalp aCoMfTCA3-5a                                                        (mjr_mat_inv aCoMfTCA3-5b))
  (assert-equalp aCoMfTCA3-5b                                                        (mjr_mat_inv aCoMfTCA3-5a))
  ;; Bad sizes
  (assert-equalp nil                                                                 (mjr_mat_inv em))
  (assert-equalp nil                                                                 (mjr_mat_inv wm))
  (assert-equalp nil                                                                 (mjr_mat_inv tm))
  ;; Square but non-invertible
  (assert-equalp nil                                                                 (mjr_mat_inv m1s))
  (assert-equalp nil                                                                 (mjr_mat_inv m))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_rank-sge
  (assert-equalp 5    (mjr_mat_rank-sge m5))
  (assert-equalp 4    (mjr_mat_rank-sge m4))
  (assert-equalp 2    (mjr_mat_rank-sge m))
  (assert-equalp 2    (mjr_mat_rank-sge mf))
  (assert-equalp 3    (mjr_mat_rank-sge mi))
  (assert-equalp 2    (mjr_mat_rank-sge wm))
  (assert-equalp 2    (mjr_mat_rank-sge tm))
  (assert-equalp 2    (mjr_mat_rank-sge mtm))
  (assert-equalp 1    (mjr_mat_rank-sge vwm))
  (assert-equalp 1    (mjr_mat_rank-sge vtm))
  (assert-equalp 1    (mjr_mat_rank-sge v3))
  (assert-equalp 1    (mjr_mat_rank-sge mv3))
  (assert-equalp 2    (mjr_mat_rank-sge tpm))
  ;; Errors
  (assert-error 'error (mjr_mat_rank-sge em))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cpoly-eval
  (assert-equalp 6196  (mjr_mat_cpoly-eval m5 -3))
  (assert-equalp 4632  (mjr_mat_cpoly-eval m5 -2))
  (assert-equalp 2252  (mjr_mat_cpoly-eval m5 -1))
  (assert-equalp 700   (mjr_mat_cpoly-eval m5 0))
  (assert-equalp 780   (mjr_mat_cpoly-eval m5 1))
  (assert-equalp 2576  (mjr_mat_cpoly-eval m5 2))
  (assert-equalp 5572  (mjr_mat_cpoly-eval m5 3))
  (assert-equalp 10820 (mjr_mat_cpoly-eval m5 5))
  (assert-equalp 8772  (mjr_mat_cpoly-eval m5 4))
  (assert-equalp 6     (mjr_mat_cpoly-eval m4 0))
  (assert-equalp 0     (mjr_mat_cpoly-eval m  0))
  ;; Error
  (assert-error 'error (mjr_mat_cpoly-eval em 0))
  (assert-error 'error (mjr_mat_cpoly-eval wm 0))
  (assert-error 'error (mjr_mat_cpoly-eval tm 0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cpoly
  (assert-equalp #(1 -30 69 846 -806 700)                                           (mjr_mat_cpoly m5))
  (assert-equalp #(1 -20 -83 84 6)                                                  (mjr_mat_cpoly m4))
  (assert-equalp #(1 -15 -18 0)                                                     (mjr_mat_cpoly m))
  (assert-equalp #(1 -6 11 -6)                                                      (mjr_mat_cpoly ncp3))
  (assert-equalp #(1 -30 319 -1410 2138)                                            (mjr_mat_cpoly ncp4))
  (assert-equalp #(1 -10 36 -56 35 -6)                                              (mjr_mat_cpoly ncp5))
  (assert-equalp #(1 -78 1001 -5005 12870 -19448 18564 -11628 4845 -1330 231 -23 1) (mjr_mat_cpoly (mjr_matt_make-test :mp-minij :n 12)))
  ;; Error
  (assert-error 'error (mjr_mat_cpoly em))
  (assert-error 'error (mjr_mat_cpoly wm))
  (assert-error 'error (mjr_mat_cpoly tm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cpoly-fl
  (assert-equalp #(1 -30 69 846 -806 700)                                           (mjr_mat_cpoly m5))
  (assert-equalp #(1 -20 -83 84 6)                                                  (mjr_mat_cpoly m4))
  (assert-equalp #(1 -15 -18 0)                                                     (mjr_mat_cpoly m))
  (assert-equalp #(1 -6 11 -6)                                                      (mjr_mat_cpoly ncp3))
  (assert-equalp #(1 -30 319 -1410 2138)                                            (mjr_mat_cpoly ncp4))
  (assert-equalp #(1 -10 36 -56 35 -6)                                              (mjr_mat_cpoly ncp5))
  (assert-equalp #(1 -78 1001 -5005 12870 -19448 18564 -11628 4845 -1330 231 -23 1) (mjr_mat_cpoly (mjr_matt_make-test :mp-minij :n 12)))
  ;; Error
  (assert-error 'error (mjr_mat_cpoly em))
  (assert-error 'error (mjr_mat_cpoly wm))
  (assert-error 'error (mjr_mat_cpoly tm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_inv-fl
  ;; Good matrix
  (assert-equalp #2A((1/2))                                                          (mjr_mat_inv-fl m1i))
  (assert-equalp #2A((-8/3 8/3 -1) (10/3 -13/3 2) (-1 2 -1))                         (mjr_mat_inv-fl mi))
  ;(assert-equalp aCoMfTCA3-3a                                                        (mjr_mat_inv-fl aCoMfTCA3-3b)) ;; Only support real matrices
  ;(assert-equalp aCoMfTCA3-3b                                                        (mjr_mat_inv-fl aCoMfTCA3-3a)) ;; Only support real matrices
  (assert-equalp aCoMfTCA3-1a                                                        (mjr_mat_inv-fl aCoMfTCA3-1b))
  (assert-equalp aCoMfTCA3-1b                                                        (mjr_mat_inv-fl aCoMfTCA3-1a))
  (assert-equalp aCoMfTCA3-4a                                                        (mjr_mat_inv-fl aCoMfTCA3-4b))
  (assert-equalp aCoMfTCA3-4b                                                        (mjr_mat_inv-fl aCoMfTCA3-4a))
  (assert-equalp aCoMfTCA3-5a                                                        (mjr_mat_inv-fl aCoMfTCA3-5b))
  (assert-equalp aCoMfTCA3-5b                                                        (mjr_mat_inv-fl aCoMfTCA3-5a))
  ;; Bad sizes
  (assert-equalp nil                                                                 (mjr_mat_inv-fl em))
  (assert-equalp nil                                                                 (mjr_mat_inv-fl wm))
  (assert-equalp nil                                                                 (mjr_mat_inv-fl tm))
  ;; Square but non-invertible
  (assert-equalp nil                                                                 (mjr_mat_inv-fl m1s))
  (assert-equalp nil                                                                 (mjr_mat_inv-fl m))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_inv-sge
  ;; Good matrix
  (assert-equalp #2A((1/2))                                                          (mjr_mat_inv-sge m1i))
  (assert-equalp #2A((-8/3 8/3 -1) (10/3 -13/3 2) (-1 2 -1))                         (mjr_mat_inv-sge mi))
  (assert-equalp aCoMfTCA3-3a                                                        (mjr_mat_inv-sge aCoMfTCA3-3b))
  (assert-equalp aCoMfTCA3-3b                                                        (mjr_mat_inv-sge aCoMfTCA3-3a))
  (assert-equalp aCoMfTCA3-1a                                                        (mjr_mat_inv-sge aCoMfTCA3-1b))
  (assert-equalp aCoMfTCA3-1b                                                        (mjr_mat_inv-sge aCoMfTCA3-1a))
  (assert-equalp aCoMfTCA3-4a                                                        (mjr_mat_inv-sge aCoMfTCA3-4b))
  (assert-equalp aCoMfTCA3-4b                                                        (mjr_mat_inv-sge aCoMfTCA3-4a))
  (assert-equalp aCoMfTCA3-5a                                                        (mjr_mat_inv-sge aCoMfTCA3-5b))
  (assert-equalp aCoMfTCA3-5b                                                        (mjr_mat_inv-sge aCoMfTCA3-5a))
  ;; Bad sizes
  (assert-equalp nil                                                                 (mjr_mat_inv-sge em))
  (assert-equalp nil                                                                 (mjr_mat_inv-sge wm))
  (assert-equalp nil                                                                 (mjr_mat_inv-sge tm))
  ;; Square but non-invertible
  (assert-equalp nil                                                                 (mjr_mat_inv-sge m1s))
  (assert-equalp nil                                                                 (mjr_mat_inv-sge m))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_eigen-rational
  (assert-equalp (sort (list 1 2 3)             #'<) (sort (mjr_mat_eigen-rational aCoMfTCA5-1)  #'<))
  (assert-equalp (sort (list 3 3 6)             #'<) (sort (mjr_mat_eigen-rational aCoMfTCA5-2)  #'<))
  (assert-equalp (sort (list 1 2 3 4)           #'<) (sort (mjr_mat_eigen-rational aCoMfTCA5-5)  #'<))
  (assert-equalp (sort (list -3 -2 2 3/2 1 1/3) #'<) (sort (mjr_mat_eigen-rational aCoMfTCA5-25) #'<))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_test-property-math
  ;; Empty matrix case
  (assert-false (mjr_mat_test-property-math em :mp-involutary))
  (assert-false (mjr_mat_test-property-math em :mp-orthogonal))
  (assert-false (mjr_mat_test-property-math em :mp-orthogonal-rev))
  (assert-false (mjr_mat_test-property-math em :mp-unitary))
  (assert-false (mjr_mat_test-property-math em :mp-special))
  (assert-true  (mjr_mat_test-property-math em :mp-singular))
  (assert-false (mjr_mat_test-property-math em :mp-invertible))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_inv-small
  (assert-equalp #2A((1/2))          (mjr_mat_inv-small m1i))
  ;; Make sure mjr_mat_inv-fl is consistent with mjr_mat_inv.
  (loop for n from 1 upto 3
        do (loop for i from 1 upto 10
                 for a  = (mjr_matt_make-random :mp-real :m n :a 100)
                 for ai = (mjr_mat_inv-small a)
                 when ai
                 do (assert-true (mjr_mat_test-property-struct (mjr_mat_* a ai) :mp-identity))
                 when (not ai)
                 do (assert-equalp 0 (mjr_mat_det-small a))))
  ;; Bad sizes
  (assert-equalp nil                 (mjr_mat_inv-small em))
  (assert-equalp nil                 (mjr_mat_inv-small wm))
  (assert-equalp nil                 (mjr_mat_inv-small tm))
  ;; Square but non-invertible
  (assert-equalp nil                 (mjr_mat_inv-small m1s))
  (assert-equalp nil                 (mjr_mat_inv-small m))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combo tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_make-poly-companion-mjr_mat_cpoly
  ;; Compute monic, random polynomial over Z, compute the associated
  ;; companion matrix, compute the cpoly, make sure they match
  (dotimes (i 20)
    (let ((pr (concatenate 'vector #(1) (mjr_prng_vector (mjr_prng_int-co 2 10) #'mjr_prng_int-co 0 10))))
      (assert-equalp pr (mjr_mat_cpoly (mjr_mat_make-poly-companion pr)))
      (assert-equalp pr (mjr_mat_cpoly (mjr_mat_make-poly-companion pr :form :matlab)))
      (assert-equalp pr (mjr_mat_cpoly (mjr_mat_make-poly-companion pr :form :mattran)))
      (assert-equalp pr (mjr_mat_cpoly (mjr_mat_make-poly-companion pr :form :standard)))
      (assert-equalp pr (mjr_mat_cpoly (mjr_mat_make-poly-companion pr :form :transpose)))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_det_matrix-properties
  ;; Test special case code: :mp-pos-def
  (dotimes (i 500)
    (let* ((rc (mjr_prng_int-co 5 15))
           (rl (mjr_matt_make-random :mp-l-triangular :m rc :a 1 :b 10))
           (rd (mjr_matt_make-random :mp-diagonal :m rc :a 1 :b 10))
           (rm (mjr_mat_* rl rd (mjr_mat_transpose rl)))
           (d1 (mjr_mat_det rm))
           (d2 (mjr_mat_det rm :mp-pos-def)))
      (assert-equalp d1 d2 (list :mp-pos-def rm))))

  ;; Test special case code: :mp-tri-diagonal
  (dotimes (i 500)
    (let* ((rm (mjr_matt_make-random :mp-m-diagonal :m (mjr_prng_int-co 5 15) :k 2 :a -10 :b 10))
           (d1 (mjr_mat_det rm))
           (d2 (mjr_mat_det rm :mp-tri-diagonal)))
      (assert-equalp d1 d2 (list :mp-tri-diagonal rm))))

  ;; Test special case code: :mp-u-triangular
  (dotimes (i 500)
    (let* ((rm (mjr_matt_make-random :mp-u-triangular :m (mjr_prng_int-co 5 15) :a -10 :b 10))
           (d1 (mjr_mat_det rm))
           (d2 (mjr_mat_det rm :mp-u-triangular)))
      (assert-equalp d1 d2 (list :mp-u-triangular rm))))

  ;; Test special case code: :mp-l-triangular
  (dotimes (i 500)
    (let* ((rm (mjr_matt_make-random :mp-l-triangular :m (mjr_prng_int-co 5 15) :a -10 :b 10))
           (d1 (mjr_mat_det rm))
           (d2 (mjr_mat_det rm :mp-l-triangular)))
      (assert-equalp d1 d2 (list :mp-l-triangular rm))))

  ;; Test special case code: :mp-diagonal
  (dotimes (i 500)
    (let* ((rm (mjr_matt_make-random :mp-diagonal :m (mjr_prng_int-co 5 15) :a -10 :b 10))
           (d1 (mjr_mat_det rm))
           (d2 (mjr_mat_det rm :mp-diagonal)))
      (assert-equalp d1 d2 (list :mp-diagonal rm))))

  ;; Test special case code: :mp-bogus -- just make sure we fall back to general method
  (let* ((rm (mjr_matt_make-random :mp-real :m (mjr_prng_int-co 5 15) :a -10 :b 10))
         (d1 (mjr_mat_det rm))
         (d2 (mjr_mat_det rm :mp-bogus)))
    (assert-equalp d1 d2 (list :mp-bogus rm)))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_xxx_det
  ;; Make sure that all the det functions agree.
  (loop for j from 4 upto 8 by 4  ;; Make sure we have lots of small matrices
        do (loop for i from 1 upto 100
                 for rc = (mjr_prng_int-cc 1 j)
                 for rr = (mjr_prng_int-cc 1 j)
                 for a  = (mjr_matt_make-random :mp-real :m rr :n rc :a 100)
                 for b  = (mjr_matt_make-random :mp-real :m rr :n rr :a 1000) ;; Much more likely to be invertible
                 for c  = (mjr_matt_make-random :mp-real :m rr :n rr :a 10)   ;; Much likely to be invertible
                 for ad = (mjr_mat_det a)
                 for bd = (mjr_mat_det b)
                 for cd = (mjr_mat_det c)
                 do (assert-equalp ad (mjr_mat_det-minor-expand a))
                 do (assert-equalp bd (mjr_mat_det-minor-expand b))
                 do (assert-equalp cd (mjr_mat_det-minor-expand c))
                 do (assert-equalp ad (mjr_mat_det-ge a))
                 do (assert-equalp bd (mjr_mat_det-ge b))
                 do (assert-equalp cd (mjr_mat_det-ge c))
                 do (assert-equalp ad (mjr_mat_det-ge-naive1 a))
                 do (assert-equalp bd (mjr_mat_det-ge-naive1 b))
                 do (assert-equalp cd (mjr_mat_det-ge-naive1 c))
                 do (assert-equalp ad (mjr_mat_det-ge-naive2 a))
                 do (assert-equalp bd (mjr_mat_det-ge-naive2 b))
                 do (assert-equalp cd (mjr_mat_det-ge-naive2 c))
                 do (assert-equalp bd (* (expt -1 rr) (mjr_mat_cpoly-eval b 0)))
                 do (assert-equalp cd (* (expt -1 rr) (mjr_mat_cpoly-eval c 0)))
                 when (and (> 5 rr) (> 5 rc))
                 do (assert-equalp bd (mjr_mat_det-small b))
                 when (> 5 rr)
                 do (progn (assert-equalp bd (mjr_mat_det-small b))
                           (assert-equalp cd (mjr_mat_det-small c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_xxx_inv
  ;; Make sure that all the inv functions agree.
  (loop for j from 3 upto 10 by 6  ;; Make sure we have lots of small matrices
        do (loop for i from 1 upto 300
                 for rc = (mjr_prng_int-cc 1 j)
                 for rr = (mjr_prng_int-cc 1 j)
                 for a  = (mjr_matt_make-random :mp-real :m rr :n rc :a 100)
                 for b  = (mjr_matt_make-random :mp-real :m rr :n rr :a 1000) ;; Much more likely to be invertible
                 for c  = (mjr_matt_make-random :mp-real :m rr :n rr :a 10)   ;; Much likely to be invertible
                 for ai = (mjr_mat_inv a)
                 for bi = (mjr_mat_inv b)
                 for ci = (mjr_mat_inv c)
                 do (assert-equalp ai (mjr_mat_inv-fl a))
                 do (assert-equalp bi (mjr_mat_inv-fl b))
                 do (assert-equalp ci (mjr_mat_inv-fl c))
                 do (assert-equalp ai (mjr_mat_inv-sge a))
                 do (assert-equalp bi (mjr_mat_inv-sge b))
                 do (assert-equalp ci (mjr_mat_inv-sge c))
                 when (> 5 rr)
                 do (progn (assert-equalp bi (mjr_mat_inv-fl b))
                           (assert-equalp ci (mjr_mat_inv-fl c)))
                 when (and (> 4 rc) (> 4 rr))
                 do (assert-equalp ai (mjr_mat_inv-fl a))
                 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_inv_det_mul_eq_1
  ;; Compute random matrix with integer elements -- det and inv done
  ;; with exact, rational arithmetic.  If det != 0, then:
  ;;   1) compute det of inverse, make sure it is 1/det of original matrix
  ;;   2) compute inverse, multiply by original matrix, and make sure
  ;;      product is the identity matrix.
  ;;   3) compute QDR factorization, multiply factors, and make sure
  ;;      product is the original matrix.
  (dotimes (i 50)
    (let* ((rc (mjr_prng_int-co 1 16))
           (mr (mjr_matt_make-random :mp-real :m rc :a 100))
           (dd (mjr_mat_det mr)))
      (if (not (zerop dd))
          (let* ((mri (mjr_mat_inv mr)))
            (assert-equalp (/ (mjr_mat_det mri)) dd)
            (assert-true (mjr_mat_test-property-struct (mjr_mat_* mr mri) :mp-identity))
            (assert-equalp mr (apply 'mjr_mat_* (multiple-value-list (mjr_mat_factor-qdr mr)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_cpoly-eq-mjr_mat_cpoly-det
  ;; Make sure mjr_mat_cpoly and mjr_mat_cpoly-det have same value.
  (dotimes (i 50)
    (let* ((rc (mjr_prng_int-co 1 10))
           (mr (mjr_matt_make-random :mp-real :m rc :a 100))
           (cp (mjr_mat_cpoly mr)))
      (assert-equalp cp (mjr_mat_cpoly-det mr))
      (assert-equalp cp (mjr_mat_cpoly-fl  mr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_apply-gauss-1!!
  ;; Some hand picked test cases
  (assert-equalp  #2a((4 5 6)(1 2 3)(7 8 8)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 0 1))
  (assert-equalp  #2a((4 5 6)(1 2 3))        (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 wm) 0 1))
  (assert-equalp  #2a((4 5)(1 2)(7 8))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 0 1))
  (assert-equalp  #2a((4 5 6)(1 2 3)(7 8 8)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 1 0))
  (assert-equalp  #2a((4 5 6)(1 2 3))        (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 wm) 1 0))
  (assert-equalp  #2a((4 5)(1 2)(7 8))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 1 0))
  (assert-equalp  #2A((7 8 8)(4 5 6)(1 2 3)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 0 2))
  (assert-equalp  #2A((7 8)(4 5)(1 2))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 0 2))
  (assert-equalp  #2A((7 8 8)(4 5 6)(1 2 3)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 2 0))
  (assert-equalp  #2A((7 8)(4 5)(1 2))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 2 0))
  (assert-equalp  #2a((1 2 3)(7 8 8)(4 5 6)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 1 2))
  (assert-equalp  #2a((1 2)(7 8)(4 5))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 1 2))
  (assert-equalp  #2a((1 2 3)(7 8 8)(4 5 6)) (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 mi) 2 1))
  (assert-equalp  #2a((1 2)(7 8)(4 5))       (mjr_mat_apply-gauss-1!! (mjr_arr_copy2 tm) 2 1))
  ;; XREF: This function is heavily tested in mjr_mat_eliminate
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_apply-gauss-2!!
  ;; Some hand picked test cases
  (assert-equalp  #2a((3 6 9)(4 5 6)(7 8 8))    (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 mi) 3 0))
  (assert-equalp  #2a((3 6 9)(4 5 6))           (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 wm) 3 0))
  (assert-equalp  #2a((3 6)(4 5)(7 8))          (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 tm) 3 0))
  (assert-equalp  #2a((1 2 3)(12 15 18)(7 8 8)) (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 mi) 3 1))
  (assert-equalp  #2a((1 2 3)(12 15 18))        (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 wm) 3 1))
  (assert-equalp  #2a((1 2)(12 15)(7 8))        (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 tm) 3 1))
  (assert-equalp  #2a((1 2 3)(4 5 6)(21 24 24)) (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 mi) 3 2))
  (assert-equalp  #2a((1 2)(4 5)(21 24))        (mjr_mat_apply-gauss-2!! (mjr_arr_copy2 tm) 3 2))
  ;; XREF: This function is heavily tested in mjr_mat_eliminate
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_apply-gauss-3!!
  ;; Some hand picked test cases
  (assert-equalp  #2A((1 2 3)(7 11 15)(7 8 8))  (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 0 1))
  (assert-equalp  #2A((1 2 3)(7 11 15))         (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 wm) 3 0 1))
  (assert-equalp  #2A((1 2)(7 11)(7 8))         (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 0 1))
  (assert-equalp  #2A((13 17 21)(4 5 6)(7 8 8)) (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 1 0))
  (assert-equalp  #2A((13 17 21)(4 5 6))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 wm) 3 1 0))
  (assert-equalp  #2A((13 17)(4 5)(7 8))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 1 0))
  (assert-equalp  #2A((1 2 3)(4 5 6)(10 14 17)) (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 0 2))
  (assert-equalp  #2A((1 2)(4 5)(10 14))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 0 2))
  (assert-equalp  #2A((22 26 27)(4 5 6)(7 8 8)) (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 2 0))
  (assert-equalp  #2A((22 26)(4 5)(7 8))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 2 0))
  (assert-equalp  #2A((1 2 3)(4 5 6)(19 23 26)) (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 1 2))
  (assert-equalp  #2A((1 2)(4 5)(19 23))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 1 2))
  (assert-equalp  #2A((1 2 3)(25 29 30)(7 8 8)) (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 mi) 3 2 1))
  (assert-equalp  #2A((1 2)(25 29)(7 8))        (mjr_mat_apply-gauss-3!! (mjr_arr_copy2 tm) 3 2 1))
  ;; XREF: This function is heavily tested in mjr_mat_eliminate
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_factor-ldlt
  (multiple-value-bind (l d) (mjr_mat_factor-ldlt #2a((4 12 -16)(12 37 -43)(-16 -43 98)))
    (assert-equalp #2A((1 0 0) (3 1 0) (-4 5 1)) l)
    (assert-equalp #2A((4 0 0) (0 1 0) (0 0 9))  d))
  ;; Generate some square, positive definite matrixes.  For each M, compute L&D, and check M==L*D*t(L)
  (dotimes (i 1000)
    (let* ((rc (mjr_prng_int-co 1 15))
           (rl (mjr_matt_make-random :mp-l-triangular :m rc :a 1 :b 10))
           (rd (mjr_matt_make-random :mp-diagonal :m rc :a 1 :b 10))
           (rm (mjr_mat_* rl rd (mjr_mat_transpose rl)))
           (m  (multiple-value-bind (l d) (mjr_mat_factor-ldlt rm) (mjr_mat_* l d (mjr_mat_transpose l)))))
      (assert-equalp rm m rm)))
  ;; TODO: like above, but with :mp-cauchy
  (dotimes (i 1000)
    (let* ((rm (mjr_matt_make-test :mp-cauchy :m (mjr_prng_int-co 1 15)))
           (m  (multiple-value-bind (l d) (mjr_mat_factor-ldlt rm) (mjr_mat_* l d (mjr_mat_transpose l)))))
      (assert-equalp rm m rm)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_mat_solve-sys-itr
    (loop for (simf simfp) in (list (list #'mjr_mat_sis-gauss-seidel)
                                    (list #'mjr_mat_sis-jacobi)
                                    (list #'mjr_mat_sis-sor :sim-fun-parm 1.53))
          do (assert-equality #'mjr_eps_= 
                              #(1/2 0 3/2 0 5/2 0 3/2 0 1/2)
                              (apply #'mjr_mat_solve-sys-itr
                                     (list 
                                      (mjr_mat_+ (mjr_mat_make-identity 9 2)
                                                 (mjr_mat_make-diag #(1 1 1 1 1 1 1 1) 1)
                                                 (mjr_mat_make-diag #(1 1 1 1 1 1 1 1) -1))
                                      #(1 2 3 4 5 4 3 2 1)
                                      (mjr_vec_make-const 9 0)
                                      :show-progress nil
                                      :max-itr 1000 :xeps 1e-20 :yeps 1e-5
                                      :sim-fun #'mjr_mat_sis-jacobi)))
          )

;; Source: http://s-mat-pcs.oulu.fi/~mpa/matreng/
;; Retrieved: 2010-12-09
;; (mjr_mat_solve-sys-sim #2a((4 1 -1)(2 7 1)(1 -3 12)) #(3 19 31) #(0 0 0) :show-progress 't :max-itr 30 :xeps 1e-20 :yeps 1e-5 :sim-fun #'mjr_mat_sis-jacobi)
;;   x0 = #(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0)
;;   x1 = #(0.750000000000000d0 2.714285714285714d0 2.583333333333333d0)
;;   x2 = #(0.717261904761904d0 2.130952380952381d0 3.199404761904761d0)
;;   x3 = #(1.017113095238095d0 2.052295918367347d0 3.056299603174603d0)
;;   x4 = #(1.001000921201814d0 1.987067743764172d0 3.011647888321995d0)
;; 
;; ANS: #(1 2 3)


;; Source: http://people.fh-landshut.de/~maurer/numeth/node55.html
;; Retrieved: 2010-12-09
;; (mjr_mat_solve-sys-sim #2a((4 1 0)(2 4 1)(1 2 2)) #(6 13 11) #(0 0 0) :show-progress 't :max-itr 30 :xeps 1e-20 :yeps 1e-5 :sim-fun #'mjr_mat_sis-sor :sim-fun-parm 1.2)
;;  x0 = #(0.000000000000000d0 0.0000000000000000d0 0.000000000000000d0)
;;  x1 = #(1.800000071525573d0 2.8200000691413862d0 2.135999958992001d0)
;;  x2 = #(0.593999917030330d0 2.3388000291347533d0 3.009840057744981d0)
;;  x3 = #(0.979560023174289d0 1.9415519471597609d0 3.080433640759854d0)
;;  x4 = #(1.021622412888623d0 1.9745860719195756d0 3.001436534672301d0)
;;  x5 = #(1.003299695118323d0 2.0026720092594354d0 2.994526464608642d0)
;; 
;; ANS: #(1 2 3)


;; Source: http://s-mat-pcs.oulu.fi/~mpa/matreng/
;; Retrieved: 2010-12-09
;; (mjr_mat_solve-sys-sim #2a((4 1 -1)(2 7 1)(1 -3 12)) #(3 19 31) #(0 0 0) :show-progress 't :max-itr 30 :xeps 1e-20 :yeps 1e-5 :sim-fun #'mjr_mat_gauss-seidel)
;;  x0 = #(0.000000000000000d0 0.0000000000000000d0 0.000000000000000d0)
;;  x1 = #(0.750000000000000d0 2.5000000000000000d0 3.145833333333333d0)
;;  x2 = #(0.911458333333333d0 2.0044642857142856d0 3.008494543650793d0)
;;  x3 = #(1.001007564484127d0 1.9984986181972788d0 2.999540690842309d0)
;;  x4 = #(1.000260518161257d0 1.9999911818335963d0 2.999976085611627d0)
;; 
;; ANS: #(1 2 3)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
;; '(mjr_mat_det_matrix-properties)
 )
