;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-arr.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-arr.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2004,2010,2012,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_ARR-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_ARR))

(in-package :MJR_ARR-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defvar tpm ) (setq tpm  #2A((1 2 3) (4 5 6) (7 8 9) (nil nil nil) (nil nil nil)
                             (nil nil nil) (nil nil nil) (nil nil nil)))                       ; integer, Tall padded matrix 9x3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_cbind
  (assert-equalp #2A((7 1) (8 2) (9 3))                               (mjr_arr_cbind #(7 8 9) #(1 2 3)))
   (assert-equalp #2A((1 2 3 1) (4 5 6 2) (7 8 9 3))                   (mjr_arr_cbind #2a((1 2 3)(4 5 6)(7 8 9)) #(1 2 3)))
   (assert-equalp m                                                    (mjr_arr_cbind #(1 4 7) #(2 5 8) #(3 6 9)))
   (assert-equalp #2A((1 2 3 1 2 3) (4 5 6 4 5 6) (7 8 9 7 8 8))       (mjr_arr_cbind m mi))
   (assert-equalp #2A((1 4) (2 5) (3 6))                               (mjr_arr_cbind v3 u3))
   (assert-equalp em                                                   (mjr_arr_cbind em em))
   ;; different sizes, but compatible
   (assert-equalp #2A((1 2 3 1 2) (4 5 6 4 5) (7 8 9 7 8))             (mjr_arr_cbind m tm))
   ;; different sizes, and incompatible
   (assert-error 'error                                                (mjr_arr_cbind m wm))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_rbind
  (assert-equalp m                                     (mjr_arr_rbind #(1 2 3) #(4 5 6) #(7 8 9)))
  (assert-equalp #2A((7 8 9) (1 2 3))                  (mjr_arr_rbind #(7 8 9) #(1 2 3)))
  (assert-equalp #2A((1 2 3) (4 5 6) (7 8 9) (1 2 3))  (mjr_arr_rbind #2a((1 2 3)(4 5 6)(7 8 9)) #(1 2 3)))
  1
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_get-subarray
  (assert-equalp #3A(((1 2 3) (4 5 6) (7 8 9)))  (mjr_arr_get-subarray #3a(((1 2 3)(4 5 6)(7 8 9))((10 20 30)(40 50 60)(70 80 90))((11 21 31)(41 51 61)(71 81 91))) 0 nil nil))
  (assert-equalp #3A(((4 5 6)))                  (mjr_arr_get-subarray #3a(((1 2 3)(4 5 6)(7 8 9))((10 20 30)(40 50 60)(70 80 90))((11 21 31)(41 51 61)(71 81 91))) 0 1 nil))
  (assert-equalp #2A((4 5 6))                    (mjr_arr_get-subarray #2a((1 2 3)(4 5 6)(7 8 9)) 1 nil))
  (assert-equalp #2A((2) (5) (8))                (mjr_arr_get-subarray #2a((1 2 3)(4 5 6)(7 8 9)) nil 1))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_reflow
  ;; Normal use
  (assert-equalp vwm                                      (mjr_arr_reflow m   '(1  9)))
  (assert-equalp vtm                                      (mjr_arr_reflow m   '(9  1)))
  ;; Target too small
  (assert-equalp #2A((1) (2) (3) (4) (5) (6) (7) (8))     (mjr_arr_reflow m   '(8  1)))
  (assert-error 'warning                                  (mjr_arr_reflow m   '(8  1)))
  (assert-equalp #2A((1) (2))                             (mjr_arr_reflow m   '(2  1)))
  (assert-error 'warning                                  (mjr_arr_reflow m   '(2  1)))
  (assert-equalp #2A((1 2) (3 4))                         (mjr_arr_reflow m   '(2  2)))
  (assert-error 'warning                                  (mjr_arr_reflow m   '(2  2)))
  (assert-equalp #2A((1 2))                               (mjr_arr_reflow m   '(1  2)))
  (assert-error 'warning                                  (mjr_arr_reflow m   '(1  2)))
  ;; Target too big
  (assert-equalp tpm                                      (mjr_arr_reflow m   '(8  3)))
  (assert-error 'warning                                  (mjr_arr_reflow m   '(8  3)))
  ;; Bad ncols and/or nrows arguments
  (assert-error 'error                                    (mjr_arr_reflow m   '(-1 9)))
  (assert-error 'error                                    (mjr_arr_reflow m   '(1  -1)))
  (assert-error 'error                                    (mjr_arr_reflow m   '(-1 0)))
  ;; Empty target matrix
  (assert-error 'error                                    (mjr_arr_reflow m   '(0  9)))
  (assert-error 'error                                    (mjr_arr_reflow m   '(1  0)))
  (assert-error 'error                                    (mjr_arr_reflow m   '(0  0)))
  ;; Bad matrix argument
  (assert-error 'error                                    (mjr_arr_reflow nil '(1  1)))
  )

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (define-test mjr_arr_get-row
;;   ;; Normal use
;;   (assert-equalp #(1 2 3)                      (mjr_arr_get-row m 0))
;;   (assert-equalp #(4 5 6)                      (mjr_arr_get-row m 1))
;;   (assert-equalp #(7 8 9)                      (mjr_arr_get-row m 2))
;;   ;; Non-square
;;   (assert-equalp #(1 2)                        (mjr_arr_get-row tm 0))
;;   (assert-equalp #(1 2 3)                      (mjr_arr_get-row wm 0))
;;   ;; The empty case
;;   (assert-equalp #()                           (mjr_arr_get-row em 0))
;;   ;; Out of bounds
;;   (assert-error 'error                         (mjr_arr_get-row m -1))
;;   (assert-error 'error                         (mjr_arr_get-row m  3))
;;   ;; Should be a column vector
;;   (assert-equalp #(1)                          (mjr_arr_get-row v3 0))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_get-rows
  ;; Normal use
  (assert-equalp '(#(1 2 3) #(4 5 6) #(7 8 9)) (mjr_arr_get-rows m))
)

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (define-test mjr_arr_get-col
;;   ;; Normal use
;;   (assert-equalp #(1 4 7)                      (mjr_arr_get-col m 0))
;;   (assert-equalp #(2 5 8)                      (mjr_arr_get-col m 1))
;;   (assert-equalp #(3 6 9)                      (mjr_arr_get-col m 2))
;;   ;; Non-square
;;   (assert-equalp #(1 4 7)                      (mjr_arr_get-col tm 0))
;;   (assert-equalp #(1 4)                        (mjr_arr_get-col wm 0))
;;   ;; The empty case
;;   (assert-equalp #()                           (mjr_arr_get-col em 0))
;;   ;; Out of bounds
;;   (assert-error 'error                         (mjr_arr_get-col m -1))
;;   (assert-error 'error                         (mjr_arr_get-col m  3))
;;   ;; Should be a column vector
;;   (assert-equalp #(1 2 3)                      (mjr_arr_get-col v3 0))
;;   (assert-equalp '(#(1 2 3))                   (mjr_arr_get-col v3 0 -1))
;;   ;; Out of bounds THE-COL on a vector is OK
;;   (assert-equalp #(1 2 3)                      (mjr_arr_get-col v3 100))
;;   (assert-equalp '(#(1 2 3))                   (mjr_arr_get-col v3 100 -1))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_get-cols
  ;; Normal use
  (assert-equalp '(#(1 4 7) #(2 5 8) #(3 6 9)) (mjr_arr_get-cols m))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_get-subarray2
  ;; Standard cases
  (assert-equalp #2a((1 2)(4 5)) (mjr_arr_get-subarray2 m  0 0   1  1))
  (assert-equalp #2a((1)(4)(7))  (mjr_arr_get-subarray2 m  0 0  -1  0))   ; get cols
  (assert-equalp #2a((2)(5)(8))  (mjr_arr_get-subarray2 m  0 1  -1  1))  ; get cols
  (assert-equalp #2a((3)(6)(9))  (mjr_arr_get-subarray2 m  0 2  -1  2))  ; get cols
  (assert-equalp #2a((1 2 3))    (mjr_arr_get-subarray2 m  0 0   0 -1))  ; get rows
  (assert-equalp #2a((4 5 6))    (mjr_arr_get-subarray2 m  1 0   1 -1))  ; get rows
  (assert-equalp #2a((7 8 9))    (mjr_arr_get-subarray2 m  2 0   2 -1))  ; get rows
  (assert-equalp #2a((1 2 3))    (mjr_arr_get-subarray2 wm 0 0   0 -1))  ; get rows. not square
  (assert-equalp #2a((4 5 6))    (mjr_arr_get-subarray2 wm 1 0   1 -1))  ; get rows. not square
  (assert-equalp #2a((1)(4)(7))  (mjr_arr_get-subarray2 tm 0 0  -1  0))  ; get cols. not square
  (assert-equalp #2a((2)(5)(8))  (mjr_arr_get-subarray2 tm 0 1  -1  1))  ; get cols. not square
  ;; Error cases
  (assert-error 'error (mjr_arr_get-subarray2 u3  3  0  3  0)) ; upper index too big
  (assert-error 'error (mjr_arr_get-subarray2 u3  3  0  5  0)) ; upper index too big
  (assert-error 'error (mjr_arr_get-subarray2 u3  2  0  2  1)) ; upper index too big
  (assert-error 'error (mjr_arr_get-subarray2 u3  2  0  2  1)) ; upper index too big
  (assert-error 'error (mjr_arr_get-subarray2 m   1  0  0  0)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m   0  1  0  0)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m   1  1  0  0)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m   2  0  1  0)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m   0  2  0  1)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m   2  2  1  1)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m5  2  0  1  0)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m5  0  2  0  1)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m5  2  2  1  1)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m5  0  0  5  0)) ; upper row index too big
  (assert-error 'error (mjr_arr_get-subarray2 m5  0  0  0  5)) ; upper col index too big
  (assert-error 'error (mjr_arr_get-subarray2 m5  0  0  9  0)) ; upper row index too big
  (assert-error 'error (mjr_arr_get-subarray2 m5  0  0  0  8)) ; upper col index too big
  (assert-error 'error (mjr_arr_get-subarray2 m5  2  2  1  1)) ; upper index smaller than lower one
  (assert-error 'error (mjr_arr_get-subarray2 m  -1  0 -1 -1)) ; index too small (negative)
  (assert-error 'error (mjr_arr_get-subarray2 m   0 -1 -1 -1)) ; index too small (negative)
  (assert-error 'error (mjr_arr_get-subarray2 m  -1 -1 -1 -1)) ; index too small (negative)
  (assert-error 'error (mjr_arr_get-subarray2 em -1  0 -1 -1)) ; index too small & matrix empty
  (assert-error 'error (mjr_arr_get-subarray2 em  0 -1 -1 -1)) ; index too small & matrix empty
  (assert-error 'error (mjr_arr_get-subarray2 em -1 -1 -1 -1)) ; index too small & matrix empty
  (assert-error 'error (mjr_arr_get-subarray2 em  0  0 -1 -1)) ; matrix empty
  (assert-error 'error (mjr_arr_get-subarray2 em  0  1 -1 -1)) ; matrix empty
  (assert-error 'error (mjr_arr_get-subarray2 em  1  0 -1 -1)) ; matrix empty
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_binary-map2
  ;; Don't need too test heavily as we will test it later in the functions that use this one
  (assert-equalp #2a((#C(1 1) #C(2 2) #C(3 3)) (#C(4 4) #C(5 5) #C(6 6)) (#C(7 7) #C(8 8) #C(9 9))) (mjr_arr_binary-map2 m m #'complex))
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 18))                                                  (mjr_arr_binary-map2 m m #'+))
  (assert-equalp #2A((1 4 9) (16 25 36) (49 64 81))                                                 (mjr_arr_binary-map2 m m #'*))
  (assert-equalp #2A()                                                                              (mjr_arr_binary-map2 em em #'*))
  ;; Make sure binary-map matches results from map.
  (assert-equalp (mjr_arr_map #'complex m m)                                                        (mjr_arr_binary-map2 m m #'complex))
  (assert-equalp (mjr_arr_map #'+ m m)                                                              (mjr_arr_binary-map2 m m #'+))
  (assert-equalp (mjr_arr_map #'* m m)                                                              (mjr_arr_binary-map2 m m #'*))
  (assert-equalp (mjr_arr_map #'* em em)                                                            (mjr_arr_binary-map2 em em #'*))
  ;; Sizes don't match
  (assert-error 'error                                                                              (mjr_arr_binary-map2 wm m #'*))
  ;; function takes only one arg
  (assert-error 'error                                                                              (mjr_arr_binary-map2 m m #'sin))
  ;; Array is not 2d
  (assert-error 'error                                                                              (mjr_arr_binary-map2 u3 u3 #'+))
  (assert-error 'error                                                                              (mjr_arr_binary-map2 #3a(((1))) #3a(((1))) #'+))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_unary-map2
  ;; Don't need too test heavily as we will test it later in the functions that use this one
  (assert-equalp m                                                                (mjr_arr_unary-map2 m #'identity))
  ;; Make sure unary-map matches results from map.
  (assert-equalp (mjr_arr_map #'identity m)                                       (mjr_arr_unary-map2 m #'identity))
  ;; function takes only two args
  (assert-error 'error                                                            (mjr_arr_unary-map2 m #'mod))
  ;; Array is not 2d
  (assert-error 'error                                                            (mjr_arr_unary-map2 u3 #'identity))
  (assert-error 'error                                                            (mjr_arr_unary-map2 #3a(((1))) #'identity))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_arr_map
  (assert-equalp #2a((#C(1 1) #C(2 2) #C(3 3)) (#C(4 4) #C(5 5) #C(6 6)) (#C(7 7) #C(8 8) #C(9 9))) (mjr_arr_map #'complex m m))
  (assert-equalp #2A((2 4 6) (8 10 12) (14 16 18))                                                  (mjr_arr_map #'+ m m))
  (assert-equalp #2A((1 4 9) (16 25 36) (49 64 81))                                                 (mjr_arr_map #'* m m))
  (assert-equalp #2A()                                                                              (mjr_arr_map #'* em em))
  (assert-equalp m                                                                                  (mjr_arr_map #'identity m))
  ;; Sizes don't match
  (assert-equalp #2A((1 4 9) (16 25 36))                                                            (mjr_arr_map #'* wm m))
  (assert-equalp #2A((1 4 9) (16 25 36) (nil nil nil))                                              (mjr_arr_map #'* m wm))
  (assert-error 'warning                                                                            (mjr_arr_map #'* m wm))
  ;; function takes diffrent number of args
  (assert-error 'error                                                                              (mjr_arr_map #'sin m m))
  (assert-error 'error                                                                              (mjr_arr_map #'mod m))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
