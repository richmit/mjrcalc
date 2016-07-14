;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-dsimp.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-dsimp.lisp
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
(defpackage :MJR_DSIMP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_VEC :MJR_DSIMP))

(in-package :MJR_DSIMP-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar a nil)
(setq a '((4 0 2 0)
          #(#(0 0 0) #(1 0 0) #(1 1 0) #(0 1 0)                                      ;; 0-simplices  #(vertexes of cube)
            #(0 0 1) #(1 0 1) #(1 1 1) #(0 1 1))
          ((:ano-nam . "Heat") (:ano-typ . :ano-typ-integer))                           ;; #1 0-simplices data set (integer)
          #(1 2 3 1 2 3 1 2)
          ((:ano-nam . "Peat") (:ano-typ . :ano-typ-real))                              ;; #2 0-simplices data set (real)
          #(3 0 1 6 4 2 9 5)
          ((:ano-nam . "HeatC") (:ano-typ . :ano-typ-rgbvec))                           ;; #3 0-simplices data set (Color)
          #(#(0 0 0) #(0 0 1) #(0 1 0) #(0 1 1)
            #(1 0 0) #(1 0 1) #(1 1 0) #(1 1 1))
          ((:ano-nam . "Dir") (:ano-typ . :ano-typ-rvec))                               ;; #4 0-simplices data set (Real Vector)
          #(#(0 2 2) #(0 0 2) #(0 2 0) #(0 2 2)
            #(2 0 0) #(2 0 2) #(2 2 0) #(2 2 2))
          nil                                                                        ;; 1-simplices
          #(#(0 1 2) #(0 2 3) #(1 5 2) #(0 4 3) #(4 7 3) #(4 7 5))                   ;; 2-simplices (two triangles on face of cube)
          ((:ano-nam . "Pie") (:ano-typ . :ano-typ-real))                               ;; #1 2-simplices data set (real)
          #(3.14 6.28 6.28 3.14 3.14 6.28)
          ((:ano-nam . "Cake") (:ano-typ . :ano-typ-real))                              ;; #2 2-simplices data set (real)
          #(9.8596 39.4384 39.4384 9.8596 9.8596 39.4384 )
          nil))                                                                      ;; 3-simplices

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dsimp_make-from-points
  (assert-equalp '((0 0 0 0) #(#(1 2 3) #(5 6 7)) NIL NIL NIL)  (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8))))
  (assert-equalp '((0 0 0 0) #(#(1 2 3 4) #(5 6 7 8)) NIL NIL NIL)  (mjr_dsimp_make-from-points #( #(1 2 3 4)#(5 6 7 8)))) ;; rethink this...
  (assert-equalp '((0 0 0 0) #(#(1 2 3 4) #(5 6 7 8)) NIL NIL NIL)  (mjr_dsimp_make-from-points '( #(1 2 3 4)#(5 6 7 8)))) ;; rethink this...
  (assert-equalp '((0 0 0 0) #(#(1 2 3) #(5 6 7)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3) (5 6 7))))
  (assert-equalp '((0 0 0 0) #(#(1 2 3) #(5 6 7)) NIL NIL NIL)      (mjr_dsimp_make-from-points #( #(1 2 3)#(5 6 7))))
  (assert-equalp '((0 0 0 0) #(#(1 2 3) #(5 6 7)) NIL NIL NIL)      (mjr_dsimp_make-from-points '( #(1 2 3)#(5 6 7))))
  (assert-equalp '((0 0 0 0) #(#(1 2 0) #(5 6 0)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2) (5 6))))
  (assert-equalp '((0 0 0 0) #(#(1 2) #(5 6)) NIL NIL NIL)          (mjr_dsimp_make-from-points #( #(1 2)#(5 6))))         ;; rethink this...
  (assert-equalp '((0 0 0 0) #(#(1 2) #(5 6)) NIL NIL NIL)          (mjr_dsimp_make-from-points '( #(1 2)#(5 6))))         ;; rethink this...
  ;; Pick the cols
  (assert-equalp '((0 0 0 0) #(#(2 3 4) #(6 7 8)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :point-columns '(1 2 3)))
  (assert-equalp '((0 0 0 0) #(#(2 3 0) #(6 7 0)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :point-columns '(1 2)))
  (assert-equalp '((0 0 0 0) #(#(2 0 0) #(6 0 0)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :point-columns '(1)))
  (assert-equalp '((0 0 0 0) #(#(0 0 0) #(0 0 0)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :point-columns '()))
  ;; Zap y cord to zero
  (assert-equalp '((0 0 0 0) #(#(2 0 4) #(6 0 8)) NIL NIL NIL)      (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :point-columns '(1 -1 3)))
  ;; Add a data element
  (assert-equalp '((1 0 0 0) #(#(1 2 3) #(5 6 7))
                   ((:ANO-NAM . "foo")
                    (:ANO-TYP . :ANO-TYP-REAL)) #(4 8)
                   NIL NIL NIL)                                     (mjr_dsimp_make-from-points #2a((1 2 3 4) (5 6 7 8)) :data-columns 3 :data-column-names "foo"))
  
  ;; Errors
  (assert-error 'error                                            (mjr_dsimp_make-from-points nil))
  (assert-error 'error                                            (mjr_dsimp_make-from-points #()))
  (assert-error 'error                                            (mjr_dsimp_make-from-points #2a()))
  
  (assert-error 'error                                            (mjr_dsimp_make-from-points 't))
  (assert-error 'error                                            (mjr_dsimp_make-from-points 1))
)

(define-test mjr_dsimp_add-data-from-map
  ;; TODO: Add some non-error cases. ;)
  ;; Errors
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1  0  :ano-nam "Heat"))                          ;; name already exists
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1  -1 :ano-nam "aBcD"))                          ;; bad simplix dim
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1  14 :ano-nam "aBcD"))                          ;; bad simplix dim
;;(assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1  0))                                           ;; missing ano-nam TODO: FIX
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1   0 :ano-nam 5))                               ;; bad ano-nam
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1   0 :ano-nam "aBcD" :ano-typ :ano-type-realX)) ;; bad ano-typ
  (assert-error 'error       (mjr_dsimp_add-data-from-map a #'mjr_vec_norm-two -1   0 :ano-nam "aBcD" :ano-typ 1))               ;; bad ano-typ
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )

