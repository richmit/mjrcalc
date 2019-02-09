;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-dquad.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-dquad.lisp
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
(defpackage :MJR_DQUAD-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_DQUAD :MJR_UTIL))

(in-package :MJR_DQUAD-TESTS)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar t1 #2a((1 1/2 1/3)(2 2/3 2/5)(3 3/5 3/7)(5 5/7 5/11)))
(defvar t2 #2A((1 2 3 5)(1/2 2/3 3/5 5/7)(1/3 2/5 3/7 5/11)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar a)
(setq a (mjr_dquad_make-from-axis "x"                                               '(:start 1 :end 10 :len 10)
                                  '((:ano-nam . "y") (:ano-typ . :ano-typ-integer)) #(2 3 4 5)))
(mjr_dquad_add-data-from-map a (lambda (x y)        (+ x y))                     :axes 't              :arg-mode :arg-number :ano-nam "z1"     :ano-typ :ano-typ-real   )
(mjr_dquad_add-data-from-map a (lambda (xi yi)      (- xi yi))         :idxes 't                       :arg-mode :arg-number :ano-nam "z2"                            )
(mjr_dquad_add-data-from-map a (lambda (xi yi x y)  (* xi yi x y))     :idxes 't :axes 't              :arg-mode :arg-number :ano-nam "z3"                            )
(mjr_dquad_add-data-from-map a (lambda (x)          (* x x))                              :data '(0)   :arg-mode :arg-number :ano-nam "z1^2"   :ano-typ :ano-typ-integer)
(mjr_dquad_add-data-from-map a (lambda (x y)        (* x y))                              :data '(0 1) :arg-mode :arg-number :ano-nam "z1*z2"  :ano-typ :ano-typ-real   )
(mjr_dquad_add-data-from-map a (lambda (L)          (apply #'+ L))                        :data '(0 1) :arg-mode :arg-list   :ano-nam "z1+z2"  :ano-typ :ano-typ-real   )
(mjr_dquad_add-data-from-map a (lambda (x y z1)     (+ x y z1))                  :axes 't :data '(0)   :arg-mode :arg-number :ano-nam "x+y+z1" :ano-typ :ano-typ-real   )

(defvar a-numa   2)
(defvar a-numd   7)
(defvar a-lena   '(10 4))
(defvar a-x      #(1 2 3 4 5 6 7 8 9 10))
(defvar a-y      #(2 3 4 5))
(defvar a-z1     #2A((3 4 5 6) (4 5 6 7) (5 6 7 8) (6 7 8 9) (7 8 9 10) (8 9 10 11) (9 10 11 12) (10 11 12 13) (11 12 13 14) (12 13 14 15)))
(defvar a-z2     #2A((0 -1 -2 -3) (1 0 -1 -2) (2 1 0 -1) (3 2 1 0) (4 3 2 1) (5 4 3 2) (6 5 4 3) (7 6 5 4) (8 7 6 5) (9 8 7 6)))
(defvar a-z3     #2A((0 0 0 0) (0 6 16 30) (0 18 48 90) (0 36 96 180) (0 60 160 300) (0 90 240 450) (0 126 336 630) (0 168 448 840) (0 216 576 1080) (0 270 720 1350)))
(defvar a-z1^2   #2A((9 16 25 36) (16 25 36 49) (25 36 49 64) (36 49 64 81) (49 64 81 100) (64 81 100 121) (81 100 121 144) (100 121 144 169) (121 144 169 196) (144 169 196 225)))
(defvar a-z1*z2  #2A((0 -4 -10 -18) (4 0 -6 -14) (10 6 0 -8) (18 14 8 0) (28 24 18 10) (40 36 30 22) (54 50 44 36) (70 66 60 52) (88 84 78 70) (108 104 98 90)))
(defvar a-z1+z2  #2A((3 3 3 3) (5 5 5 5) (7 7 7 7) (9 9 9 9) (11 11 11 11) (13 13 13 13) (15 15 15 15) (17 17 17 17) (19 19 19 19) (21 21 21 21)))
(defvar a-x+y+z1 #2A((6 8 10 12) (8 10 12 14) (10 12 14 16) (12 14 16 18) (14 16 18 20) (16 18 20 22) (18 20 22 24) (20 22 24 26) (22 24 26 28) (24 26 28 30)))

(defvar a-x-n      "x")
(defvar a-y-n      "y")
(defvar a-z1-n     "z1")
(defvar a-z2-n     "z2")
(defvar a-z3-n     "z3")
(defvar a-z1^2-n   "z1^2")
(defvar a-z1*z2-n  "z1*z2")
(defvar a-z1+z2-n  "z1+z2")
(defvar a-x+y+z1-n "x+y+z1")

(defvar a-x-t      :ano-typ-real)
(defvar a-x-a      '((:ANO-NAM . "x") (:ANO-TYP . :ANO-TYP-REAL)))
(defvar a-y-t      :ano-typ-integer)
(defvar a-z1-t     :ano-typ-real)
(defvar a-z1-a     '((:ANO-NAM . "z1") (:ANO-TYP . :ANO-TYP-REAL)))
(defvar a-z2-t     :ano-typ-real)
(defvar a-z3-t     :ano-typ-real)
(defvar a-z1^2-t   :ano-typ-integer)
(defvar a-z1*z2-t  :ano-typ-real)
(defvar a-z1+z2-t  :ano-typ-real)
(defvar a-x+y+z1-t :ano-typ-real)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar b)
(setq b (mjr_dquad_make-from-axis "x" (list :start (* -2 pi) :end (* 2 pi) :len 100) "y" (list :start (* -2 pi) :end (* 2 pi) :len 100)))
(mjr_dquad_add-data-from-map b (lambda (x y) (sin (sqrt (+ (* x x) (* y y))))) :axes 't :arg-mode :arg-number :ano-nam "sindst" :ano-typ :ano-typ-real)
(mjr_dquad_colorize b :data '(0) :color-method "rgb" :max-color 1 :auto-scale 't :ano-nam "cRGB" :ano-typ :ano-typ-rgbvec)
(mjr_dquad_colorize b :data '(0) :color-method "01"  :max-color 1 :auto-scale 't :ano-nam "cBW"  :ano-typ :ano-typ-rgbvec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar c '(1
            ((:ano-nam . "X Values") (:ano-typ . :ano-typ-real))                      #(1 2 3)
            ((:ano-nam . "Y Values") (:ano-typ . :ano-typ-real))                      #(1 4 9)
            ((:ano-nam . "C") (:ano-typ . :ano-typ-rgbvec))                           #(#(0.1 0.2 0.3) #(0.4 0.5 0.6) #(0.7 0.8 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar d nil)
(setq d (mjr_dquad_make-from-axis "x" '(:start -2 :end 2 :len 5) "y" '(:start -2 :end 2 :len 5)))
(mjr_dquad_add-data-from-map d (lambda (x y) (* x y)) :axes 't :ano-nam "z")
(mjr_dquad_add-data-from-map d (lambda (x y) (vector (* x x) (* y y) (* x y))) :axes 't :ano-nam "uvw")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar e nil)
(setq e (mjr_dquad_make-from-axis "u" '(:start 0 :end 7.0 :len 5)))
(mjr_dquad_add-data-from-map e (lambda (u) (vector (sin u) (cos u) (/ u 7))) :axes 't :ano-nam "xyz")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_read-from-file
  (assert-equalp a (mjr_dquad_read-from-file "tst-dquad-REG-a.qgd"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_write-to-file
  (let ((p (probe-file "tst-dquad-OUT-a.qgd"))) (if p (delete-file p)))
  (mjr_dquad_write-to-file a "tst-dquad-OUT-a.qgd")
  (assert-equal (mjr_util_read-file "tst-dquad-OUT-a.qgd")
                (mjr_util_read-file "tst-dquad-REG-a.qgd"))

  (let ((p (probe-file "tst-dquad-OUT-c.qgd"))) (if p (delete-file p)))
  (mjr_dquad_write-to-file a "tst-dquad-OUT-c.qgd")
  (assert-equal (mjr_util_read-file "tst-dquad-OUT-c.qgd")
                (mjr_util_read-file "tst-dquad-REG-c.qgd"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_data-array-size
  (assert-equalp a-lena (mjr_dquad_data-array-size a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_axis-vector-lengths
  (assert-equalp a-lena (mjr_dquad_axis-vector-lengths a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_data-count
  (assert-equal a-numd (mjr_dquad_data-count a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_axis-count
  (assert-equal a-numa (mjr_dquad_axis-count a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_get-data-array

  ;; Ints
  (assert-equalp a-z1     (mjr_dquad_get-data-array a 0))
  (assert-equalp a-z2     (mjr_dquad_get-data-array a 1))
  (assert-equalp a-z3     (mjr_dquad_get-data-array a 2))
  (assert-equalp a-z1^2   (mjr_dquad_get-data-array a 3))
  (assert-equalp a-z1*z2  (mjr_dquad_get-data-array a 4))
  (assert-equalp a-z1+z2  (mjr_dquad_get-data-array a 5))
  (assert-equalp a-x+y+z1 (mjr_dquad_get-data-array a 6))
  ;; Errors
  (assert-error 'error     (mjr_dquad_get-data-array a 't))
  (assert-error 'error     (mjr_dquad_get-data-array a nil))
  (assert-error 'error     (mjr_dquad_get-data-array a 1.4))
  (assert-error 'error     (mjr_dquad_get-data-array a -1))
  (assert-error 'error     (mjr_dquad_get-data-array a 7))
  ;; Strings
  (assert-equalp a-z1     (mjr_dquad_get-data-array a "z1"))
  (assert-equalp a-z2     (mjr_dquad_get-data-array a "z2"))
  (assert-equalp a-z3     (mjr_dquad_get-data-array a "z3"))
  (assert-equalp a-z1^2   (mjr_dquad_get-data-array a "z1^2"))
  (assert-equalp a-z1*z2  (mjr_dquad_get-data-array a "z1*z2"))
  (assert-equalp a-z1+z2  (mjr_dquad_get-data-array a "z1+z2"))
  (assert-equalp a-x+y+z1 (mjr_dquad_get-data-array a "x+y+z1"))
  ;; Missing
  (assert-equalp nil      (mjr_dquad_get-data-array a "missing"))
  (assert-equalp nil      (mjr_dquad_get-data-array a ""))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_get-axis-vector
  ;; Ints
  (assert-equalp a-x    (mjr_dquad_get-axis-vector a 0))
  (assert-equalp a-y    (mjr_dquad_get-axis-vector a 1))
  ;; Errors
  (assert-error 'error  (mjr_dquad_get-axis-vector a -1))
  (assert-error 'error  (mjr_dquad_get-axis-vector a 5))
  ;; Strings
  (assert-equalp a-x     (mjr_dquad_get-axis-vector a "x"))
  (assert-equalp a-y     (mjr_dquad_get-axis-vector a "y"))
  ;; Missing
  (assert-equalp nil      (mjr_dquad_get-axis-vector a "missing"))
  (assert-equalp nil      (mjr_dquad_get-axis-vector a ""))
  ;; Errors
  (assert-error 'error    (mjr_dquad_get-axis-vector a 't))
  (assert-error 'error    (mjr_dquad_get-axis-vector a nil))
  (assert-error 'error    (mjr_dquad_get-axis-vector a 1.4))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_get-axis-ano
  (assert-equalp a-x-n      (mjr_dquad_get-axis-ano a 0   :ano-nam))
  (assert-equalp a-y-n      (mjr_dquad_get-axis-ano a 1   :ano-nam))
  (assert-equalp a-x-t      (mjr_dquad_get-axis-ano a 0   :ano-typ))
  (assert-equalp a-y-t      (mjr_dquad_get-axis-ano a 1   :ano-typ))
  (assert-equalp a-x-a      (mjr_dquad_get-axis-ano a 0   nil ))
  ;; Errors (bad index)
  (assert-error 'error      (mjr_dquad_get-axis-ano a 't  :ano-nam))
  (assert-error 'error      (mjr_dquad_get-axis-ano a nil :ano-nam))
  (assert-error 'error      (mjr_dquad_get-axis-ano a 1.4 :ano-nam))
  (assert-error 'error      (mjr_dquad_get-axis-ano a -1  :ano-nam))
  (assert-error 'error      (mjr_dquad_get-axis-ano a 5   :ano-nam))
  ;; Errors (bad attr name) 
  (assert-error 'error      (mjr_dquad_get-axis-ano a 0   't  ))
  (assert-error 'error      (mjr_dquad_get-axis-ano a 0   1.4 ))
  (assert-error 'error      (mjr_dquad_get-axis-ano a 0   -1  ))
  (assert-error 'error      (mjr_dquad_get-axis-ano a 0   5   ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_get-data-ano
  (assert-equalp a-z1-t     (mjr_dquad_get-data-ano a 0   :ano-typ))
  (assert-equalp a-z2-t     (mjr_dquad_get-data-ano a 1   :ano-typ))
  (assert-equalp a-z3-t     (mjr_dquad_get-data-ano a 2   :ano-typ))
  (assert-equalp a-z1^2-t   (mjr_dquad_get-data-ano a 3   :ano-typ))
  (assert-equalp a-z1*z2-t  (mjr_dquad_get-data-ano a 4   :ano-typ))
  (assert-equalp a-z1+z2-t  (mjr_dquad_get-data-ano a 5   :ano-typ))
  (assert-equalp a-x+y+z1-t (mjr_dquad_get-data-ano a 6   :ano-typ))
  (assert-equalp a-z1-n     (mjr_dquad_get-data-ano a 0   :ano-nam))
  (assert-equalp a-z2-n     (mjr_dquad_get-data-ano a 1   :ano-nam))
  (assert-equalp a-z3-n     (mjr_dquad_get-data-ano a 2   :ano-nam))
  (assert-equalp a-z1^2-n   (mjr_dquad_get-data-ano a 3   :ano-nam))
  (assert-equalp a-z1*z2-n  (mjr_dquad_get-data-ano a 4   :ano-nam))
  (assert-equalp a-z1+z2-n  (mjr_dquad_get-data-ano a 5   :ano-nam))
  (assert-equalp a-x+y+z1-n (mjr_dquad_get-data-ano a 6   :ano-nam))
  (assert-equalp a-z1-a     (mjr_dquad_get-data-ano a 0   nil))
  ;; Errors (bad index)
  (assert-error 'error  (mjr_dquad_get-data-ano a 't  :ano-nam))
  (assert-error 'error  (mjr_dquad_get-data-ano a nil :ano-nam))
  (assert-error 'error  (mjr_dquad_get-data-ano a 1.4 :ano-nam))
  (assert-error 'error  (mjr_dquad_get-data-ano a -1  :ano-nam))
  (assert-error 'error  (mjr_dquad_get-data-ano a 7   :ano-nam))
  ;; Errors (bad attr name)
  (assert-error 'error  (mjr_dquad_get-data-ano a 0 't  ))

  (assert-error 'error  (mjr_dquad_get-data-ano a 0 1.4 ))
  (assert-error 'error  (mjr_dquad_get-data-ano a 0 -1  ))
  (assert-error 'error  (mjr_dquad_get-data-ano a 0 7   ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_copy
  (assert-equalp a (mjr_dquad_copy a))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_dquad_slab
  (assert-equalp a                                                                                                (mjr_dquad_slab a nil nil))
  (assert-equalp '(1
                   ((:ANO-NAM . "x")      (:ANO-TYP . :ANO-TYP-REAL))    #(1 2 3 4 5 6 7 8 9 10)
                   ((:ANO-NAM . "z1")     (:ANO-TYP . :ANO-TYP-REAL))    #(4 5 6 7 8 9 10 11 12 13)
                   ((:ANO-NAM . "z2")     (:ANO-TYP . :ANO-TYP-REAL))    #(-1 0 1 2 3 4 5 6 7 8)
                   ((:ANO-NAM . "z3")     (:ANO-TYP . :ANO-TYP-REAL))    #(0 6 18 36 60 90 126 168 216 270)
                   ((:ANO-NAM . "z1^2")   (:ANO-TYP . :ANO-TYP-INTEGER)) #(16 25 36 49 64 81 100 121 144 169)
                   ((:ANO-NAM . "z1*z2")  (:ANO-TYP . :ANO-TYP-REAL))    #(-4 0 6 14 24 36 50 66 84 104)
                   ((:ANO-NAM . "z1+z2")  (:ANO-TYP . :ANO-TYP-REAL))    #(3 5 7 9 11 13 15 17 19 21)
                   ((:ANO-NAM . "x+y+z1") (:ANO-TYP . :ANO-TYP-REAL))    #(8 10 12 14 16 18 20 22 24 26))            (mjr_dquad_slab a nil 1))
  (assert-equalp '(1
                   ((:ANO-NAM . "y")      (:ANO-TYP . :ANO-TYP-INTEGER)) #(2 3 4 5)
                   ((:ANO-NAM . "z1")     (:ANO-TYP . :ANO-TYP-REAL))    #(4 5 6 7)
                   ((:ANO-NAM . "z2")     (:ANO-TYP . :ANO-TYP-REAL))    #(1 0 -1 -2)
                   ((:ANO-NAM . "z3")     (:ANO-TYP . :ANO-TYP-REAL))    #(0 6 16 30)
                   ((:ANO-NAM . "z1^2")   (:ANO-TYP . :ANO-TYP-INTEGER)) #(16 25 36 49)
                   ((:ANO-NAM . "z1*z2")  (:ANO-TYP . :ANO-TYP-REAL))    #(4 0 -6 -14)
                   ((:ANO-NAM . "z1+z2")  (:ANO-TYP . :ANO-TYP-REAL))    #(5 5 5 5)
                   ((:ANO-NAM . "x+y+z1") (:ANO-TYP . :ANO-TYP-REAL))    #(8 10 12 14))                              (mjr_dquad_slab a 1   nil))
  ;; Errors
  (assert-error 'error  (mjr_dquad_slab a 1 1))
  (assert-error 'error  (mjr_dquad_slab a nil -1))
  (assert-error 'error  (mjr_dquad_slab a nil 4))
  (assert-error 'error  (mjr_dquad_slab a 10 nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
