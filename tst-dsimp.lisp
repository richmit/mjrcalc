;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-dsimp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995-2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for use-dsimp.lisp@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_DSIMP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_DSIMP :MJR_UTIL))

(in-package :MJR_DSIMP-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar a '((4 0 2 0)
            #(#(0.0 0.0 0.0) #(1.0 0.0 0.0) #(1.0 1.0 0.0) #(0.0 1.0 0.0)              ;; 0-simplices  #(vertexes of cube)
              #(0.0 0.0 1.0) #(1.0 0.0 1.0) #(1.0 1.0 1.0) #(0.0 1.0 1.0))
            ((:gd-nam . "Heat") (:gd-typ . :gd-typ-integer))                           ;; First 0-simplices data set (integer)
            #(1 2 3 1 2 3 1 2)
            ((:gd-nam . "Peat") (:gd-typ . :gd-typ-real))                              ;; Second 0-simplices data set (real)
            #(3 0 1 6 4 2 9 5)
            ((:gd-nam . "HeatC") (:gd-typ . :gd-typ-color) (:gd-colorspace :cs-rgb))   ;; Third 0-simplices data set (Color)
            #(#(0.0 0.0 0.0) #(0.0 0.0 1.0) #(0.0 1.0 0.0) #(0.0 1.0 1.0)              
              #(1.0 0.0 0.0) #(1.0 0.0 1.0) #(1.0 1.0 0.0) #(1.0 1.0 1.0))
            ((:gd-nam . "Dir") (:gd-typ . :gd-typ-rvec))                               ;; Fourth 0-simplices data set (Real Vector)
            #(#(0.0 2.0 2.0) #(0.0 0.0 2.0) #(0.0 2.0 0.0) #(0.0 2.0 2.0)              
              #(2.0 0.0 0.0) #(2.0 0.0 2.0) #(2.0 2.0 0.0) #(2.0 2.0 2.0))
            nil                                                                        ;; 1-simplices
            #(#(0 1 2) #(0 2 3) #(1 5 2) #(0 4 3) #(4 7 3) #(4 7 5))                   ;; 2-simplices (two triangles on face of cube)
            ((:gd-nam . "Pie") (:gd-typ . :gd-typ-real))                               ;; First 2-simplices data set
            #(3.14 6.28 6.28 3.14 3.14 6.28)
            ((:gd-nam . "Cake") (:gd-typ . :gd-typ-real))                              ;; Second 2-simplices data set
            #(9.8596 39.4384 39.4384 9.8596 9.8596 39.4384 )  
            nil))                                                                      ;; 3-simplices
(mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0) 0 :gd-nam "zsn2")     ;; Fifth 0-simplices data set (Real)

;; (define-test mjr_dsimp_make-from-points
;; ((0 0 0 0) #(#(1 2 3) #(4 5 6)) NIL NIL NIL)    (mjr_dsimp_make-from-points #2a((1 2 3)(4 5 6)))
;; ((0 0 0 0) #(#(1 2 3) #(4 5 6)) NIL NIL NIL)    (mjr_dsimp_make-from-points #(#(1 2 3)#(4 5 6)))
;; ((0 0 0 0) #(#(1 2 3) #(4 5 6)) NIL NIL NIL)    (mjr_dsimp_make-from-points '(#(1 2 3)#(4 5 6)))
;; 'error                                          (mjr_dsimp_make-from-points #3a())
;; 'error                                          (mjr_dsimp_make-from-points 't)
;; 'error                                          (mjr_dsimp_make-from-points 1)

;; (define-test mjr_dsimp_add-data
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam "zsn2")
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0) -1 :gd-nam "zsn3")
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  4 :gd-nam "zsn3")
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  5 :gd-nam "zsn3")
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam nil)
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam 5)
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam "zsn2" :gd-typ :gd-type-realX)
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam "zsn2" :gd-typ :gd-type-real  :gd-colorspace :cs-tru)
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam "zsn2"                        :gd-colorspace :cs-tru)
;; 'error                                         (mjr_dsimp_add-data a (mjr_dsimp_map a  #'mjr_vec_norm-two -1 0)  0 :gd-nam "zsn2" :gd-typ :gd-type-color :gd-colorspace :cs-truX)


