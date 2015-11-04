;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-color.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_color.@EOL
;; @Keywords  lisp interactive color space theory processing
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_COLOR-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_COLOR :MJR_PRNG :MJR_EPS))

(in-package :MJR_COLOR-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_color_convert-real-consistency

    (loop with step = 0.05
          for x from 0 upto 1 by step
          do (loop for y from 0 upto 1 by step
                   do (loop for z from 0 upto 1 by step
                            for v = (vector x y z)
                            do (assert-equality #'mjr_eps_= v (mjr_color_convert-hsv2rgb (mjr_color_convert-rgb2hsv v)))
                            do (assert-equality #'mjr_eps_= v (mjr_color_convert-hsl2rgb (mjr_color_convert-rgb2hsl v)))
                            do (cond
                                 ((< y step) (progn (assert-equality #'mjr_eps_= (vector 0 0 z) (mjr_color_convert-hsv2hsl (mjr_color_convert-hsl2hsv v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 z) (mjr_color_convert-rgb2hsl (mjr_color_convert-hsl2rgb v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 z) (mjr_color_convert-hsl2hsv (mjr_color_convert-hsv2hsl v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 z) (mjr_color_convert-rgb2hsv (mjr_color_convert-hsv2rgb v)))))
                                 ((< z step) (progn (assert-equality #'mjr_eps_= (vector 0 0 0) (mjr_color_convert-hsv2hsl (mjr_color_convert-hsl2hsv v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 0) (mjr_color_convert-rgb2hsl (mjr_color_convert-hsl2rgb v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 0) (mjr_color_convert-hsl2hsv (mjr_color_convert-hsv2hsl v)))
                                                    (assert-equality #'mjr_eps_= (vector 0 0 0) (mjr_color_convert-rgb2hsv (mjr_color_convert-hsv2rgb v)))))
                                 ('t         (progn (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-hsv2hsl (mjr_color_convert-hsl2hsv v)))
                                                    (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-rgb2hsl (mjr_color_convert-hsl2rgb v)))
                                                    (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-hsl2hsv (mjr_color_convert-hsv2hsl v)))
                                                    (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-rgb2hsv (mjr_color_convert-hsv2rgb v)))))))))
    )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_color_convert-int-consistency

    (loop with step = 0.4
          for x from 0 upto 1 by step
          do (loop for y from 0 upto 1 by step
                   do (loop for z from 0 upto 1 by step
                            do (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-tru2rgb (mjr_color_convert-rgb2tru (vector x y z))))
                            when (and (> y 0) (> z 0))
                            do (progn ;(assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-tru2hsl (mjr_color_convert-hsl2tru (vector x y z))))
                                      ;(assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-tru2hsv (mjr_color_convert-hsv2tru (vector x y z))))
                                      ))))

    (loop for x from 0 upto 255 by 8
          do (loop for y from 0 upto 255 by 8
                   do (loop for z from 0 upto 255 by 8
                            do (assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-rgb2tru (mjr_color_convert-tru2rgb (vector x y z))))
                            when (and (> y 0) (> z 0))
                            do (progn ;(assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-hsv2tru (mjr_color_convert-tru2hsv (vector x y z))))
                                      ;(assert-equality #'mjr_eps_= (vector x y z) (mjr_color_convert-hsl2tru (mjr_color_convert-tru2hsl (vector x y z))))
                                      ))))
    )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_color_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
