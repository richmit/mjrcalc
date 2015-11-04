;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-color.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-color.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2010,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_COLOR-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_COLOR :MJR_EPS))

(in-package :MJR_COLOR-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_color_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
