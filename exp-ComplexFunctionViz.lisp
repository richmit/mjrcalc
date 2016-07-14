;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ComplexFunctionViz.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Complex function visualization via VTK files.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2010,2011,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(mjr_vtk_from-dquad "exp-ComplexFunctionViz-OUT.vtk"
                    (mjr_dfunc_c1-c1 
                     ;;(lambda (z) (* (+ z 2) (+ z 2) (- z #C(1 2)) (+ z #C(0 1))))
                     ;;(lambda (z) (+ z (/ z)))
                     ;;(lambda (z) (/ (* (- z 2) (- z 2) (+ z #C(1 -2)) (+ z #C(2 2))) (expt z 3)))
                     ;;(lambda (z) (- (expt z 3) 1))
                     #'mjr_numu_gamma-lanczos
                     ;;(lambda (z) (expt (- (expt z 3) 1) 2))
                     ;;(lambda (z) (+ (/ (+ z 1)) (/ (- z 1))))
                     ;;(lambda (z) (- (/ (+ z 1)) (/ (- z 1))))
                     ;;(lambda (z) (/ (expt z 2)))
                     ;;(lambda (z) (mjr_poly_eval #(1/10 0 -1 1) z))
                     ;;(lambda (z) (expt z z))
                     ;;(lambda (z) (exp (- (z z))))
                     ;;(lambda (z) (+ z (/ (* z z) (sin (- (* z z z z) 1)))))
                     ;;(lambda (z) (* #C(1 1) (log (/ (sin (- (* z z z) 1)) z))))
                     ;;(lambda (z) (log (sin z)))
                     ;;(lambda (z) z)
                     :f_color
                     ;;nil
                     ;;#'mjr_colorizer_r2-1rgb1
                     ;;#'mjr_colorizer_r2-1r.gb.yc.m0
                     ;;#'mjr_colorizer_r2-1yr0
                     ;;#'mjr_colorizer_r2-gr
                     ;;#'mjr_colorizer_r2-hsl-thaller
                     ;;#'mjr_colorizer_r2-hsv-thaller
                     ;;#'mjr_colorizer_r2-hsv-full-v
                     #'mjr_colorizer_r2-hsl-richardson
                     ;;#'mjr_colorizer_r2-checker
                     :rdat (list :start -1.250 :end 1.250 :len 200)
                     :idat (list :start -1.250 :end 1.250 :len 200)))
