;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-MandelbrotTGA2.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Compute Mandelbrot set, and dump it to a TGA file using a nice pallet.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @filedetails
;;
;;  No complex arithmetic for performance.  Nice pic.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop with xmax fixnum = (expt 2 11)
      with ymax fixnum = (expt 2 11)
      with grd = "0RMGY1CB0"
      with cmax fixnum = (1- (mjr_colorized_ut-gradient-length grd)) ;; so we get potentially as many levels as we have colors
      for cf fixnum in '(  10    50      2      2       1)
      for x0 float  in '(-2.0  -2.0  -0.70  -0.67  -0.642)
      for x1 float  in '( 2.0   2.0  -0.63  -0.63  -0.630)
      for y0 float  in '(-2.0  -2.0  -0.50  -0.40  -0.394)
      for y1 float  in '( 2.0   2.0  -0.40  -0.37  -0.373)
      for i from 1
      for fname = (format nil "exp-MandelbrotTGA2-OUT-~d.tga" i)
      do (format 't "~%Compute ~a~%" fname)
      do (time (let ((daData (mjr_fsamp_dq-func-r123-r123 (lambda (cx cy)
                                                            (mod (* cf (loop with lcx short-float = (float cx 1.0e0)
                                                                             with lcy short-float = (float cy 1.0e0)
                                                                             for tz short-float = lcx then (+ (- (* zx zx) (* zy zy)) lcx)
                                                                             for zy short-float = lcy then (+ (* 2 zx zy) lcy)
                                                                             for zx short-float = tz
                                                                             for ct fixnum from 1 upto cmax
                                                                             count 1
                                                                             while (and (< (+ (* zx zx) (* zy zy)) 4.0))))
                                                                 cmax))
                                                          :xdat (list :start x0 :end x1 :len xmax)                                             
                                                          :ydat (list :start y0 :end y1 :len ymax)
                                                          :f-color-meth grd :f-color-ano-typ :ano-typ-truvec)))
                 (mjr_tga_from-dquad fname daData))))


