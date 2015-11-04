;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-MandelbrotTGA3.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Compute Mandelbrot set, and dump it to a TGA file using unique coloring scheme.@EOL
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
;;  Two TGAs are output:
;;    Gradient based color scheme is by the number of times each pixel is visited by the orbits computed.
;;    Povray height field based on the number of times each pixel is visited by the orbits computed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let* ((img-xmax (expt 2 10))
             (img-ymax (expt 2 10))
             (smp-xmax (* 4 img-xmax))
             (smp-ymax (* 4 img-ymax))
             (cmax     512)
             (left   -2.0)
             (top     1.5)
             (xside   3.0)
             (yside   3.0)
             (daData  (mjr_dquad_make-from-axis "x" (list :start left :end (+ left xside) :len img-xmax)
                                                "y" (list :start top  :end (- top yside)  :len img-ymax)))
             (ocount  (make-array (list img-xmax img-ymax)))); :element-type 'fixnum)))
        (declare (fixnum smp-xmax smp-ymax img-xmax img-ymax cmax))
        (declare (short-float left top xside yside))
        (format 't "Mandelbrot Compute~%")
        (let ((maxv (loop with smp-xscale short-float = (/ xside smp-xmax)
                          with smp-yscale short-float = (/ yside smp-ymax)
                          with img-xscale short-float = (/ xside img-xmax)
                          with img-yscale short-float = (/ yside img-ymax)
                          for y fixnum from 0 upto (1- smp-ymax)
                          for cy short-float = (- top (* y smp-yscale))
                          do (format 't "LINE: ~a~%" y)
                          maximize (loop for x fixnum from 0 upto (1- smp-xmax)
                                         for cx short-float = (+ (* x smp-xscale) left)
                                         maximize (loop for tz short-float = 0.0 then (+ (- (* zx zx) (* zy zy)) cx)
                                                        for zy short-float = cy then (+ (* 2 zx zy) cy)
                                                        for zx short-float = cx then tz
                                                        for cnt fixnum from 0 upto cmax
                                                        for zxi fixnum = (truncate (- zx left) img-xscale)
                                                        for zyi fixnum = (truncate (+ zy  top) img-yscale)
                                                        when (array-in-bounds-p ocount zxi zyi)
                                                        maximize (incf (aref ocount zxi zyi))
                                                        until (> (+ (* zx zx) (* zy zy)) 4))))))
          (mjr_dquad_add-data daData ocount :ano-nam "ocount" :ano-typ :ano-typ-integer)
          (format 't "log-max compute~%")
          (mjr_dquad_add-data-from-map daData (lambda (c) (log (max 1 c))) :data 0 :ano-nam "ocount-log-max" :ano-typ :ano-typ-real)
          (format 't "rainbow colorize ocount-log-max~%")
          (mjr_dquad_colorize daData :data "ocount-log-max" :ano-nam "c-gl" :color-method "0RMGY1CB0")
          (format 't "Dump GL TGA~%")
          (mjr_tga_from-dquad "exp-MandelbrotTGA3-OUT-gl.tga" daData :data "c-gl")
          (format 't "pov colorize ocount~%")
          (mjr_dquad_colorize daData :data "ocount-log-max" :ano-nam "c-p" :color-method #'mjr_colorized_povray :max-color #xFFFF)
          (format 't "Dump P TGA~%")
          (mjr_tga_from-dquad "exp-MandelbrotTGA3-OUT-p.tga" daData :data "c-p")
          (format 't "rainbow colorize ocount~%")
          (mjr_dquad_colorize daData :data "ocount" :ano-nam "c-g" :color-method "0RMGY1CB0")
          (format 't "Dump G TGA~%")
          (mjr_tga_from-dquad "exp-MandelbrotTGA3-OUT-g.tga" daData :data "c-g")
          (format 't "wacky scale compute (c-r)~%")
          (mjr_dquad_add-data-from-map daData (lambda (c) (* (truncate #xFFFFFF maxv) c)) :data 0 :ano-nam "c-r" :ano-typ :ano-typ-truint)
          (format 't "Dump R TGA~%")
          (mjr_tga_from-dquad "exp-MandelbrotTGA3-OUT-r.tga" daData :data "c-r"))))
