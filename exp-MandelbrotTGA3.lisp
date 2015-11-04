;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotTGA3.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute Mandelbrot set, and dump it to a TGA file using unique coloring scheme.@EOL
;; @Std       Common Lisp
;;
;;            Two TGAs are output:
;;               Gradient based color scheme is by the number of times each pixel is visited by the orbits computed.
;;               Povray height field based on the number of times each pixel is visited by the orbits computed.
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (let* ((img-xmax 1024)
             (img-ymax 1024)
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
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-gl.tga" (mjr_dquad_get-data-array daData "c-gl") :color-space :cs-rgb :color-unpacker #'identity)
          (format 't "pov colorize ocount~%")
          (mjr_dquad_colorize daData :data "ocount-log-max" :ano-nam "c-p" :color-method #'mjr_colorized_povray :max-color #xFFFF)
          (format 't "Dump P TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-p.tga" (mjr_dquad_get-data-array daData "c-p") :color-space :cs-tru :color-unpacker #'identity)
          (format 't "rainbow colorize ocount~%")
          (mjr_dquad_colorize daData :data "ocount" :ano-nam "c-g" :color-method "0RMGY1CB0")
          (format 't "Dump G TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-g.tga" (mjr_dquad_get-data-array daData "c-g") :color-space :cs-rgb :color-unpacker #'identity)
          (format 't "wacky scale compute (c-r)~%")
          (mjr_dquad_add-data-from-map daData (lambda (c) (* (truncate #xFFFFFF maxv) c)) :data 0 :ano-nam "c-r" :ano-typ :ano-typ-integer)
          (format 't "Dump R TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-r.tga" (mjr_dquad_get-data-array daData "c-r")))))