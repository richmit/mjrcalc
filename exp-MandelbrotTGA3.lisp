;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotTGA3.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute Mandelbrot set, and dump it to a TGA file using unique coloring scheme.@EOL
;; @Keywords  mandelbrot tga
;; @Std       Common Lisp
;;
;;            Two TGAs are output:
;;               Gradient based color scheme is by the number of times each pixel is visited by the orbits computed.
;;               Povray height field based on the number of times each pixel is visited by the orbits computed.
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (let* ((img-xmax 1024)
             (img-ymax 1024)
             (smp-xmax (* 4 img-xmax))
             (smp-ymax (* 4 img-ymax))
             (cmax     512)
             (img      (mjr_img_make img-xmax img-ymax :color-space :cs-tru)))
        (declare (fixnum smp-xmax smp-ymax img-xmax img-ymax cmax))
        (format 't "Mandelbrot Compute~%")
        (let ((maxv (loop with left  short-float =  -2.0
                          with top   short-float =   1.5
                          with xside short-float =   3.0
                          with yside short-float =   3.0
                          with smp-xscale short-float = (/ xside smp-xmax)
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
                                                        when (array-in-bounds-p img zxi zyi)
                                                        maximize (incf (aref img zxi zyi))
                                                        until (> (+ (* zx zx) (* zy zy)) 4))))))
          (format 't "Dump GL TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-gl.tga" (mjr_img_make-from-func-r2-r1 (lambda (x y) (log (max 1 (aref img x y))))
                                                                              :xdat (list :len img-xmax) :ydat (list :len img-ymax)
                                                                              :color-method "0RMGY1CB0" :auto-scale 't))
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-p.tga" (mjr_img_make-from-gndata img :color-method #'mjr_colorized_povray :auto-scale (list 0 maxv) :max-color #xFFFF))
          (format 't "Dump G TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-g.tga" (mjr_img_make-from-gndata img :color-method "0RMGY1CB0" :auto-scale 't))
          (format 't "Wacky Scale~%")
          (dotimes (y img-ymax)
            (dotimes (x img-xmax)
              (setf (aref img x y) (* (truncate #xFFFFFF maxv) (aref img x y)))))
          (format 't "Dump R TGA~%")
          (mjr_img_tga-write "exp-MandelbrotTGA3-OUT-r.tga" img))))
