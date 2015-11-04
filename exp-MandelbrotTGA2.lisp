;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotTGA2.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute Mandelbrot set, and dump it to a TGA file using a nice pallet.@EOL
;; @Keywords  Mandelbrot set TGA
;; @Std       Common Lisp
;;
;;            No complex arithmetic for performance.  Nice pic.
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (let* ((xmax 1536)
             (ymax 1536)
             (grd  "0RMGY1CB0")
             (cmax (1- (mjr_colorized_ut-gradient-length grd))))
        (declare (fixnum xmax ymax cmax))
        (mjr_img_tga-write "exp-MandelbrotTGA2-OUT.tga"
                           (mjr_img_make-from-func-r2-r1 (lambda (cx cy)
                                                           (mod (* 10 (loop with lcx short-float = (float cx 1.0e0)
                                                                            with lcy short-float = (float cy 1.0e0)
                                                                            for tz short-float = lcx then (+ (- (* zx zx) (* zy zy)) lcx)
                                                                            for zy short-float = lcy then (+ (* 2 zx zy) lcy)
                                                                            for zx short-float = tz
                                                                            for ct fixnum from 1 upto cmax
                                                                            count 1
                                                                            while (and (< (+ (* zx zx) (* zy zy)) 4.0))))
                                                                cmax))
                                                         :xdat (list :start     -2.0   ;;    -2  -0.70  -0.67  -0.642
                                                                     :end        1.0   ;;     2  -0.63  -0.63  -0.630
                                                                     :len xmax)                                             
                                                         :ydat (list :start     -1.5   ;;    -2  -0.50  -0.40  -0.394
                                                                     :end        1.5   ;;     2  -0.40  -0.37  -0.373
                                                                     :len ymax)
                                                         :auto-scale 't ;; while count will always be in range, we auto-scale to maximize the contrast
                                                         :color-method grd))))


