;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotTGA1.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute Mandelbrot set, and dump it to a TGA file.@EOL
;; @Std       Common Lisp
;;
;;            An appropriate count is computed such that the histogram will be maximized in all three colors.  No complex arithmetic for performance.
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (let* ((xmax 1536)
             (ymax 1536)
             (smax 512)
             (bpp  (truncate (mjr_intu_log-floor smax 2) 3))
             (cmax (expt 2 (* 3 bpp)))
             (fac  (truncate 255 (expt 2 bpp)))
             (img  (mjr_img_make xmax ymax)))
        (declare (fixnum bpp smax fac xmax ymax cmax))
        (format 't "COUNT: ~d~%" cmax)
        (loop with left  short-float =  -2.0   ;;    -2   -0.70  -0.67  -0.642
              with top   short-float =   1.5   ;;     2    0.50   0.40   0.394
              with xside short-float =   3.0   ;;     4    0.07   0.04   0.012
              with yside short-float =   3.0   ;;     4    0.10   0.03   0.021
              with xscale short-float = (/ xside xmax)
              with yscale short-float = (/ yside ymax)
              for y fixnum from 0 upto (1- ymax)
              for cy short-float = (- top (* y yscale))
              do (loop for x fixnum from 0 upto (1- xmax)
                       for cx short-float = (+ (* x xscale) left)
                       do (loop for tz short-float = cx then (+ (- (* zx zx) (* zy zy)) cx)
                                for zy short-float = cy then (+ (* 2 zx zy) cy)
                                for zx short-float = tz
                                for cnt fixnum from 0
                                do (if (or (> (+ (* zx zx) (* zy zy)) 4.0) (>= cnt cmax))
                                       (return (mjr_img_set-px-color img x y (vector (* fac (ldb (byte bpp (* 0 bpp)) cnt))
                                                                                     (* fac (ldb (byte bpp (* 1 bpp)) cnt))
                                                                                     (* fac (ldb (byte bpp (* 2 bpp)) cnt)))))))))
        (mjr_img_tga-write "exp-MandelbrotTGA1-OUT.tga" img)))
