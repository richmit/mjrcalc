;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotDraw.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute Mandelbrot set, and draw it to the screen with MJR_VDRAW.@EOL
;; @Keywords  Mandelbrot set draw screen mjr_vdraw
;; @Std       Common Lisp
;;
;;            Note that MJR_VDRAW is designed for vector graphics, an using it for this kind of raster work is quite slow. An
;;            appropriate count is computed such that the histogram will be maximized in all three colors.  No complex
;;            arithmetic for performance.
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(let* ((xmax 750)
       (ymax 750)
       (smax 512)
       (bpp  (truncate (mjr_intu_log-floor smax 2) 3))
       (cmax (expt 2 (* 3 bpp)))
       (fac  (truncate 255 (expt 2 bpp))))
  (declare (fixnum bpp))
  (declare (fixnum smax))
  (declare (fixnum fac))
  (declare (fixnum xmax))
  (declare (fixnum ymax))
  (declare (fixnum cmax))
  (format 't "COUNT: ~d~%" cmax)
  (loop with left  short-float =  -0.642   ;;    -2   -0.70  -0.67  -0.642
        with top   short-float =  -0.394   ;;    -2   -0.50  -0.40  -0.394
        with xside short-float =   0.012   ;;     4    0.07   0.04   0.012
        with yside short-float =   0.021   ;;     4    0.10   0.03   0.021
        with xscale short-float = (/ xside xmax)
        with yscale short-float = (/ yside ymax)
        for y fixnum from 0 upto (1- ymax)
        for cy short-float = (+ (* y yscale) top)
        do (loop for x fixnum from 0 upto (1- xmax)
                 for cx short-float = (+ (* x xscale) left)
                 do (loop for tz short-float = 0.0 then (+ (- (* zx zx) (* zy zy)) cx)
                          for zy short-float = 0.0 then (+ (* 2 zx zy) cy)
                          for zx short-float = 0.0 then tz
                          for cnt fixnum from 0
                          do (if (or (> (+ (* zx zx) (* zy zy)) 4.0) (>= cnt cmax))
                                 (return (progn (mjr_vdraw_set-line-color (* fac (ldb (byte bpp (* 0 bpp)) cnt))
                                                                          (* fac (ldb (byte bpp (* 1 bpp)) cnt))
                                                                          (* fac (ldb (byte bpp (* 2 bpp)) cnt)))
                                                (mjr_vdraw_draw-point (float x) (float y)))))))))
