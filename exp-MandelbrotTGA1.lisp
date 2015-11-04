;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-MandelbrotTGA1.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Compute Mandelbrot set, and dump it to a TGA file.@EOL
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
;;  An appropriate count is computed such that the histogram will be maximized in all three colors.  No complex arithmetic for performance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
