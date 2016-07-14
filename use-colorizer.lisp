;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-colorizer.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Colorization of continuous spaces (R, C, R^2, R^3, C, I, I^2, and I^3).@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2010,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_COLORIZER
  (:USE :COMMON-LISP
        :MJR_COLOR
        :MJR_NUMU
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Colorization of continuous spaces.;")
  (:EXPORT #:mjr_colorizer_help
           ;; Utility functions: gradients & multi-gradients
           #:mjr_colorizer_ut-rgb-from-gradient
           #:mjr_colorizer_ut-rgb-from-multi-gradient
           ;; Factory functions: gradients & multi-gradients
           #:mjr_colorizer_factory-from-gradient
           #:mjr_colorizer_factory-from-multi-gradient
           ;; Filter functions used to modify inputs and then pass the result to a color function
           #:mjr_colorizer_filter_log #:mjr_colorizer_filter_scale
           #:mjr_colorizer_filter_shift #:mjr_colorizer_filter_shift-log #:mjr_colorizer_filter_zero-log
           ;; Colorize $R^1$
           #:mjr_colorizer_r1-rgb #:mjr_colorizer_r1-checker
           ;; Colorize $R^2$
           #:mjr_colorizer_r2-hsl-richardson #:mjr_colorizer_r2-hsl-thaller
           #:mjr_colorizer_r2-hsv-thaller    #:mjr_colorizer_r2-hsv-full-v
           #:mjr_colorizer_r2-1r.gb.yc.m0    #:mjr_colorizer_r2-1yr0    #:mjr_colorizer_r2-1rgb1  #:mjr_colorizer_r2-gr
           #:mjr_colorizer_r2-checker        #:mjr_colorizer_r2-strip-x #:mjr_colorizer_r2-strip-y
           ;; Colorize $R^3$
           #:mjr_colorizer_r3-rgb
           ;; Colorize $I=[0,1]$
           #:mjr_colorizer_i1-rgb-cube  #:mjr_colorizer_i1-rg    #:mjr_colorizer_i1-hsv-cone
           ;; Colorize $I^2$
           #:mjr_colorizer_i2-rgb-cube  #:mjr_colorizer_i2-rg-yb #:mjr_colorizer_i2-hsv-cone #:mjr_colorizer_i2-hsl-cone
           ;; Colorize $I^3$
           #:mjr_colorizer_i3-rgb-cube  #:mjr_colorizer_i3-rg-yb-cm
           ))

(in-package :MJR_COLORIZER)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_filter_log (func &rest rest)
  "Compute the logarithm of each element of REST, and apply FUNC to the result"
  (apply func (mapcar #'log rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_filter_scale (func scale-list &rest rest)
  "Multiply the scale list components to the corresponding values in rest, and  apply FUNC to the result"
  (apply func (mapcar #'* scale-list rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_filter_shift (func shift-list &rest rest)
  "Add the shift list components to the corresponding values in rest, and  apply FUNC to the result"
  (apply func (mapcar #'+ shift-list rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_filter_shift-log (func s &rest rest)
  "Add S and compute the logarithm of each element of REST, and apply FUNC to the result"
  (apply func (mapcar (lambda (x) (log (+ s x))) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_filter_zero-log (func &rest rest)
  "Compute logarithm of non-zero elements of REST, and apply FUNC to the result"
  (apply func (mapcar (lambda (x) (if (zerop x) 0 (log x))) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_help ()
 "Colorize euclidean spaces of dimensions one, two, and three -- used for visualization.

All colors are returned as :cs-rgb (i.e. RGB components are real numbers in [0,1]).

For integer RGB components in [0,255], see :MJR_COLORIZED.
For color theory computations (space conversions, mixing, etc...), see :MJR_COLOR

Several common gradients include (see ramCanvas for more info):

   * $R^1$
     * mjr_colorizer_r1-rgb
     * mjr_colorizer_r1-checker

   * $R^2$
     * mjr_colorizer_r2-hsl-thaller mjr_colorizer_r2-hsv-thaller mjr_colorizer_r2-hsl-richardson
     * mjr_colorizer_r2-hsv-full-v
     * mjr_colorizer_r2-1yr0 mjr_colorizer_r2-1rgb1 mjr_colorizer_r2-1r.gb.yc.m0 mjr_colorizer_r2-gr
     * mjr_colorizer_r2-checker mjr_colorizer_r2-strip-x mjr_colorizer_r2-strip-y

   * $R^3$
     * mjr_colorizer_r3-rgb

   * $I=[0,1]$
     * mjr_colorizer_i1-rg
     * mjr_colorizer_i1-rgb-cube
     * mjr_colorizer_i1-hsv-cone

   * $I^2$
     * mjr_colorizer_i2-rg-yb
     * mjr_colorizer_i2-rgb-cube
     * mjr_colorizer_i2-hsv-cone
     * mjr_colorizer_i2-hsl-cone

   * $I^3$
     * mjr_colorizer_i3-rg-yb-cm
     * mjr_colorizer_i3-rgb-cube"
  (documentation 'mjr_colorizer_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_ut-rgb-from-gradient (x &optional (gradient "RYGCBMR"))
  "x should be between 0 and 1 -- less than zero is first color in gradient def while greater than 1 is last color in gradient def"
  (let* ((len   (length gradient))
         (len-1 (1- len))
         (wid   (/ len-1))
         (buk   (floor (/ x wid)))
         (xn    (* wid buk))
         (d     (/ (- x xn) wid)))
    (cond ((< buk     0)  (mjr_color_make-rgb-from-spec (aref gradient 0)))
          ((>= buk len-1) (mjr_color_make-rgb-from-spec (aref gradient len-1)))
          ('t             (map 'vector
                               (lambda (c1 c2) (+ (* c1 (- 1 d)) (* c2 d)))
                               (mjr_color_make-rgb-from-spec (aref gradient buk))
                               (mjr_color_make-rgb-from-spec (aref gradient (1+ buk))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_ut-rgb-from-multi-gradient (x &optional (multi-gradient (vector "RYGCBMR")))
  "Vector of grads"
  (let* ((len   (length multi-gradient))
         (len-1 (1- len))
         (wid   (/ len))
         (buk   (floor (/ x wid)))
         (xn    (* wid buk))
         (y     (/ (- x xn) wid)))
    (cond ((< buk     0)  (mjr_colorizer_ut-rgb-from-gradient 0 (aref multi-gradient 0)))
          ((>= buk len-1) (mjr_colorizer_ut-rgb-from-gradient 1 (aref multi-gradient len-1)))
          ('t             (mjr_colorizer_ut-rgb-from-gradient y (aref multi-gradient buk))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-1yr0 (realpart-or-complex-number &optional imagpart-or-nil)
  "Function of phase only. When mapped onto graph of the abs surface surface, all complex information is captured. Top of positive real axis is white while
bottom is black"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z  (complex x y))
           (a  (mjr_numu_argument z)))
      (mjr_colorizer_ut-rgb-from-gradient (/ a (* 2 pi)) "1YR0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-1rgb1 (realpart-or-complex-number &optional imagpart-or-nil)
  "Color is a function of phase.  Axis are identifiable by color.

                R=100
                |
                |
       010=G---------1=111
                |
                |
                B=001"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z  (complex x y))
           (a  (mjr_numu_argument z)))
      (mjr_colorizer_ut-rgb-from-gradient (/ a (* 2 pi)) "1RGB1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-1r.gb.yc.m0 (realpart-or-complex-number &optional imagpart-or-nil)
  "Color is a function of phase.  Positive and negative of each axis is identifiable as well as each quadrant

           001=G|R=100
                |
       001=B    |    1=111
         ---------------
       110=Y    |    0=000
                |
           011=C|M=101"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z  (complex x y))
           (a  (mjr_numu_argument z)))
      (mjr_colorizer_ut-rgb-from-multi-gradient (/ a (* 2 pi)) #("1r" "gb" "yc" "m0")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-gr (realpart-or-complex-number &optional imagpart-or-nil)
  "Blue is a function of magnitude while red and green are functions of phase.
The sides of the positive real axis are identifiable

            |
            |
            |    G
     ---------------
            |    R
            |
            |    "
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z  (complex x y))
           (c  (mjr_colorizer_ut-rgb-from-gradient (/ (mjr_numu_argument z) (* 2 pi)) "gr"))
           (s  (* 2 (/ (atan (* .4 (abs z))) (* pi)))))
      (setf (aref c 2) s)
      c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_helper-r2-hs* (realpart-or-complex-number imagpart-or-nil hs-fun max-v)
  "Map color to the complex plane by mapping the plane to a unit sphere, and then mapping to the HSL or HSV color space.

References:
  Bernd Thaller (2000); Visual Quantum Mechanics; pp 2-8"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z  (complex x y))
           (h  (/ (mjr_numu_argument z) (* 2 pi)))
           (v  (if max-v 1 (/ (* 2 (atan (abs z))) pi))))
      (funcall hs-fun h 1 v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-hsl-thaller (realpart-or-complex-number &optional imagpart-or-nil)
  "HSL like color map across entire complex plane.  Function of phase and abs!  Positive real axis is shades of red while
negative real axis is shades of cyan.  zero is black."
  (mjr_colorizer_helper-r2-hs* realpart-or-complex-number imagpart-or-nil #'mjr_color_convert-hsl2rgb nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-hsv-thaller (realpart-or-complex-number &optional imagpart-or-nil)
  "HSV like color map across entire complex plane.  Function of phase and abs!  Positive real axis is shades of red while
negative real axis is shades of cyan.  zero is black."
  (mjr_colorizer_helper-r2-hs* realpart-or-complex-number imagpart-or-nil #'mjr_color_convert-hsv2rgb nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-hsv-full-v (realpart-or-complex-number &optional imagpart-or-nil)
  "Like MJR_COLORIZE_R2-HSV-THALLER but with v=1 or MJR_COLORIZE_R2-HSL-THALLER with v=1/2 -- i.e. no dependence on magnitude.
Very colorful. The real axis is identifiable (Positive real axis is red while negative real axis is cyan -- not shades of same, but really red and cyan."
  (mjr_colorizer_helper-r2-hs* realpart-or-complex-number imagpart-or-nil #'mjr_color_convert-hsv2rgb 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-hsl-richardson (realpart-or-complex-number &optional imagpart-or-nil)
  "HSL like color map across entire complex plane Function of phase and abs!  Positive real axis is shades of red while negative real axis is shades of cyan.
Zero is black.

References:
  Richardson (1991); Visualizing quantum scattering on the CM-2 supercomputer; Computer Physics Communications 63; pp 84-94"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (let* ((z     (complex x y))
           (r2    (+ (* x x) (* y y)))
           (r     (abs z))
           (xscl  (/ x (sqrt 30/5)))
           (yscl  (/ y (sqrt 2)))
           (r2+1  (+ 1 r2))
           (ofs   (* (- 1/2 (/ r r2+1)) (if (< r 1) -1 1)))
           (red   (+ ofs (+ 1/2 (/ (* (sqrt 2/3) x) r2+1))))
           (green (+ ofs (- 1/2 (/ (- xscl yscl)    r2+1))))
           (blue  (+ ofs (- 1/2 (/ (+ xscl yscl)    r2+1)))))
      (vector red green blue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-checker (realpart-or-complex-number &optional imagpart-or-nil)
  "red and blue checkers"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (if (oddp (floor x))
        (if (oddp (floor y))
            (vector 0 0 1)
            (vector 1 0 0))
        (if (evenp (floor y))
            (vector 0 0 1)
            (vector 1 0 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2-strip-x (realpart-or-complex-number &optional imagpart-or-nil)
  "red and blue strips"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (declare (ignore y))
    (if (oddp (floor x))
        (vector 0 0 1)
        (vector 1 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r2--strip-y (realpart-or-complex-number &optional imagpart-or-nil)
  "red and blue strips"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args realpart-or-complex-number imagpart-or-nil)
    (declare (ignore x))
    (if (oddp (floor y))
        (vector 0 0 1)
        (vector 1 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r1-rgb (x)
  "Map color to the complex plane by mapping the plane to a unit sphere, and then mapping to the HSL or HSV color space.

References:
  Bernd Thaller (2000); Visual Quantum Mechanics; pp 2-8"
    (let ((d (+ 1/2 (/ (atan x) pi))))
      (mjr_colorizer_ut-rgb-from-gradient d "rgb")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r1-checker (x)
  "Map color to the real line with one unit wide red and blue intervals."
  (if (oddp (floor x))
      (vector 0 0 1)
      (vector 1 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_r3-rgb (x-or-vec &optional y-or-nil z-or-nil)
  "Map color to R^3 by stretching out the RGB color cube to infinite size."
  (multiple-value-bind (x y z) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil z-or-nil)
    (let ((r (+ 1/2 (/ (atan x) pi)))
          (g (+ 1/2 (/ (atan y) pi)))
          (b (+ 1/2 (/ (atan z) pi))))
      (vector r g b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i1-rg (x-or-vec)
  "Map color to I^1 red-black-green"
  (mjr_color_mix-max (mjr_colorizer_ut-rgb-from-gradient (if (numberp x-or-vec) x-or-vec (elt x-or-vec 0)) "r0g")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i2-rg-yb  (x-or-vec &optional y-or-nil)
  "Map color to I^3"
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil)
    (mjr_color_mix-max (mjr_colorizer_ut-rgb-from-gradient x "r0g")
                       (mjr_colorizer_ut-rgb-from-gradient y "y0b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i3-rg-yb-cm (x-or-vec &optional y-or-nil z-or-nil)
  "Map color to I^3"
  (multiple-value-bind (x y z) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil z-or-nil)
    (mjr_color_mix-max (mjr_colorizer_ut-rgb-from-gradient x "r0g")
                       (mjr_colorizer_ut-rgb-from-gradient y "y0b")
                       (mjr_colorizer_ut-rgb-from-gradient z "c0m"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i1-rgb-cube (x-or-vec)
  "Map color to I^1 (the R axis of RGB cube -- black to red)."
  (multiple-value-bind (x) (mjr_util_get-all-elements-or-args x-or-vec)
    (vector x 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i2-rgb-cube (x-or-vec &optional y-or-nil)
  "Map color to $I^2=[0,1]\times [0,1]$ by mapping the front face of the RGB cube to the unit square."
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil)
    (vector x y 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i3-rgb-cube (x-or-vec &optional y-or-nil z-or-nil)
  "Map color to $I^3=[0,1]\times [0,1]\times [0,1]$ by taking $R\rightarrow X$, $G\rightarrow Y$, and $B\rightarrow Z$."
  (multiple-value-bind (x y z) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil z-or-nil)
    (vector x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i1-hsv-cone (x-or-vec)
  "Map color to $I=[0,1]$ with the colors around the top of the HSV cone (or around the center of the HSL double cone)."
  (multiple-value-bind (x) (mjr_util_get-all-elements-or-args x-or-vec)
    (mjr_color_convert-hsv2rgb x 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i2-hsv-cone (x-or-vec &optional y-or-nil)
  "Map color to $I^2=[0,1]\times [0,1]$ by wrapping the outside of the HSV cone (cylinder really) onto the a unit square."
  (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil)
    (mjr_color_convert-hsv2rgb x 1 y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_i2-hsl-cone (x-or-vec &optional y-or-nil)
  "Map color to $I^2=[0,1]\times [0,1]$ by wrapping the outside of the HSL double cone (cylinder really) onto the a unit square."
 (multiple-value-bind (x y) (mjr_util_get-all-elements-or-args x-or-vec y-or-nil)
    (mjr_color_convert-hsl2rgb x 1 y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_factory-from-gradient (gradient)
  "Return a function that takes a real number in [0,1], and returns a color."
    (lambda (z) (mjr_colorizer_ut-rgb-from-gradient z gradient)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorizer_factory-from-multi-gradient (multi-gradient)
  "Return a function that takes a real number in [0,1], and returns a color."
    (lambda (z) (mjr_colorizer_ut-rgb-from-multi-gradient z multi-gradient)))
