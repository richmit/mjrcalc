;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-svg.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Simple API to create SVG files
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2010,2012,2015,2017,2019 Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_SVG
  (:USE :COMMON-LISP
        :MJR_COLOR)
  (:DOCUMENTATION "Brief: Write SVG files.;")
  (:EXPORT #:mjr_svg_help
           #:mjr_svg_create
           #:mjr_svg_finish
           #:mjr_svg_line
           #:mjr_svg_circle
           #:mjr_svg_ellipse
           #:mjr_svg_rect
           #:mjr_svg_polyline
           #:mjr_svg_polygon
           #:mjr_svg_text
           ;; Not exported
           ;;  #:mjr_svg_cart2nativeX
           ;;  #:mjr_svg_cart2nativeY
           ;;  #:mjr_svg_cart2nativeXlen
           ;;  #:mjr_svg_cart2nativeYlen
           ))

(in-package :MJR_SVG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_help ()
  "Help for MJR_SVG: Create SVG files

Super simple API to generate SVG files.  This library that made Postscript files in the past.  I copied the PS library over to this new name so that people could
keep the older PS library in place if the wished.  

Some effort is taken to make sure the resulting SVG files are human readable.

When an SVG is created, a Cartesian coordinates system is specified.  This coordinate system may be used to scale various arguments provided to drawing
functions.  Such functions have arguments with names like scale-foo -- if set non-NIL then the foo argument will be scaled appropriately.

Several functions accept some common drawing modification arguments:
  * linecap ........... butt, square, round
  * stroke-linejoin ... miter, round, bevel
  * font-weight ....... bold, normal. 
  * font-style ........ italic, normal.
  * font-decoration ... none, underline, overline, line-through.
  * font-family ....... serif, sans-serif, monospace, fantasy, cursive"
  (documentation 'mjr_svg_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_cart2nativeX (x xmin xmax xpix)
  "Convert Cartesian x coordinate to SVG native coordinate"
  (/ (- x xmin) (/ (- xmax xmin) xpix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_cart2nativeY (y ymin ymax ypix)
  "Convert Cartesian y coordinate to SVG native coordinate"
  (/ (- ymax y) (/ (- ymax ymin) ypix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_cart2nativeXlen (x-len xmin xmax xpix)
  "Convert lenth in Cartesian x direction to SVG native length"
  (/ x-len (/ (- xmax xmin) xpix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_cart2nativeYlen (y-len ymin ymax ypix)
  "Convert lenth in Cartesian y direction to SVG native length"
  (/ y-len (/ (- ymax ymin) ypix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_create (file-name &key (width 1920) (height 1080) background xmin xmax ymin ymax)
  "Open an SVG file, and return data required to write to that file"
  (let ((svg-file   (open file-name :direction :output :if-exists :supersede :if-does-not-exist :create))
        (xmin       (or xmin 0))
        (xmax       (or xmax width))
        (ymin       (or ymin 0))
        (ymax       (or ymax height))
        (background (if (and background (listp background))
                        (apply #'mjr_color_tru2hex-color-string background)
                        background)))
    (if svg-file
        (progn (format svg-file "<?xml version='1.0'?>~%")
               (format svg-file "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'~%")
               (format svg-file "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>~%")
               (format svg-file "<svg xmlns='http://www.w3.org/2000/svg'~%")
               (format svg-file "     width='~dpx' height='~dpx'>~%" width height)
               (if background
                   (format svg-file "  <rect x='0' y='0' width='~f' height='~f' fill='~a' stroke='none' />~%" width height background))
               (list svg-file xmin xmax ymin ymax width height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_finish (svg)
  "Close an SVG file"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (declare (ignore xmin xmax ymin ymax svg-width svg-height))
        (format svg-file "</svg>~%")
        (close svg-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_line (svg x1 y1 x2 y2 &key
                                       (scale-x1 't) (scale-y1 't) (scale-x2 't) (scale-y2 't)
                                       (stroke-opacity 1) (stroke "black") (stroke-width 1) (stroke-linecap "butt"))
  "Draw a line"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((x1     (if scale-x1 (mjr_svg_cart2nativeX x1 xmin xmax svg-width)  x1))
              (y1     (if scale-y1 (mjr_svg_cart2nativeY y1 ymin ymax svg-height) y1))
              (x2     (if scale-x2 (mjr_svg_cart2nativeX x2 xmin xmax svg-width)  x2))
              (y2     (if scale-y2 (mjr_svg_cart2nativeY y2 ymin ymax svg-height) y2))
              (stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke)))
          (format svg-file "    <line x1='~f' y1='~f' x2='~f' y2='~f' stroke-opacity='~f' stroke='~a' stroke-width='~f' stroke-linecap='~a'/>~%"
                  x1 y1 x2 y2 stroke-opacity stroke stroke-width stroke-linecap)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_circle (svg cx cy rx &key
                                      (scale-cx 't) (scale-cy 't) (scale-rx 't)
                                      (stroke "black") (stroke-opacity 1) (stroke-width 1)
                                      (fill "none") (fill-opacity 1))
  "Draw a circle"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((cx     (if scale-cx (mjr_svg_cart2nativeX cx xmin xmax svg-width)  cx))
              (cy     (if scale-cy (mjr_svg_cart2nativeY cy ymin ymax svg-height) cy))
              (rx     (if scale-rx (mjr_svg_cart2nativeXlen rx xmin xmax svg-width)   rx))
              (stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke))
              (fill   (if (and fill (listp fill))
                          (apply #'mjr_color_tru2hex-color-string fill)
                          fill)))
          (format svg-file "    <circle cx='~f' cy='~f' r='~f' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' />~%"
                  cx cy rx fill-opacity fill stroke-opacity stroke stroke-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_ellipse (svg cx cy rx ry &key
                                          (scale-cx 't) (scale-cy 't) (scale-rx 't) (scale-ry 't)
                                          (stroke "black") (stroke-opacity 1) (stroke-width 1)
                                          (fill "none") (fill-opacity 1))
  "Draw an ellipse"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((cx     (if scale-cy (mjr_svg_cart2nativeX cx xmin xmax svg-width)  cx))
              (cy     (if scale-cx (mjr_svg_cart2nativeY cy ymin ymax svg-height) cy))
              (rx     (if scale-rx (mjr_svg_cart2nativeXlen rx xmin xmax svg-width) rx))
              (ry     (if scale-ry (mjr_svg_cart2nativeYlen ry ymin ymax svg-height) ry))
              (stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke))
              (fill   (if (and fill (listp fill))
                          (apply #'mjr_color_tru2hex-color-string fill)
                          fill)))
          (format svg-file "    <ellipse cx='~f' cy='~f' rx='~f' ry='~f' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' />~%"
                  cx cy rx ry fill-opacity fill stroke-opacity stroke stroke-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_rect (svg x y width height &key
                                            (scale-x 't) (scale-y 't) (scale-width 't) (scale-height 't)
                                            (stroke "black") (stroke-opacity 1) (stroke-width 1) (stroke-linejoin "bevel")
                                            (fill "none") (fill-opacity 1))
  "Draw a rectangle"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((x      (if scale-x (mjr_svg_cart2nativeX x xmin xmax svg-width)          x))
              (y      (if scale-y (mjr_svg_cart2nativeY y ymin ymax svg-height)         y))
              (width  (if scale-width  (mjr_svg_cart2nativeXlen width xmin xmax svg-width)    width))
              (height (if scale-height (mjr_svg_cart2nativeYlen height ymin ymax svg-height) height))
              (stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke))
              (fill   (if (and fill (listp fill))
                          (apply #'mjr_color_tru2hex-color-string fill)
                          fill)))
          (format svg-file "    <rect x='~f' y='~f' width='~f' height='~f' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' stroke-linejoin='~a'/>~%"
                  x y width height fill-opacity fill stroke-opacity stroke stroke-width stroke-linejoin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_polyline (svg points &key
                                      (scale-points 't)
                                      (stroke "black") (stroke-opacity 1) (stroke-width 1) (stroke-linecap "butt") (stroke-linejoin "miter")
                                      (fill "none") (fill-opacity 1))

  "Draw a sequence of lines"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke))
              (fill   (if (and fill (listp fill))
                          (apply #'mjr_color_tru2hex-color-string fill)
                          fill)))
          (format svg-file "    <polyline points='~a' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' stroke-linecap='~a' stroke-linejoin='~a' />~%"
                  (with-output-to-string (point-str)
                    (loop for pt being the elements of points
                          for x = (if scale-points (mjr_svg_cart2nativeX (elt pt 0) xmin xmax svg-width)  (elt pt 0))
                          for y = (if scale-points (mjr_svg_cart2nativeY (elt pt 1) ymin ymax svg-height) (elt pt 1))
                          do (format point-str "~f ~f " x y)))
                  fill-opacity fill stroke-opacity stroke stroke-width stroke-linecap stroke-linejoin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_polygon (svg points &key
                                     (scale-points 't)
                                     (stroke "black") (stroke-opacity 1) (stroke-width 1) (stroke-linejoin "miter")
                                     (fill "none") (fill-opacity 1))
  "Draw a polygon"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((stroke (if (and stroke (listp stroke))
                          (apply #'mjr_color_tru2hex-color-string stroke)
                          stroke))
              (fill   (if (and fill (listp fill))
                          (apply #'mjr_color_tru2hex-color-string fill)
                          fill)))
        (format svg-file "    <polygon points='~a' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' stroke-linejoin='~a' />~%"
                (with-output-to-string (point-str)
                  (loop for pt being the elements of points
                        for x = (if scale-points (mjr_svg_cart2nativeX (elt pt 0) xmin xmax svg-width)  (elt pt 0))
                        for y = (if scale-points (mjr_svg_cart2nativeY (elt pt 1) ymin ymax svg-height) (elt pt 1))
                        do (format point-str "~f ~f " x y)))
                fill-opacity fill stroke-opacity stroke stroke-width stroke-linejoin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_svg_text (svg x y text &key
                                    (scale-x 't)
                                    (scale-y 't)
                                    (scale-font-size 't)
                                    (stroke "black") (stroke-opacity 1) (stroke-width 1)
                                    (fill "black") (fill-opacity 1)
                                    (font-weight "normal") (font-style "normal") (font-decoration "none") (font-size 10) (font-family "serif"))
  "Render text"
  (if svg
      (destructuring-bind (svg-file xmin xmax ymin ymax svg-width svg-height) svg
        (let ((x          (if scale-x (mjr_svg_cart2nativeX x xmin xmax svg-width)                     x))
              (y          (if scale-y (mjr_svg_cart2nativeY y ymin ymax svg-height)                    y))
              (font-size  (if scale-font-size (mjr_svg_cart2nativeYlen font-size ymin ymax svg-height) font-size))
              (stroke     (if (and stroke (listp stroke))
                              (apply #'mjr_color_tru2hex-color-string stroke)
                              stroke))
              (fill       (if (and fill (listp fill))
                              (apply #'mjr_color_tru2hex-color-string fill)
                              fill)))
          (format svg-file "    <text x='~f' y='~f' fill-opacity='~f' fill='~a' stroke-opacity='~f' stroke='~a' stroke-width='~f' font-weight='~a' font-style='~a' font-family='~a' font-size='~apx' text-decoration='~a'>~a</text>~%"
                  x y fill-opacity fill stroke-opacity stroke stroke-width
                  font-weight font-style font-family font-size font-decoration text)))))

