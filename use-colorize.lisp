;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-colorize.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Interface for :MJR_COLORIZED and :MJR_COLORIZER.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2010,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_COLORIZE
  (:USE :COMMON-LISP
        :MJR_COLORIZED
        :MJR_COLORIZER)
  (:DOCUMENTATION "Brief: Interface for :MJR_COLORIZED and :MJR_COLORIZER.;")
  (:EXPORT #:mjr_colorize_help
           #:mjr_colorize_make-colorize-function
           ))

(in-package :MJR_COLORIZE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorize_help ()
  "Handy wrapper and interface homogenization for colorization packages.

This is the one package that tools consuming colorization specifications need to use so that they can generate internal, optimized colorization functions.

    We can colorize four kinds of scalar domains:

       |------------------------+----------+-----------+-------------+-------------|
       |                        | Z_n      | Z         | I=[0,1]     | R           |
       |------------------------+----------+-----------+-------------+-------------|
       | cardinality            | finite   | countable | uncountable | uncountable |
       | bounded/unbounded      | bounded  | unbounded | bounded     | unbounded   |
       | discrete/continuous    | discrete | discrete  | continuous  | continuous  |
       | colorspace             | :cs-tru  | :cs-tru   | :cs-rgb     | :cs-rgb     |
       |------------------------+----------+-----------+-------------+-------------|

    We can also color four kinds of non-scalar domains:

       |------------------------+-------------+-------------+-------------+-------------|
       |                        | I^3         | R^3         | I^2         | R^2         |
       |------------------------+-------------+-------------+-------------+-------------|
       | cardinality            | uncountable | uncountable | uncountable | uncountable |
       | bounded/unbounded      | bounded     | unbounded   | bounded     | unbounded   |
       | discrete/continuous    | continuous  | continuous  | continuous  | continuous  |
       | colorspace             | :cs-rgb     | :cs-rgb     | :cs-rgb     | :cs-rgb     |
       |------------------------+-------------+-------------+-------------+-------------|

    Gradients, multi-gradients, and pallets all can be mapped quite directly to scalar colorization schemes.  When passed as function arguments, these objects
    can be recognized and transformed into colorize functions.  The type of the passed object along with the continuity of the input domain are enough to
    recognize what to do:

       |------------+-------------------+----------------+---------|
       | input      | object type       | color method   | support |
       |------------+-------------------+----------------+---------|
       | continuous | string            | gradient       | yes     |
       | continuous | vector of strings | multi-gradient | yes     |
       | continuous | function          | color-atom     | yes     |
       |------------+-------------------+----------------+---------|
       | discrete   | string            | gradient       | yes     |
       | discrete   | vector            | pallet         | yes     |
       | discrete   | vector of strings | multi-gradient | no      |
       | discrete   | function          | color-atom     | yes     |
       |------------+-------------------+----------------+---------|"
  (documentation 'mjr_colorize_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_colorize_make-colorize-function (color-method color-space max-color data-range auto-scale packer)
  "Return a colorize function from a function, gradient, or pallet with optional argument scaling.
Arguments:
     * color-method .. Function, gradient, multi-gradient, pallet
                         It is interpreted via it's type and the value of :color-space like so:
                           * string .. gradient (when :color-space is :cs-tru or :cs-rgb)
                           * vector .. pallet (when :color-space is :cs-tru)
                           * vector .. multi-gradient (when :color-space is :cs-rgb)
                           * else .... colorize atom (a function taking one or more numbers and returning a color)
     * color-space ... Must be :cs-tru or :cs-rgb.  Default is :cs-tru
     * max-color ..... Positive integer, nil=auto/infinite, or a real number in (0,1].  NIL means:
                           * Automatically computed when color-space is :cs-tru or :cs-rgb and color-method
                             is a string or vector
                           * Infinity (auto-scale will not happen)
     * auto-scale .... Use max-color and data-range to auto-scale the input data
     * data-range .... The range of the input data
                           * nil ...... all variable ranges are (infinite)
                           * vector ... 1st two elements -> range for 1st arg, next 2 are for next var, etc...
Examples
  * Take real numbers in [-5 5], and map them to the color gradient black-red
    (mjr_colorize_make-colorize-function \"0R\" :cs-rgb 1 #(-5 5) 't))
  * Take a tuple in [-5 5]x[-4 4]x[-3 3] and map it to the RGB color cube
    (mjr_colorize_make-colorize-function #'mjr_colorizer_i3-rgb-cube :cs-rgb 1 #(-5 5 -4 4 -3 3) 't)"
  (let* ((color-space (or color-space :cs-tru))
         (fun         (typecase color-method
                        (string    (case color-space
                                     (:cs-tru (mjr_colorized_factory-from-gradient color-method))
                                     (:cs-rgb (mjr_colorizer_factory-from-gradient color-method))
                                     (otherwise (error "mjr_colorize_make-colorize-function: Gradients supported only for :cs-tru and :cs-rgb"))))
                        (vector    (case color-space
                                     (:cs-tru (mjr_colorized_factory-from-pallet color-method))
                                     (:cs-rgb (mjr_colorizer_factory-from-multi-gradient color-method))
                                     (otherwise (error "mjr_colorize_make-colorize-function: Multi-Gradients supported only for :cs-rgb, and pallets supported only for :cs-tru"))))
                        (otherwise color-method)))
         (max-color   (or max-color
                          (if auto-scale
                              (case color-space
                                (:cs-tru (typecase color-method
                                           (string    (mjr_colorized_ut-gradient-length color-method))
                                           (vector    (mjr_colorized_ut-pallet-length   color-method))
                                           (otherwise (error "mjr_colorize_make-colorize-function: max-color must be provided for auto-scale when color-method is not a string or vector!"))))
                                (:cs-rgb (typecase color-method
                                           (string    1.0)
                                           (vector    1.0)
                                           (otherwise (error "mjr_colorize_make-colorize-function: max-color must be provided for auto-scale when color-method is not a string or vector!"))))
                                (otherwise (error "mjr_colorize_make-colorize-function: max-color must be provided for auto-scale when color-space is not :cs-tru or :cs-rgb!"))))))
         (colfun      (if (and auto-scale data-range max-color)
                          (lambda (&rest rest) (apply fun (loop for vval in rest
                                                                for vidx from 0 by 2
                                                                for vmin = (aref data-range vidx)
                                                                for vmax = (aref data-range (1+ vidx))
                                                                for invm = (- vmax vmin)
                                                                for sval = (if (zerop invm) 0 (/ (* max-color (- vval vmin)) invm))
                                                                collect (if (eq color-space :cs-tru)
                                                                            (truncate sval)
                                                                            sval))))
                          (lambda (&rest rest) (apply fun rest)))))
    (if (and packer (not (equalp packer #'identity)))
        (lambda (&rest rest) (funcall packer (apply colfun rest)))
        colfun)))
