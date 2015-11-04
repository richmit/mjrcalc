;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-colorize.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010 by Mitch Richling.  All rights reserved.
;; @brief     Interface for :MJR_COLORIZED and :MJR_COLORIZER.@EOL
;; @Keywords  wrap colorize colorizer colorized
;; @Std       Common Lisp
;;
;;            Notes here
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_COLORIZE
  (:USE :COMMON-LISP
        :MJR_COLORIZED
        :MJR_COLORIZER)
  (:DOCUMENTATION "Brief: Interface for :MJR_COLORIZED and :MJR_COLORIZER.;")
  (:EXPORT #:mjr_colorize_help
           #:mjr_colorize_make-colorize-function
           ))

(in-package :MJR_COLORIZE)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorize_help ()
  "Handy wrapper and interface homogenization for colorization packages.

This is the one package that tools consuming colorization specifications need to use so that they can generate internal,
optimized colorization functions.

    We can colorize four kinds of scalar domains:
    
       |------------------------+----------+-----------+-------------+-------------|
       | colorize input domain  | Z_n      | Z         | I=[0,1]     | R           |
       |------------------------+----------+-----------+-------------+-------------|
       | cardinality            | finite   | countable | uncountable | uncountable |
       | b/ub                   | bounded  | unbounded | bounded     | unbounded   |
       | d/c                    | discrete | discrete  | continuous  | continuous  |
       | colorize output range  | tru rgb  | tru rgb   | real rgb    | real rgb    |
       |------------------------+----------+-----------+-------------+-------------|
    
    We can also color four kinds of non-scalar domains:
    
       |------------------------+-------------+-------------+-------------+-------------|
       | colorize input domain  | I^3         | R^3         | I^2         | R^2         |
       |------------------------+-------------+-------------+-------------+-------------|
       | cardinality            | uncountable | uncountable | uncountable | uncountable |
       | b/ub                   | bounded     | unbounded   | bounded     | unbounded   |
       | d/c                    | continuous  | continuous  | continuous  | continuous  |
       | colorize output range  | real rgb    | real rgb    | real rgb    | real rgb    |
       |------------------------+-------------+-------------+-------------+-------------|
    
    Gradients, multi-gradients, and pallets all can be mapped quite directly to scalar colorization schemes.  When passed as
    function arguments, these objects can be recognized and transformed into colorize functions.  The type of the passed object
    along with the continuity of the input domain are enough to recognize what to do:
    
       |------------+-------------------+----------------+---------|
       | input      | object type       | color method   | support |
       |------------+-------------------+----------------+---------|
       | continuous | string            | gradient       | yes     |
       | continuous | vector of strings | multi-gradient | yes     |
       |------------+-------------------+----------------+---------|
       | discrete   | string            | gradient       | yes     |
       | discrete   | vector            | pallet         | yes     |
       | discrete   | vector of strings | multi-gradient | no      |
       |------------+-------------------+----------------+---------|"
  (documentation 'mjr_colorize_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorize_make-colorize-function (color-method color-space max-color data-range auto-scale)
  "Return a colorize function from a function, gradient, or pallet with optional argument scaling.
Arguments:
     * method ...... Function, gradient, multi-gradient, pallet
                       It is interperted via it's type and the value of :color-space like so:
                         * string .. gradient
                         * vector .. pallet (when :color-space is :cs-tru)
                         * vector .. multi-gradient (when :color-space is :cs-rgb)
                         * else .... colorize atom (a function takeing one or more numbers and returning a color)
     * color-space . Must be :cs-tru or :cs-rgb.  Default is :cs-tru
     * max-color ... Positive integer, nil=infinite, or a real number in (0,1]
     * auto-scale .. Use max-color and data-range to autoscale the input data
     * data-range .. The range of the input data
                       For z-color-method:
                         * nil ...... all variable ranges are (infinite)
                         * vector ... 1st two elements -> range for 1st arg, next 2 are for next var, etc...
Examples
  * Take real numbers in [-5 5], and map them to the color gradient black-red
    (mjr_colorize_make-colorize-function #(\"0R\" :cs-rgb 1 #(-5 5) 't))
  * Take a tuple in [-5 5]x[-4 4]x[-3 3] and map it to the RGB color cube
    (mjr_colorize_make-colorize-function #'mjr_colorizer_i3-rgb-cube :cs-rgb 1 #(-5 5 -4 4 -3 3) 't)"
  (let* ((color-space (or color-space :cs-tru))
         (fun         (if (eq color-space :cs-tru)
                          (typecase color-method
                            (string    (mjr_colorized_factory-from-gradient color-method))
                            (vector    (mjr_colorized_factory-from-pallet color-method))
                            (otherwise color-method))
                          (typecase color-method
                            (string    (mjr_colorizer_factory-from-gradient color-method))
                            (vector    (mjr_colorizer_factory-from-multi-gradient color-method))
                            (otherwise color-method))))
         (max-color   (or max-color
                          (if auto-scale
                              (if (eq color-space :cs-tru)
                                  (typecase color-method
                                    (string    (mjr_colorized_ut-gradient-length color-method))
                                    (vector    (mjr_colorized_ut-pallet-length   color-method)))
                                  (typecase color-method
                                    (string    1.0)
                                    (vector    1.0)))))))
    (if (and auto-scale data-range max-color)
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

