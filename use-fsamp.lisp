;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-fsamp.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Sample mathematical functions and create DQUADs and DSIMPs.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1991, 1992, 1994, 1997, 2008, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Smooth function -- returns derivative info.  Include derivative info, normal vectors, etc... in dquad@EOL@EOL
;; @todo      Adaptive 2D/3D functions with data stored in binary quadrilateral trees@EOL@EOL
;; @todo      Adaptive 2D functions on triangular meshes.@EOL@EOL
;; @todo      Use MXP for functions in algebraic notation.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_FSAMP
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_VEC
        :MJR_A
        :MJR_NUMU
        :MJR_COMBC
        :MJR_GEOM
        :MJR_ARR
        :MJR_UTIL
        :MJR_DQUAD
        :MJR_DSIMP
        :MJR_RPTREE
        :MJR_POLY)
  (:DOCUMENTATION "Brief: Sample functions and store structured data.;")
  (:EXPORT #:mjr_fsamp_help
           ;; Real and complex polynomials
           #:mjr_fsamp_dq-poly-r1
           #:mjr_fsamp_dq-poly-c1
           ;; Generic real functions: $f:\mathbb{R}^n\to\mathbb{R}^m$ with $m,n\in\{1,2,3\}$
           #:mjr_fsamp_dq-func-r123-r123
           #:mjr_fsamp_dq-func-r123-color
           #:mjr_fsamp_ds-func-r123-r123
           ;; Generic complex functions: $f:\mathbb{C}\to\mathbb{C}$
           #:mjr_fsamp_dq-func-c1-c1
           ;; Specialized sampling for complex functions
           #:mjr_fsamp_dq-complex-circle-image
           #:mjr_fsamp_dq-complex-circle-image-mag
           #:mjr_fsamp_dq-mahler-samp
           ;; Specialized adaptive sampling for real functions
           #:mjr_fsamp_ds-func-r1-rn-adaptive
           #:mjr_fsamp_rp-func-r2-r1-adaptive             ;; This one's complicated
           ))

(in-package :MJR_FSAMP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_help ()
  "Help for MJR_FSAMP:  Function SAMPle -- sample mathematical functions and store results DQUAD, DSIMP, and RPTREE objects

Naming conventions:
   * mjr_fsamp_dq-* -- Produce a DQUAD
   * mjr_fsamp_ds-* -- Produce a DSIMP

         On regular grids
              
              |        |                    | VTK | GNUplot | POVray | PLY3d | OBJ3D | TGA |
              |--------+--------------------+-----+---------+--------+-------+-------+-----|
              | R1->R1 | Curve              | YES | YES     | YES    |       |       |     |
              | R1->R2 | Parametric Curve   | YES | YES     | YES    |       |       |     |
              | R1->R3 | Parametric Curve   | YES | YES     | YES    |       |       |     |
              | R2->R1 | Surface            | YES | YES     | YES    | YES   | YES   |     |
              | R2->R1 | Scalar Field       | YES |         |        |       |       | YES |
              | R2->R3 | Parametric Surface | YES | YES     | YES    | YES   | YES   |     |
              | C1->C1 | Magnitude Surface  | YES | YES     | YES    | YES   | YES   |     |
              | C1->C1 | Phase Surface      | YES | YES     | YES    | YES   | YES   |     |
              | C1->C1 | Image Color        | YES |         | YES    | YES   |       |     |
              | C1->C1 | Analytic Landscape | YES |         | YES    | YES   |       |     |
              | C1->C1 | Circle Images      | YES | YES     |        |       |       |     |
              | C1->C1 | Circle Mag Images  | YES | YES     |        |       |       |     |
              
         On adaptive 1D Grid 
              
              |        |                    | VTK | GNUplot | POVray | PLY3d | OBJ3D | TGA |
              |--------+--------------------+-----+---------+--------+-------+-------+-----|
              | R1->R1 | Curve              | YES | YES     | YES    |       |       |     |
              | R1->R2 | Parametric Curve   | YES | YES     | YES    |       |       |     |
              | R1->R3 | Parametric Curve   | YES | YES     | YES    |       |       |     |
              
              
         On adaptive 2D Grid
              
              |        |                    | VTK | GNUplot | POVray | PLY3d | OBJ3D | TGA |
              |--------+--------------------+-----+---------+--------+-------+-------+-----|
              | R2->R1 | Surface            | YES | YES     | YES    | YES   | YES   |     |  <- TODO
              | R2->R1 | Scalar Field       | YES |         |        |       |       | YES |  <- TODO
              | R2->R3 | Parametric Surface | YES | YES     | YES    | YES   | YES   |     |  <- TODO
              | R2->R1 | Implicit Curve     | YES | YES     |        |       |       |     |  <- TODO

In practice DQUAD lists are frequently generated by sampling mathematical functions on a regular gird, and DSIMP lists are
frequently created from the DQUADs produced in this way.  This package is intended to make this process as simple as possible."
  (documentation 'mjr_fsamp_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-func-c1-c1 (f &key rdat idat f_color z_color zf_color)

  "Return a dquad list containing data useful for complex function visualization.

The returned dquad list will contain the following (in this order today, but use the data names in case the order changes):
  - AXIS: real     -- Re(z)
  - AXIS: imag     -- Im(z)
  - DATA: z        -- A complex value in the domain.  When exported to VTK, this will yield two scalar values: z_real and z_imag
  - DATA: f        -- f(z)                            When exported to VTK, this will yield two scalar values: f_real and f_imag
  - DATA: f_abs    -- abs(f(z))
  - DATA: f_phase  -- arg(f(z))
  - DATA: z_color  -- z_color(z)                      Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp
  - DATA: f_color  -- f_color(f(z))                   Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp
  - DATA: zf_color -- zf_color(z, f(z))               Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp

  Note: Some tools will only use one color scalar in a data set

Example:
  * (mjr_gnupl_dquad (mjr_fsamp_dq-func-c1-c1 #'sin
                                              :rdat '(:start -1.5d0 :end 1.5d0 :len 50)
                                              :idat '(:start -1.5d0 :end 1.5d0 :len 50))
                     :data \"f_abs\" :type :f)"
  (let* (;;(c-func  (mjr_mxp_string-or-func-to-lambda c-func "Z"))
         ;;(f       (mjr_mxp_string-or-func-to-lambda f      "Z"))
         (daDquad (mjr_dquad_make-from-axis "real" rdat
                                            "imag" idat)))
    (mjr_dquad_add-data-from-map daDquad #'complex    :axes 't     :ano-nam "z"        :ano-typ :ano-typ-complex)
    (mjr_dquad_add-data-from-map daDquad f            :data 0      :ano-nam "f"        :ano-typ :ano-typ-complex)
    (mjr_dquad_add-data-from-map daDquad #'abs        :data 1      :ano-nam "f_abs"    :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map daDquad #'phase      :data 1      :ano-nam "f_phase"  :ano-typ :ano-typ-real)
    (if z_color
        (mjr_dquad_add-data-from-map daDquad z_color  :data 0      :ano-nam "z_color"  :ano-typ :ano-typ-rgbvec))
    (if zf_color
        (mjr_dquad_add-data-from-map daDquad zf_color :data '(0 1) :ano-nam "zf_color" :ano-typ :ano-typ-rgbvec))
    (if f_color
        (mjr_dquad_add-data-from-map daDquad f_color  :data 1      :ano-nam "f_color"  :ano-typ :ano-typ-rgbvec))
    daDquad))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-poly-c1 (poly &key rdat idat f_color z_color zf_color do-log)
  "Use MJR_FSAMP_DQ-FUNC-C1-C1 to sample the polynomial POLY

Examples:
  * (mjr_gnupl_dquad (mjr_fsamp_dq-poly-c1 #(1 0 0 1) :rdat '(:start -1.5d0 :end 1.5d0 :len 50) :idat '(:start -1.5d0 :end 1.5d0 :len 50))
                     :data \"f_abs\" :type :f)
  * Lehmer's polynomial:
    (mjr_gnupl_dquad (mjr_fsamp_dq-poly-c1 #(1 0 -1 1 0 -1 0 1 -1 0 1)
                                           :rdat '(:start -1.5d0 :end 1.5d0 :len 150)
                                           :idat '(:start -1.5d0 :end 1.5d0 :len 150)
                                           :do-log 't)
                     :data \"f_abs\"
                     :type :f)"
  (mjr_fsamp_dq-func-c1-c1 (if do-log
                               (lambda (z) (log (mjr_poly_eval poly z)))
                               (lambda (z) (mjr_poly_eval poly z)))
                           :rdat rdat
                           :idat idat
                           :f_color f_color
                           :z_color z_color
                           :zf_color zf_color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-poly-r1 (poly-olos &key (order 0) (xdat '(:start -1 :end 1 :len 10)) (ylab "p") (xlab "x"))
  "Construct dquad with values from polynomial and ORDER derivatives evaluated on :xdat

If ylab is 'p' and the xlab is 'x', then 1) the polynomial :ano-nam will be p(x) and 2) the derivative :ano-nam values will look
like 'd^{n}p(x)/dx^{n}.

Examples:
  * (mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 #(2310 -7201 8949 -5541 1709 -210) :order 2 :xdat '(:start 0.45L0 :end 0.75L0 :len 1000))
                     :data '(0 1) :type :l :ylim '(-0.002 0.015))
  * (mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 #(1 0 -1 1 0 -1 0 1 -1 0 1) :order 4 :xdat '(:start -1.45d0 :end 1.25d0 :len 2000))
                     :data '(0 1) :type :l :ylim '(-2 6))"
  (let* ((list-of-poly  (mjr_util_non-list-then-list poly-olos))
         (num-polys     (length list-of-poly))
         (list-of-order (typecase order
                          (integer   (loop repeat num-polys
                                           collect order))
                          (list      order)
                          (otherwise (error "mjr_fsamp_dq-poly-r1: The order parameter must be an integer or list of intgers"))))
         (list-of-ylab  (typecase ylab
                          (string    (if (= 1 num-polys)
                                         (list ylab)
                                         (loop for i from 1 upto num-polys
                                               collect (format nil "~a_{~d}" ylab i))))
                          (list      ylab)
                          (otherwise (error "mjr_fsamp_dq-poly-r1: The ylab parameter must be a string or list of strings")))))
    (cond ((not (= (length list-of-poly) (length list-of-order))) (error "mjr_fsamp_dq-poly-r1: poly-olos order length mismatch"))
          ((not (= (length list-of-poly) (length list-of-ylab)))  (error "mjr_fsamp_dq-poly-r1: poly-olos ylab length mismatch")))
    (let* ((xdat      (mjr_vvec_to-vec xdat))
           (new-dquad (mjr_dquad_make-from-axis xlab xdat)))
      (loop for i from 1 upto num-polys
            for order in list-of-order
            for poly in list-of-poly
            for ylab in list-of-ylab
            do (mjr_dquad_add-multi-data new-dquad
                                         (mjr_combc_gen-all-cross-product (list xdat)
                                                                          :collect-value (lambda (x) (multiple-value-list
                                                                                                      (mjr_poly_eval-poly-and-first-n-derivatives poly x order)))
                                                                          :arg-mode :arg-number
                                                                          :result-type :array)
                                         :ano-typ :ano-typ-real
                                         :ano-nam-olos (loop for o from 0 upto order
                                                             collect (if (zerop o)
                                                                         (format nil "~a(~a)" ylab xlab)
                                                                         (format nil "d^{~d}~a(~a)/d~a^{~d}" o ylab xlab xlab o)))))
      new-dquad)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-func-r123-color (color-func-olos &key (func-lab "c") (ano-typ :ano-typ-rgbvec) xdat ydat zdat (arg-mode :arg-number) (xlab "x") (ylab "y") (zlab "z"))
  "Transform one or more color functions (i.e. a function returning a color when given 1, 2, or 3 real arguments) into a DQUAD"
  (flet ((mkLabs (labs num) ;; Create NUM unique labels from list of labels in LABS
           (if (<= num (length labs))
               labs
               (let ((good-labs nil))
                 (loop for i from 0 upto (1- num)
                       for b = (mjr_util_elt-mod labs i)
                       do (loop for j from 0
                                for n = (format nil "~a~d" b j)
                                when (not (member n good-labs :test #'string=))
                                return (push n good-labs))
                       finally (return (reverse good-labs)))))))
    (let* ((list-of-funcs     (mjr_util_non-list-then-list color-func-olos))
           (list-of-ano-typs  (mjr_util_non-list-then-list ano-typ))
           (list-of-func-labs (mkLabs (mjr_util_non-list-then-list func-lab) (length list-of-funcs))))
      (cond ((zerop (length list-of-funcs))         (error "mjr_fsamp_dq-func-r123-color: FUNC must be provided!"))
            ((not xdat)                             (error "mjr_fsamp_dq-func-r123-color: XDAT must be provided!"))
            ((and zdat (not ydat))                  (error "mjr_fsamp_dq-func-r123-color: YDAT must be provided when ZDAT is provided!")))
      (let ((daDquad (apply #'mjr_dquad_make-from-axis (append (list xlab xdat) (if ydat (list ylab ydat)) (if zdat (list zlab zdat))))))
        (loop for i from 0
              for f in list-of-funcs
              for d-lab in list-of-func-labs
              for d = (mjr_dquad_map daDquad f :axes 't :arg-mode arg-mode)
              for d-val = (mjr_arr_aref-row-major d 0)
              for d-typ = (mjr_util_elt-mod list-of-ano-typs i)
              do (mjr_dquad_add-data daDquad d :ano-nam d-lab :ano-typ d-typ))
        daDquad))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-func-r123-r123 (func-olos &key
                                                (func-lab "f")
                                                xdat ydat zdat (arg-mode :arg-number) (xlab "x") (ylab "y") (zlab "z")
                                                ax-color-meth  ax-color-max-color  (ax-color-lab  "c") (ax-color-auto-scale  't) (ax-color-ano-typ  :ano-typ-rgbvec)
                                                f-color-meth   f-color-max-color   (f-color-lab   "c") (f-color-auto-scale   't) (f-color-ano-typ   :ano-typ-rgbvec)
                                                all-color-meth all-color-max-color (all-color-lab "c") (all-color-auto-scale 't) (all-color-ano-typ :ano-typ-rgbvec))
  "Transform one or more mathematical functions into a dquad
     Arguments:
       - func-olos .. A function or list of same
       - func-lab .............. Labels (names) for functions
       - xdat ydat zdat ........ VVECs that define the domain
       - xlab ylab zlab ........ Labels (names) for domain axes (xdat, ydat, and zdat)
       - arg-mode .............. How args are passed to the functions.  May be a list
       - u-close v-close ....... For :dsimp output.  Close the curve or surface
       - X-color-lab ........... Labels (names) for *-color-meth
       - X-color-meth .......... color-method or list of same.  See: MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-ano-typ ....... :ano-typ or list of same.  See: :MJR_ANNOT and MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-auto-scale .... auto-scale or list of same.  See: MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-max-color ..... max-color or list of same.  MJR_See: COLORIZE_MAKE-COLORIZE-FUNCTION
                           -  X=ax .... axis              -- color functions receive x, y, z
                           -  X=f ..... function          -- color functions receive f0, ..., n_{n-1}
                           -  X=all ... axis and function -- color functions receive x, y, z, f0, ..., n_{n-1})
     Examples:
      * Single rectilinear function graph (curve)
       (let ((data (mjr_fsamp_dq-func-r123-r123 #'sin
                                                :xdat '(:start 0 :end 7 :len 100))))
            (mjr_gnupl_dquad data :data 0 :type :l))
      * Single rectilinear univariate polynomial graph (curve)
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x) (mjr_poly_eval #(2310 -7201 8949 -5541 1709 -210) x))
                                                :xdat '(:start 0.45L0 :end 0.75L0 :len 100))))
            (mjr_gnupl_dquad data :data 0 :type :l :ylim '(-0.002 0.015)))
      * Two rectilinear polynomial graph
       (let ((data (mjr_fsamp_dq-func-r123-r123 (list #'sin #'cos) :xdat (list :start (* -2 pi) :end (* 2 pi) :len 100))))
            (mjr_gnupl_dquad data :data '(0 1) :type :l))
      * Single parametric curve plot in 2D
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x) (vector (sin x) (cos x)))
                                                :xdat (list :start 0 :end (* 2 pi) :len 100))))
            (mjr_gnupl_dquad data :data 0 :type :l))
      * Single parametric curve plot in 3D
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x) (vector (sin x) (cos x) x))
                                                :xdat (list :start 0 :end (* 2 pi) :len 100))))
            (mjr_gnupl_dquad data :data 0 :type :l))
      * Single rectilinear surface graph
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x y) (sin (sqrt (+ (* x x) (* y y)))))
                                                :xdat '(:start -7 :end 7 :len 10)
                                                :ydat '(:start -7 :end 7 :len 10))))
            (mjr_gnupl_dquad data :data 0 :type :f))
      * Single 2D image graph
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x y) (sin (sqrt (+ (* x x) (* y y)))))
                                                :xdat '(:start -7 :end 7 :len 100)
                                                :ydat '(:start -7 :end 7 :len 100))))
            (mjr_gnupl_dquad data :data 0 :type :i))
      * Single parametric surface plot
       (let ((data (mjr_fsamp_dq-func-r123-r123 (lambda (x y)
                                                         (let ((a 1)
                                                               (c 3))
                                                           (vector (* (cos x) (+ c (* a (cos y))))
                                                                   (* (sin x) (+ c (* a (cos y))))
                                                                   (* a (sin y)))))
                                                :xdat '(:start 0 :end 5.0 :len 20)
                                                :ydat '(:start 0 :end 6.0 :len 20))))
                (mjr_gnupl_dquad data :data 0 :type :l))"
  (flet ((mkLabs (labs num) ;; Create NUM unique labels from list of labels in LABS
           (if (<= num (length labs))
               labs
               (let ((good-labs nil))
                 (loop for i from 0 upto (1- num)
                       for b = (mjr_util_elt-mod labs i)
                       do (loop for j from 0
                                for n = (format nil "~a~d" b j)
                                when (not (member n good-labs :test #'string=))
                                return (push n good-labs))
                       finally (return (reverse good-labs)))))))
    (let* ((list-of-funcs           (mjr_util_non-list-then-list func-olos))
           (list-of-func-labs       (mkLabs (mjr_util_non-list-then-list func-lab) (length list-of-funcs)))
           (list-of-ax-color-meths  (mjr_util_non-list-then-list ax-color-meth))
           (list-of-ax-color-mc     (mjr_util_non-list-then-list ax-color-max-color))
           (list-of-ax-color-as     (mjr_util_non-list-then-list ax-color-auto-scale))
           (list-of-ax-color-at     (mjr_util_non-list-then-list ax-color-ano-typ))
           (list-of-ax-color-labs   (mkLabs (mjr_util_non-list-then-list ax-color-lab) (length list-of-ax-color-meths)))
           (list-of-f-color-meths   (mjr_util_non-list-then-list f-color-meth))
           (list-of-f-color-mc      (mjr_util_non-list-then-list f-color-max-color))
           (list-of-f-color-as      (mjr_util_non-list-then-list f-color-auto-scale))
           (list-of-f-color-at      (mjr_util_non-list-then-list f-color-ano-typ))
           (list-of-f-color-labs    (mkLabs (mjr_util_non-list-then-list f-color-lab) (length list-of-f-color-meths)))
           (list-of-all-color-meths (mjr_util_non-list-then-list all-color-meth))
           (list-of-all-color-mc    (mjr_util_non-list-then-list all-color-max-color))
           (list-of-all-color-as    (mjr_util_non-list-then-list all-color-auto-scale))
           (list-of-all-color-at    (mjr_util_non-list-then-list all-color-ano-typ))
           (list-of-all-color-labs  (mkLabs (mjr_util_non-list-then-list all-color-lab) (length list-of-all-color-meths))))
      (cond ((zerop (length list-of-funcs))         (error "mjr_fsamp_dq-func-r123-r123: FUNC must be provided!"))
            ((not xdat)                             (error "mjr_fsamp_dq-func-r123-r123: XDAT must be provided!"))
            ((and zdat (not ydat))                  (error "mjr_fsamp_dq-func-r123-r123: YDAT must be provided when ZDAT is provided!")))
      (let ((daDquad (apply #'mjr_dquad_make-from-axis (concatenate 'list (list xlab xdat) (if ydat (list ylab ydat)) (if zdat (list zlab zdat))))))
        (loop for i from 0
              for f in list-of-funcs
              for d-lab in list-of-func-labs
              for d = (mjr_dquad_map daDquad f :axes 't :arg-mode arg-mode)
              for d-val = (mjr_arr_aref-row-major d 0)
              for d-typ = (typecase d-val
                            (complex   :ano-typ-complex)
                            (number    :ano-typ-real)
                            (vector    (typecase (aref d-val 0)
                                         (complex   :ano-typ-cvec)
                                         (number    :ano-typ-rvec)
                                         (otherwise (error "mjr_fsamp_dq-func-r123-r123: Function return type not supported: ~a!" d-val))))
                            (otherwise (error "mjr_fsamp_dq-func-r123-r123: Function return type not supported: ~a!" d-val)))
              do (mjr_dquad_add-data daDquad d :ano-nam d-lab :ano-typ d-typ))
        (if list-of-ax-color-meths
            (loop for i from 0
                  for c-cm  in list-of-ax-color-meths
                  for c-lab in list-of-ax-color-labs
                  for c-as  = (if list-of-ax-color-as (mjr_util_elt-mod list-of-ax-color-as i))
                  for c-at  = (if list-of-ax-color-at (mjr_util_elt-mod list-of-ax-color-at i))
                  for c-mc  = (if list-of-ax-color-mc (mjr_util_elt-mod list-of-ax-color-mc i))
                  do (apply #'mjr_dquad_colorize daDquad :ano-nam c-lab :axes 't (append (if c-cm (list :color-method c-cm))
                                                                                         (if c-as (list :auto-scale   c-as))
                                                                                         (if c-mc (list :max-color    c-mc))))))
        (if list-of-f-color-meths
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec (length list-of-funcs)))))

              (loop for i from 0
                    for c-cm  in list-of-f-color-meths
                    for c-lab in list-of-f-color-labs
                    for c-as  = (if list-of-f-color-as (mjr_util_elt-mod list-of-f-color-as i))
                    for c-at  = (if list-of-f-color-at (mjr_util_elt-mod list-of-f-color-at i))
                    for c-mc  = (if list-of-f-color-mc (mjr_util_elt-mod list-of-f-color-mc i))
                    do (apply #'mjr_dquad_colorize daDquad :ano-nam c-lab :data data-idxs (append (if c-cm (list :color-method c-cm))
                                                                                                  (if c-as (list :auto-scale   c-as))
                                                                                                  (if c-mc (list :max-color    c-mc)))))))
        (if list-of-all-color-meths
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec (length list-of-funcs)))))
              (loop for i from 0
                    for c-cm  in list-of-all-color-meths
                    for c-lab in list-of-all-color-labs
                    for c-as  = (if list-of-all-color-as (mjr_util_elt-mod list-of-all-color-as i))
                    for c-at  = (if list-of-all-color-at (mjr_util_elt-mod list-of-all-color-at i))
                    for c-mc  = (if list-of-all-color-mc (mjr_util_elt-mod list-of-all-color-mc i))
                    do (apply #'mjr_dquad_colorize daDquad :ano-nam c-lab :axes 't :data data-idxs (append (if c-cm (list :color-method c-cm))
                                                                                                           (if c-as (list :auto-scale   c-as))
                                                                                                           (if c-mc (list :max-color    c-mc)))))))
        daDquad))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_ds-func-r123-r123 (f &key
                                        (func-lab "f")
                                        xdat ydat (arg-mode :arg-number) (xlab "x") (ylab "y")
                                        ax-color-meth  (ax-color-max-color  1) (ax-color-lab  "c") (ax-color-auto-scale  't) (ax-color-ano-typ  :ano-typ-rgbvec)
                                        f-color-meth   (f-color-max-color   1) (f-color-lab   "c") (f-color-auto-scale   't) (f-color-ano-typ   :ano-typ-rgbvec)
                                        all-color-meth (all-color-max-color 1) (all-color-lab "c") (all-color-auto-scale 't) (all-color-ano-typ :ano-typ-rgbvec)
                                        u-close v-close)
  "Transform a mathematical function into a dsimp list
     All arguments are as documented in mjr_fsamp_dq-func-r123-r123, with the following additions:
       - u-close v-close ............ Close the curve or surface
Examples:
 * (let* ((n     5)
          (data (mjr_fsamp_ds-func-r123-r123 (lambda (x y)
                                               (let ((a 1)
                                                     (c 3))
                                                 (vector (* (cos x) (+ c (* a (cos y))))
                                                         (* (sin x) (+ c (* a (cos y))))
                                                         (* a (sin y)))))
                                             :xdat (list :start 0 :end (- (* 2 pi) (/ (* 2 pi) n)) :len n)
                                             :ydat (list :start 0 :end (- (* 2 pi) (/ (* 2 pi) n)) :len n)
                                             :u-close 't
                                             :v-close 't)))
     (mjr_ply3d_from-dsimp \"torus.ply\" data))"
  (mjr_dsimp_make-from-dquad (mjr_fsamp_dq-func-r123-r123 f
                                                          :xdat xdat :ydat ydat :arg-mode arg-mode :xlab xlab :ylab ylab
                                                          :func-lab func-lab
                                                          :ax-color-meth ax-color-meth :ax-color-max-color
                                                          ax-color-max-color :ax-color-lab ax-color-lab :ax-color-auto-scale
                                                          ax-color-auto-scale :ax-color-ano-typ
                                                          ax-color-ano-typ :f-color-meth f-color-meth :f-color-max-color
                                                          f-color-max-color :f-color-lab f-color-lab :f-color-auto-scale
                                                          f-color-auto-scale :f-color-ano-typ
                                                          f-color-ano-typ :all-color-meth
                                                          all-color-meth :all-color-max-color
                                                          all-color-max-color :all-color-lab
                                                          all-color-lab :all-color-auto-scale
                                                          all-color-auto-scale :all-color-ano-typ all-color-ano-typ)
                             (apply #'list 0 (if ydat (list 1)))
                             0
                             :data 't :u-close u-close :v-close v-close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-complex-circle-image (func &key (radii 1) (len 3000) (include-circles nil) return-complex)
  "Compute LEN sample points in FUNC(z) for |z|=r for each r in RADII

Arguments:
 * INCLUDE-CIRCLES .. Also compute LEN sample points for the circle |z|=r for each r in RADII
 * RADII ............ May be a single positive real number or a list of same

Examples:
 * (mjr_gnupl_dquad (mjr_fsamp_dq-complex-circle-image #'sin :radii (loop for i from 1 upto 5 by .1 collect i) :len 1000) :main \"Complex Circle Images\")"
  (let ((data     (mjr_dquad_make-from-axis "phase" (list :start 0.0D0 :end (* 2.0d0 pi) :len len)))
        (radii (mjr_util_non-list-then-list radii)))
    (dolist (radius radii)
      (if include-circles
          (mjr_dquad_add-data-from-map data
                                       (lambda (a) (if return-complex
                                                       (complex (* radius (cos a)) (* radius (sin a)))        ;; (exp (* i a))
                                                       (vector  (* radius (cos a)) (* radius (sin a)))))
                                       :axes 0
                                       :ano-nam (format nil "c=~a" radius)
                                       :ano-typ :ano-typ-rvec))
      (mjr_dquad_add-data-from-map data
                                   (lambda (a) (let ((v (funcall func (complex (* radius (cos a)) (* radius (sin a))))))
                                                 (if return-complex
                                                     v
                                                     (mjr_numu_complex-to-vector v))))
                                   :axes 0
                                   :ano-nam (format nil "r=~a" radius)
                                   :ano-typ :ano-typ-rvec))
    data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-complex-circle-image-mag (func &key (radii 1) (len 3000) (do-log 't) (include-circles nil) return-complex (polar 't))
  "Compute LEN sample points of $\exp(i\cdot a)\cdot \vert \mathrm{FUNC}(\mathrm{RADII}\cdot\exp(i\cdot a))\vert$ on $a\in[0,2\pi]$

In other words, we take a point $x+yi$ on the circle $|x+yi|=r$ for $r\in\mathrm{RADII}$, and map it to $(x+yi)\cdot\vert\mathrm{FUNC}(x+yi)\vert$

When FUNC is a polynomial, $r=1$, and DO-LOG is non-NIL, the resulting function is closely related to the Mahler measure:

   $$\ln(M(p))=\frac{1}{2\pi}\int_0^{2\pi}ln(\vert p(e^{i\theta})\vert)\,\mathrm{d}\theta$$

Arguments:
 * INCLUDE-CIRCLES .. Also compute LEN sample points for the circle |z|=r for each r in RADII
 * RADII ............ May be a single positive real number or a list of same
 * DO-LOG ........... Compute the natural logarithm of the magnitude instead of the magnitude.

Examples:
 * (mjr_gnupl_dquad (mjr_fsamp_dq-complex-circle-image-mag #'sin :radii (loop for i from .8 upto .9 by .01 collect i) :len 1000) :main \"Complex Circle Magnitude\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-complex-circle-image-mag (lambda (z) (mjr_poly_eval #(1 0 -1 1 0 -1 0 1 -1 0 1) z)) :len 4000)            :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-complex-circle-image-mag (lambda (z) (mjr_poly_eval #(1 0 -1 1 0 -1 0 1 -1 0 1) z)) :len 4000 :polar nil) :main \"Mahler Plot\")"
  (cond ((and (not polar) include-circles) (error "mjr_fsamp_dq-complex-circle-image-mag: non-NIL :INCLUDE-CIRCLES requires non-NIL :POLAR may not be used in the same call"))
        )
  (let ((data  (mjr_dquad_make-from-axis "phase" (list :start 0.0D0 :end (* 2.0d0 pi) :len len)))
        (radii (mjr_util_non-list-then-list radii)))
    (dolist (radius radii)
      (if include-circles
          (mjr_dquad_add-data-from-map data
                                       (lambda (a) (if return-complex
                                                       (complex (* radius (cos a)) (* radius (sin a)))
                                                       (vector  (* radius (cos a)) (* radius (sin a)))))
                                       :axes 0
                                       :ano-nam (format nil "c=~a" radius)
                                       :ano-typ :ano-typ-rvec))
      (mjr_dquad_add-data-from-map data
                                   (lambda (a) (let* ((x (* radius (cos a)))
                                                      (y (* radius (sin a)))
                                                      (m (if do-log
                                                             (log (abs (funcall func (complex x y))))
                                                             (abs (funcall func (complex x y))))))
                                                 (if polar
                                                     (if return-complex
                                                         (complex (* m x) (* m y))
                                                         (vector  (* m x) (* m y)))
                                                     m)))
                                   :axes 0
                                   :ano-nam (format nil "r=~a" radius)
                                   :ano-typ (if polar
                                                (if return-complex
                                                    :ano-typ-complex
                                                    :ano-typ-rvec)
                                                :ano-typ-real)))
    data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-mahler-samp (p &rest rest)
  "Preform complex-circle-image-mag on the polynomial P.

Examples:
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 2 0 5/3 0 1 0 -5) :radii (loop for i from 1/6 upto 5/4 by 1/30 collect i) :do-log nil :len 1000) :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 2 0 5/3 0 1 0 -5) :radii (loop for i from 1/6 upto 5/4 by 1/30 collect i) :do-log 't  :len 1000) :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 2 0 5/3 0 1 0 -5) :radii '(5/6 11/10) :do-log 't :len 1000 :include-circles 't)                  :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 -1 1 0 -1 0 1 -1 0 1) :len 4000 :do-log 't  :polar 't ) :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 -1 1 0 -1 0 1 -1 0 1) :len 4000 :do-log nil :polar 't ) :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 -1 1 0 -1 0 1 -1 0 1) :len 4000 :do-log 't  :polar nil) :main \"Mahler Plot\")
 * (mjr_gnupl_dquad (mjr_fsamp_dq-mahler-samp #(1 0 -1 1 0 -1 0 1 -1 0 1) :len 4000 :do-log nil :polar nil) :main \"Mahler Plot\")"
  (apply #'mjr_fsamp_dq-complex-circle-image-mag (lambda (z) (mjr_poly_eval p z)) rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_ds-func-r1-rn-adaptive (f initial-mesh &key
                                                          (n                             1) ;; dimension of return from f
                                                          (clip-max                    nil) ;; Bounding box minimum.
                                                          (clip-min                    nil) ;; Bounding box maximum.
                                                          (do-clipped-1                 't) ;; refine segments with one clipped endentpoint
                                                          (do-clipped-2                nil) ;; refine segments with two clipped endpoints
                                                          (render-space-widths         nil) ;; Render space size
                                                          (fine-mesh-level              10) ;; Set finest mesh width to (expt 2 fine-mesh-level)/range
                                                          (coarse-mesh-level             4) ;; Set coarsest mesh width to (expt 2 fine-mesh-level)/range
                                                          (do-singularity-1             't) ;; refine segments with one signular endentpoint
                                                          (do-singularity-2            nil) ;; refine segments with two singular endpoints
                                                          (max-simplex-length          nil) ;; (max-min)/100
                                                          (min-simplex-length          nil) ;; (max-min)/10000
                                                          (max-bounding-box-width      nil)
                                                          (min-bounding-box-width      nil) ;; May be more useful than min-simplex-length
                                                          (max-bounding-box-area       nil)
                                                          (join-angle-limit            nil) ;; 0.5 degrees
                                                          (divergence-angle-limit      nil) ;; 0.5 degrees
                                                          (h-cutoff-eps                nil) ;; 0.01d0
                                                          (sandwich-area-cutoff        nil) ;;
                                                          (do-axis-crossing            nil)
                                                          (simplify-mesh-angle         nil) ;; 0.1 degrees
                                                          (show-progress               nil))
  ;; TODO: Move clipping & axis cross finding to end.  Don't keep all the bisection step simplexes, find corssings via bisection and add one vertex 
  "Adaptively sample a function $f:\\mathbb{R}\\rightarrow\\mathbb{R}^n$ and produce a set of 1-simplexes, in a DSIMP, approximating the parametric curve

The N argument is the dimension of the function F:
  - N = 1 ....... F must return a real number -- in this case the points on the curve are (t, f(t))
  - N = 2 or 3 .. F must return a vector of real numbers

Primary Use Case

   Many tools (Maple, Matlab, Mathematica, and Yacas) use some form of adaptive sampling to graph a function.  Most of the time this is all down without user
   intervention in order to automatically produce nice graphs.  Very few tools expose the results or provide much flexibility in how the sampling is
   preformed.  This function is designed to provide several methods of adaptability which may be composed together under user control.  The results are a
   DSIMP object (see MJR_DSIMP) which may be easily used by other tools.  The idea is to enable the end user to inject a bit of human guidance into the
   meshing process in order to obtain better results than a fully automatic scheme.  My primary use case is to create publication quality, low point count,
   meshes for inclusion into vector documents (PDF & Postscipt via LaTeX), and vector graphics formats (SVG).  My secondary use case is high quality, low
   simplex count, geometry for tools like VisIT, Paraview, PovRay, Meshlab, & Blender.  My tertiary use case is high point count graphs on raster displays
   where we can have as many sample points as we like, but it is wasteful to plot any pixel more than once.  Other use cases include visualization of 'method
   of lines' solutions, slice visualization for high dimensional manifolds, and high quality grid lines for 2D surfaces rendered in PovRay.

Meshing Method

   The initial approximation is computed using the mesh provided via the INITIAL-MESH argument -- which may be a two element vector containing just the x-min
   and x-max or a VVEC object (See: MJR_VVEC).  The simplex set is repeatedly refined (i.e. V0--V1 -> V0--VM--V2) until it can no longer be refined.  Note:
   VM=f((T0+T1)/2).  Each simplex is evaluated in turn as follows:

          -- Skip if marked good
          -- Domain (T-space) decisions
             -- Skip and mark good if T1-T0 is greater than (2^:FINE-MESH-LEVEL)/RANGE
             -- Refine if T1-T0 is greater than (2^:FINE-MESH-LEVEL)/RANGE
             -- Refine if V0 and V1 are both singularities and :DO-SINGULARITY-2 non-NIL
             -- Skip and mark good if V0 and V1 are both singularities and :DO-SINGULARITY-2 is NIL
          -- Range (Render space) decisions
             -- Refine Simplex if one of V0, VM, or V1 is a singularity and :DO-SINGULARITY-1 non-NIL
             -- Skip and mark good if one of V0, VM, or V1 is a singularity and :DO-SINGULARITY-1 is NIL
             -- Skip and mark good if both V0 and V1 are clipped and :DO-CLIPPED-2 is NIL
             -- Skip and mark good if V0--VM shorter than :MIN-SIMPLEX-LENGTH
             -- Skip and mark good if V1--VM shorter than :MIN-SIMPLEX-LENGTH
             -- Skip and mark good if any side of the v0, v1, vm bounding is shorter than :MIN-BOUNDING-BOX-WIDTH
             -- Refine if v0--v1 longer than :MAX-SIMPLEX-LENGTH
             -- Refine if v0--v1 crosses an axis and :DO-AXIS-CROSSING is non-NIL
             -- Refine if angle A0 (VM--V0--V1) greater than :DIVERGENCE-ANGLE-LIMIT degrees
             -- Refine if angle A1 (VM--V1--V0) greater than :DIVERGENCE-ANGLE-LIMIT degrees
             -- Refine if angle AM (V1--VM--V0) deviates from 180 degrees by :JOIN-ANGLE-LIMIT
             -- Refine if bounding box of {V0, VM, V1} has a side longer than :MAX-BOUNDING-BOX-WIDTH
             -- Refine if bounding box of {V0, VM, V1} has an area/volume larger than :MAX-BOUNDING-BOX-AREA
             -- Refine if A is greater than :SANDWICH-AREA-CUTOFF
             -- Refine if H (VM--VC) is longer than :H-CUTOFF-EPS
             -- Refine if V0 and V1 are both clipped and :DO-CLIPPED-2 is non-NIL
             -- Refine if V0 OR V1 is clipped and do-clipped-1 is non-NIL
             -- Skip and mark good if V0 OR V1 is clipped and do-clipped-1 is NIL
             -- Refine if vm is clipped and do-clipped-1 is non-NIL
             -- Skip and mark good if vm is clipped and do-clipped-1 is NIL
             -- Skip and mark good

                                         VM o------------------------------------------------------------o V1     ---
                                           / .                                                         /           |
                                          /   AM                                             A1     /--            |
                                         /     .                                                 /--               |
                                        /       .                                            /---                  |
                                       /         .                                        /--                      |
                                      /           .   H                                /--                         |
                                     /             .                               /---                            |
                                    /               .                           /--                             Bounding
                                   /          A      .                      /---                                   |
                                  /                   .                  /--                                      Box
                                 /                     .              /--                                          |
                                /                       .         /---                                           Height
                               /                         .     /--                                                 |
                              /                           . /--                                                    |
                             /                          /--o VC                                                    |
                            /                        /--         V0--V1 :: Unrefined simplex                       |
                           /                      /--            V0--VM :: Refined simplex 0                       |
                          /                   /---               VM--V1 :: Refined simplex 1                       |
                         /                 /--                       A0 :: Angle V1--V0--VM                        |
                        /              /---                          A1 :: Angle V0--V1--VM                        |
                       /            /--                              AM :: Angle V0--VM--V1                        |
                      /   A0     /--                                 AS :: Angle V0--V1--V2                        |
                     /       /---                                     A :: Area of triangle V0--VM--V1             |
                    /     /--                                         H :: Length of VC--VM                        |
                   /   /--                                                 VC-VM is perpendicular to V0--V1)       |
               V0 o----                                                                                           ---

                  |-------------------------------------- Bounding Box Width ----------------------------|

Render Space

   We must generally apply scale transformations to our graphs to get them to fit on our screens and journal pages, and in some cases that scaling is quite
   extreme.  Therefore lengths in the original space in which our objects naturally live will be quite different from perceived distance after we render the
   final product.  For this reason geometric calculations and decisions about when to subdivide an interval sometimes need to be made in this so called
   'render space' where our plots take physical form.  This is achieved via three arguments:

        * :CLIP-MIN ............. Minimum point in the overall plot bounding box
        * :CLIP-MAX ............. Maximum point in the overall plot bounding box
        * :RENDER-SPACE-WIDTHS .. The length of render space in render space units.
          Examples:
            * #(11 8.5) for US letter in landscape orientation (units: inches)
            * #(1920 1080) FHD Display (units: pixels)
            * #(1024 1024 1024) For a 3D volume (units: voxels)

   When all three arguments are provided, then all geometric properties are evaluated in render space.  Note that distance and area arguments provided to this
   function must also be in render space units.  For example one would set :MIN-SIMPLEX-LENGTH in terms of inches when the render space is specified as in the
   example above for US letter paper.  If :clip-max, :clip-min, or :render-space-widths are missing, then all geometric computations are done in the original
   real space in which the function naturally lives -- this works well only when the final plot has the same aspect ratio as the real space.

Final Mesh simplification and conversion to dsimp object:
  Some intervals may be removed or combined with others:
     * N/A                 :: If an endpoint of a simplex is a singularity
     * N/A                 :: If an endpoint of a simplex is clipped
     * SIMPLIFY-MESH-ANGLE :: If two simplexes are joined at a nearly 180 degree angle

Usage Notes

  -- Limit the number of non-singularity samples 
     -- Always set :FINE-MESH-LEVEL -- this will limit the total number of samples and delta in t-space
     -- Always set :MIN-SIMPLEX-LENGTH or :MIN-BOUNDING-BOX-WIDTH -- this will prevent absurdly small intervals
  -- :do-singularity-1 and :do-singularity-2
     -- Singularity refinement occurs *before* geometric tests (i.e. things like :MIN-SIMPLEX-LENGTH don't matter)
     -- Singularity refinement can produce a great many samples (only limited by :FINE-MESH-LEVEL)
     -- :DO-SINGULARITY-2 (nil by default) is useful for *searching* for defined regions of a function
     -- :DO-SINGULARITY-1 ('t by default) is useful for finding the edges of undefined regions
  -- If the X vs Y scales are not similar, then *always* define :CLIP-MIN, :CLIP-MAX, AND :RENDER-SPACE-WIDTHS
  -- :MIN-BOUNDING-BOX-WIDTH  vs :MIN-SIMPLEX-LENGTH
     -- :MIN-BOUNDING-BOX-WIDTH implicitly sets :MIN-SIMPLEX-LENGTH to SQRT(2)*:MIN-BOUNDING-BOX-WIDTH
     -- MIN-BOUNDING-BOX-WIDTH may be more natural in when rendering onto a raster
  -- :JOIN-ANGLE-LIMIT vs :DIVERGENCE-ANGLE-LIMIT
     -- Just about the same concept.  If you use one, then you generally don't need to use the other.
  -- Include special points for F in :INITIAL-MESH.  Interesting points include:
     -- Asymptotes. ex: $0$ for $f=\\frac{1}{x^2}$ or $f=\\frac{1}{x}$
     -- Removable discontinuities. ex: $1$ for $f=\\frac{x^2-1}{x-1}$
     -- Jump discontinuities. ex: $\\mathbb{Z}$ for $f=\\mathrm{ceiling}(x)$
     -- Boundaries of undefined regions. ex: $\\pm 1$ for $f=\\sqrt{1-x^2}$ or $f=\\sqrt{x^2-1}$
     -- Infliction points. ex: $0$ for $f=x^3-x$
     -- Extrema. ex: $\\frac{\\pm 1}{\\sqrt{3}}$ for $f=x^3-x$
     -- Roots. ex: $0$ \\& $\\pm 1$ for $f=x^3-x$
     -- Axis intersections. ex: $1$ for $f=(x+1)^2$
     -- Points without a derivative. ex: $0$ for $f=\\vert x\\vert$
  -- Recipe for high sample plots on raster displays
     -- Set: :CLIP-MIN, :CLIP-MAX, AND :RENDER-SPACE-WIDTHS
     -- Set: :MIN-BOUNDING-BOX-WIDTH to 1
     -- Set: :MAX-BOUNDING-BOX-WIDTH to 5
     -- Set: :COARSE-MESH-LEVEL to 10
     -- Set: :FINE-MESH-LEVEL > 10 (say 15 or 18)

Examples:

     * (mjr_gnupl_dsimp (mjr_fsamp_ds-func-r1-rn-adaptive (lambda (x) (sin x))                                           ;; We graph the sin function
                                                          (mapcar (lambda (x) (* x pi)) (list -3/2 -1 -1/2 0 1/2 1 3/2)) ;; Zeros and Extrema included in initial mesh
                                                          :show-progress 't
                                                          :fine-mesh-level 15                                            ;; More of a safety bound
                                                          :coarse-mesh-level 0                                           ;; No forced extra samples 
                                                          :join-angle-limit 2                                            ;; Smooth line joins 
                                                          :min-simplex-length 10                                         ;; Simplexes no shorter than 10px
                                                          :clip-min #(-5 -1.1)                                           ;; Our plot range and render space
                                                          :clip-max #(5 1.1)                                             ;;   must be specified or our geometric
                                                          :render-space-widths #(1920 1080)                              ;;   calculations will be wrong!!
                                                          )
                        :xlim '(-5 5) :ylim '(-1.1 1.1)
                        :type :b)
     *  (mjr_gnupl_dsimp (mjr_fsamp_ds-func-r1-rn-adaptive (lambda (x) (sqrt (- 1 (* x x))))  ;; Upper half circle.
                                                           '(-1.3 1.2)                        ;; Asymetric starting interval -- '(-1 1) for less work getting a nice plot
                                                           :show-progress 't
                                                           :fine-mesh-level 20                ;; Pretty small fine mesh size
                                                           :coarse-mesh-level 2               ;; Refine twice no matter what
                                                           :max-simplex-length 0.1            ;; Make sure all initial simplexes are small
                                                           :do-axis-crossing 't               ;; Find the y-axis crossing
                                                           :clip-max #(.5 5.)                 ;; Just for fun, chop off some of the circle
                                                           :simplify-mesh-angle 2             ;; Simplify mesh at the end
                                                           )
                         :type :b
                         :xlim '(-1.2 1.2) :ylim '(-.1 1.2))                                  ;; Almost square plotting region, so no :render-space-widths
     *  (mjr_gnupl_dsimp (mjr_fsamp_ds-func-r1-rn-adaptive (lambda (x) (- (* x x x) (* 2 x x) -1))
                                                           '(-2.3 2.2)                        ;; Asymetric starting interval -- use '(-1 1) for less work getting a nice plot
                                                           :show-progress 't
                                                           :fine-mesh-level 20                ;; Pretty small fine mesh size
                                                           :coarse-mesh-level 2               ;; Refine twice no matter what
                                                           :max-simplex-length 20             ;; Make sure all initial simplexes no longer than 20 pixels
                                                           :do-axis-crossing 5                ;; Find all axis crossings to within 5 pixels
                                                           :clip-min #(-3 -15)
                                                           :clip-max #(3 5)
                                                           :render-space-widths #(1920 1080)
                                                           :simplify-mesh-angle 0.5        ;; Pretty small.  For a super smooth plot
                                                           )
                         :type :l)
     *  (mjr_gnupl_dsimp (mjr_fsamp_ds-func-r1-rn-adaptive (lambda (x) (/ (* x x))) '(-12 12)
                                                           :show-progress 't
                                                           :fine-mesh-level 18
                                                           :coarse-mesh-level 0
                                                           :max-simplex-length 10
                                                           :clip-min #(-15 -1)
                                                           :clip-max #(15 150)                    ;; Super tall vs width
                                                           :render-space-widths #(1920 1080)
                                                           :simplify-mesh-angle 2
                                                           )
                         :xlim '(-16 16) :ylim '(-2 151)
                         :type :b)
     *  (mjr_gnupl_dsimp (mjr_fsamp_ds-func-r1-rn-adaptive (lambda (x) (vector (* x (sin x) (cos x) (sin x)) (sin x) (cos x)))
                                                           (list 0 (* 10 pi))
                                                           :n 3
                                                           :show-progress 't
                                                           :fine-mesh-level 12
                                                           :coarse-mesh-level 5
                                                           :max-simplex-length 40
                                                           :clip-min #(-12 -1 -1)
                                                           :clip-max #(12 1 1)
                                                           :render-space-widths #(1000 1000 1000))
                         :type :b :dimz 2)"
  (if render-space-widths
      (cond ((not (vectorp render-space-widths))           (error "mjr_fsamp_ds-func-r1-rn-adaptive: :RENDER-SPACE-WIDTHS must be a vector!"))
            ((not (every #'realp render-space-widths))     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :RENDER-SPACE-WIDTHS must contain only real numbers!"))
            ((not (every #'plusp render-space-widths))     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :RENDER-SPACE-WIDTHS must contain only positive numbers!"))))
  (if clip-min
      (cond ((not (vectorp clip-min))                      (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MIN must be a vector!"))
            ((not (every #'realp clip-min))                (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MIN must contain only real numbers!"))))
  (if clip-max
      (cond ((not (vectorp clip-max))                      (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MAX must be a vector!"))
            ((not (every #'realp clip-max))                (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MAX must contain only real numbers!"))))
  (if (and clip-min clip-max)
      (cond ((not (= (length clip-max) (length clip-min))) (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MIN and clip-max must be the same length!"))
            ((some #'<= clip-max clip-min)                 (error "mjr_fsamp_ds-func-r1-rn-adaptive: :CLIP-MAX must not be smaller than clip-min"))))
  (if join-angle-limit
      (cond ((>= 0   join-angle-limit)                     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :JOIN-ANGLE-LIMIT must be positive"))
            ((<= 180 join-angle-limit)                     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :JOIN-ANGLE-LIMIT must be less than 180 degrees"))
            ((< 5    join-angle-limit)                     (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :JOIN-ANGLE-LIMIT works best when less than 5 degrees."))
            ((> .1   join-angle-limit)                     (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :JOIN-ANGLE-LIMIT works best when greater than .1 degrees."))))
  (if divergence-angle-limit
      (cond ((>= 0   divergence-angle-limit)               (error "mjr_fsamp_ds-func-r1-rn-adaptive: :DIVERGENCE-ANGLE-LIMIT must be positive"))
            ((<= 180 divergence-angle-limit)               (error "mjr_fsamp_ds-func-r1-rn-adaptive: :DIVERGENCE-ANGLE-LIMIT must be less than 180 degrees"))
            ((< 5    divergence-angle-limit)               (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :DIVERGENCE-ANGLE-LIMIT works best when less than degrees."))
            ((> .1   divergence-angle-limit)               (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :DIVERGENCE-ANGLE-LIMIT works best when greater than .1 degrees."))))
  (if simplify-mesh-angle
    (cond ((>= 0 simplify-mesh-angle)                      (error "mjr_fsamp_ds-func-r1-rn-adaptive: :SIMPLIFY-MESH-ANGLE must be positive"))
          ((<= 180 simplify-mesh-angle)                    (error "mjr_fsamp_ds-func-r1-rn-adaptive: :SIMPLIFY-MESH-ANGLE must be less than 180"))
          ((< 5 simplify-mesh-angle)                       (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :SIMPLIFY-MESH-ANGLE WORKS best when less than 5 degrees."))
          ((> .1 simplify-mesh-angle)                      (warn  "mjr_fsamp_ds-func-r1-rn-adaptive: :SIMPLIFY-MESH-ANGLE WORKS best when greater than .1 degrees."))))
  (if fine-mesh-level
      (cond ((not (integerp fine-mesh-level))              (error "mjr_fsamp_ds-func-r1-rn-adaptive: :FINE-MESH-LEVEL must be an integer!"))
            ((minusp fine-mesh-level)                      (error "mjr_fsamp_ds-func-r1-rn-adaptive: :FINE-MESH-LEVEL must be non-negative!"))
            ((< 21 fine-mesh-level)                        (error "mjr_fsamp_ds-func-r1-rn-adaptive: :FINE-MESH-LEVEL must be less than 21!"))))
  (if coarse-mesh-level
      (cond ((not (integerp coarse-mesh-level))            (error "mjr_fsamp_ds-func-r1-rn-adaptive: :COARSE-MESH-LEVEL must be an integer!"))
            ((minusp coarse-mesh-level)                    (error "mjr_fsamp_ds-func-r1-rn-adaptive: :COARSE-MESH-LEVEL must be non-negative!"))
            ((< 21 coarse-mesh-level)                      (error "mjr_fsamp_ds-func-r1-rn-adaptive: :COARSE-MESH-LEVEL must be less than 21!"))))
  (if (and fine-mesh-level coarse-mesh-level)
      (cond ((< fine-mesh-level coarse-mesh-level)         (error "mjr_fsamp_ds-func-r1-rn-adaptive: FINE-MESH-LEVEL must not be smaller than COARSE-MESH-LEVEL"))))
  (if n
      (cond ((not (integerp n))                            (error "mjr_fsamp_ds-func-r1-rn-adaptive: :N must be an integer!"))
            ((> 1 n)                                       (error "mjr_fsamp_ds-func-r1-rn-adaptive: :N must be at least 1!"))
            ((< 3 n)                                       (error "mjr_fsamp_ds-func-r1-rn-adaptive: :N must be at most 3!"))))
  (if h-cutoff-eps
      (cond ((not (realp h-cutoff-eps))                    (error "mjr_fsamp_ds-func-r1-rn-adaptive: :H-CUTOFF-EPS must be a real number!"))
            ((>= 0 h-cutoff-eps)                           (error "mjr_fsamp_ds-func-r1-rn-adaptive: :H-CUTOFF-EPS must be positive"))))
  (if sandwich-area-cutoff
      (cond ((not (realp sandwich-area-cutoff))            (error "mjr_fsamp_ds-func-r1-rn-adaptive: :SANDWICH-AREA-CUTOFF must be a real number!"))
            ((>= 0 sandwich-area-cutoff)                   (error "mjr_fsamp_ds-func-r1-rn-adaptive: :SANDWICH-AREA-CUTOFF must be positive"))))
  (if max-bounding-box-width
      (cond ((not (realp max-bounding-box-width))          (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-BOUNDING-BOX-WIDTH must be a real number!"))
            ((>= 0 max-bounding-box-width)                 (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-BOUNDING-BOX-WIDTH must be positive"))))
  (if min-bounding-box-width
      (cond ((not (realp min-bounding-box-width))          (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MIN-BOUNDING-BOX-WIDTH must be a real number!"))
            ((>= 0 min-bounding-box-width)                 (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MIN-BOUNDING-BOX-WIDTH must be positive"))))
  (if max-bounding-box-area
      (cond ((not (realp max-bounding-box-area))           (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-BOUNDING-BOX-AREA must be a real number!"))
            ((>= 0 max-bounding-box-area)                  (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-BOUNDING-BOX-AREA must be positive"))))
  (if (and max-bounding-box-width min-bounding-box-width)
      (cond ((< max-bounding-box-width
                min-bounding-box-width)                    (error "mjr_fsamp_ds-func-r1-rn-adaptive: MAX-BOUNDING-BOX-WIDTH must not be smaller than min-bounding-box-width"))))
  (if min-simplex-length
      (cond ((not (realp min-simplex-length))              (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MIN-SIMPLEX-LENGTH must be a real number!"))
            ((>= 0 min-simplex-length)                     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MIN-SIMPLEX-LENGTH must be positive"))))
  (if max-simplex-length
      (cond ((not (realp max-simplex-length))              (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-SIMPLEX-LENGTH must be a real number!"))
            ((>= 0 max-simplex-length)                     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-SIMPLEX-LENGTH must be positive"))))
  (if (and max-simplex-length min-simplex-length)
      (cond ((< max-simplex-length min-simplex-length)     (error "mjr_fsamp_ds-func-r1-rn-adaptive: :MAX-SIMPLEX-LENGTH must not be smaller than :MIN-SIMPLEX-LENGTH"))))
  (if (not (or fine-mesh-level
               min-simplex-length
               min-bounding-box-width))                    (error "mjr_fsamp_ds-func-r1-rn-adaptive: Set at least one of: :FINE-MESH-LEVEL, :MIN-SIMPLEX-LENGTH, or :MIN-BOUNDING-BOX-WIDTH"))
    (labels ((fadpt (x) (let* ((y  (ignore-errors (funcall f x)))
                             (yv (if (= 1 n) (vector x y) y)))
                        (if (and (vectorp yv)
                                 (every #'realp yv))
                            yv))))
    (let* ((max-simplex-length-sq  (if max-simplex-length (expt max-simplex-length 2)))
           (min-simplex-length-sq  (if min-simplex-length (expt min-simplex-length 2)))
           (initial-mesh           (mjr_vvec_to-vec initial-mesh))
           (xmax                   (aref initial-mesh (1- (length initial-mesh))))
           (xmin                   (aref initial-mesh 0))
           (coarse-mesh-delta      (if coarse-mesh-level (/ (- xmax xmin) (expt 2 coarse-mesh-level))))
           (fine-mesh-delta        (if fine-mesh-level   (/ (- xmax xmin) (expt 2 fine-mesh-level))))
           (working-state          (loop for i from 2 upto (length initial-mesh) collect -1))              ; Simplex state:  Mark all non-good at start
           (bad-state-count        (1- (length initial-mesh)))                                             ; Number of non-good simplexes
           (working-t              (map 'list (lambda (x) (float x 1.0d0)) initial-mesh))                  ; working sample t values
           (working-pnt            (map 'list #'fadpt working-t))                                          ; Working vector list (simplex endpoints)
           (join-angle-limit       (and join-angle-limit (mjr_a_d2r join-angle-limit)))                    ; Convert join-angle-limit to radians
           (divergence-angle-limit (and divergence-angle-limit (mjr_a_d2r divergence-angle-limit)))        ; Convert divergence-angle-limit to radians
           (simplify-mesh-angle    (and simplify-mesh-angle (mjr_a_d2r simplify-mesh-angle)))              ; Convert simplify-mesh-angle to radians
           (render-space-xform     (if (and clip-max clip-min render-space-widths) (mjr_vec_ewbo render-space-widths (mjr_vec_- clip-max clip-min) #'/))))
      (labels ((xform2render (v) (if render-space-xform (map 'vector #'* render-space-xform v) v)))
        (if show-progress (format 't "~3@a: ~15@a ~15@a ~15@a ~15@a~%" "i" "Total" "Bad" "Good" "Bad Counter"))
        (loop for i from 0 upto 33
              do (loop with working-loc-state = working-state
                       with working-loc-pnt   = working-pnt
                       with working-loc-t     = working-t
                       for s = (first working-loc-state)
                       do (multiple-value-bind (vm tm sn)
                              (if (plusp s)
                                  (values nil nil s)                                                                                                                                                          ;;   S: No Refine.  Previously marked ineligable
                                  (let* ((t0 (first working-loc-t))   ; "left" sample point
                                         (t1 (second working-loc-t))  ; "right" sample point                                                                                                                                       
                                         (td (- t1 t0)))              ; sample delta for simplex
                                    (if (and fine-mesh-delta (<= td fine-mesh-delta))
                                        (values nil nil 2)                                                                                                                                                    ;;   2: No Refine.  delta t too small                            -- :FINE-MESH-LEVEL is defined
                                        (let* ((v0 (first working-loc-pnt))  ; Simplex "left" vertex (corrisponds to t0)
                                               (v1 (second working-loc-pnt)) ; Simplex "left" vertex (corrisponds to t1)
                                               (tm (/ (+ t0 t1) 2.0d0))      ; New sample point midway between t0 and t1
                                               (vm (fadpt tm))               ; New simplex vertex for tm (might not get used)
                                               (sp (not (and v0 vm v1))))    ; At last one of v0, vm, v1 signular
                                          (if (and coarse-mesh-delta (>= td coarse-mesh-delta))                                                                                                                      
                                              (values vm tm -1)                                                                                                                                               ;;  -1: Refine: delta t too big                                  -- :COARSE-MESH-DELTA is defined
                                              (if sp
                                                  (let* ((sp2 (and sp (not v0) (not v1)))           ; Both end points are signular
                                                        (sp1 (and sp (not sp2) (not (and v0 v1))))  ; One, but not both, end point is singular
                                                        (spm (and sp v0 v1 (not vm))))              ; center, but not ends, is singular
                                                    (cond (sp2  (if do-singularity-2
                                                                    (values vm  tm -3)                                                                                                                        ;;  -3: Refine: Both endpoints signular                          -- :DO-SINGULARITY-2 is 't
                                                                    (values nil nil 3)))                                                                                                                      ;;   3: No Refine. Both endpoints signular                       -- :DO-SINGULARITY-2 is nil
                                                          (sp1  (if do-singularity-1
                                                                    (values vm tm -4)                                                                                                                         ;;  -4: Refine: One endpoint signular                            -- :DO-SINGULARITY-1 is 't
                                                                    (values nil nil 4)))                                                                                                                      ;;   4: No Refine. One endpoint signular                         -- :DO-SINGULARITY-1 is nil
                                                          (spm  (if do-singularity-1
                                                                    (values vm tm -5)                                                                                                                         ;;  -5: Refine: Midpoint is signular                             -- :DO-SINGULARITY-1 is 't
                                                                    (values nil nil 5)))))                                                                                                                    ;;   5: No Refine. Midpoint is signular                          -- :DO-SINGULARITY-1 is nil
                                                  (let* ((c0 (not (mjr_geom_point-inside-bounding-box-f? v0 clip-min clip-max)))  ; v0 clipped
                                                         (c1 (not (mjr_geom_point-inside-bounding-box-f? v1 clip-min clip-max)))  ; v1 clipped
                                                         (cm (not (mjr_geom_point-inside-bounding-box-f? vm clip-min clip-max)))  ; vm clipped
                                                         (cp  (or c0 c1 cm))                 ; At last one of v0, vm, v1 clipped
                                                         (cp2 (and cp (and c0 c1)))          ; Both end points clipped
                                                         (cp1 (and cp (not cp2) (or c0 c1))) ; One, but not both, end points clipped
                                                         (cpm (and cp (not cp1) cm))         ; center, but not ends, clipped
                                                         (v0t (xform2render v0))  ; v0 transformed to redner space
                                                         (vmt (xform2render vm))  ; vm transformed to redner space
                                                         (v1t (xform2render v1))  ; v1 transformed to redner space
                                                         (bbw (if (or max-bounding-box-width max-bounding-box-area min-bounding-box-width) (mjr_geom_bounding-widths v0t v1t)))
                                                         (a   (if (or h-cutoff-eps sandwich-area-cutoff) (mjr_geom_simplex-area v0t vmt v1t)))) ; Area of v0-vm-vt 2-simplex
                                                    (cond ((and (not do-clipped-2) cp2)                                                                          (values nil nil   6)) ;;   6: No Refine: both ends clipped                                -- :do-clipped-2 is NIL
                                                          ((and min-simplex-length-sq (>= min-simplex-length-sq (mjr_geom_distance-euclidean-squared v0t vmt)))  (values nil nil   7)) ;;   7: No Refine: V0-VM too short                                  -- :MIN-SIMPLEX-LENGTH-SQ is defined
                                                          ((and min-simplex-length-sq (>= min-simplex-length-sq (mjr_geom_distance-euclidean-squared v1t vmt)))  (values nil nil   8)) ;;   8: No Refine: VM-V1 too short                                  -- :MIN-SIMPLEX-LENGTH-SQ is defined
                                                          ((and min-bounding-box-width (some (lambda (x) (<= x min-bounding-box-width)) bbw))                    (values nil nil   9)) ;;   9: No Refine: bounding box too small                        -- :MIN-BOUNDING-BOX-WIDTH is defined
                                                          ((and max-simplex-length-sq (> (mjr_geom_distance-euclidean-squared v0t v1t) max-simplex-length-sq))   (values vm  tm   -6)) ;;  -6: Refine: Simplex too long                                 -- :MAX-SIMPLEX-LENGTH is defined
                                                          ((and do-axis-crossing (some #'minusp (map 'list (lambda (a b) (* (signum a) (signum b))) v0t v1t)))   (values vm  tm   -7)) ;;  -7: Refine: axis crossing                                    -- :DO-AXIS-CROSSING is 't
                                                          ((and divergence-angle-limit (> (mjr_geom_tri-angle-f vmt v0t v1t) divergence-angle-limit))            (values vm  tm   -8)) ;;  -8: Refine: left angle too big                               -- :DIVERGENCE-ANGLE-LIMIT is defined
                                                          ((and divergence-angle-limit (> (mjr_geom_tri-angle-f vmt v1t v0t) divergence-angle-limit))            (values vm  tm   -9)) ;;  -9: Refine: right angle too big                              -- :DIVERGENCE-ANGLE-LIMIT is defined
                                                          ((and join-angle-limit (> (mjr_numu_absdif pi (mjr_geom_tri-angle-f v1t vmt v0t)) join-angle-limit))   (values vm  tm  -10)) ;; -10: Refine: center angle not near 180                        -- :JOIN-ANGLE-LIMIT is defined
                                                          ((and max-bounding-box-width (some (lambda (x) (> (abs x) max-bounding-box-width)) bbw))               (values vm  tm  -11)) ;; -11: Refine: unrefined simplex bb area too large              -- :MAX-BOUNDING-BOX-WIDTH is defined
                                                          ((and max-bounding-box-area (> (reduce #'* bbw) max-bounding-box-area))                                (values vm  tm  -12)) ;; -12: Refine: unrefined simplex bb side too long               -- :MAX-BOUNDING-BOX-AREA is defined
                                                          ((and sandwich-area-cutoff (> a sandwich-area-cutoff))                                                 (values vm  tm  -13)) ;; -13: Refine: Area between old and new simplex                 -- :SANDWICH-AREA-CUTOFF is defined
                                                          ((and h-cutoff-eps (> (/ (* 2 a) (mjr_geom_distance-euclidean-squared v0t v1t)) h-cutoff-eps))         (values vm  tm  -14)) ;; -14: Refine: New vertex is too far away from origonal simplex -- :H-CUTOFF-EPS is defined
                                                          ((and do-clipped-2 cp2)                                                                                (values vm  tm  -15)) ;; -15: Refine: Both endpoints clipped                           -- :DO-CLIPPED-2 is 't
                                                          (cp1 (if do-clipped-1
                                                                   (values vm  tm  -16)                                                                                                ;; -16: Refine: One endpoint clipped                             -- :DO-CLIPPED-1 is 't
                                                                   (values nil nil  10)))                                                                                              ;;  10: No Refine: V0 or V1 clipped                             -- :DO-CLIPPED-1 is NIL
                                                          (cpm (if do-clipped-1
                                                                   (values vm  tm  -17)                                                                                                ;; -17: Refine: Midpoint is clipped                              -- :DO-CLIPPED-1 is 't                                                                              
                                                                   (values nil nil  11)))                                                                                              ;;  11: No Refine: VM clipped                                  -- :DO-CLIPPED-1 is NIL
                                                          ('t (values nil nil 1))))))))))                                                                                              ;;   1: No Refine. Good for all the right reasons
                            (if (minusp sn)
                                (progn
                                  (setf (first working-loc-state) sn)
                                  (push vm  (cdr working-loc-pnt))
                                  (push sn  (cdr working-loc-state))
                                  (push tm  (cdr working-loc-t))
                                  (incf bad-state-count)
                                  (setf working-loc-state (cddr working-loc-state))
                                  (setf working-loc-pnt   (cddr working-loc-pnt))
                                  (setf working-loc-t     (cddr working-loc-t)))
                                (progn (setf (first working-loc-state) sn)
                                       (if (minusp s) (decf bad-state-count))
                                       (setf working-loc-state (cdr working-loc-state))
                                       (setf working-loc-pnt   (cdr working-loc-pnt))
                                       (setf working-loc-t     (cdr working-loc-t)))))
                       while (cdr working-loc-pnt))
              do (if show-progress (format 't "~3d: ~15d ~15d ~15d ~15d~%" i (length working-state) (count-if #'minusp working-state) (count-if #'plusp working-state) bad-state-count))
              while (not (zerop bad-state-count)))
        (if show-progress (format 't "Sampled Points ............ ~d~%" (length working-pnt)))
        (let ((good-pnt        nil)
              (good-pnt-size   0)
              (simp-pnt-cnt    0)
              (sing-pnt-cnt    0)
              (clip-pnt-cnt    0)
              (good-edges      nil)
              (good-edges-size 0))
          (if simplify-mesh-angle
              (loop for simp-pnt-cnt-b4 = simp-pnt-cnt
                    do (loop with pv0 = working-pnt
                             with pvm = (cdr working-pnt)
                             with pv1 = (cddr working-pnt)
                             for v0 = (car pv0)
                             for vm = (car pvm)
                             for v1 = (car pv1)
                             do (if (and v0 vm v1
                                         (mjr_geom_point-inside-bounding-box-f? (list v0 vm v1) clip-min clip-max) ; wasteful to do this over and over...
                                         (< (mjr_numu_absdif pi (mjr_geom_tri-angle-f (xform2render v0) (xform2render vm) (xform2render v1))) simplify-mesh-angle))
                                    (progn
                                      (incf simp-pnt-cnt)
                                      (setf (cdr pv0) pv1)
                                      (setq pv0 (cdr  pv0))
                                      (setq pvm (cddr pvm))
                                      (setq pv1 (cddr pv1)))
                                    (progn
                                      (setq pv0 (cdr pv0))
                                      (setq pvm (cdr pvm))
                                      (setq pv1 (cdr pv1))))
                             while pv1)
                    until (= simp-pnt-cnt simp-pnt-cnt-b4)))
          (loop with pushed-last = nil
                for v0 = (first working-pnt) then v1
                for v1 in (cdr working-pnt)
                do (if (some #'null (list v0 v1))
                       (progn (setf pushed-last nil)
                              (incf sing-pnt-cnt))
                       (if (mjr_geom_point-inside-bounding-box-f? (list v0 v1) clip-min clip-max)
                           (progn (if (not pushed-last)
                                      (progn (push v0 good-pnt)
                                             (incf good-pnt-size)))
                                  (setf pushed-last 't)
                                  (push v1 good-pnt)
                                  (incf good-pnt-size)
                                  (push (vector (- good-pnt-size 2) (- good-pnt-size 1)) good-edges)
                                  (incf good-edges-size))
                           (progn (setf pushed-last nil)
                                  (incf clip-pnt-cnt)))))
          (if show-progress (format 't "Simplified Point Count .... ~d~%" simp-pnt-cnt))
          (if show-progress (format 't "Singular Point Count ...... ~d~%" sing-pnt-cnt))
          (if show-progress (format 't "Clipped Point Count ....... ~d~%" clip-pnt-cnt))
          (if show-progress (format 't "Total Points Removed ...... ~d~%" (+ simp-pnt-cnt sing-pnt-cnt clip-pnt-cnt)))
          (if show-progress (format 't "Final Sample Point Count .. ~d~%" good-pnt-size))
          (if show-progress (format 't "Final Simplex Count ....... ~d~%" good-edges-size))
          (list '(0 0 0 0)
                (make-array good-pnt-size    :initial-contents (reverse good-pnt))
                (make-array good-edges-size  :initial-contents (reverse good-edges))
                nil
                nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_rp-func-r2-r1-adaptive (rptree-instance func
                                         &key
                                           (arg-mode                 :arg-number)
                                           (func-check               #'realp)
                                         ;;(clip-max                 nil) ;; REALPlot bounding box minimum.
                                         ;;(clip-min                 nil) ;; Plot bounding box maximum.
                                         ;;(max-depth                nil) ;; Smallest refinement we will do
                                         ;;(dim-scale-factors        nil) ;; LIST-OF-NUMBERS .... Define scale factors for computations mixing dimensions
                                        ;; Reasons to not refine
                                           (max-depth                nil) ;; INTEGER-OR-NIL ... Do not refine if depth is greater (quad is smaller)
                                         ;;(min-bounding-box-width   nil) ;; REAL ............. do not refine if bb width is smaller
                                         ;;(min-simplex-area         nil) ;; REAL ............. do not refine if simplex area is smaller
                                         ;;(domain-ROX               nil) ;; REGION-OR-LOS .... Do not refine if all vertexes inside region. ROX == Region Of eXclusion.  LOS == List Of Same
                                         ;;(range-ROX                nil) ;; REGION-OR-LOS .... Do not refine if all vertexes inside region.
                                         ;;(space-ROX                nil) ;; REGION-OR-LOS .... Do not refine if all vertexes inside region.
                                        ;; Reasons to refine
                                           (min-depth                  4) ;; INTEGER .......... Refine if quad depth in is smaller (quad is bigger)
                                         ;;(do-clipped-some           't) ;; BOOL ............. Refine quad with some, but not all, vertices clipped
                                         ;;(do-clipped-all           nil) ;; BOOL ............. Refine quad with all vertices clipped
                                           (refine-on-axis           nil) ;; AXES-OR-LOS ...... Refine if quad crosses axis
                                           (do-singularity-some       't) ;; BOOL ............. refine quad with at some, but not all, vertices singular
                                           (do-singularity-all       nil) ;; BOOL ............. refine quad with all vertices singular
                                           (slice                    nil) ;; SLICE-OR-LOS ..... Refine if quad crosses slice plane.  Slices are: '(axis offset)
                                         ;;(max-simplex-area         nil) ;; REAL ............. Refine if quad area is larger
                                         ;;(max-bounding-box-volume  nil) ;; REAL ............. Refine if bb volume is larger
                                         ;;(max-bounding-box-width   nil) ;; REAL ............. Refine if bb width is larger
                                         ;;(max-interquad-tri-angle  nil) ;; REAL ............. Refine if angle is larger -- Angles of triangles across quad neighbors
                                           (domain-POI               nil) ;; POINT-OR-LOS ..... refine if quad contains point or if it is "near" when domain-POI-near is REAL
                                           (domain-POI-near            1) ;; REAL ............. Define "near" for domain-POI
                                         ;;(domain-ROI               nil) ;; REGION-OR-LOS .... Refine if all quad verts are in region.  Regions are defined by signed distance functions
                                         ;;(domain-BOI               nil) ;; REGION-OR-LOS .... Refine if some vertices are in the region while others are outside
                                         ;;(range-ROI                nil) ;; REGION-OR-LOS .... Refine if all quad verts are in region.  Regions are defined by signed distance functions
                                         ;;(range-BOI                nil) ;; REGION-OR-LOS .... Refine if some vertices are in the region while others are outside
                                         ;;(space-ROI                nil) ;; REGION-OR-LOS .... Refine if all quad verts are in region.  Regions are defined by signed distance functions
                                         ;;(space-BOI                nil) ;; REGION-OR-LOS .... Refine if some vertices are in the region while others are outside
                                       ;; Other args
                                           (show-progress            nil))
  "Adaptively sample a function $f:\\mathbb{R}^2\\rightarrow\\mathbb{R}$ and produce a RPTREE.

       ############################################################################################################################################
       ############################################################################################################################################
       ==== Under Construction == Under Construction == Under Construction ==== Under Construction == Under Construction == Under Construction ====
       ============================================================================================================================================
       --------------------------------------------------------------------------------------------------------------------------------------------
       
                   #     #                                 #####                                                                                  
                   #     # #    # #####  ###### #####     #     #  ####  #    #  ####  ##### #####  #    #  ####  ##### #  ####  #    #           
                   #     # ##   # #    # #      #    #    #       #    # ##   # #        #   #    # #    # #    #   #   # #    # ##   #           
                   #     # # #  # #    # #####  #    #    #       #    # # #  #  ####    #   #    # #    # #        #   # #    # # #  #           
                   #     # #  # # #    # #      #####     #       #    # #  # #      #   #   #####  #    # #        #   # #    # #  # #           
                   #     # #   ## #    # #      #   #     #     # #    # #   ## #    #   #   #   #  #    # #    #   #   # #    # #   ##           
                    #####  #    # #####  ###### #    #     #####   ####  #    #  ####    #   #    #  ####   ####    #   #  ####  #    #           

       --------------------------------------------------------------------------------------------------------------------------------------------
       ============================================================================================================================================
       ==== Under Construction == Under Construction == Under Construction ==== Under Construction == Under Construction == Under Construction ====
       ############################################################################################################################################
       ############################################################################################################################################"
  (flet ((thing-to-axis (thing) (if (integerp thing)
                                    thing
                                    (let ((ax-number (position thing '(x y z) :test #'string-equal)))
                                      (if (numberp ax-number)
                                          ax-number
                                          (error (format nil "Unknown axis descripter: ~s" thing))))))
         (not-list-to-list (thingy) (if (listp thingy) thingy (list thingy))))
    (let ((slice           (mapcar (lambda (s) (cons (thing-to-axis (car s)) (cdr s))) (if (and (listp slice) (listp (car slice)))
                                                                                           slice
                                                                                           (list slice))))
          (refine-on-axis  (mapcar #'thing-to-axis (not-list-to-list refine-on-axis)))
          (domain-POI      (not-list-to-list domain-POI))
          )
      (let* ((topq (mjr_rptree_level-0-quad-center rptree-instance))
             (need-3 (or refine-on-axis
                         slice))
             (need-f (or do-singularity-some
                         do-singularity-all))
             (need-i (or need-f 
                         need-3))
             )
        (print slice)
    (labels ((refine-quad (quad-coord)
               (let ((children (mjr_rptree_quad-get-children rptree-instance quad-coord 1)))
                 (if (and children (mjr_rptree_coord-has-value rptree-instance (first children)))
                     (loop for c in children
                           sum (refine-quad c))
                     (let ((quad-depth (mjr_rptree_quad-get-level rptree-instance quad-coord)))
                       (if (null quad-depth)
                           0
                           (if (or (>= quad-depth (1- (mjr_rptree_struct-mesh-power rptree-instance)))
                                   (and max-depth (>= quad-depth max-depth)))                                                                                 ;; max-depth
                               0
                               (let* ((vertices-i  (if need-i (mjr_rptree_quad-get-points rptree-instance quad-coord)))
                                      (vertices-f  (if need-f (mapcar (lambda (c) (mjr_rptree_coord-get-value rptree-instance c)) vertices-i)))
                                      (vertices-3d (if need-3 (mjr_rptree_coord-2-real-points rptree-instance vertices-i))))
                                 (if (or
                                      (and min-depth (>= min-depth quad-depth))                                                                               ;; min-depth
                                      (and do-singularity-some (some  (lambda (z) (not (funcall func-check z))) vertices-f))                                  ;; do-singularity-some
                                      (and do-singularity-all  (every (lambda (z) (not (funcall func-check z))) vertices-f))                                  ;; do-singularity-all
                                      (and refine-on-axis (some (lambda (ax) (mjr_geom_points-cross-axis-plane? 0 ax vertices-3d)) refine-on-axis))           ;; refine-on-axis
                                      (and slice (some (lambda (s) (mjr_geom_points-cross-axis-plane? (second s) (first s) vertices-3d)) slice))              ;; slices
                                      (and domain-POI (some (lambda (p) (mjr_rptree_quad-near-real-point rptree-instance quad-coord p domain-POI-near)) domain-POI)) ;; domain-POI
                                      ;;(and do-clipped-some
                                      ;;(and do-clipped-all
                                      )
                                     (progn (mjr_rptree_uniform-fsamp rptree-instance func arg-mode func-check quad-coord 1)
                                            1)
                                     0)))))))))
      (loop for rc = (refine-quad topq)
            for p from 1 upto 10
            do (if show-progress
                   (format 't "QTREE Adaptive Sample: Pass ~5d: Refined: ~10d~%" p rc))
            until (zerop rc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO in order of priority
;;   - mjr_fsamp_rp-r2-r1-adaptive -- in work!
;;   - Integrate an optional "balance" step at the end of the sampling routines.
;;   - mjr_fsamp_rp-r1-r1-adaptive -- for (x, f(x)) type graphs
;;   - mjr_fsamp_rp-r3-r1-adaptive -- for implicit surfaces
;;   - mjr_fsamp_rp-r1-r2-adaptive --
;;   - mjr_fsamp_rp-r1-r3-adaptive
;;   - mjr_fsamp_rp-r2-r2-adaptive
;;   - mjr_fsamp_rp-r2-r3-adaptive
;;   - mjr_fsamp_rp-r3-r3-adaptive

