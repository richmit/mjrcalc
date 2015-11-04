;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      pre-fsamp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Sample mathematical functions and create DQUADs and DSIMPs.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1991, 1992, 1994, 1997, 2008, 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      Use MXP for functions in algebraic notation.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_FSAMP
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_NUMU
        :MJR_COMBC
        :MJR_ARR
        :MJR_UTIL
        :MJR_DQUAD
        :MJR_DSIMP
        :MJR_POLY)
  (:DOCUMENTATION "Brief: Data sets on QUADrilateral (rectilinear) grids.;")
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
           ))

(in-package :MJR_FSAMP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_help ()
  "Help for MJR_FSAMP:  Function SAMPle -- sample mathematical functions on a regular grid, and store results in a DQUAD or DSIMP

In practice DQUAD lists are frequently generated by sampling mathematical functions on a regular gird, and DSIMP lists are
frequently created from the DQUADs produced in this way.  This package is intended to make this process as simple as possible."
  (documentation 'mjr_fsamp_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_fsamp_dq-func-c1-c1 (f &key rdat idat f_color z_color zf_color)

  "Return a dquad list containing data useful for complex function visualization.

The returned dquad list will contain the following (in this order today, but use the data names in case the order changes):
  - AXIS: real     -- Re(z)
  - AXIS: imag     -- Im(z)
  - DATA: z        -- A complex value in the domain.  When exported to VTK, this will yield two scalar values: z_real & z_imag
  - DATA: f        -- f(z)                            When exported to VTK, this will yield two scalar values: f_real & f_imag
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
    (let* ((xdat      (mjr_vvec_to-vec-maybe xdat))
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
  "Transform one or more color functions (i.e. a functino returning a color when given 1, 2, or 3 real arguments) into a dquad"
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
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec-maybe (length list-of-funcs)))))
              
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
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec-maybe (length list-of-funcs)))))
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
       - u-close v-close ............ Close the curve or surface"
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
