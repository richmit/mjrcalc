;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-vtk.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Write VTK files.@EOL
;; @Keywords  lisp interactive vtk
;; @Std       Common Lisp
;;
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_VTK
  (:USE :COMMON-LISP
        :MJR_UTIL
        :MJR_VVEC
        :MJR_COMBC
        :MJR_MXP
        :MJR_ARR
        :MJR_ANNOT
        :MJR_DQUAD
        :MJR_DSIMP)
  (:DOCUMENTATION "Brief: Write VTK files.;")
  (:EXPORT #:mjr_vtk_help

           #:mjr_vtk_from-gndata
           #:mjr_vtk_grid-from-func

           #:mjr_vtk_polydata-from-func-r12-r123
           #:mjr_vtk_grid-from-func-c1-c1

           ;; Very experimental
           #:mjr_vtk_from-dsimp
           #:mjr_vtk_from-dquad
           ))

(in-package :MJR_VTK)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_help ()
  "Help for MJR_VTK: Create VTK files

The goal is to provide some basic functionality for transforming mathematical data (in the form of an array or embedded in a
function) into VTK files.  The real workhorses in this package are the mjr_vtk_from-gndata functions."
  (documentation 'mjr_vtk_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_from-gndata (out-file &key xdat ydat zdat scalar-array (scalar-names "s") vector-array (vector-names "v") color-array (color-names "c") file-note dims poly)
  "Write scalar and/or vector data from one or more arrays into a VTK file along with an appropriate geometric structure.

Arguments:
  * :XDAT, :YDAT, & :ZDAT describe the axis data.  They may each be an array (1D, 2D, or 3D) of numbers or a list of keyword
    arguments describing an arithmetic sequence (see MJR_VVEC_KW-NORMALIZE for details).
  * :SCALAR-ARRAY must be a numeric array (1D, 2D, or 3D), or or a list of numeric arrays.
  * :VECTOR-ARRAY is just like :SCALAR-ARRAY except that the array(s) must contain 3D vectors.
  * :COLOR-SCALAR-ARRAY is just like :VECTOR-ARRAY except that the array(s) must contain 3D floats in [0,1] representing RGB.
  * :SCALAR-NAMES & :VECTOR-NAMES are strings, or lists of strings, with names that will become the VTK data variable names.
  * :FILE-NOTE is a string used for the VTK file note.  It must not be more than 256 chars long.
  * :POLY is a boolean.  If non-NIL, then polygon (VERTEX, LINE, POLYGON) data will be produced.

If not directly supplied by DIMS, the geometric grid structure written into the VTK file will be determined from the dimensions
of the the first SCALAR-ARRAY or the first VECTOR-ARRAY if no SCALAR-ARRAY is non-NIL.  The file will contain POLYDATA if :POLY
is non-NIL, otherwise this function will automatically determine if the VTK file will contain a STRUCTURED_GRID or
RECTILINEAR_GRID.  :SCALAR-ARRAY, :VECTOR-ARRAY, and :COLOR-SCALAR-ARRAY are all POINT DATA.

The best source of examples for how to use this function is the MJR_VTK_FUNCTION2GRID function.

Limitations:
  * Tensors are not yet supported.
  * Multiple scalars will be put into different variables -- i.e. no multi-component scalars are used as some tools don't
    understand them.
  * Most tools don't understand 1D data (elevated or not).
  * Only floating point VTK data and points/vectors may be created.
  * VTK files are ASCII
  * This function is not terribly fast."
  (cond ((not (stringp out-file))   (error "mjr_vtk_from-gndata: OUT-FILE must be a string!")))
  (let* ((sdat (if (listp scalar-array)       scalar-array       (list scalar-array)))
         (vdat (if (listp vector-array)       vector-array       (list vector-array)))
         (cdat (if (listp color-array)        color-array        (list color-array)))
         (npts (apply #'max (mapcar (lambda (x) (if (arrayp x) (array-total-size x) 0))
                                    (append (list xdat ydat zdat) sdat vdat cdat))))         
         (dims (subseq (append (or dims
                                   (and (first cdat) (array-dimensions (first cdat)))
                                   (and (first vdat) (array-dimensions (first vdat)))
                                   (and (first sdat) (array-dimensions (first sdat))))
                               '(1 1 1)) 0 3))
         (kdat (mapcar (lambda (dat dim) (or (mjr_vvec_to-vec-maybe dat)
                                             (mjr_vvec_gen-0sim 'vector dim)))
                       (list xdat ydat zdat)
                       dims))
         (ksiz (mapcar #'array-total-size kdat))
         (d1ct (count 1 dims))
         (dest (open out-file :direction :output :if-exists :supersede :if-does-not-exist :create))
         (note (or file-note "File generated by Mitch Richling's MJR_VTK_ARRAY2VTK-TO-IMAGE package")))
    (format dest "# vtk DataFile Version 3.0~%")
    (format dest "~a~%" note)
    (format dest "ASCII~%")
    (if (and (null poly)
             (every #'vectorp  kdat)
             (every #'= dims (mapcar #'length kdat)))
        (loop for cur-array in kdat
              for npts   in dims
              for arr-name  in (list "X" "Y" "Z")
              initially (progn (format dest "DATASET RECTILINEAR_GRID~%")
                               (apply #'format dest "DIMENSIONS ~d ~d ~d~%" dims))
              do (format dest "~a_COORDINATES ~d float~%" arr-name npts)
              do (loop for v across cur-array
                       do (format dest "~f " v))
              do (format dest "~%"))
        (let ((lxdat (first kdat))
              (lydat (second kdat))
              (lzdat (third kdat))
              (lxsiz (first  ksiz))
              (lysiz (second ksiz))
              (lzsiz (third  ksiz))
              (lxdim (first  dims))
              (lydim (second dims))
              (lzdim (third  dims)))
          (if poly
              (format dest "DATASET POLYDATA~%")
              (progn (format dest "DATASET STRUCTURED_GRID~%")
                     (apply #'format dest "DIMENSIONS ~d ~d ~d~%" dims)))
          (format dest "POINTS ~d float~%" npts)
          (if (= 3 d1ct)
              (dotimes (i npts)
                (format dest "~f ~f ~f~%"
                        (mjr_arr_aref-cmo-as-vector-mod lxdat i)
                        (mjr_arr_aref-cmo-as-vector-mod lydat i)
                        (mjr_arr_aref-cmo-as-vector-mod lzdat i)))
              (let ((i 0))
                (dotimes (zi lzdim)
                  (dotimes (yi lydim)
                    (dotimes (xi lxdim)
                      (format dest "~f ~f ~f~%"
                              (mjr_arr_aref-cmo-as-vector-mod lxdat (if (> lxsiz lxdim) i xi))
                              (mjr_arr_aref-cmo-as-vector-mod lydat (if (> lysiz lydim) i yi))
                              (mjr_arr_aref-cmo-as-vector-mod lzdat (if (> lzsiz lzdim) i zi)))
                      (incf i))))))))
    (if poly
        (let ((d1ct (count 1 dims)))
          (cond ((= 1 d1ct) (let ((nx (first (remove-if (lambda (x) (= 1 x)) dims)))
                                  (ny (second (remove-if (lambda (x) (= 1 x)) dims))))
                              (format dest "POLYGONS ~d ~d~%" (* 2 (1- ny) (1- nx)) (* 4 2 (1- ny) (1- nx)))                             
                              (dotimes (xi (1- nx))//////
                                (dotimes (yi (1- ny))
                                  (format dest "3 ~d ~d ~d~%" (+ xi (* nx yi)) (+ (1+ xi) (* nx yi))      (+ (1+ xi) (* nx (1+ yi))))
                                  (format dest "3 ~d ~d ~d~%" (+ xi (* nx yi)) (+ (1+ xi) (* nx (1+ yi))) (+ xi      (* nx (1+ yi))))))))
                ((= 2 d1ct) (progn
                              (format dest "LINES ~d ~d~%" (1- npts) (* 3 (1- npts)))
                              (dotimes (xi (1- npts))
                                (format dest "2 ~d ~d~%" xi (1+ xi)))))
                ((= 3 d1ct) (progn
                              (format dest "VERTICES ~d ~d~%" npts (* 2 npts))
                              (dotimes (i npts)
                                (format dest "1 ~d~%" i)))))))
    (if (or scalar-array vector-array color-array)
        (progn
          (format dest "POINT_DATA ~d~%" npts)
          (if color-array ;; Color data must be FIRST for VisIt
              (loop for cur-array in cdat
                    for dset-idx from 0
                    do (format dest "COLOR_SCALARS ~a 3~%" (mjr_util_elt-mod color-names dset-idx))
                    do (dotimes (i npts)
                         (let ((v (mjr_arr_aref-cmo-as-vector-mod cur-array i)))
                           (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2))))))
          (if scalar-array
              (loop for cur-array in sdat
                    for dset-idx from 0
                    do (format dest "SCALARS ~a~a float 1~%"
                               (mjr_util_elt-mod scalar-names dset-idx) (if (> (length sdat) (length scalar-names)) dset-idx ""))
                    do (format dest "LOOKUP_TABLE default~%")
                    do (dotimes (i npts)
                         (format dest "~f " (mjr_arr_aref-cmo-as-vector-mod cur-array i)))
                    do (format dest "~%")))
          (if vector-array
              (loop for cur-array in vdat
                    for dset-idx from 0
                    do (format dest "VECTORS ~a~d float~%"
                               (mjr_util_elt-mod vector-names dset-idx) (if (> (length vdat) (length vector-names)) dset-idx ""))
                    do (dotimes (i npts)
                         (let ((v (mjr_arr_aref-cmo-as-vector-mod cur-array i)))
                           (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2))))))))
    (close dest)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_grid-from-func (out-file &key c-func v-func s-func xdat ydat zdat arg-mode (s-names "s") (v-names "v") (c-names "c"))
  "Sample one or more functions on a regular grid, and write the resulting scalar or vector field to a VTK file named OUT-FILE.

Cases
  domain-rank  func-type  Representation          Paraview       VisIt
  1            s-func     Scalar Field in 1D      Excellent(3)   Poor
  1            2D v-func  2D Vector Field in 1D   Good(3)        Poor
  1            3D v-func  3D Vectors Field in 1D  Poor           Poor
  2            s-func     Scalar Field in 2D      Good(1)        Excellent(2)
  2            2D v-func  2D Vector Field in 2D   Poor           Excellent
  2            3D v-func  3D Vector Field in 2D   Poor           Poor
  3            s-func     Scalar Field in 3D      Excellent      Excellent
  3            2D v-func  2D Vector Field in 3D   Poor           Poor
  3            3D v-func  3D Vector Field in 3D   Good           Excellent
  (1) Use the 'extract surface' filter followed by the 'warp scalar' filter to get a geometric surface
  (2) Use the 'Elevate' operator to get a geometric surface
  (3) Rendered as an x-y plot in 2D
  (4) Need to play around in the 'display' box to get x-vs-y

Arguments
  * XDAT, YDAT, & ZDAT: Describe the rectilinear space upon which the function F is sampled
  * c-func: color valued function to be sampled (takes DOMAIN-RANK variables)
  * v-func: vector valued function (or list of such functions) to be sampled (takes DOMAIN-RANK variables)
  * s-func: scalar valued function (or list of such functions) to be sampled (takes DOMAIN-RANK variables)"
  (cond ((not (stringp out-file))    (error "mjr_vtk_grid-from-func: OUT-FILE must be a string!")))
  (let* ((drnk    (count-if #'identity (list xdat ydat zdat))) ;; domain-rank
         (xdat    (or (mjr_vvec_to-vec-maybe xdat) #(0)))
         (ydat    (or (mjr_vvec_to-vec-maybe ydat) #(0)))
         (zdat    (or (mjr_vvec_to-vec-maybe zdat) #(0)))
         (s-func  (apply #'mjr_mxp_string-or-func-to-list-o-lambda s-func "x" (if ydat (list "y"))))
         (v-func  (apply #'mjr_mxp_string-or-func-to-list-o-lambda v-func "x" (if ydat (list "y"))))
         (c-func  (apply #'mjr_mxp_string-or-func-to-list-o-lambda c-func "x" (if ydat (list "y"))))
         (osdat   (loop for f-i in s-func
                        for nf-i = (lambda (v) 
                                     (mjr_util_fun-adapt-eval-v f-i (subseq v 0 drnk) arg-mode))
                        collect (mjr_combc_gen-all-cross-product (list xdat ydat zdat) :collect-value nf-i :result-type :array :arg-mode :arg-vector)))
         (ovdat   (loop for f-i in v-func
                        for nf-i = (lambda (v) 
                                     (let* ((vo (mjr_util_fun-adapt-eval-v f-i (subseq v 0 drnk) arg-mode))
                                            (lo (length vo)))
                                       (if (< lo 3)
                                           (let ((vo2 (make-array 3 :initial-element 0)))
                                             (dotimes (i lo vo2)
                                               (setf (aref vo2 i) (aref vo i))))
                                           vo)))
                        collect (mjr_combc_gen-all-cross-product (list xdat ydat zdat) :collect-value nf-i :result-type :array :arg-mode :arg-vector)))
         (ocdat   (loop for f-i in c-func
                        for nf-i = (lambda (v) 
                                     (mjr_util_fun-adapt-eval-v f-i (subseq v 0 drnk) arg-mode))
                        collect (mjr_combc_gen-all-cross-product (list xdat ydat zdat) :collect-value nf-i :result-type :array :arg-mode :arg-vector))))
      (mjr_vtk_from-gndata out-file
                         :xdat xdat :ydat ydat :zdat zdat
                         :scalar-array osdat :scalar-names s-names :vector-array ovdat :vector-names v-names :color-array ocdat :color-names c-names)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_polydata-from-func-r12-r123 (out-file f &key c-func udat vdat arg-mode)
  "Sample a single function on a regular grid, and write results to a POLYDATA VTK file.

Cases
  domain-rank range-rank Representation             TheGrid       z      scalar-data       Tool Support
                                                    x      y      z                        Paraview    VisIt
  1           1          Rectilinear curve          u      f(u)   N/A    (u=x       y  )   Poor        Excellent
  2           1          Rectilinear Surface in 3D  u      v      f(u,v) (u=x v=y     z)   Excellent   Excellent
  1           2          Parametric Curve in 2D     f(u)   f(u)   0      (u       x y  )   Poor        Excellent
  1           3          Parametric Curve in 3D     f(u)   f(u)   f(u)   (u       x y z)   Excellent   Excellent
  2           3          Parametric Surface in 3D   f(u,v) f(u,v) f(u,v) (u   v   x y z)   Excellent   Excellent

Arguments
  * UDAT & VDAT: Describe the rectilinear space upon which the function F is sampled
  * F: Function that takes 1 or 2 variables (determined by :UDAT & :VDAT) and returns a number or vector
  * XDAT, YDAT, & ZDAT: Describe the rectilinear space upon which the function F is sampled
  * c-func: color valued function to be sampled (takes DOMAIN-RANK variables)
  * f: a function to be sampled (takes DOMAIN-RANK variables)"
  (cond ((not (stringp out-file))    (error "mjr_vtk_polydata-from-func-r12-r123: OUT-FILE must be a string!")))
  (let* ((drnk   (count-if #'identity (list udat vdat))) ;; domain-rank
         (udat   (or (mjr_vvec_to-vec-maybe udat) #(0)))
         (vdat   (or (mjr_vvec_to-vec-maybe vdat) #(0)))
         (xnum   (length udat))
         (ynum   (length vdat))
         (f      (apply #'mjr_mxp_string-or-func-to-lambda f "u" (if vdat (list "v"))))
         (sdat   (mjr_combc_gen-all-cross-product (if (= drnk 1) (list udat) (list udat vdat))
                                                  :collect-value f :result-type :array :arg-mode arg-mode))
         (c-func (apply #'mjr_mxp_string-or-func-to-lambda c-func "u" (if vdat (list "v"))))
         (cdat   (if c-func (mjr_combc_gen-all-cross-product (if (= drnk 1) (list udat) (list udat vdat))
                                                             :collect-value c-func :result-type :array :arg-mode arg-mode)))
         (elt1   (apply #'aref sdat (if (= drnk 1) (list 0) (list 0 0))))
         (rrnk   (if (or (numberp elt1) (= 1 (length elt1))) 1 (length elt1))))
    (if (= rrnk 1)
        (mjr_vtk_from-gndata out-file :poly 't
                           :xdat udat :ydat (if (= drnk 1) sdat vdat) :zdat (if (not (= drnk 1)) sdat)
                           :dims (list xnum ynum 1)
                           :color-array cdat
                           :color-names (list "c")
                           :scalar-array (if (= drnk 1) (list udat sdat) (list udat vdat sdat))
                           :scalar-names (if (= drnk 1) (list "u=x" "y") (list "u=x" "v=y" "z")))
        (let ((oxdat (if (<= 1 rrnk) (mjr_arr_map (lambda (v) (aref v 0)) sdat)))
              (oydat (if (<= 2 rrnk) (mjr_arr_map (lambda (v) (aref v 1)) sdat)))
              (ozdat (if (<= 3 rrnk) (mjr_arr_map (lambda (v) (aref v 2)) sdat))))
          (mjr_vtk_from-gndata out-file :poly 't
                             :xdat oxdat
                             :ydat oydat
                             :zdat ozdat
                             :dims (list xnum ynum 1)
                             :color-array cdat
                             :color-names (list "c")
                             :scalar-array (delete-if #'null (list udat vdat oxdat oydat ozdat))
                             :scalar-names (append (if (= drnk 1) (list "u") (list "u" "v"))
                                                   (if (= rrnk 2) (list "x" "y") (list "x" "y" "z"))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_grid-from-func-c1-c1 (out-file f &key rdat idat c-func)
  "Build a visualization for a complex function.

A color map is placed in the file if COLOR-MAP is non-NIL as well as four scalar values: imagpart, realpart, abs, and phase
F and/or C-FUNC may be an infix encoded string in the variable Z.

The COLOR-MAP argument is a function that takes a complex number and returns an RGB value (see use-colorize.lisp and
use-colorizer.lisp)."
  (let* ((c-func  (mjr_mxp_string-or-func-to-lambda c-func "Z"))
         (f       (mjr_mxp_string-or-func-to-lambda f      "Z"))
         (rdat    (mjr_vvec_to-vec-maybe rdat))
         (idat    (mjr_vvec_to-vec-maybe idat))
         (fdat    (mjr_combc_gen-all-cross-product (list rdat idat)
                                                   :collect-value (lambda (x y) (funcall f (complex x y)))
                                                   :result-type :array :arg-mode :arg-number))
         (cdat    (if c-func (mjr_arr_map c-func fdat))))
             (mjr_vtk_from-gndata out-file
                            :xdat rdat
                            :ydat idat
                            :scalar-array (list (mjr_arr_map #'realpart fdat)
                                                (mjr_arr_map #'imagpart fdat)
                                                (mjr_arr_map #'abs fdat)
                                                (mjr_arr_map #'phase fdat))
                            :scalar-names '("real" "imag" "abs" "phase")
                            :color-array cdat
                            :color-names "c")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_from-dquad (out-file dquad &key axes data-arrays file-note)
  "Write an ASCII VTK file with the contents of a DQUAD list.

The 0-simplices (the points in VTK speak) are always put in the file.  DATA-ARRAYS specifies which point data sets should be put in
the file -- it is a data index, data name, or a list of same.  When missing, DATA-ARRAYS defaults to every point data set in the
DQUAD list.  SIMPLICES is an integer, or list of integers, specifying which sets of simplices should be put in the file -- ex: 2
means put the 2-simplices (triangles) in the file.  This argument has no default behavour when NIL."
  (let* ((data-arrays (sort (or (mjr_util_non-list-then-list data-arrays) ;; Some apps (like VisIT) need colors first, scalars next, vectors last
                                (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_data-count dquad))))
                            (lambda (x y)
                              (mjr_annot_ano-typ< (mjr_dquad_get-data-attr dquad x :ano-typ)
                                                   (mjr_dquad_get-data-attr dquad y :ano-typ)))))
         (axes        (subseq (concatenate 'list
                                           (mapcar (lambda (x) (mjr_dquad_get-axis-vector dquad x))
                                                   (or (mjr_util_non-list-then-list axes)
                                                       (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_axis-count dquad)))))
                                           '(#(0) #(0) #(0)))
                              0 3))
         (axes-len    (mapcar #'length axes))
         (npts        (apply #'* axes-len)))
    (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format dest "# vtk DataFile Version 3.0~%")
      (format dest "~a~%" (or file-note "File generated by Mitch Richling's :MJR_VTK package"))
      (format dest "ASCII~%")
      (format dest "DATASET RECTILINEAR_GRID~%")
      (apply #'format dest "DIMENSIONS ~d ~d ~d~%" axes-len)
      ;;Dump axis vectors
      (loop for axis in axes
            for axis-len in axes-len
            for coord-prefix in (list "X" "Y" "Z")
            do (format dest "~a_COORDINATES ~d float~%" coord-prefix axis-len)
            do (loop for v across axis
                     do (format dest "~f " v))
            do (format dest "~%"))
      ;; Dump data sets -- this is almost just like the dquad case.   
      (if data-arrays
          (progn
            (format dest "POINT_DATA ~d~%" npts)
            (dolist (da-dat data-arrays)
              (let* ((data-array (mjr_dquad_get-data-array dquad da-dat))
                     (data-type  (mjr_dquad_get-data-attr dquad da-dat :ano-typ))
                     (data-name  (mjr_dquad_get-data-attr dquad da-dat :ano-nam)))
                (case data-type
                  ((:ano-typ-ivec :ano-typ-rvec)    (progn (format dest "VECTORS ~a float~%" data-name)
                                                         (loop for i from 0 upto (1- npts)
                                                               for v = (mjr_arr_aref-as-vector data-array i)
                                                               do (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2)))))
                  ((:ano-typ-integer :ano-typ-real) (progn (format dest "SCALARS ~a float 1~%" data-name)
                                                         (format dest "LOOKUP_TABLE default~%")
                                                         (loop for i from 0 upto (1- npts)
                                                               for v = (mjr_arr_aref-as-vector data-array i)
                                                               do (format dest "~f~%" v))))
                  (:ano-typ-color                   (progn (format dest "COLOR_SCALARS ~a 3~%" data-name)
                                                          (loop for i from 0 upto (1- npts)
                                                                for v = (mjr_arr_aref-as-vector data-array i)
                                                                do (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2)))))
                  (otherwise                       (error "mjr_vtk_from-dquad: Unsupported :ANO-TYPE!"))))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_from-dsimp (out-file dsimp &key point-data simplices file-note)
  "Write an ASCII VTK file with the contents of a DSIMP list.

The 0-simplices (the points in VTK speak) are always put in the file.  POINT-DATA specifies which point data sets should be put in
the file -- it is a data index, data name, or a list of same.  When missing, POINT-DATA defaults to every point data set in the
DSIMP list.  SIMPLICES is an integer, or list of integers, specifying which sets of simplices should be put in the file -- ex: 2
means put the 2-simplices (triangles) in the file.  This argument has no default behavour when NIL."
  (let* ((point-data (sort (or (mjr_util_non-list-then-list point-data) ;; Some apps (like VisIT) need colors first, scalars next, vectors last
                               (let ((n (mjr_dsimp_data-count dsimp 0)))
                                 (if (> n 0)
                                     (concatenate 'list (mjr_vvec_to-vec-maybe n)))))
                           (lambda (x y)
                             (mjr_annot_ano-typ< (mjr_dsimp_get-data-attr dsimp x :ano-typ 0)
                                                  (mjr_dsimp_get-data-attr dsimp y :ano-typ 0)))))
         (simplices  (sort (mjr_util_non-list-then-list simplices) #'<));; They must be in order for some VTK readers.
         (points     (mjr_dsimp_get-simplex-array dsimp 0))
         (npts       (length points)))
    (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format dest "# vtk DataFile Version 3.0~%")
      (format dest "~a~%" (or file-note "File generated by Mitch Richling's :MJR_VTK package"))
      (format dest "ASCII~%")
      (format dest "DATASET POLYDATA~%")
      (format dest "POINTS ~d float~%" npts)
      (loop for pt across points
            do (format dest "~f ~f ~f~%" (aref pt 0) (aref pt 1) (aref pt 2)))
      (dolist (simp-dim simplices)
        (let* ((polys (mjr_dsimp_get-simplex-array dsimp simp-dim))
               (nply   (length polys))
               (polyd (1- (length (aref polys 0)))))
          (format dest "~a ~d ~d~%"
                  (case polyd
                    (2 "POLYGONS")
                    (1 "LINES")
                    (0 "VERTICES")
                    (otherwise (error "mjr_vtk_from-dsimp: Unsupported simplix dimension!")))
                  nply
                  (* (+ 2 polyd) nply))
          (loop for poly across polys
                do (format dest "~d" (1+ polyd))
                do (loop for ve  across poly
                         do (format dest " ~d" ve))
                do (format dest "~%"))))
      (if point-data
          (progn
            (format dest "POINT_DATA ~d~%" npts)
            (dolist (da-dat point-data)
              (let* ((data-array (mjr_dsimp_get-data-array dsimp da-dat 0))
                     (data-type  (mjr_dsimp_get-data-attr dsimp da-dat :ano-typ 0))
                     (data-name  (mjr_dsimp_get-data-attr dsimp da-dat :ano-nam 0)))
                (case data-type
                  ((:ano-typ-ivec :ano-typ-rvec)    (progn (format dest "VECTORS ~a float~%" data-name)
                                                         (loop for v across data-array
                                                               do (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2)))))
                  ((:ano-typ-integer :ano-typ-real) (progn (format dest "SCALARS ~a float 1~%" data-name)
                                                         (format dest "LOOKUP_TABLE default~%")
                                                         (loop for v across data-array
                                                               do (format dest "~f~%" v))))
                  (:ano-typ-color                   (progn (format dest "COLOR_SCALARS ~a 3~%" data-name)
                                                          (loop for v across data-array
                                                                do (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2)))))
                  (otherwise                       (error "mjr_vtk_from-dsimp: Unsupported :ANO-TYPE!"))))))))))
