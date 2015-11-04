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
        :MJR_NUMU
        :MJR_DQUAD
        :MJR_DSIMP)
  (:DOCUMENTATION "Brief: Write VTK files.;")
  (:EXPORT #:mjr_vtk_help

           ;; Marked for death!!!  This will go away once I finish the dsimp/dquad work....
           #:mjr_vtk_from-gndata

           ;; Very experimental
           #:mjr_vtk_from-dsimp
           #:mjr_vtk_from-dquad

           ;; Not exported
           #:mjr_vtk_print-data-set
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
                        (mjr_arr_aref-col-major-mod lxdat i)
                        (mjr_arr_aref-col-major-mod lydat i)
                        (mjr_arr_aref-col-major-mod lzdat i)))
              (let ((i 0))
                (dotimes (zi lzdim)
                  (dotimes (yi lydim)
                    (dotimes (xi lxdim)
                      (format dest "~f ~f ~f~%"
                              (mjr_arr_aref-col-major-mod lxdat (if (> lxsiz lxdim) i xi))
                              (mjr_arr_aref-col-major-mod lydat (if (> lysiz lydim) i yi))
                              (mjr_arr_aref-col-major-mod lzdat (if (> lzsiz lzdim) i zi)))
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
                         (let ((v (mjr_arr_aref-col-major-mod cur-array i)))
                           (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2))))))
          (if scalar-array
              (loop for cur-array in sdat
                    for dset-idx from 0
                    do (format dest "SCALARS ~a~a float 1~%"
                               (mjr_util_elt-mod scalar-names dset-idx) (if (> (length sdat) (length scalar-names)) dset-idx ""))
                    do (format dest "LOOKUP_TABLE default~%")
                    do (dotimes (i npts)
                         (format dest "~f " (mjr_arr_aref-col-major-mod cur-array i)))
                    do (format dest "~%")))
          (if vector-array
              (loop for cur-array in vdat
                    for dset-idx from 0
                    do (format dest "VECTORS ~a~d float~%"
                               (mjr_util_elt-mod vector-names dset-idx) (if (> (length vdat) (length vector-names)) dset-idx ""))
                    do (dotimes (i npts)
                         (let ((v (mjr_arr_aref-col-major-mod cur-array i)))
                           (format dest "~f ~f ~f~%" (aref v 0) (aref v 1) (aref v 2))))))))
    (close dest)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_print-data-set  (dest data-array data-anno  npts)
  "Dump a data set from a dquad list to DEST using VTK syntax."
  (let ((data-type (mjr_annot_get-value :ano-typ data-anno))
        (data-name (mjr_annot_get-value :ano-nam data-anno)))
    (case data-type
      ((:ano-typ-ivec :ano-typ-rvec)    (progn (format dest "VECTORS ~a float~%" data-name)
                                               (loop for i from 0 upto (1- npts)
                                                     for v = (mjr_arr_aref-col-major data-array i)
                                                     do (format dest "~a ~a ~a~%"
                                                                (mjr_numu_code (aref v 0)                         :lang :lang-vtk)
                                                                (mjr_numu_code (if (< 1 (length v)) (aref v 1) 0) :lang :lang-vtk)
                                                                (mjr_numu_code (if (< 2 (length v)) (aref v 2) 0) :lang :lang-vtk)))))
      ((:ano-typ-integer :ano-typ-real) (progn (format dest "SCALARS ~a float 1~%" data-name)
                                               (format dest "LOOKUP_TABLE default~%")
                                               (loop for i from 0 upto (1- npts)
                                                     for v = (mjr_arr_aref-col-major data-array i)
                                                     do (format dest "~a " (mjr_numu_code v :lang :lang-vtk)))
                                               (format dest "~%")))
      (:ano-typ-color                   (let ((cunpacker (mjr_annot_make-cunpacker (mjr_annot_get-value :ano-colorspace data-anno)
                                                                                   (mjr_annot_get-value :ano-cpacking   data-anno)
                                                                                   :cs-rgb)))
                                          (format dest "COLOR_SCALARS ~a 3~%" data-name)
                                          (loop for i from 0 upto (1- npts)
                                                for v = (funcall cunpacker (mjr_arr_aref-col-major data-array i))
                                                do (format dest "~a ~a ~a~%"
                                                           (mjr_numu_code (aref v 0) :lang :lang-vtk)
                                                           (mjr_numu_code (aref v 1) :lang :lang-vtk)
                                                           (mjr_numu_code (aref v 2) :lang :lang-vtk)))))
      (:ano-typ-complex                 (progn (format dest "SCALARS ~a float 1~%" (format nil "~a_real" data-name))
                                               (format dest "LOOKUP_TABLE default~%")
                                               (loop for i from 0 upto (1- npts)
                                                     for v = (mjr_arr_aref-col-major data-array i)
                                                     do (format dest "~a " (mjr_numu_code (realpart v) :lang :lang-vtk)))
                                               (format dest "~%"))
                                        (format dest "SCALARS ~a float 1~%" (format nil "~a_imag" data-name))
                                        (format dest "LOOKUP_TABLE default~%")
                                        (loop for i from 0 upto (1- npts)
                                              for v = (mjr_arr_aref-col-major data-array i)
                                              do (format dest "~a " (mjr_numu_code (imagpart v) :lang :lang-vtk)))
                                        (format dest "~%"))
      (otherwise                       (error "mjr_vtk_from-dquad: Unsupported :ANO-TYPE!")))))

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
                              (mjr_annot_ano-typ< (mjr_dquad_get-data-ano dquad x :ano-typ)
                                                   (mjr_dquad_get-data-ano dquad y :ano-typ)))))
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
                     do (format dest "~a " (mjr_numu_code v :lang :lang-vtk)))
            do (format dest "~%"))
      ;; Dump data sets
      (if data-arrays
          (progn (format dest "POINT_DATA ~d~%" npts)
                 (dolist (da-dat data-arrays)
                   (mjr_vtk_print-data-set dest
                                           (mjr_dquad_get-data-array dquad da-dat)
                                           (mjr_dquad_get-data-ano  dquad da-dat)
                                           npts)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vtk_from-dsimp (out-file dsimp &key point-data simplices file-note)
  "Write an ASCII VTK file with the contents of a DSIMP list.

Arguments:
  - POINT-DATA specifies which point data sets should be put in the file -- it is a data index, data name, or a list of same.  When
    missing, POINT-DATA defaults to every point data set in the DSIMP list.
  - SIMPLICES is an integer, or list of integers, specifying which sets of simplices should be put in the file -- ex: 2 means put
    the 2-simplices (triangles) in the file.  This argument has no default behavior when NIL.  Note that including 0 in :SIMPLICES
    is meaningful as this will include a VERTICES section in the VTK file to generate actual geometry for each point -- Simply having
    the 'POINTS' section is not enough for most tools."
  (let* ((point-data (sort (or (mjr_util_non-list-then-list point-data) ;; Some apps (like VisIT) need colors first, scalars next, vectors last
                               (let ((n (mjr_dsimp_data-count dsimp 0)))
                                 (if (> n 0)
                                     (concatenate 'list (mjr_vvec_to-vec-maybe n)))))
                           (lambda (x y)
                             (mjr_annot_ano-typ< (mjr_dsimp_get-data-ano dsimp x :ano-typ 0)
                                                 (mjr_dsimp_get-data-ano dsimp y :ano-typ 0)))))
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
            do (format dest "~a ~a ~a~%"
                       (mjr_numu_code (aref pt 0) :lang :lang-vtk)
                       (mjr_numu_code (aref pt 1) :lang :lang-vtk)
                       (mjr_numu_code (aref pt 2) :lang :lang-vtk)))
      (dolist (simp-dim simplices)
        (let* ((polys (mjr_dsimp_get-simplex-array dsimp simp-dim))
               (nply   (length polys)))
          (format dest "~a ~d ~d~%"
                  (case simp-dim
                    (2 "POLYGONS")
                    (1 "LINES")
                    (0 "VERTICES")
                    (otherwise (error "mjr_vtk_from-dsimp: Unsupported simplix dimension!")))
                  nply
                  (* (+ 2 simp-dim) nply))
          (loop for poly across polys
                for i from 0
                do (format dest "~d" (1+ simp-dim))
                do (if (zerop simp-dim)
                       (format dest " ~d" i)
                       (loop for ve  across poly
                         do (format dest " ~d" ve)))
                do (format dest "~%"))))
      (if point-data
          (progn (format dest "POINT_DATA ~d~%" npts)
                 (dolist (da-dat point-data)
                   (mjr_vtk_print-data-set dest
                                           (mjr_dsimp_get-data-array dsimp da-dat 0)
                                           (mjr_dsimp_get-data-ano  dsimp da-dat nil 0)
                                           npts)))))))
