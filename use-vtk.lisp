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
function) into VTK files."
  (documentation 'mjr_vtk_help 'function))

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
