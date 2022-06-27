;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-ply3d.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     PLY 3d files.@EOL
;; @std       Common Lisp
;; @see       N/A
;; @copyright
;;  @parblock
;;  Copyright (c) 1995, 2013, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PLY3D
  (:USE :COMMON-LISP
        :MJR_DSIMP
        :MJR_COLOR
        :MJR_ANNOT)
  (:DOCUMENTATION "Brief: Create PLY3D files.;")
  (:EXPORT #:mjr_ply3d_help
           #:mjr_ply3d_from-dsimp
           ))

(in-package :MJR_PLY3D)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ply3d_help ()
  "Help for MJR_PLY3D:

Library for produceing PLY 3D files -- from DSIMP objects.  In the future other source types and/or more complex PLY files may be added."
  (documentation 'mjr_ply3d_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ply3d_from-dsimp (out-file dsimp &key color-data file-note (coord-order '(0 2 1)) (triangle-orientation :reverse))
  "Generate a Polygon File Format (PLY) file from a DSIMP object.

The PLY file will always contan 0-simplices.  By default 2-simplices will be included as well (see argument :INCLUDE-TRIANGLES.  Vertex color data may also be
included (see argument :COLOR-DATA).

The primary use case is to easily get a triangulation into more artistically oriented 3D modeling tools like Blender and meshlab.  This function is also handy
for quickly looking at a surface (ex: file exporer in Windows or mview in Linux) before loading into more cumbersome tools.

See the :PLYOB package for doing something similar with OBJ files. See the :POV packages fo more sophisticated rendering options.  See the :VTK package for
more sophisticated data export options.

Note this format is sometimes called the 'Stanford Triangle Format'.  See: https://en.wikipedia.org/wiki/PLY_%28file_format%29

Note: Several aspects of this fiel format are poorly supported, and thus not provided here:
  - Edges
  - Face colors (blender & meshlab have support)
  - Backface colors
  - alpha channel
  - Vertex normals

Arguments:
  - COLOR-DATA a 0-simplex COLOR data set (dataset name or list containing the dataset simplex dimension and dataset index).  Used for vertex colors.
  - COORD-ORDER a list of integers defining the order vertex cooridnates will be placed in the file.  Default: '(0 2 1)
  - FILE-NOTE a string that will be included as a comment near the top of the file
  - TRIANGLE-ORIENTATION: 
      - REVERSE Reverse the orientation of the triangles (default)
      - EVEN    Reverse the orientation of every other triangle
      - NORMAL  Normal orientation

  - Simple closed surface
      (let* ((n     5)
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
        (mjr_ply3d_from-dsimp \"torus.ply\" data))

  - Simple surface dump to ply file
      (mjr_ply3d_from-dsimp \"surf.ply\"
                                 (mjr_fsamp_ds-func-r123-r123 (lambda (x y) (* .5 (abs (- (expt (complex x y) 3) 1))))
                                                              :xdat '(:start -1.1 :end 1.1 :len 50)
                                                              :ydat '(:start -1.1 :end 1.1 :len 50)
                                                              :arg-mode :arg-number))

  - Simple surface dump to ply file with vertex color
      (let ((a-dquad (mjr_fsamp_dq-func-c1-c1 (lambda (c) (* 0.5 (- (expt c 3) 1)))
                                              :rdat '(:start -1.1 :end 1.1 :len 50)
                                              :idat '(:start -1.1 :end 1.1 :len 50))))
        (mjr_dquad_colorize a-dquad :data \"f_abs\" :color-method \"BCGYR\" :ano-nam \"col\")
        (let ((a-dsimp (mjr_dsimp_make-from-dquad a-dquad '(\"real\" \"imag\") \"f_abs\" :data \"col\")))
          (mjr_ply3d_from-dsimp \"surf.ply\" a-dsimp :color-data \"col\")))"
  (let* ((pnts (mjr_dsimp_get-simplex-array dsimp 0))
         (tris (mjr_dsimp_get-simplex-array dsimp 2))
         (npts (length pnts))
         (ntri (length tris)))
    (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format dest "ply~%")
      (format dest "format ascii 1.0~%")
      (format dest "comment author: Mitch Richling~%")
      (format dest "comment software: Mitch Richling's *mjrcalc* package~%")
      (if file-note (format dest "comment note: ~s~%" file-note))
      (format dest "element vertex ~d~%" npts)
      (format dest "property float x~%")
      (format dest "property float y~%")
      (format dest "property float z~%")
      (if color-data
          (progn 
            (format dest "property uchar red~%")
            (format dest "property uchar green~%")
            (format dest "property uchar blue~%")))
      (format dest "element face ~d~%" ntri)
      (format dest "property list uchar int vertex_index~%")
      (format dest "end_header~%")
      (if color-data
          (let* ((colors (mjr_dsimp_get-data-array dsimp color-data))
                 (dattyp (mjr_dsimp_get-data-ano dsimp color-data :ano-typ))
                 (cuc    (mjr_color_make-unpacker-color-space-converter-and-packer (mjr_annot_get-colorpacking dattyp)
                                                                                   (mjr_annot_get-colorspace dattyp)
                                                                                   :cs-tru :cp-none)))
            (loop for clr across colors
                  for tcl = (funcall cuc clr)
                  for pnt across pnts
                  do (format dest "~f ~f ~f ~d ~d ~d~%" (aref pnt 0) (aref pnt 1) (aref pnt 2) (aref tcl 0) (aref tcl 1) (aref tcl 2))))
          (loop for pnt across pnts
                do (apply #'format dest "~f ~f ~f~%" (mapcar (lambda (i) (aref pnt i)) coord-order))))
      (loop for  tri across tris
            for  ti from 1
            do (if (or (equalp triangle-orientation :reverse) (and (equalp triangle-orientation :even) (evenp ti)))
                   (format dest "3 ~d ~d ~d~%" (aref tri 2) (aref tri 1) (aref tri 0))
                   (format dest "3 ~d ~d ~d~%" (aref tri 0) (aref tri 1) (aref tri 2)))))))
