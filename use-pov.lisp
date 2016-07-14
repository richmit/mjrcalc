;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-pov.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Produce Povray files!@EOL
;; @std       Common Lisp
;; @see       tst-pov.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2012,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_pov_make-from-dsimp -- dump height field.@EOL@EOL
;; @todo      better unit tests.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_POV
  (:USE :COMMON-LISP
        :MJR_VEC
        :MJR_ARR
        :MJR_GEOM
        :MJR_COLOR
        :MJR_ANNOT
        :MJR_UTIL
        :MJR_DQUAD
        :MJR_DSIMP)
  (:DOCUMENTATION "Brief: Write Povray files!;")
  (:EXPORT #:mjr_pov_help
           #:mjr_pov_make-from-dsimp
           ;; NOT EXPORTED
           ;; #:mjr_pov_code-curve
           ))

(in-package :MJR_POV)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pov_help ()
  "Help for MJR_POV: Create POVray files

The goal is to provide some basic functionality for transforming mathematical data (in the form of an array or embedded in a
function) into Povray files."
  (documentation 'mjr_pov_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pov_code-curve (pts spline &optional (no-degenerate-check nil))
  "Return string with code to draw curve.

Arguments:
 - PTS ................... Array of points that will be connected end-to-end.
 - SPLINE ................ Spline type to use (NIL or a STRING)
                             - NIL ....... Draw a cylinder connecting the points
                             - 'cubic' ... Cubic interpolation using a sphere_sweep
                             - 'linear' .. Linear interpolation using a sphere_sweep
                             - 'b' ....... ???
 - NO-DEGENERATE-CHECK .. Do not check for degenerate segments.  Faster if you know the segments are good."
  (let* ((pts (if no-degenerate-check
                  pts
                  (loop for y = nil then x
                        for x in pts
                        when (or no-degenerate-check (null y) (not (mjr_geom_simplex-degeneratep nil x y)))
                        collect x)))
         (npts (length pts)))
    (if (<= npts 1)
        ""
        (with-output-to-string (outStr)
          (if spline
              (let* ((spline  (if (< npts 3) "linear" spline))
                     (non-lin (not (string= "linear" spline))))
                (format outStr "sphere_sweep { ~a_spline ~a" spline (if non-lin (+ 2 npts) npts))
                (loop initially (if non-lin (format outStr ", ~a, lineDiam" (mjr_vec_code (mjr_vec_- (mjr_vec_* 2 (first pts)) (second pts)) :lang :lang-povray)))
                      for y = nil then x
                      for x in pts
                      do (format outStr ", ~a, lineDiam" (mjr_vec_code x :lang :lang-povray))
                      finally (if non-lin  (format outStr ", ~a, lineDiam" (mjr_vec_code (mjr_vec_- (mjr_vec_* 2 x) y) :lang :lang-povray))))
                (format outStr " texture { lineTex } }~%"))
              (loop for x in pts
                    for y in (cdr pts)
                    when (or no-degenerate-check (not (mjr_geom_simplex-degeneratep nil x y)))
                    do (format outStr "cylinder {~a, ~a, lineDiam texture { lineTex } }~%" (mjr_vec_code x :lang :lang-povray) (mjr_vec_code y :lang :lang-povray))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pov_make-from-dsimp (out-file dsimp &key simplices color-data curve-spline surface-normal-data no-degenerate-check
                                                 draw-0-simplex-vertexes
                                                 draw-1-simplex-vertexes draw-1-simplex-edges
                                                 draw-2-simplex-vertexes draw-2-simplex-edges draw-2-simplex-triangles)
  "Generate POV-Ray data file from dsimp.

  :COLOR-DATA ........... 0-simplex COLOR data set (a dataset name or list containing the dataset simplex dimension and dataset index).  This dataset defines
                          colors for 0-simplex spheres (not vertexes of 1-simplices & 2-simplices), and 2-simplex colors (color is interpolated across the
                          triangle face based on vertex colors).  This argument has no impact on the colors of one simplex objects (the cylinders or
                          sphere_sweep objects representing one simplices).
  :NO-DEGENERATE-CHECK .. Print all simplices includeing degenerate ones.
  :SURFACE-NORMAL-DATA .. Surface normals (a vector) at each vertex
  :CURVE-SPLINE ......... If non-NIL, will be used as the spline type for a sphere_sweep object representing curves (1-simplices that are 'end-to-end').  When
                          NIL, one simplices are simple cylinders.

  Notes about the generated POV-Ray:
   Various primitives represent different dsimp components:
    * Spheres:   0-simplices, vertexes for 1-simplices, and vertexes for 2-simplices
    * Cylinders: 1-simplices and edges for 2-simplices (sphere_sweep objects are used for 1-simplices when CURVE-SPLINE is non-NIL)
    * Triangles: 2-simplices (grouped into a POV-Ray mesh2 object)
   Names are defined for different parameters:
    * sphTex0simp & sphDia0simp .. sphere primitive diameter & texture for a 0-simplex (not used if COLOR-DATA provided)
    * sphTex1simp & sphDia1simp .. sphere primitive diameter & texture for a 1-simplex vertex
    * cylTex1simp & cylDia1simp .. cylinder primitive diameter & texture for a 1-simplex line
    * sphTex2simp & sphDia2simp .. sphere primitive diameter & texture for a 2-simplex vertex
    * cylTex2simp & cylDia2simp .. cylinder primitive diameter & texture for a 2-simplex line
    * spsTex1simp & spsDia1simp .. sphere sweep diameter & texture for curves constructed from 1-simplices
    * triTex2simp ................ triangle primitive texture for a 2-simplex triangle (not used if COLOR-DATA provided)"
  (let* ((simplices  (mjr_util_non-list-then-list simplices))
         (do-0-simp  (member 0 simplices))
         (do-1-simp  (member 1 simplices))
         (do-2-simp  (member 2 simplices))
         (points     (mjr_dsimp_get-simplex-array dsimp 0))
         (npts       (length points))
         (d-balls    (make-array (length points) :initial-element nil))
         (d-cyls     (make-hash-table :test 'equal)))
    (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (labels ((pvc   (pv) (mjr_vec_code pv :lang :lang-povray))
               (oSph  (pt-idx s-dim) (if (not (aref d-balls pt-idx))
                                         (progn (setf (aref d-balls pt-idx) 't)
                                                (format dest "sphere { ~a, sphDia~dsimp texture { sphTex~dsimp } }~%" (pvc (aref points pt-idx)) s-dim s-dim))))
               (oCyl  (pt-idx-1 pt-idx-2 s-dim) (let ((pt1 (aref points pt-idx-1))
                                                      (pt2 (aref points pt-idx-2))
                                                      (cky (if (<= pt-idx-1 pt-idx-2)
                                                               (format nil "~d|~d" pt-idx-1 pt-idx-2)
                                                               (format nil "~d|~d" pt-idx-2 pt-idx-1))))
                                                  (if (not (gethash cky d-cyls))
                                                      (progn (setf (gethash cky d-cyls ) 't)
                                                             (if (or no-degenerate-check (not (mjr_geom_simplex-degeneratep nil pt1 pt2)))
                                                                 (format dest "cylinder {~a, ~a, cylDia~dsimp texture { cylTex~dsimp } }~%" (pvc pt1) (pvc pt2) s-dim s-dim))))))
               (oSS (pt-idxs)  (let* ((pts     (if no-degenerate-check
                                                   pt-idxs
                                                   (loop for y = nil then x
                                                         for x-idx in pt-idxs
                                                         for x = (aref points x-idx)
                                                         when (or no-degenerate-check (null y) (not (mjr_geom_simplex-degeneratep nil x y)))
                                                         collect x)))
                                      (npts    (length pts))
                                      (spline  (if (< npts 4) "linear" curve-spline))
                                      (non-lin (not (string= "linear" spline))))
                                 (if (> npts 1)
                                     (progn (format dest "sphere_sweep { ~a_spline ~a" spline (if non-lin (+ 2 npts) npts))
                                            (loop initially (if non-lin (format dest ", ~a, spsDia1simp" (pvc (mjr_vec_- (mjr_vec_* 2 (first pts)) (second pts)))))
                                                  for y = nil then x
                                                  for x in pts
                                                  do (format dest ", ~a, spsDia1simp" (pvc x))
                                                  finally (if non-lin  (format dest ", ~a, spsDia1simp" (pvc (mjr_vec_- (mjr_vec_* 2 x) y)))))
                                            (format dest " texture { spsTex1simp } }~%"))))))
        (if do-0-simp
            (if draw-0-simplex-vertexes
                (dotimes (pt-idx npts)
                  (oSph pt-idx 0))))
        (if do-1-simp
            (if curve-spline
                (loop with curve = nil
                      for seg across (mjr_dsimp_get-simplex-array dsimp 1)
                      do (cond ((null curve)                  (setf curve (list (aref seg 1) (aref seg 0))))
                               ((= (car curve) (aref seg 0))  (push (aref seg 1) curve))
                               ('t                            (progn (oSS curve)
                                                                     (setf curve (list (aref seg 1) (aref seg 0))))))
                      finally (oSS curve))
                (progn (if draw-1-simplex-vertexes
                           (loop for cur-simp across (mjr_dsimp_get-simplex-array dsimp 1)
                                 do (loop for pt-idx across cur-simp
                                          do (oSph pt-idx 1))))
                       (if draw-1-simplex-edges
                           (loop for cur-simp across (mjr_dsimp_get-simplex-array dsimp 1)
                                 do (oCyl (aref cur-simp 0) (aref cur-simp 1) 1))))))
        (if do-2-simp
            (progn (if draw-2-simplex-vertexes
                       (loop for cur-simp across (mjr_dsimp_get-simplex-array dsimp 2)
                             do (loop for pt-idx across cur-simp
                                      do (oSph pt-idx 2))))
                   (if draw-2-simplex-edges
                       (loop for cur-simp across (mjr_dsimp_get-simplex-array dsimp 2)
                             do (loop for i from 0 upto 2
                                      do (oCyl (mjr_arr_svref-mod cur-simp i) (mjr_arr_svref-mod cur-simp (1+ i)) 2))))
                   (if draw-2-simplex-triangles
                       (progn (format dest "mesh2 {~%")
                              ;; POINTS
                              (format dest "  vertex_vectors {~%")
                              (format dest "    ~d" npts)
                              (loop for pt across points
                                    do  (format dest ",~a" (pvc pt)))
                              (format dest "~%")
                              (format dest "  }~%")
                              ;; NORMALS
                              (if surface-normal-data
                                  (let* ((normals (mjr_dsimp_get-data-array dsimp surface-normal-data)))
                                    (format dest "  normal_vectors {~%")
                                    (format dest "    ~d" (length normals))
                                    (loop for a-nrml across normals
                                          do  (format dest ",~a" (pvc a-nrml)))
                                    (format dest "~%")
                                    (format dest "  }~%")))
                              ;; TEXTURES
                              (if color-data
                                  (let* ((colors (mjr_dsimp_get-data-array dsimp color-data))
                                         (dattyp (mjr_dsimp_get-data-ano dsimp color-data :ano-typ))
                                         (cuc (mjr_color_make-unpacker-color-space-converter-and-packer (mjr_annot_get-colorpacking dattyp)
                                                                                                        (mjr_annot_get-colorspace dattyp)
                                                                                                        :cs-rgb
                                                                                                        :cp-none))
                                         (ncol   (length colors)))
                                    (format dest "  texture_list {~%")
                                    (format dest "    ~d,~%" ncol)
                                    (loop for  a-clr across colors
                                          for  lft downfrom (1- ncol)
                                          do   (format dest "    texture { pigment { rgb ~a } }" (pvc (funcall cuc a-clr)))
                                          when (not (zerop lft))
                                          do   (format dest ",~%"))
                                    (format dest "  }~%")))
                              ;; TRIANGLES
                              (let* ((triangles (mjr_dsimp_get-simplex-array dsimp 2))
                                     (ntri      (length triangles)))
                                (format dest "  face_indices {~%")
                                (format dest "    ~d,~%" ntri)
                                (loop for  a-tri across triangles
                                      for  lft downfrom (1- ntri)
                                      do   (format dest "    ~a" (pvc a-tri))
                                      do (if color-data
                                             (format dest ", ~d, ~d, ~d" (aref a-tri 0) (aref a-tri 1) (aref a-tri 2)))
                                      when (not (zerop lft))
                                      do   (format dest ",~%"))
                                (format dest "~%")
                                (format dest "  }~%"))
                              ;; MESH COLOR
                              (if (not color-data)
                                  (format dest "  texture { triTex2simp }~%"))
                              ;; MESH OBJECT CLOSE
                              (format dest "}~%")))))))))
