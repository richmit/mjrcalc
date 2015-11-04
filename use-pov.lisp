;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-pov.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Produce Povray files!@EOL
;; @Keywords  lisp interactive povray
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_POV
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_VEC
        :MJR_ARR
        :MJR_GEOM
        :MJR_COMBC
        :MJR_COLORIZED
        :MJR_MXP
        :MJR_IMG)
  (:DOCUMENTATION "Brief: Produce Povray files!;")
  (:EXPORT #:mjr_pov_help
           #:mjr_pov_make-from-gndata
           #:mjr_pov_make-from-func-r12-r13          
           #:mjr_pov_tga-write-from-func-r2-r1
           ;; NOT EXPORTED
           ;; #:mjr_pov_code-curve
           ))

(in-package :MJR_POV)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_pov_help ()
  "Help for MJR_POV: Create POVray files

The goal is to provide some basic functionality for transforming mathematical data (in the form of an array or embedded in a
function) into Povray files.  The real workhorse in this package is the mjr_pov_make-from-gndata function."
  (documentation 'mjr_pov_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_pov_code-curve (pts spline &optional (no-degenerate-check nil))
  "Return string with code to draw curve.
Draw cylinder if spline is nil, a sphere_sweep otherwise (spline must be a string: 'cubic', 'linear', or 'b')"
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
              (let* ((spline  (if (< npts 2) "linear" spline))
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_pov_make-from-gndata (out-file &key 
                               xdat ydat zdat dims
                               surface-color-dat
                               (draw-surfaces 't) (draw-curves 't) (draw-points 't) (draw-surface-grid (list :x :y :d))
                               (use-mesh nil) (curve-spline nil) (surface-smooth nil))
                                        ; MJR TODO NOTE mjr_pov_make-from-gndata: Suppress degenerate simplexes for mesh and sphere_sweep objects.
  "Generate PovRay data file from array -- assuming the data has a regular structure.  
This function outputs geometric primitives (points as spheres, lines as cylinders, triangles) for the grid structure defined by
DIMS and the arrays XDAT, YDAT, and ZDAT -- the geometry is determined just as it is in the function MJR_VTK_ARRAY2VTK.  We only
need deal with 0D, 1D, and 2D objects in POVRay, so DIMS has fewer possible values than in MJR_VTK_ARRAY2VTK -- i.e. at least
one element of DIMS must be 1 (no solid 3D objects.

  :USE-MESH ........... Use a mesh2 object instead of triangles
  :MESH-SMOOTH ........ Compute normals at vertex points on surfaces, and output a mesh2 object instead of triangles
                        Setting :SURFACE-SMOOTH non-NIL will set :USE-MESH non-NIL as well.
  :SURFACE-COLOR-DAT .. 2D array of :cs-rgb colors used for smooth mesh vertexes (i.e. :MESH-SMOOTH must be non-NIL)
                        These colors are NOT used for vertex objects (spheres) or line objects (cylinders & sphere_sweeps)
  :CURVE-SPLINE ....... If non-NIL, will be used as the spline type for a sphere_sweep object.  Otherwise cylinders are used.
                        Strings that may be used: b, cubic, linear (linear looks just like nil, but uses a sphere_sweep object)
  :DRAW-SURFACE-GRID .. How to draw the edges of the triangles on a surface
                        nil -- draw none
                         :x -- draw x lines
                         :y -- draw y lines
                         :d -- draw diagonal lines 
                         't -- draw everything
                       list -- a list containing one or more of :x, :y, and :d
  :DRAW-SURFACES ..... Draw the 2D part of the surface (i.e. the triangles or mesh2 object)
  :DRAW-CURVES ....... Draw the 1D part of a curve (i.e. the cylinders or sphere_sweep object)
  :DRAW-POINTS ....... Draw the vertexes of a surface or curve

 * '(1 1 1) => 0D geometry -- No grid structure at all (points)
 * '(1 1 n) => 1D geometry -- N points all on a single curve
   '(1 n 1)    The location of the N is irrelevant
   '(n 1 1)    This is the assumed case if (null dims) & xdat/ydat/zdat are all sequences
 * '(n m 1) => 2D geometry -- NxM grid structure of points on a surface
   '(n 1 m)    The location of the 1 is irrelevant
   '(1 n m)    This is the assumed case if (null dims), one of xdat/ydat/zdat is a 2D array, & the others are sequences"
  (cond ((and (not use-mesh) surface-color-dat)        (error "mjr_pov_make-from-gndata: :SURFACE-COLOR-DAT may only be used when :USE-MESH is non-NIL"))
        ((and (not surface-smooth) surface-color-dat)  (error "mjr_pov_make-from-gndata: :SURFACE-COLOR-DAT may only be used when :SURFACE-SMOOTH is non-NIL")))
  (let* ((use-mesh          (or surface-smooth use-mesh))
         (draw-surface-grid (if (listp draw-surface-grid) 
                                draw-surface-grid
                                (if (equalp draw-surface-grid 't)
                                    '(:x :d :y)
                                    (list draw-surface-grid))))
         (dest              (open out-file :direction :output :if-exists :supersede :if-does-not-exist :create))
         (cdat              (list xdat ydat zdat))
         (dims              (or dims
                                (if (every #'vectorp cdat)
                                    (list 1 1 (apply #'max (mapcar #'length cdat))))
                                (let ((rnks (mapcar #'array-rank cdat)))
                                  (if (and (= 2 (count 1 rnks)) (= 1 (count 2 rnks)))
                                      (append (list 1) (array-dimensions (nth (position 2 rnks) cdat)))))))
         (npts              (apply #'max (mapcar #'array-total-size cdat)))
         (d1ct              (count 1 dims))
         (lxdim             (first  dims))
         (lydim             (second dims))
         (lzdim             (third  dims))
         (lxdat             (or (mjr_vvec_to-vec-maybe xdat) (mjr_vvec_gen-0sim 'vector lxdim)))
         (lydat             (or (mjr_vvec_to-vec-maybe ydat) (mjr_vvec_gen-0sim 'vector lydim)))
         (lzdat             (or (mjr_vvec_to-vec-maybe zdat) (mjr_vvec_gen-0sim 'vector lzdim)))
         (lxsiz             (array-total-size lxdat))
         (lysiz             (array-total-size lydat))
         (lzsiz             (array-total-size lzdat))
         (pts               (make-array (list npts))))
    (flet ((pt2st (pt) (mjr_vec_code (aref pts pt) :lang :lang-povray)))
      ;; Populate the array PTS (a linear point list we use later)
      (if (= 3 d1ct)
          (dotimes (i npts)
            (setf (aref pts i) (vector (mjr_arr_aref-cmo-as-vector-mod lxdat i)
                                       (mjr_arr_aref-cmo-as-vector-mod lydat i)
                                       (mjr_arr_aref-cmo-as-vector-mod lzdat i))))
          (let ((i 0))
            (dotimes (zi lzdim)
              (dotimes (yi lydim)
                (dotimes (xi lxdim)
                  (setf (aref pts i) (vector (mjr_arr_aref-cmo-as-vector-mod lxdat (if (> lxsiz lxdim) i xi)) 
                                             (mjr_arr_aref-cmo-as-vector-mod lydat (if (> lysiz lydim) i yi))
                                             (mjr_arr_aref-cmo-as-vector-mod lzdat (if (> lzsiz lzdim) i zi))))
                  (incf i))))))
      (if (= 1 d1ct) ;; Surface case (2d grid)
          (let ((nx (first  (remove-if (lambda (x) (= 1 x)) dims)))
                (ny (second (remove-if (lambda (x) (= 1 x)) dims))))
            (if draw-surfaces
                (if use-mesh
                    (progn (format dest "mesh2 { vertex_vectors { ~a " npts)
                           (dotimes (xi npts)
                             (format dest ",~a" (pt2st xi)))
                           (format dest "}~%")
                           (if surface-smooth
                               (let ((norms (make-array npts))
                                     (normc (mjr_vec_make-const npts 0)))
                                 (dotimes (i npts)
                                   (setf (aref norms i) (vector 0 0 0)))
                                 (dotimes (xi (1- nx))
                                   (dotimes (yi (1- ny))
                                     (let* ((t1 (list (+ xi (* nx yi)) (+ (1+ xi) (* nx yi)) (+ (1+ xi) (* nx (1+ yi)))))
                                            (t2 (list (+ xi (* nx yi)) (+ (1+ xi) (* nx (1+ yi))) (+ xi (* nx (1+ yi)))))
                                            (n1 (mjr_geom_triangle-normal (mapcar (lambda (i) (aref pts i)) t1)))
                                            (n2 (mjr_geom_triangle-normal (mapcar (lambda (i) (aref pts i)) t2))))
                                       (mapc (lambda (i) (setf (aref norms i) (mjr_vec_+ (aref norms i) n1))) t1)
                                       (mapc (lambda (i) (setf (aref norms i) (mjr_vec_+ (aref norms i) n2))) t2)
                                       (mapc (lambda (i) (incf (aref normc i))) (append t1 t2)))))
                                 (format dest "        normal_vectors { ~a " npts)
                                 (dotimes (i npts)
                                   (let ((apt (mjr_vec_/ (aref norms i) (aref normc i))))
                                     (format dest ",~a" (mjr_vec_code apt :lang :lang-povray))))
                                 (format dest "}~%")))
                           (if surface-color-dat
                               (progn (format dest "        texture_list { ~d, " npts)
                                      (dotimes (xi npts)
                                        (format dest "texture{triTex pigment{rgb ~a}} " (mjr_vec_code (mjr_arr_aref-cmo-as-vector-mod surface-color-dat xi) :lang :lang-povray)))
                                      (format dest "}~%")))
                           (format dest "        face_indices { ~a " (* 2 (1- nx) (1- ny)))
                           (dotimes (xi (1- nx))
                             (dotimes (yi (1- ny))
                               (let ((v1 (+ xi (* nx yi)))
                                     (v2 (+ (1+ xi) (* nx yi)))
                                     (v3 (+ (1+ xi) (* nx (1+ yi))))
                                     (v4 (+ xi (* nx yi)))
                                     (v5 (+ (1+ xi) (* nx (1+ yi))))
                                     (v6 (+ xi (* nx (1+ yi)))))
                                 (if surface-color-dat
                                     (progn (format dest ",~a,~d,~d,~d" (mjr_vec_code (vector v1 v2 v3) :lang :lang-povray) v1 v2 v3)
                                            (format dest ",~a,~d,~d,~d" (mjr_vec_code (vector v4 v5 v6) :lang :lang-povray) v4 v5 v6))
                                     (progn (format dest ",~a" (mjr_vec_code (vector v1 v2 v3) :lang :lang-povray))
                                            (format dest ",~a" (mjr_vec_code (vector v4 v5 v6) :lang :lang-povray)))))))
                           (format dest "}~%")
                           (if surface-color-dat
                               (format dest "}~%")
                               (format dest "        texture { triTex } }~%")))
                    (dotimes (xi (1- nx))
                      (dotimes (yi (1- ny))
                        (let ((t1 (mapcar (lambda (i) (aref pts i)) (list (+ xi (* nx yi)) (+ (1+ xi) (* nx yi))      (+ (1+ xi) (* nx (1+ yi))))))
                              (t2 (mapcar (lambda (i) (aref pts i)) (list (+ xi (* nx yi)) (+ (1+ xi) (* nx (1+ yi))) (+ xi (* nx (1+ yi)))))))
                          (if (not (mjr_geom_simplex-degeneratep nil t1))
                              (apply #'format dest "triangle { ~a, ~a, ~a texture { triTex } }~%" (mapcar (lambda (i) (mjr_vec_code i :lang :lang-povray)) t1)))
                          (if (not (mjr_geom_simplex-degeneratep nil t2))
                              (apply #'format dest "triangle { ~a, ~a, ~a texture { triTex } }~%" (mapcar (lambda (i) (mjr_vec_code i :lang :lang-povray)) t2))))))))
            (if (member :x draw-surface-grid)
                (dotimes (xi nx)
                  (format dest (mjr_pov_code-curve (loop for yi from 0 upto (1- ny)
                                                         collect (aref pts (+ xi (* nx yi))))
                                                   curve-spline))))
            (if (member :y draw-surface-grid)
                (dotimes (yi ny)
                  (format dest (mjr_pov_code-curve (loop for xi from 0 upto (1- nx)
                                                         collect (aref pts (+ xi (* nx yi))))
                                                   curve-spline))))
            (if (member :d draw-surface-grid)
                (progn (dotimes (xii (1- nx))
                         (format dest (mjr_pov_code-curve (loop for yi from 0   upto (1- ny)
                                                                for xi from xii upto (1- nx)
                                                                collect (aref pts (+ xi (* nx yi))))
                                                          curve-spline)))
                       (dotimes (yii (1- ny))
                         (format dest (mjr_pov_code-curve (loop for yi from yii upto (1- ny)
                                                                for xi from 0   upto (1- nx)
                                                                collect (aref pts (+ xi (* nx yi))))
                                                          curve-spline)))))))
      (if (and (= 2 d1ct) draw-curves) ; 1d case (curve)
          (format dest (mjr_pov_code-curve (concatenate 'list pts) curve-spline)))
      (if draw-points ; Draw points for 0D, 1D, and 3D cases
          (dotimes (i npts)
            (format dest "sphere { ~a, vertexDiam texture { vertexTex } }~%" (pt2st i)))))
    (close dest)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_pov_make-from-func-r12-r13 (out-file f &key udat vdat c-func
                                       (draw-surfaces 't) (draw-curves 't) (draw-points 't) (draw-surface-grid (list :x :y :d))
                                       (use-mesh nil) (curve-spline nil) (surface-smooth nil) (arg-mode :arg-number))
  "Sample a single function on a regular grid, and write results to a POLYDATA VTK file.

Cases
  domain-rank   udat   vdat   func-type   range-rank   Representation             x      y      z        c-func allowed?
  1             seq    nil    3D v-func   3            Parametric Curve in 3D     f(u)   f(u)   f(u)     NO
  2             seq    seq    s-func      1            Rectilinear Surface in 3D  u      v      f(u,v)   NO
  2             seq    seq    3D v-func   3            Parametric Surface in 3D   f(u,v) f(u,v) f(u,v)   OK
  1             seq    nil    s-func      1            Rectilinear Curve in 2D    u      f(u)   0        NO

Arguments
  * UDAT & VDAT: Describe the rectilinear space upon which the function F is sampled
  * F: Function that takes 1 or 2 variables (determined by :UDAT & :VDAT) and returns a number or vector
  * XDAT, YDAT, & ZDAT: Describe the rectilinear space upon which the function F is sampled
  * c-func: color valued function to be sampled (takes DOMAIN-RANK variables)
  * f: a function to be sampled (takes DOMAIN-RANK variables)"
  (cond ((not (stringp out-file))    (error "mjr_pov_make-from-func-r12-r132: OUT-FILE must be a string!")))
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
        (mjr_pov_make-from-gndata out-file
                                :xdat udat :ydat vdat :zdat sdat
                                :dims (list xnum ynum 1)
                                :surface-color-dat cdat
                                :draw-surfaces draw-surfaces :draw-curves draw-curves :draw-points draw-points :draw-surface-grid draw-surface-grid
                                :use-mesh use-mesh :curve-spline curve-spline :surface-smooth surface-smooth)
        (mjr_pov_make-from-gndata out-file 
                                :xdat (mjr_arr_map (lambda (v) (aref v 0)) sdat)
                                :ydat (mjr_arr_map (lambda (v) (aref v 1)) sdat)
                                :zdat (mjr_arr_map (lambda (v) (aref v 2)) sdat)
                                :dims (list xnum ynum 1)
                                :draw-surfaces draw-surfaces :draw-curves draw-curves :draw-points draw-points :draw-surface-grid draw-surface-grid
                                :use-mesh use-mesh :curve-spline curve-spline :surface-smooth surface-smooth))))


;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_pov_tga-write-from-func-r2-r1 (field-file map-file f &key 
                                          xdat ydat
                                          color-method max-color auto-scale arg-mode show-progress)
  "Sample a real valued function on a regular 2D grid, and create a PovRay height field and image map TGA files."
  (cond ((not (stringp field-file))    (error "mjr_pov_tga-write-from-func-r2-r1: FIELD-FILE must be a string!"))
        ((not (stringp map-file))      (error "mjr_pov_tga-write-from-func-r2-r1:: MAP-FILE must be a string!")))
  (if show-progress
      (format 't "PROGRESS: mjr_pov_tga-write-from-func-r2-r1: Computing function values~%"))
  (let* ((f    (apply #'mjr_mxp_string-or-func-to-lambda f "x" (if ydat (list "y"))))
         (xdat (mjr_vvec_to-vec-maybe xdat))
         (ydat (mjr_vvec_to-vec-maybe ydat))
         (zdat (mjr_combc_gen-all-cross-product (list xdat ydat) :collect-value f :result-type :array :arg-mode arg-mode :show-progress show-progress)))
    (if show-progress
        (format 't "PROGRESS: mjr_pov_tga-write-from-func-r2-r1: Processing height field file.~%"))
    (mjr_img_tga-write field-file
                       (mjr_img_make-from-gndata zdat 
                                                 :max-color      #xFFFF
                                                 :auto-scale     't
                                                 :color-space    :cs-tru
                                                 :color-method   #'mjr_colorized_povray
                                                 :show-progress  show-progress)
                       :show-progress show-progress)
    (if show-progress
        (format 't "PROGRESS: mjr_pov_tga-write-from-func-r2-r1: Processing image map file.~%"))
    (mjr_img_tga-write map-file
                       (mjr_img_make-from-gndata zdat
                                                 :color-method  color-method     
                                                 :max-color     max-color     
                                                 :auto-scale    auto-scale
                                                 :color-space   :cs-tru
                                                 :show-progress show-progress)
                       :show-progress show-progress)))
