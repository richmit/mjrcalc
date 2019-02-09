;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-geom.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Computational Geometry.@EOL
;; @std       Common Lisp
;; @see       tst-geom.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1994,1995,1997,1998,2004,2011,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo
;; @todo      BOOLEAN: simplex (point/segment/triangle/tetrahedron) intersect simplex (point/segment/triangle/tetrahedron).@EOL@EOL
;; @todo      point:   ray-ray intersection.@EOL@EOL
;; @todo      BOOLEAN: does ray intersect simplex (point/segment/triangle/tetrahedron).@EOL@EOL
;; @todo      point:   "first" ray intersection with simplex (point/segment/triangle/tetrahedron).@EOL@EOL
;; @todo      BOOLEAN: is polygon convex.@EOL@EOL
;; @todo      polygon: compute convex hull.@EOL@EOL
;; @todo      area of triangle spanned by two vectors
;; @todo      area of quad spanned by two vectors
;; @todo      area of tetehreda spanned by three vectors
;; @todo      area of prism spanned by three vectors
;; @todo      minimum angle, and cos of said angle, defined by three points -- central point connected to other two by segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GEOM
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_CHK
        :MJR_EPS
        :MJR_VEC
        :MJR_COMBE
        :MJR_MAT
        :MJR_ARR
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Computational Geometry.;")
  (:EXPORT #:mjr_geom_help
           ;; Misc
           #:mjr_geom_simplex-area
           #:mjr_geom_simplex-degeneratep
           #:mjr_geom_triangle-normal
           #:mjr_geom_point-inside-simplex?
           ;; bounding boxes
           #:mjr_geom_bounding-box           
           #:mjr_geom_point-inside-bounding-box-f?
           #:mjr_geom_bounding-area
           #:mjr_geom_bounding-widths
           ;; Angles between vectors
           #:mjr_geom_vec-cos-angle-f
           #:mjr_geom_vec-angle-f
           ;; Angles between segments
           #:mjr_geom_tri-cos-angle-f
           #:mjr_geom_tri-angle-f           
           ;; Length between points
           #:mjr_geom_distance-euclidean
           #:mjr_geom_distance-euclidean-squared
           #:mjr_geom_distance-taxicab
           #:mjr_geom_distance-chebyshev
           ;; Intersections
           #:mjr_geom_segment-intersect
           ))

(in-package :MJR_GEOM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_help ()
  "This library provides computational geometric support -- mostly for other libraries, but is useful interactively too.

Some functions are optimized for speed, and do very little error checking (-f or -f? suffix)"
  (documentation 'mjr_geom_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_point-inside-simplex? (points &rest vertexes)
  "A non-NIL insures all POINTS (a single vector or list of vectors) are inside the simplex (sure in the MJR_CMP sense)."
  (let* ((points   (if (listp points) points (list points)))
         (num-vert (length vertexes))
         (v0       (car vertexes))
         (len      (length v0)))
    (cond ((some (lambda (x) (not (= len (length x)))) (append points vertexes))
           (error "mjr_geom_point-inside-simplex?: POINTS and SIMPLEX vertexes must be of same dimensions"))
          ((not (= len (1- num-vert)))
           (error "mjr_geom_point-inside-simplex?: POINTS dimensions must match SIMPLEX dimension")))
    (cond ((= 1 num-vert) (every (lambda (x) (mjr_cmp_= x (aref v0 0))) points))
          ('t             (let ((a (mjr_arr_cbind (mapcar (lambda (x) (mjr_vec_- x v0)) (cdr vertexes)))))
                            (every (lambda (point)
                                     (let* ((bc (mjr_mat_solve-sys-sge a (mjr_vec_- point v0)))
                                            (bs (reduce #'+ bc)))
                                       (and (every (lambda (x) (mjr_cmp_>= x 0)) bc)
                                            (mjr_cmp_<= bs 1))))
                                   points))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_bounding-box (first-point &rest rest-points)
  "Return a bounding box for the given points -- i.e. two vectors $m$ and $M$ such that $m_i<=p_i<=M_i$ for all i and points provided."
  (let ((minp (copy-seq first-point))
        (maxp (copy-seq first-point))
        (lenp (length first-point)))
    (dolist (p rest-points)
      (dotimes (i lenp)
        (if (> (aref p i) (aref maxp i)) (setf (aref maxp i) (aref p i)))
        (if (< (aref p i) (aref minp i)) (setf (aref minp i) (aref p i)))))
    (values minp maxp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_point-inside-bounding-box-f? (points minp maxp)
  "A non-NIL means all POINTS (a single vector or list of vectors) are probably (<= & >= are used to test) inside the bounding box.  

The MINP, and MAXP arguments should be real vectors of the same length.  The POINTS argument should be a single real vector, or a list of real vectors -- all
of the same length as MINP & MAXP.  This is a low level function tuned for speed.  Note that no error checking is preformed regard to the lengths of the input
vectors or data types they contain."
  (if (listp points)
      (every (lambda (point) (and (every #'>= point minp)
                                  (every #'<= point maxp)))
             points)
      (and (every #'>= points minp)
           (every #'<= points maxp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_bounding-area (minp maxp)
  "The length/area/volume/hypervolume of the given bounding box"
  (abs (reduce #'* (map 'vector #'- maxp minp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_bounding-widths (minp maxp)
  "The length of each of the sides of the given bounding box"
  (map 'vector #'- maxp minp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_simplex-area (&rest vertexes)
  "Return area of simplex.

      |----------------------------------------------------------------------------|
      |           Cases for which this function works (returns non-NIL)            |
      |-----------------+----------------------+-----------------------------------|
      | 2-simplex in R3 | triangle in space    |                                   |
      | 2-simplex in R2 | triangle in plane    | optimized case of n-simplex in Rn |
      | 3-simplex in R3 | tetrahedron in space | optimized case of n-simplex in Rn |
      | 1-simplex in Rn | line segment in Rn   |                                   |
      | n-simplex in Rn |                      | A bit slow                        |
      |-----------------+----------------------+-----------------------------------|"
  (destructuring-bind (v1 &rest rest-verts) vertexes
    (let ((num-vert  (length vertexes))
          (space-dim (length v1)))
      (case num-vert
        (3         (case space-dim
                     (3 (destructuring-bind (v2 v3) rest-verts
                          (* 1/2 (mjr_numu_sqrt (+ (expt (- (* (- (svref v2 1) (svref v1 1)) (- (svref v1 2) (svref v3 2)))
                                                            (* (- (svref v2 2) (svref v1 2)) (- (svref v1 1) (svref v3 1)))) 2)
                                                   (expt (- (* (- (svref v2 2) (svref v1 2)) (- (svref v1 0) (svref v3 0)))
                                                            (* (- (svref v2 0) (svref v1 0)) (- (svref v1 2) (svref v3 2)))) 2)
                                                   (expt (- (* (- (svref v2 0) (svref v1 0)) (- (svref v1 1) (svref v3 1)))
                                                            (* (- (svref v2 1) (svref v1 1)) (- (svref v1 0) (svref v3 0)))) 2))))))
                     (2 (destructuring-bind (v2 v3) rest-verts
                          (abs (* 1/2 (- (* (- (aref v2 0) (aref v1 0)) (- (aref v3 1) (aref v1 1)))
                                         (* (- (aref v2 1) (aref v1 1)) (- (aref v3 0) (aref v1 0))))))))))
        (4         (if (= 3 space-dim)
                       (destructuring-bind (v2 v3 v4) rest-verts
                         (mjr_vec_triple-cross (mjr_vec_- v2 v1) (mjr_vec_- v3 v1) (mjr_vec_- v4 v1)))))
        (2         (destructuring-bind (v2) rest-verts
                     (abs (mjr_vec_norm-two (mjr_vec_- v2 v1)))))
        (otherwise (if (= space-dim (1- num-vert))
                       (abs (* (/ (mjr_combe_! space-dim))
                               (mjr_mat_det-small (mjr_arr_rbind (loop for vn in rest-verts
                                                                      collect (mjr_vec_- v1 vn))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_simplex-degeneratep (eps &rest vertexes)
  "degenerate means at least two vertexes are within eps of each other."
  (if (and vertexes (listp (first vertexes)))
      (apply #'mjr_geom_simplex-degeneratep eps (first vertexes))
      (loop for fst in vertexes
            for rst = (cdr vertexes) then (cdr rst)
            do (loop for v in rst
                     do (if (every (lambda (x) (< (abs x) (or eps 0.0001))) (mjr_vec_- fst v))
                            (return-from mjr_geom_simplex-degeneratep 't))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_triangle-normal (&rest vertexes)
  "ONLY WORKS IN R^3"
  (if (and vertexes (listp (first vertexes)))
      (apply #'mjr_geom_triangle-normal (first vertexes))
      (mjr_vec_cross (mjr_vec_- (first vertexes) (second vertexes)) (mjr_vec_- (third vertexes) (second vertexes)))))

(defun mjr_geom_vec-cos-angle-f (v0 v1)
  "Cosine of minimum angle between vectors v0 and v1 in $R^n$ space.

No error checking regarding length and content of vectors. Works for any $n$, but is optimized for n=2,3."
  (let ((len (length v0)))
    (case len
      (2 (/ (+ (* (aref v0 0) (aref v1 0))
               (* (aref v0 1) (aref v1 1)))
            (sqrt (+ (expt (aref v0 0) 2) (expt (aref v0 1) 2)))
            (sqrt (+ (expt (aref v1 0) 2) (expt (aref v1 1) 2)))))
      (3 (/ (+ (* (aref v0 0) (aref v1 0))
               (* (aref v0 1) (aref v1 1))
               (* (aref v0 2) (aref v1 2)))
            (sqrt (+ (expt (aref v0 0) 2) (expt (aref v0 1) 2) (expt (aref v0 2) 2)))
            (sqrt (+ (expt (aref v1 0) 2) (expt (aref v1 1) 2) (expt (aref v1 2) 2)))))
      (otherwise (/ (mjr_vec_dot v0 v1) (mjr_vec_norm-two v0) (mjr_vec_norm-two v1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_vec-angle-f (v0 v1)
  "Minimum angle in radians between vectors v0 and v1 in $R^n$ space.

No error checking regarding length and content of vectors. Works for any $n$, but is optimized for n=2,3."
  (min pi (max 0 (acos (/ (mjr_vec_dot v0 v1) (mjr_vec_norm-two v0) (mjr_vec_norm-two v1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_tri-cos-angle-f (u v w)
  "Cosine of minimum angle between vectors u-v and w-v in $R^n$ space.

No error checking regarding length and content of vectors. Works for any $n$, but is optimized for n=2,3."
  (mjr_geom_vec-cos-angle-f (map 'vector #'- u v) (map 'vector #'- w v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_tri-angle (u v w)
  "Minimum angle in radians between vectors u-v and w-v in $R^n$ space."
  (mjr_geom_vec-angle-f (map 'vector #'- u v) (map 'vector #'- w v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_distance-euclidean (u v)
  "Compute distance between two vectors with the two norm."
  (sqrt (loop for ui across u
              for vi across v
              sum (expt (- ui vi) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_distance-euclidean-squared (u v)
  "Compute distance between two vectors with the two norm."
  (loop for ui across u
        for vi across v
        sum (expt (- ui vi) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_distance-taxicab (u v)
  "Compute distance between two vectors with the one norm.

Other names: rectilinear distance, L1 distance, snake distance, city block distance, and Manhattan distance."
  (mjr_vec_norm-one (mjr_vec_- u v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_distance-chebyshev (u v)
  "Compute distance between two vectors with the infinity norm.

In 2D this norm is the minimum number of moves required by a king to travel between two squares on a chessboard."
  (mjr_vec_norm-infinity (mjr_vec_- u v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_segment-intersect (v1 v2 v3 v4 &key (coord-eps 1.0d-4) (zero-eps 1.0d-5))
  "Return the point of intersection of v1--v2 and v3--v4, or nil if they don't intersect.

Works for any size vectors, but the 2D case is optimized.
TODO: Optimize 3D case."
  (let ((len (length v1)))
    (case len
      (20         (let ((v11 (aref v1 0))                      ;; (%i60) a:matrix([v41-v31, v11-v21], [v42-v32, v12-v22]);                     
                        (v12 (aref v1 1))                      ;; (%o60) matrix([v41-v31,v11-v21],[v42-v32,v12-v22])                           
                        (v21 (aref v2 0))                      ;; (%i61) determinant(a);                                                       
                        (v22 (aref v2 1))                      ;; (%o61) (v12-v22)*(v41-v31)-(v11-v21)*(v42-v32)                               
                        (v31 (aref v3 0))                      ;; (%i62) expand(invert(a)*determinant(a)) . matrix([v11-v31],[v12-v32]);       
                        (v32 (aref v3 1))                      ;; (%o62) matrix([(v21-v11)*(v12-v32)+(v12-v22)*(v11-v31)],                     
                        (v41 (aref v4 0))                      ;;               [(v11-v31)*(v32-v42)+(v12-v32)*(v41-v31)])                     
                        (v42 (aref v4 1)))                     ;; that's t2 and t1 in the vector above.                                              
                    (let ((d (- (* (- v12 v22) (- v41 v31)) (* (- v11 v21) (- v42 v32)))))
                      (if (mjr_chk_!=0 d zero-eps)
                          (let ((t2 (/ (+ (* (- v21 v11) (- v12 v32)) (* (- v12 v22) (- v11 v31))) d)))
                            (if (and (>= t2 0) (<= t2 1))
                                (let ((t1 (/ (+ (* (- v11 v31) (- v32 v42)) (* (- v12 v32) (- v41 v31))) d)))
                                  (if (and (>= t1 0) (<= t1 1))
                                      (vector (+ v11 (* t1 (- v21 v11)))
                                              (+ v12 (* t1 (- v22 v12))))))))))))
      (otherwise  (let ((v1-v3      (map 'vector #'- v1 v3))                  
                        (v1-v2      (map 'vector #'- v1 v2))                   
                        (v4-v3      (map 'vector #'- v4 v3))
                        (c1maxi1    nil)           ;; Index of element wih maximal magnitude in v1-v2
                        (c2maxi1    nil)           ;; Index of element wih maximal magnitude in v4-v3
                        (c2maxi2    nil))          ;; Index of element wih second largest magnitude in v4-v3
                    (loop with c1maxv1 = nil
                          with c2maxv1 = nil
                          with c2maxv2 = nil
                          for c1v across v1-v2
                          for c2v across v4-v3
                          for c1av = (abs c1v)
                          for c2av = (abs c2v)
                          for i from 0
                          do (if (or (null c1maxv1) (> c1av c1maxv1))
                                 (psetq c1maxv1 c1av
                                        c1maxi1 i))
                          do (cond ((or (null c2maxv1) (> c2av c2maxv1)) (psetq c2maxv1 c2av
                                                                                c2maxi1 i
                                                                                c2maxv2 c2maxv1
                                                                                c2maxi2 c2maxi1))
                                   ((or (null c2maxv2) (> c2av c2maxv2)) (psetq c2maxv2 c2av
                                                                                c2maxi2 i))))
                    (let ((w11 (aref v1-v2 c1maxi1))              ;; [[w11 w12]] [[t1]]   [[b1]]        
                          (w12 (aref v4-v3 c1maxi1))              ;; [[       ]] [[  ]] = [[  ]]
                          (b1  (aref v1-v3 c1maxi1))              ;; [[w21 w22]] [[t2]]   [[b2]]
                          (w21 (aref v1-v2 (if (= c1maxi1 c2maxi1) c2maxi2 c2maxi1)))
                          (w22 (aref v4-v3 (if (= c1maxi1 c2maxi1) c2maxi2 c2maxi1)))          ;; (%i63) A:matrix([w11, w12], [w21, w22]);                     
                          (b2  (aref v1-v3 (if (= c1maxi1 c2maxi1) c2maxi2 c2maxi1))))         ;; (%o63) matrix([w11,w12],[w21,w22])                           
                      (let ((d (- (* w11 w22) (* w12 w21))))                                   ;; (%i64) determinant(A);                                       
                        (if (mjr_chk_!=0 d zero-eps)                                           ;; (%o64) w11*w22-w12*w21                                       
                            (let ((t1 (/ (- (* b1 w22) (* b2 w12)) d)))                        ;; (%i65) expand(invert(A)*determinant(A));                     
                              (if (and (>= t1 0) (<= t1 1))                                    ;; (%o65) matrix([w22,-w12],[-w21,w11])                         
                                  (let ((t2 (/ (- (* b2 w11) (* b1 w21)) d)))                  ;; (%i66) expand(invert(A)*determinant(A)).matrix([b1],[b2]);   
                                    (if (and (>= t2 0) (<= t2 1))                              ;; (%o66) matrix([b1*w22-b2*w12],[b2*w11-b1*w21])               
                                        (let ((p1 (map 'vector (lambda (x y) (- x y)) v1 (map 'vector (lambda (x) (* t1 x)) v1-v2)))
                                              (p2 (map 'vector (lambda (x y) (+ x y)) v3 (map 'vector (lambda (x) (* t2 x)) v4-v3))))
                                          (if (every (lambda (x y) (mjr_eps_= x y coord-eps)) p1 p2)
                                              p1))))))))))))))









