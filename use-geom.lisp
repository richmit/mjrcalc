;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-geom.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Computational Geometry.@EOL
;; @std       Common Lisp
;; @see       tst-geom.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1994,1995,1997,1998,2004,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GEOM
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_VEC
        :MJR_COMBE
        :MJR_MAT
        :MJR_ARR
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Computational Geometry.;")
  (:EXPORT #:mjr_geom_help
           #:mjr_geom_simplex-area
           #:mjr_geom_simplex-degeneratep
           #:mjr_geom_triangle-normal
           #:mjr_geom_point-inside-simplex?
           ))

(in-package :MJR_GEOM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geom_help ()
  "This library provides computational geometric support -- mostly for other libraries, but is useful interactively too."
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
