;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-qmesh.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Quadrilateral Mesh.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1995,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_qmesh_make-search.@EOL@EOL
;; @todo      mjr_qmesh_search-vec that works for 1D, 2D & 3D meshes.@EOL@EOL
;; @todo      mjr_qmesh_search-seq that works for 1D, 2D & 3D meshes.@EOL@EOL
;; @todo      mjr_qmesh_vvec2search: take a one or more vvec objects, and return a mjr_qmesh_search function for the associated mesh.@EOL@EOL
;; @warning   Experimental!!  Not even close to being finished.  Just some simple 1D stuff for now...@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_QMESH
  (:USE :COMMON-LISP 
        :MJR_VVEC)
  (:DOCUMENTATION "Brief: Quadrilateral Mesh.;")
  (:EXPORT #:mjr_qmesh_help
           #:mjr_qmesh_search-vec
           #:mjr_qmesh_search-seq


           ))

(in-package :MJR_QMESH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmesh_help ()
  "Help for MJR_QMESH: 

NOTE: This is EXPERIMENTAL & INCOMPLETE.  Only 1D meshes (represented as vectors or arithmetic sequences) are supported right now.

Cover a 1, 2, or 3 dimensional rectangle with some number of $n$ dimensional sub-rectangles.  

   * Split a closed interval of the real line into sub-intervals
     Useful for histograms, 1D numerical integration, ODE solutions
   * Split a 2D rectangle in R^2 into smaller rectangles.
     Useful for rectilinear numerical PDE solutions and 2D integration.
   * Split a 3D rectangular prism in R^3 into smaller rectangular prisms
     Useful for finite volume PDE solutions.

An $n$ dimensional rectangular mesh is defined by $n$ real vectors each with strictly increaseing real elements:
    $$\\{\\overline{v_i}\\in\\mathbb{R}^{k_i} \\vert i\\in\\mathbb{Z}^n \\,\\mathrm{and}\\, k\\in\\mathbb{N}, k>0
      \\,\\mathrm{and}\\, (\\overline{v_i})_j<(\\overline{v_i})_m \\,\\forall j,m\\in\\mathbb{Z}^{k_i}, j<m\\}$$

For the 1 dimensional case, we have $n=1$, and thus a single vector:
$$\\overline{v}=(v_0, v_1, ..., v_{k-1}, v_k)\\in\\mathbb{R}^{k}$$

The intervals are defined in one of two ways depending on the value of the INTERVAL-TYPE argument:

   * :INTERVAL-TYPE-LEFT-CLOSED:   $$I_0=[v_0, v_1), ..., I_i=[v_i, v_{i+1}), ..., I_{k-1}=[v_{k-1}, v_k]$$

   * :INTERVAL-TYPE-LEFT-OPEN:     $$I_0=[v_0, v_1], ..., I_i=(v_i, v_{i+1}], ..., I_{k-1}=(v_{k-1}, v_k]$$
"
  (documentation 'mjr_qmesh_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmesh_search-vec (interval-type breaks x)
  "Return the index of the sub-interval that contains x.  The mesh is described as a vector of break points (interval end points)."
  (let* ((len (length breaks)))
    (cond ((< x (aref breaks 0))        -1)
          ((> x (aref breaks (1- len))) -2)
          ('t                           (loop with lo = 0
                                              with hi = (- len 2)
                                              for g = (truncate (+ lo hi) 2)
                                              do (if (= lo hi)
                                                     (return g))
                                              do (if (case interval-type
                                                       (:interval-type-left-closed (<  x (aref breaks g)))
                                                       (:interval-type-left-open (<= x (aref breaks g)))
                                                       ('t                (error "Unsupported interval-type")))
                                                     (setf hi (1- g))
                                                     (if (case interval-type
                                                           (:interval-type-left-closed (>= x (aref breaks (1+ g))))
                                                           (:interval-type-left-open (>  x (aref breaks (1+ g))))
                                                           ('t                (error "Unsupported interval-type")))
                                                         (setf lo (1+ g))
                                                         (return g))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmesh_search-seq (interval-type interval-start interval-end num-intervals x)
  "Return the index of the sub-interval that contains x.  

num-intervals is the number of intervals.

Same as mjr_qmesh_search-vec with BREAKS such that $b_i = start+\\frac{i\\cdot(end-start)}{num}$."
  (cond ((< x interval-start) -1)
        ((> x interval-end)   -2)
        ('t                   (let* ((wid (/ (- interval-end interval-start) num-intervals))
                                     (dst (/ (- x interval-start) wid))
                                     (idx (floor dst)))
                                (min (1- num-intervals) (max 0 (if (and (= idx (ceiling dst))
                                                                        (not (equalp interval-type :interval-type-left-closed)))
                                                                   (1- idx)
                                                                   idx)))))))


;; Need to rethink this....
;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_qmesh_make-search (interval-type vvec)
;;   "Take arguments describing a vvec and an interval type, and return a fast search function"
;;   (if (equalp vvec-type :vvt-aseq)
;;       (eval `(lambda (x) 
;;                (mjr_qmesh_search-seq ,interval-type ,start ,end ,(1- len) x)))
;;       (eval `(lambda (x) 
;;                (mjr_qmesh_search-vec ,(mjr_vvec_gen-0sim 'vector vvec) x))))))




;; (dotimes (j 100)
;;   (let* ((start (rationalize (random 20.0)))
;;          (end   (rationalize (+ start 3 (random 20.0))))
;;          (len   (+ 2 (random 20)))
;;          (brk   (mjr_part_gen-0sim 'vector :start start :end end :len (1+ len))))
;;     (loop for i from 1 upto 10000
;;           for x = (rationalize (random (* end 1.2)))
;;           for iv1 = (mjr_qmesh::mjr_qmesh_search-seq :interval-type-left-open start end len x)
;;           for iv2 = (mjr_qmesh::mjr_qmesh_search-vec :interval-type-left-open brk x)
;;           when (not (equalp iv1 iv2))
;;           do (format 't "~15a ~5a ~5a ~10a ~10a ~10a~%" x iv1 iv2 start end len))))


;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_intrp_which-cut-segment (x-data x)

;;   "Given an array of breaks on an interval, return the index of the lower limit of the interval containing x. 
;; When x is properly contained in the interval, the index and 't are returned.  When x is not contained, then 0 or n-2 is returned
;; along with nil."
;;   (let* ((len-x (length x-data)))
;;     (if (< x (aref x-data 0))
;;         (values 0 nil)
;;         (if (>= x (aref x-data (1- len-x)))
;;             (values (- len-x 2) nil)
;;             (loop for i from 1 upto (1- len-x)
;;                   when (< x (aref x-data i))
;;                   do (return (values (1- i) 't)))))))
