;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-dsimp.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Data sets on SIMPlicial complexes.@EOL
;; @std       Common Lisp
;; @see       tst-dsimp.lisp
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
;; @todo      Refine via Ruppert algorithm.@EOL@EOL
;; @todo      Refine via stupid refinement for integration.@EOL@EOL
;; @todo      mjr_dsimp_colorize.@EOL@EOL
;; @warning   Still a bit experimental, but useful.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_DSIMP
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_DQUAD
        :MJR_UTIL
        :MJR_COLOR
        :MJR_ARR
        :MJR_VEC
        :MJR_GEOM
        :MJR_UTIL
        :MJR_ANNOT)
  (:DOCUMENTATION "Brief: Data sets on SIMPlicial complexes.;")
  (:EXPORT #:mjr_dsimp_help
           ;; Create dsimp lists
           #:mjr_dsimp_make-from-dquad
           #:mjr_dsimp_make-from-points
           #:mjr_dsimp_make-from-xyz
           ;; Create data arrays from dsimp lists
           #:mjr_dsimp_map
           ;; Add data data to a dsimp list
           #:mjr_dsimp_add-data-from-map
           #:mjr_dsimp_convert-points-do-data
           #:mjr_dsimp_add-data
           #:mjr_dsimp_add-simplex-array
           #:mjr_dsimp_connect-points
           ;; New dsimp lists from old ones
           #:mjr_dsimp_data-count
           #:mjr_dsimp_simplex-count
           ;; Get size elements from a dsimp list
           #:mjr_dsimp_get-simplex-array
           #:mjr_dsimp_get-data-array
           #:mjr_dsimp_get-data-ano
           ;; Persistence
           #:mjr_dsimp_read-from-file
           #:mjr_dsimp_write-to-file
           ;; 2D Simplex Persistence
           #:mjr_dsimp_dump2d-ply-file
           #:mjr_dsimp_dump2d-obj-file
           ))

(in-package :MJR_DSIMP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_help ()
  "Help for MJR_DSIMP:

Library for dealing with triangulated (simplicial complex'ish) data sets.

The canonical example is computer graphics: When drawing surfaces in 3D, it is common to approximate the surfaces with triangles.  For this application, the
data payload for each vertex is frequently the actual surface normal at that point.  The canonical example from mechanical engineering is finite element
analysis of machine components.  Here the component is broken up into tetrahedra, and each tetrahedra has some data associated with it (like density or
temperature).

About 'triangles':

  A k-simplex is a k-dimensional polytope defined as the convex hull of its $k+1$ vertices.  The 0-simplex is a single point, 1-simplex is a line segment, a
  2-simplex is a triangle (with it's interior), the 3-simplex is a tetrahedron (with it's interior), and the 4-simplex is a 5-cell.  We shall use 'simplices'
  as the plural form of simplex.

I have chosen to implement the data storage as a simple list instead of a complex, opaque object -- the wisdom of that decision has yet to be proven. :)

  (list (list NUM-0-DATA            ;; Number of meta-data/data-array pairs for 0-simplex data.
              NUM-1-DATA            ;; Number of meta-data/data-array pairs for 1-simplex data.
              NUM-2-DATA            ;; Number of meta-data/data-array pairs for 2-simplex data.
              NUM-3-DATA            ;; Number of meta-data/data-array pairs for 3-simplex data.
        )
        0-simplex                   ;; Array of points
        0-data-meta 0-data-array    ;; Meta data and data-array for 0-simplex data
        ...
        1-simplex                   ;; Array of vectors of indexes into 0-simplex.  NIL if no simplices.
        1-data-meta 1-data-array    ;; Meta data and data-array for 1-simplex data
        ...
        2-simplex                   ;; Array of vectors of indexes into 0-simplex.  NIL if no simplices.
        2-data-meta 2-data-array    ;; Meta data and data-array for 2-simplex data
        ...
        3-simplex                   ;; Array of vectors of indexes into 0-simplex.  NIL if no simplices.
        3-data-meta 3-data-array    ;; Meta data and data-array for 3-simplex data
        ...
        )

Today only simplices up to dimension 3 are supported, but that may change.  Frankly, only 2-simplices are well supported.

The N-data-meta objects are meta data alists as described by MJR_ANNOT_HELP.  Note that all meta data alists MUST contain both the :ano-nam and :ano-typ keys,
and the :ano-nam values must be unique across ALL meta data lists.

Examples of some DSIMP lists may be found in tst-dsimp.lisp."
  (documentation 'mjr_dsimp_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_read-from-file (file-name)
  "Read a dsimp list from a file.

Data in the file after the first DSIMP list will be ignored.  This space is traditionally used for free text commentary regarding the data set."
  (with-open-file (stream file-name)
    (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_write-to-file (dsimp file-name)
  "Write a dsimp list from a file"
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (null (write  dsimp :stream stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_data-count (dsimp &optional simplex-dimension)
  "Return the number of data elements for the simplices of dimension SIMPLEX-DIMENSION"
  (cond ((and simplex-dimension (> 0 simplex-dimension))   (error "mjr_dsimp_data-count: simplex-dimension must be positive!"))
        ((and simplex-dimension (< 4 simplex-dimension))   (error "mjr_dsimp_data-count: simplex-dimension must be less than 4!")))
  (if simplex-dimension
      (nth simplex-dimension (car dsimp))
      (car dsimp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_simplex-indexes (dsimp &optional simplex-dimension)
  "Return a list of simplex indexes as a list."
  (let ((all-dims (loop with i = 1
                        for c in (mjr_dsimp_data-count dsimp)
                        collect i
                        do (incf i (+ 1 (* 2 c))))))
    (if simplex-dimension
        (mapcar (lambda (i) (nth i all-dims)) (mjr_util_non-list-then-list simplex-dimension))
        all-dims)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_simplex-count (dsimp &optional simplex-dimension)
  "Return a list of the lengths of each simplex array in DSIMP."
  (mapcar (lambda (v) (length (nth v dsimp))) (mjr_dsimp_simplex-indexes dsimp simplex-dimension)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_get-simplex-array (dsimp simplex-dimension)
  "Return the simplex array for simplices of dimension SIMPLEX-DIMENSION"
  (cond ((> 0 simplex-dimension)   (error "mjr_dsimp_get-simplex-array: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)   (error "mjr_dsimp_get-simplex-array: simplex-dimension must be less than 4!")))
  (nth (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp)) dsimp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_get-data-array (dsimp data-index-or-name &optional simplex-dimension)
  "Return the requested data array.  NIL if no such data-array.  Return may not be a new object.

DATA-INDEX-OR-NAME may be:
  - string  -- the name of the data
  - integer -- the index of the data-array (SIMPLEX-DIMENSION must be provided)
  - list    -- a list containing the data-index and simplex-dimension (SIMPLEX-DIMENSION must NOT be provided)"
  (typecase data-index-or-name
    (integer (cond ((null simplex-dimension)  (error "mjr_dsimp_get-data-array: simplex-dimension must be provided with a data-index!"))
                   ((> 0 simplex-dimension)   (error "mjr_dsimp_get-data-array: simplex-dimension must be positive!"))
                   ((< 4 simplex-dimension)   (error "mjr_dsimp_get-data-array: simplex-dimension must be less than 4!")))
             (if (and (<  0 (mjr_dsimp_data-count dsimp simplex-dimension))
                      (>=  data-index-or-name 0)
                      (<  data-index-or-name (mjr_dsimp_data-count dsimp simplex-dimension)))
                 (nth (+ 2 (* 2 data-index-or-name) (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp))) dsimp)))
    (string (loop for smp-idx in (mjr_dsimp_simplex-indexes dsimp)
                  for num-dat in (mjr_dsimp_data-count dsimp)
                  for si from 0
                  do (loop for di from 0 upto (1- num-dat)
                           for dat-idx = (+ smp-idx 2) then (+ dat-idx 2)
                           when (string-equal data-index-or-name (mjr_annot_get-value :ano-nam (nth (1- dat-idx) dsimp)))
                           do (return-from mjr_dsimp_get-data-array (nth dat-idx dsimp)))))
    (list  (if simplex-dimension
               (error "mjr_dsimp_get-data-array: SIMPLEX-DIMENSION must not be provided if DATA-INDEX-OR-NAME is an data-index/simplex-dimension pair!")
               (mjr_dsimp_get-data-array dsimp (first data-index-or-name) (second data-index-or-name))))
    (otherwise  (error "mjr_dsimp_get-data-array: DATA-INDEX-OR-NAME must be a string or integer!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_get-data-ano (dsimp data-index-or-name &optional ano-key simplex-dimension )
  "Return the annotation for the data-index-or-name'th data set for simplices of dimension SIMPLEX-DIMENSION

ano-key should be one of :ano-nam, :ano-typ, or :ano-units.

DATA-INDEX-OR-NAME may be:
  - string  -- the name of the data
  - integer -- the index of the data-array (SIMPLEX-DIMENSION must be provided)
  - list    -- a list containing the data-index and simplex-dimension (SIMPLEX-DIMENSION must NOT be provided)"
  (if ano-key (mjr_annot_check-ano-key ano-key))
  (let ((anno-alist (typecase data-index-or-name
                      (integer (cond ((not simplex-dimension)  (error "mjr_dsimp_get-data-ano: when data-index-or-name is an index, simplex-dimension must be provided!"))
                                     ((> 0 simplex-dimension)  (error "mjr_dsimp_get-data-ano: simplex-dimension must be positive!"))
                                     ((< 4 simplex-dimension)  (error "mjr_dsimp_get-data-ano: simplex-dimension must be less than 4!")))
                               (nth (+ 1 (* 2 data-index-or-name) (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp))) dsimp))
                      (string  (loop for smp-idx in (mjr_dsimp_simplex-indexes dsimp)
                                     for num-dat in (mjr_dsimp_data-count dsimp)
                                     for si from 0
                                     for fn = (loop for di from 0 upto (1- num-dat)
                                                    for dat-idx = (+ smp-idx 2) then (+ dat-idx 2)
                                                    when (string-equal data-index-or-name (mjr_annot_get-value :ano-nam (nth (1- dat-idx) dsimp)))
                                                    do (return (nth (1- dat-idx) dsimp)))
                                     do (if fn (return fn))))
                      (list  (if simplex-dimension
                                 (error "mjr_dsimp_get-data-ano: SIMPLEX-DIMENSION must not be provided if DATA-INDEX-OR-NAME is an data-index/simplex-dimension pair!")
                               (print (nth (+ 1 (* 2 (first data-index-or-name)) (nth (second data-index-or-name) (mjr_dsimp_simplex-indexes dsimp))) dsimp))))
                      (otherwise  (error "mjr_dsimp_get-data-ano: DATA-INDEX-OR-NAME must be a string or integer!")))))
    (if ano-key
        (mjr_annot_get-value ano-key anno-alist)
        anno-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_map (dsimp fun data simplex-dimension)
  "Evaluate fun on the data-arrays specified in DATA (by index).

DATA may be a list of indexes or a single integer index.  An index of -1 (simplex-dimension is 0) means the 0-simplices themselves -- each 0-simplex will be
passed to FUN as a 3 element vector."
  (let ((data (mjr_util_non-list-then-list data)))
    (cond ((null data)               (error "mjr_dsimp_map: DATA must be non-NIL"))
          ((> 0 simplex-dimension)   (error "mjr_dsimp_map: simplex-dimension must be positive!"))
          ((< 4 simplex-dimension)   (error "mjr_dsimp_map: simplex-dimension must be less than 4!")))
    (flet ((get-dat (data-index)
             (if (> 0 data-index)
                 (if (= 0 simplex-dimension)
                     (mjr_dsimp_get-simplex-array dsimp simplex-dimension)
                     (error "mjr_dsimp_map: Data index of -1 only valid when simplex-dimension is 0"))
                 (mjr_dsimp_get-data-array dsimp data-index simplex-dimension))))
      (apply #'map 'vector fun (mapcar #'get-dat data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_add-simplex-array (dsimp simplex-array simplex-dimension)
  "Add the simplex-array, possibly overwriting an existing array, to dsimp.  Return dsimp."
  (cond ((> 0 simplex-dimension)   (error "mjr_dsimp_add-simplex-array: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)   (error "mjr_dsimp_add-simplex-array: simplex-dimension must be less than 4!")))
  (setf (nth (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp)) dsimp) simplex-array)
  dsimp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_add-data (dsimp data-array simplex-dimension &key ano-nam ano-typ ano-units)
  "Add the data-array to the dsimp list.

ANO-TYP defaults to :ANO-TYP-REAL if NIL."
  (cond ((mjr_dsimp_get-data-array dsimp ano-nam) (error "mjr_dsimp_add-data: Duplicate :ano-nam!"))
        ((> 0 simplex-dimension)                  (error "mjr_dsimp_add-data: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)                  (error "mjr_dsimp_add-data: simplex-dimension must be less than 4!")))
  (let ((idx (+ 2 (* 2 (1- (mjr_dsimp_data-count dsimp simplex-dimension))) (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp))))
        (met (mjr_annot_make-alist ano-nam (or ano-typ :ano-typ-real) ano-units)))
    (push met        (cdr (nthcdr idx      dsimp)))
    (push data-array (cdr (nthcdr (1+ idx) dsimp))))
  (incf (nth simplex-dimension (nth 0 dsimp)))
  dsimp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_add-data-from-map (dsimp fun data simplex-dimension &key ano-nam ano-typ)
  "Use MJR_DSIMP_MAP and MJR_DSIMP_ADD-DATA to add data to data frame."
  (mjr_dsimp_add-data dsimp
                      (mjr_dsimp_map dsimp fun data simplex-dimension)
                      simplex-dimension
                      :ano-nam ano-nam :ano-typ ano-typ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_convert-points-do-data (dsimp x-nam y-nam z-nam)
  "Add scalar data sets corresponding to the x, y, and/or z components of the 0-simplices in dsimp

x-nam, y-nam, and z-nam should be set to strings for the names of each scalar data set.  A coordinate may be suppressed by
setting the corresponding name to NIL."
  (if x-nam
      (mjr_dsimp_add-data-from-map dsimp (lambda (v) (aref v 0)) -1 0 :ano-nam x-nam :ano-typ :ano-typ-real))
  (if y-nam
      (mjr_dsimp_add-data-from-map dsimp (lambda (v) (aref v 1)) -1 0 :ano-nam y-nam :ano-typ :ano-typ-real))
  (if z-nam
      (mjr_dsimp_add-data-from-map dsimp (lambda (v) (aref v 2)) -1 0 :ano-nam z-nam :ano-typ :ano-typ-real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_connect-points (dsimp &key connect-closed)
  "Add 1-simplices, replacing any that might already exist, connecting the points together.  Return dsimp.

If CONNECT-CLOSED is non-NIL, then the first and last points will be connected."
  (mjr_dsimp_add-simplex-array dsimp
                               (let* ((npts (car (mjr_dsimp_simplex-count dsimp 0)))
                                      (new-lines (make-array (if connect-closed npts (1- npts)))))
                                 (loop for ci from 1 upto (length new-lines)
                                       for li from 0
                                       do (setf (aref new-lines li) (vector li (mod ci npts))))
                                 new-lines)
                                1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_make-from-xyz (&key xdat ydat zdat connect-points connect-closed data-vectors data-vector-names data-vector-types)
  "Construct a dsimp list from vectors of point components and 0-simplex data.

At least one of them must be a vector of numbers.  Each non-nil xdat, ydat, and zdat should be a vector of the same length.  These will be the x, y, and z
components of the new points element in the new dsimp.

When connect-points is non-NIL, the resulting DSIMP will contain 1-simplices connecting the points -- see MJR_DSIMP_CONNECT-POINTS.  When CONNECT-POINTS is
non-NIL, CONNECT-CLOSED is passed to MJR_DSIMP_CONNECT-POINTS.

DATA-VECTORS is a vector or list of vectors.  DATA-VECTOR-NAMES is a name or list of names for each vector in DATA-VECTORS.  Similarly DATA-VECTOR-TYPES is
a :ano-typ or a list of same.

Note that the elements of DATA-VECTORS are referenced in the new DSIMP -- no copies are made."
  (let* ((npts              (or (and xdat (length xdat))
                                (and ydat (length ydat))
                                (and zdat (length zdat))))
         (new-dsimp         (list (list 0 0 0 0)
                                  (let ((new-points (make-array npts)))
                                    (dotimes (i npts new-points)
                                      (setf (aref new-points i) (vector (if xdat (aref xdat i) 0)
                                                                        (if ydat (aref ydat i) 0)
                                                                        (if zdat (aref zdat i) 0)))))
                                  nil
                                  nil
                                  nil)))
    (if connect-points
        (mjr_dsimp_connect-points new-dsimp :connect-closed connect-closed))
    (if data-vectors
        (let ((data-vectors      (mjr_util_non-list-then-list data-vectors))
              (data-vector-names (mjr_util_non-list-then-list data-vector-names))
              (data-vector-types (mjr_util_non-list-then-list data-vector-types)))
          (loop for cur-dat-vec in data-vectors
                for cur-dat-nam in data-vector-names
                for di from 0
                do (mjr_dsimp_add-data new-dsimp cur-dat-vec 0 :ano-nam cur-dat-nam :ano-typ (nth di data-vector-types)))))
    new-dsimp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_make-from-points (points &key connect-points connect-closed (point-columns '(0 1 2)) data-columns data-column-names data-column-types)
  "Return a new DSIMP list having 0-simplices taken from POINTS.

POINTS may be a list of point vectors, a vector of point vectors, or a 2D array.  The 0-simplex array will be a new object even when POINTS is a vector of
vectors.  The contents of POINTS is not validated -- it is simply copied and stuffed into a DSIMP object.

POINT-COLUMNS is used if POINTS is a 2D array, in which case it is the columns to use for the points.  If points has fewer than three elements, the trailing
vector elements will be zero -- i.e. if you provide one integer, then the y and z components of each point will be zero.  If point-columns is just one column,
it may be an integer; otherwise it must be a list.  Similarly DATA-COLUMNS is used to specify columns that will be used as 0-simplex data arrays -- meta data
for the arrays comes from DATA-COLUMN-NAMES and DATA-COLUMN-TYPES.  If only one data column is used, then these last three arguments may be individual data
elements (an integer, string, and :ano-typ); otherwise, they must be lists.

When CONNECT-POINTS is non-NIL, the resulting DSIMP will contain 1-simplices connecting the points -- see MJR_DSIMP_CONNECT-POINTS.
When CONNECT-POINTS is non-NIL, CONNECT-CLOSED is passed to MJR_DSIMP_CONNECT-POINTS."
  (let* ((new-points (typecase points
                      (list      (make-array (length points) :initial-contents points))
                      (array     (case (array-rank points)
                                   (0         (error "mjr_dsimp_make-from-points: If an array, POINTS must have non-zero rank!"))
                                   (1         (copy-seq points))
                                   (2         (let* ((inrows        (array-dimension points 0))
                                                     (new-points    (make-array inrows))
                                                     (point-columns (subseq (append (mjr_util_non-list-then-list point-columns) (list -1 -1 -1)) 0 3)))
                                                (dotimes (i inrows new-points)
                                                  (setf (aref new-points i)
                                                        (map 'vector (lambda (j) (if (array-in-bounds-p points i j) (aref points i j) 0)) point-columns)))))
                                   (otherwise (error "mjr_dsimp_make-from-points: If an array, POINTS must be of rank 1 or 2!"))))
                      (otherwise (error "mjr_dsimp_make-from-points: POINTS must be a list or array!"))))
        (new-dsimp  (list (list 0 0 0 0)
                          new-points
                          nil
                          nil
                          nil)))
    (if (zerop (length new-points))
        (error "mjr_dsimp_make-from-points: POINTS was empty!"))
    (if connect-points
        (mjr_dsimp_connect-points new-dsimp :connect-closed connect-closed))
    (if data-columns
        (let ((data-columns      (mjr_util_non-list-then-list data-columns))
              (data-column-names (mjr_util_non-list-then-list data-column-names))
              (data-column-types (mjr_util_non-list-then-list data-column-types)))
          (loop for cur-dat-col in data-columns
                for cur-dat-nam in data-column-names
                for di from 0
                do (let ((new-data (make-array (array-dimension points 0))))
                     (dotimes (i (array-dimension points 0))
                       (setf (aref new-data i) (aref points i cur-dat-col)))
                     (mjr_dsimp_add-data new-dsimp new-data 0 :ano-nam cur-dat-nam :ano-typ (nth di data-column-types))))))
    new-dsimp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_make-from-dquad (dquad domain range &key data
                                                       u-close v-close
                                                       (surface-grid '(:x :y :d)) (surface-poly 't) (curve-line 't) (surface-orient 't) (surface-tlbr 't) (surface-normals 't)
                                                       (surface-normal-name "normals")
                                                       (domain-data-names '("u" "v")) (range-data-names  '("x" "y" "z")))
  "Create a dsimp list from a dquad list.  Note that new dsimp may contain references to arrays in dquad!

The idea is to create curves and surfaces based on data contained in a DQUAD list.  The primary use cases:
  - Create geometry for tools that may have poor support for the type of gridded data being used
    Ex: Some tools do not know how to draw a curve from a 1D vector field of 3D vectors
  - Save time using tools that require extra steps to visualize gridded data.
    Ex: Some tools may require extra steps to view a 2D scalar field as a surface.  As concrete examples, VisIT requires one to
        'Elevate' the data set while ParaView requires one to use the 'Warp Scalar' filter.
  - Generate geometry for 3D geometric rendering tools (ray-trace tools like Povray are good examples)

Objects:
     doamin range                            ----------Points---------- domain  range
     rank   rank  Representation             x        y        z        data    data         simplices
     1      1     Rectilinear curve          u        f(u)     0        u       f            1-simplices
     2      1     Rectilinear Surface in 3D  u        v        f(u,v)   u v     f            2-simplices & 1-simplices
     1      2     Parametric Curve in 2D     f_x(u)   f_y(u)   0        u       f_x f_y      1-simplices
     1      3     Parametric Curve in 3D     f_x(u)   f_y(u)   f_z(u)   u       f_x f_y f_z  1-simplices
     2      3     Parametric Surface in 3D   f_x(u,v) f_y(u,v) f_z(u,v) u v     f_x f_y f_z  2-simplices & 1-simplices

Arguments:
  - General arguments:
    - DATA .................. The data sets to include
                               - Directly specify data sets with an Integer/String or list of same
                               - All data sets with 't
                               - NIL no data sets
    - RANGE ................. The data set to use for f
    - DOMAIN ................ axis-index, axis-name, or list of same In what follows, we call the first axis in the DOMAIN 'u' and the second one 'v'.
    - DOMAIN-DATA-NAMES ..... The :ano-nam values used for the DOMAIN data placed in the new DQUAD list.  Setting a name to NIL prevent the corresponding data
                              set from being included in the new DQUAD list.
    - RANGE-DATA-NAMES ...... The :ano-nam values used for the RANGE data placed in the new DQUAD list.  Setting a name to NIL prevent the corresponding data
                              set from being included in the new DQUAD list.
    - U-CLOSE ............... The curve/surface is 'closed' in the u-direction (first component of domain)
    - V-CLOSE ............... the surface is 'closed' in the v-direction (second component of domain)
  - Curve arguments
    - CURVE-LINE ............ non-NIL to create line segments for a curve
  - surface arguments
    - SURFACE-POLY .......... non-NIL to create surface triangle patches for a surface
    - SURFACE-NORMALS ....... non-NIL to create surface normals at each point -- average normal vector across all triangles containing each point.  Data set
                              is named via the SURFACE-NORMAL-NAME argument.
    - SURFACE-GRID .......... A symbol, or list of same, indicating which 1-simplices to create for a surface
                                 - :u -- in the u direction
                                 - :v -- in the v direction
                                 - :d -- on the diagonal (the 'manufactured' edge across each quad cell)
    - SURFACE-ORIENT ........ non-NIL and triangle points will all be ordered clockwise, otherwise they alternate so that edges all go in the same direction
    - SURFACE-TLBR .......... non-NIL and triangles break quads top left to bottom right

                                   ------------------------------------
                                   |                |                 |
                                   | 00 *-----* 01  |  00 *-----* 01  |
                                   |    |    /|     |     |\    |     |
                                   |    |   / |     |     | \   |     |
                                   |    |  /  |     |     |  \  |     |
                                   |    | /   |     |     |   \ |     |
                                   |    |/    |     |     |    \|     |
                                   | 10 *-----* 11  |  10 *-----* 11  |
                                   |                |                 |
                                   ------------------------------------------------------------------------
                                   |   00 01 10     |    00 01 11     | Clockwise around | SURFACE-ORIENT |
                                   |   10 01 11     |    00 11 10     |  triangles       |      'T        |
                                   ------------------------------------------------------------------------
                                   |   00 01 10     |    00 01 11     | Compatible edge  | SURFACE-ORIENT |
                                   |   10 11 01     |    00 10 11     |  directions      |      NIL       |
                                   ------------------------------------------------------------------------
                                   |   Top Right    |    Top Left     |
                                   |      to        |       to        |   1-simplices always go in the
                                   |  Bottom Left   |   Bottom Right  |   direction of increasing x
                                   ------------------------------------   values for :x & :d, and
                                   |  SURFACE-TLBR  |   SURFACE-TLBR  |   increasing y values for :y
                                   |       NIL      |        'T       |
                                   ------------------------------------"

  (cond ((and surface-normals (not surface-poly)) (error "mjr_dsimp_make-from-dquad: non-NIL SURFACE-NORMALS requires non-NIL SURFACE-POLY"))
        )

  (let* ((dom   (mapcar (lambda (x) (mjr_dquad_get-axis-vector dquad x))
                        (or (mjr_util_non-list-then-list domain)
                            (concatenate 'list (mjr_vvec_to-vec (mjr_dquad_axis-count dquad))))))
         (rng   (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x))
                        (or (mjr_util_non-list-then-list range)
                            (concatenate 'list (mjr_vvec_to-vec (mjr_dquad_data-count dquad))))))
         (dom0  (first  dom))
         (dom1  (second dom))
         (rng0  (first  rng))
         (rng1  (second rng))
         (rng2  (third  rng))
         (data  (typecase data
                  (integer   (list data))
                  (string    (list data))
                  (list      data)
                  (otherwise (let ((dciq  (mjr_dquad_data-count dquad)))
                               (if (not (zerop dciq)) (concatenate 'list (mjr_vvec_to-vec dciq)))))))
         (ddnam (mjr_util_non-list-then-list domain-data-names))
         (rdnam (mjr_util_non-list-then-list range-data-names)))

    ;; Step 1: Create functions that will spit out u/v data as required.
    (destructuring-bind (u u-len v v-len dom-dim)
        (case (length dom)
          (1   (typecase (aref dom0 0)
                 (vector    (list          (lambda (i) (aref (aref dom0 i) 0)) (length dom0) (lambda (i) (aref (aref dom0 i) 1)) (length dom0) 2))
                 (otherwise (list          (lambda (i) (aref dom0 i))          (length dom0) nil                                 nil           1))))
          (2   (list                       (lambda (i) (aref dom0 i))          (length dom0) (lambda (i) (aref dom1 i))          (length dom1) 2))
          (otherwise (error "mjr_dsimp_make-from-dquad: Domain must be 1 or 2 dimensional")))

      ;; Step 2: Create functions that will spit out fx/fy/fz data as required.
      (destructuring-bind (fx fy fz rng-dim)
          (case dom-dim
            (1     (case (length rng)
                     (1   (case (if (vectorp (aref rng0 0))
                                    (length (aref rng0 0))
                                    -1)
                            (-1        (list (lambda (i) (funcall u i))            (lambda (i) (aref rng0 i))          (lambda (i) (declare (ignore i)) 0) 1))
                            (1         (list (lambda (i) (funcall u i))            (lambda (i) (aref (aref rng0 i) 0)) (lambda (i) (declare (ignore i)) 0) 1))
                            (2         (list (lambda (i) (aref (aref rng0 i) 0))   (lambda (i) (aref (aref rng0 i) 1)) (lambda (i) (declare (ignore i)) 0) 2))
                            (3         (list (lambda (i) (aref (aref rng0 i) 0))   (lambda (i) (aref (aref rng0 i) 1)) (lambda (i) (aref (aref rng0 i) 2)) 3))
                            (otherwise (error "mjr_dsimp_make-from-dquad: Range must be 1, 2, or 3 dimensional when domain is 1 dimensional"))))
                     (2         (list        (lambda (i) (aref rng0 i))            (lambda (i) (aref rng1 i))          (lambda (i) (declare (ignore i)) 0) 2))
                     (3         (list        (lambda (i) (aref rng0 i))            (lambda (i) (aref rng1 i))          (lambda (i) (aref rng2 i))          3))
                     (otherwise (error "mjr_dsimp_make-from-dquad: Range must be 1, 2, or 3 dimensional when domain is 1 dimensional"))))
            (2       (case (length rng)
                       (1         (case (if (vectorp (aref rng0 0 0))
                                            (length (aref rng0 0 0))
                                            -1)
                                    (-1        (list (lambda (i j) (declare (ignore j)) (funcall u i))   (lambda (i j) (declare (ignore i)) (funcall v j)) (lambda (i j) (aref rng0 i j))          1))
                                    (1         (list (lambda (i j) (declare (ignore j)) (funcall u i))   (lambda (i j) (declare (ignore i)) (funcall v j)) (lambda (i j) (aref (aref rng0 i j) 0)) 1))
                                    (3         (list (lambda (i j) (aref (aref rng0 i j) 0))             (lambda (i j) (aref (aref rng0 i j) 1))           (lambda (i j) (aref (aref rng0 i j) 2)) 3))
                                    (otherwise (error "mjr_dsimp_make-from-dquad: Range must be 1 or 3 dimensional when domain is 2 dimensional"))))
                       (3         (list              (lambda (i j) (aref rng0 i j))                      (lambda (i j) (aref rng1 i j))                    (lambda (i j) (aref rng2 i j))          3))
                       (otherwise (error "mjr_dsimp_make-from-dquad: Range must be 1 or 3 dimensional dimensional when domain is 2 dimensional"))))
            (otherwise (error "mjr_dsimp_make-from-dquad: Domain must be 1 or 2 dimensional")))

        (declare (ignore rng-dim))
        ;; Step 3: Create the new dsimp list
        (case dom-dim
          (1  (let ((points (make-array u-len))
                    (u-data (make-array u-len)))
                ;; Populate the points & u-data
                (loop for i from 0 upto (1- u-len)
                      do (setf (aref points i) (vector (funcall fx i) (funcall fy i) (funcall fz i)))
                      do (setf (aref u-data i) (funcall u i)))
                ;; Create our new dsimp list
                (let ((new-dsimp (mjr_dsimp_make-from-points points)))
                  ;; Add the u-data
                  (if (first ddnam)
                      (mjr_dsimp_add-data new-dsimp u-data 0 :ANO-NAM (first ddnam) :ano-typ :ano-typ-real))
                  ;; Add the lines for curve
                  (if curve-line
                      (let ((new-lines (make-array (if u-close u-len (1- u-len)))))
                        (loop for ci from 1 upto (length new-lines)
                              for li from 0
                              do (setf (aref new-lines li) (vector li (mod ci u-len))))
                        (mjr_dsimp_add-simplex-array new-dsimp new-lines 1)))
                  ;; Add data
                  (dolist (cur-dat data)
                    (mjr_dsimp_add-data new-dsimp
                                        (mjr_dquad_get-data-array dquad cur-dat)
                                        0
                                        :ano-nam        (mjr_dquad_get-data-ano dquad cur-dat :ano-nam)
                                        :ano-typ        (mjr_dquad_get-data-ano dquad cur-dat :ano-typ)))
                  ;; Add range data
                  (apply #'mjr_dsimp_convert-points-do-data new-dsimp rdnam)
                  new-dsimp)))
          (2  (let* ((plen   (* u-len v-len))
                     (points (make-array plen))
                     (u-data (make-array plen))
                     (v-data (make-array plen)))
                ;; Populate points, u-data and v-data
                (loop with k = -1
                      for i from 0 upto (1- u-len)
                      do (loop for j from 0 upto (1- v-len)
                               do (setf (aref points (incf k)) (vector (funcall fx i j) (funcall fy i j) (funcall fz i j)))
                               do (setf (aref u-data k) (funcall u i))
                               do (setf (aref v-data k) (funcall v j))))
                ;; Create our new dsimp list
                (let ((new-dsimp (mjr_dsimp_make-from-points points)))
                  ;; Add domain-data
                  (if (first ddnam)
                      (mjr_dsimp_add-data new-dsimp u-data 0 :ANO-NAM (first ddnam) :ano-typ :ano-typ-real))
                  (if (second ddnam)
                      (mjr_dsimp_add-data new-dsimp v-data 0 :ANO-NAM (second ddnam) :ano-typ :ano-typ-real))
                  ;; Add the simplices
                  (let* ((nx (if u-close (1+ u-len) u-len))
                         (ny (if v-close (1+ v-len) v-len)))
                    ;; Add the surface triangles
                    (if surface-poly
                        (let ((new-tris (make-array (* 2 (1- nx) (1- ny))))
                              (k -1))
                          (dotimes (xi (1- nx))
                            (dotimes (yi (1- ny))
                              (let ((p00 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      yi))
                                    (p01 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      (1+ yi)))
                                    (p11 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) (1+ xi) (1+ yi)))
                                    (p10 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) (1+ xi) yi)))
                                (if surface-orient
                                    (if surface-tlbr
                                        (progn (setf (aref new-tris (incf k)) (vector p00 p01 p11))
                                               (setf (aref new-tris (incf k)) (vector p00 p11 p10)))
                                        (progn (setf (aref new-tris (incf k)) (vector p00 p01 p10))
                                               (setf (aref new-tris (incf k)) (vector p10 p01 p11))))
                                    (if surface-tlbr
                                        (progn (setf (aref new-tris (incf k)) (vector p00 p01 p11))
                                               (setf (aref new-tris (incf k)) (vector p00 p10 p11)))
                                        (progn (setf (aref new-tris (incf k)) (vector p00 p01 p10))
                                               (setf (aref new-tris (incf k)) (vector p10 p11 p01))))))))
                          (mjr_dsimp_add-simplex-array new-dsimp new-tris 2)
                          ;; Add the surface normals
                          (if surface-normals
                              (let ((new-norms (make-array plen))
                                    (pt-2-tri  (make-array plen :initial-element nil))
                                    (tri-norm  (make-array (* 2 (1- nx) (1- ny)))))
                                (loop for tri-t across new-tris
                                      for tri-i from 0
                                      do (push tri-i (aref pt-2-tri (aref tri-t 0)))
                                      do (push tri-i (aref pt-2-tri (aref tri-t 1)))
                                      do (push tri-i (aref pt-2-tri (aref tri-t 2)))
                                      do (setf (aref tri-norm tri-i) (mjr_vec_normalize (mjr_geom_triangle-normal (map 'list (lambda (i) (aref points i)) tri-t)))))
                                (loop for tri-lst across pt-2-tri
                                      for pnt-idx from 0
                                      do (setf (aref new-norms pnt-idx) (mjr_vec_/ (apply #'mjr_vec_+ (mapcar (lambda (i) (aref tri-norm i)) tri-lst)) (length tri-lst))))
                                (mjr_dsimp_add-data new-dsimp new-norms 0 :ano-nam surface-normal-name :ano-typ :ano-typ-rvec)))))
                    ;; Add the surface lines
                    (if surface-grid
                        (let ((k -1)
                              (new-lines (make-array (+ (if (member :x surface-grid) (* nx      (1- ny))       0)
                                                        (if (member :y surface-grid) (* (1- nx) ny)            0)
                                                        (if (member :d surface-grid) (* (1- nx) (1- ny))       0)))))
                          (if (member :x surface-grid)
                              (dotimes (xi nx)
                                (dotimes (yi  (1- ny))
                                  (let ((p00 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      yi))
                                        (p01 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      (1+ yi))))
                                    (setf (aref new-lines (incf k)) (vector p00 p01))))))
                          (if (member :y surface-grid)
                              (dotimes (yi ny)
                                (dotimes (xi (1- nx))
                                  (let ((p00 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      yi))
                                        (p10 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) (1+ xi) yi)))
                                    (setf (aref new-lines (incf k)) (vector p00 p10))))))
                          (if (member :d surface-grid)
                              (if surface-tlbr
                                  (loop for i from (- (- ny 2)) upto (- nx 2)
                                        for ix = (max 0 i)
                                        for iy = (if (>= i 0) 0 (abs i))
                                        do (loop for xi from ix upto (- nx 2)
                                                 for yi from iy upto (- ny 2)
                                                 do (let ((p00 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      yi))
                                                          (p11 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) (1+ xi) (1+ yi))))
                                                      (setf (aref new-lines (incf k)) (vector p00 p11)))))
                                  (loop for i from (- (- ny 2)) upto (- nx 2)
                                        for ix = (max 0 i)
                                        for iy = (if (>= i 0) (- ny 2) (+ i (- ny 2)))
                                        do (loop for xi from ix upto (- nx 2)
                                                 for yi downfrom iy downto 0
                                                 do (let ((p01 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) xi      (1+ yi)))
                                                          (p10 (mjr_arr_subscripts-mod-to-row-major-subscript (list u-len v-len) (1+ xi) yi)))
                                                      (setf (aref new-lines (incf k)) (vector p01 p10)))))))
                          (mjr_dsimp_add-simplex-array new-dsimp new-lines 1)))
                    ;; Add the data sets
                    (dolist (cur-dat data)
                      (let ((tmp-data (make-array plen))
                            (src-data (mjr_dquad_get-data-array dquad cur-dat)))
                        (loop with k = -1
                              for i from 0 upto (1- u-len)
                              do (loop for j from 0 upto (1- v-len)
                                       do (setf (aref tmp-data (incf k)) (aref src-data i j))))
                        (mjr_dsimp_add-data new-dsimp
                                            tmp-data
                                            0
                                            :ano-nam        (mjr_dquad_get-data-ano dquad cur-dat :ano-nam)
                                            :ano-typ        (mjr_dquad_get-data-ano dquad cur-dat :ano-typ))))
                    ;; Add range data
                    (apply #'mjr_dsimp_convert-points-do-data new-dsimp rdnam)
                    new-dsimp)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_dump2d-ply-file (out-file dsimp &key color-data)
  "Generate a Polygon File Format (PLY) file with 2-simplices with optional vertex color data from dsimp.

The use primary use case is to easily get a triangulation into more artistically oriented 3D modeling tools like Blender and meshlab.
This function is also handy for quickly looking at a surface with 3D object viewers like mview before loading into more cumbersome tools.

See the :POV and :VTK packages for more sophisticated rendering options and data export options respectively.

Note this format is sometimes called the 'Stanford Triangle Format'.  See: https://en.wikipedia.org/wiki/PLY_%28file_format%29

The :COLOR-DATA argument is a 0-simplex COLOR data set (a dataset name or list containing the dataset simplex dimension and dataset index).  This
dataset defines colors for vertexes of 2-simplices.  Note meshlab supports vertex color, but many tools do not (ParaView & Blender)

  - Simple surface dump to ply file

      (mjr_dsimp_dump2d-ply-file \"surf.ply\"
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
          (mjr_dsimp_dump2d-ply-file \"surf.ply\" a-dsimp :color-data \"col\")))"
  (let* ((pnts (mjr_dsimp_get-simplex-array dsimp 0))
         (tris (mjr_dsimp_get-simplex-array dsimp 2))
         (npts (length pnts))
         (ntri (length tris)))
    (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format dest "ply~%")
      (format dest "format ascii 1.0~%")
      (format dest "comment author: Mitch Richling~%")
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
                do (format dest "~f ~f ~f~%" (aref pnt 0) (aref pnt 1)(aref pnt 2))))
      (loop for  tri across tris
            do (format dest "3 ~d ~d ~d~%" (aref tri 0) (aref tri 1) (aref tri 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_dsimp_dump2d-obj-file (out-file dsimp)
  "Generate an OBJ file 2-simplices from dsimp.

The use primary use case is to easily get a triangulation into more artistically oriented 3D modeling tools like Blender and meshlab.
This function is also handy for quickly looking at a surface with 3D object viewers like g3dviewer before loading into more cumbersome tools.

See the :POV and :VTK packages for more sophisticated rendering options and data export options respectively.

Typical example of use:

  (mjr_dsimp_dump2d-obj-file \"surf.obj\"
                             (mjr_fsamp_ds-func-r123-r123 (lambda (x y) (* .5 (abs (- (expt (complex x y) 3) 1))))
                                                          :xdat '(:start -1.1 :end 1.1 :len 50)
                                                          :ydat '(:start -1.1 :end 1.1 :len 50)
                                                          :arg-mode :arg-number))"
  (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for pnt across (mjr_dsimp_get-simplex-array dsimp 0)
          do (format dest "v ~f ~f ~f~%" (aref pnt 0) (aref pnt 1) (aref pnt 2)))
    (loop for  tri across (mjr_dsimp_get-simplex-array dsimp 2)
          do (format dest "f ~d ~d ~d~%" (1+ (aref tri 0)) (1+ (aref tri 1)) (1+ (aref tri 2))))))
