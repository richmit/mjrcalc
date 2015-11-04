;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      pre-dsimp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995,2013 by Mitch Richling.  All rights reserved.
;; @brief     Data sets on SIMPlicial complexes.@EOL
;; @Keywords  lisp interactive math simplicial complexes data triangles triangulation
;; @Std       Common Lisp
;;
;;            This is what I want to do:
;;                * Create DSIMPs
;;                  * Create simplices from a function sampled on an adaptive triangulation (hard)
;;                * Modify existing triangulation
;;                  * Refine
;;                    * Ruppert triangle refine
;;                    * Stupid refinement for integration
;;                * Dump a DSIMP list to
;;                  * POVRay File
;;                  * VTK File
;;                  * Image File (?)
;;                  * GNUPlot PIPE
;;                * Create and add data sets
;;                  * Function on 0-simplices
;;                  * Function on N-simplices
;;                  * Colorize data and/or points
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_DSIMP
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_COMBC
        :MJR_DQUAD
        :MJR_UTIL
        :MJR_ARR
        :MJR_UTIL
        :MJR_ANNOT)
  (:DOCUMENTATION "Brief: Data sets on SIMPlicial complexes.;")
  (:EXPORT #:mjr_dsimp_help
           ;; Create dsimp lists
           #:mjr_dsimp_make-from-dquad             ;; TODO
           #:mjr_dsimp_make-from-func-rX-rX        ;; TODO/MAYBE
           #:mjr_dsimp_colorize                    ;; TODO/MAYBE
           #:mjr_dsimp_make-from-points            ;; DONE
           #:mjr_dsimp_make-from-xyz               ;; DONE
           ;; Create data arrays from dsimp lists
           #:mjr_dsimp_map                         ;; DONE
           #:mjr_dsimp_colorize                    ;; TODO
           ;; Add data data to a dsimp list
           #:mjr_dsimp_add-data-from-map           ;; DONE
           #:mjr_dsimp_add-data                    ;; DONE
           #:mjr_dsimp_add-simplex-array           ;; DONE
           #:mjr_dsimp_connect-points              ;; DONE
           ;; New dsimp lists from old ones
           #:mjr_dsimp_data-count                  ;; DONE
           #:mjr_dsimp_simplex-count               ;; DONE
           ;; Get size elements from a dsimp list
           #:mjr_dsimp_get-simplex-array           ;; DONE
           #:mjr_dsimp_get-data-array              ;; DONE   
           #:mjr_dsimp_get-data-attr               ;; DONE 
           ;; Persistence
           #:mjr_dsimp_read-from-file              ;; DONE
           #:mjr_dsimp_write-to-file               ;; DONE
           ))

(in-package :MJR_DSIMP)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_help ()
  "Help for MJR_DSIMP:

Library for dealing with triangulated (simplicial complex'ish) data sets.

The canonical example is a computer graphics: When drawing surfaces in 3D, it is common to approximate the surfaces with triangles.
For thsi application, the data payload for each vertex is frequently the actuall surface normal at that point.  The canonical
example from mechanical engenering is finite element analysis of machine componets.  Here the component is broken up into
tetrahedra, and each tetrahedra has some data associated with it (like density or temprature).

About 'triangles':

  A k-simplex is a k-dimensional polytope defined as the convex hull of its $k+1$ vertices.  The 0-simplex is a single point,
  1-simplex is a line segment, a 2-simplex is a triangle (with it's interior), the 3-simplex is a tetrahedron (with it's interior),
  and the 4-simplex is a 5-cell.  We shall use 'simplices' as the plural form of simplex.

I have chosen to implement the data storage as a simple list instead of a complex, opaque object -- the wisdom of that decision has
yet to be proven. :)

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

Today only simplices upto dimension 3 are supported, but that may change.  Frankly, only 2-simplices are well supported.

The N-data-meta objects are meta data alists as described by MJR_ANNOT_HELP.  Note that all meta data alists MUST contain both
the :ano-nam and :ano-typ attributes, and the :ano-nam values must be unique across ALL meta data lists.

Examples of some DSIMP lists may be found in tst-dsimp.lisp."
  (documentation 'mjr_dsimp_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_read-from-file (file-name)
  "Read a dsimp list from a file.

Data in the file after the first DSIMP list will be ignored.  This space is traditionally used for free text commentary regarding
the data set."
  (with-open-file (stream file-name)
    (read stream)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_write-to-file (dsimp file-name)
  "Write a dsimp list from a file"
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (null (write  dsimp :stream stream))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_data-count (dsimp &optional simplex-dimension)
  "Return the number of data elements for the simplices of dimension SIMPLEX-DIMENSION"
  (cond ((and simplex-dimension (> 0 simplex-dimension))   (error "mjr_dsimp_data-count: simplex-dimension must be positive!"))
        ((and simplex-dimension (< 4 simplex-dimension))   (error "mjr_dsimp_data-count: simplex-dimension must be less than 4!")))
  (if simplex-dimension
      (nth simplex-dimension (car dsimp))
      (car dsimp)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_simplex-indexes (dsimp &optional simplex-dimension)
  "Return a list of simplex indexes as a list."
  (let ((all-dims (loop with i = 1
                        for c in (mjr_dsimp_data-count dsimp)
                        collect i
                        do (incf i (+ 1 (* 2 c))))))
    (if simplex-dimension
        (mapcar (lambda (i) (nth i all-dims)) (mjr_util_non-list-then-list simplex-dimension))
        all-dims)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_simplex-count (dsimp &optional simplex-dimension)
  "Return a list of the lengths of each simplex array in DSIMP."
  (mapcar (lambda (v) (length (nth v dsimp))) (mjr_dsimp_simplex-indexes dsimp simplex-dimension)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_get-simplex-array (dsimp simplex-dimension)
  "Return the simplex array for simplices of dimension SIMPLEX-DIMENSION"
  (cond ((> 0 simplex-dimension)   (error "mjr_dsimp_get-simplex-array: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)   (error "mjr_dsimp_get-simplex-array: simplex-dimension must be less than 4!")))
  (nth (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp)) dsimp))

;;----------------------------------------------------------------------------------------------------------------------------------
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
                           when (string-equal data-index-or-name (cdr (assoc :ano-nam (nth (1- dat-idx) dsimp))))
                           do (return-from mjr_dsimp_get-data-array (nth dat-idx dsimp)))))
    (list  (if simplex-dimension
               (error "mjr_dsimp_get-data-array: SIMPLEX-DIMENSION must not be provided if DATA-INDEX-OR-NAME is an data-index/simplex-dimension pair!")
               (mjr_dsimp_get-data-array dsimp (first data-index-or-name) (second data-index-or-name))))
    (otherwise  (error "mjr_dsimp_get-data-array: DATA-INDEX-OR-NAME must be a string or integer!"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_get-data-attr (dsimp data-index-or-name attr-name &optional simplex-dimension )
  "Return the attribute for the data-index-or-name'th data set for simplices of dimension SIMPLEX-DIMENSION

ATTR-NAME should be one of :ano-nam, :ano-typ, or :ano-colorspace.

DATA-INDEX-OR-NAME may be:
  - string  -- the name of the data
  - integer -- the index of the data-array (SIMPLEX-DIMENSION must be provided)
  - list    -- a list containing the data-index and simplex-dimension (SIMPLEX-DIMENSION must NOT be provided)"
  (cond ((not (member attr-name '(:ano-nam :ano-colorspace :ano-typ))) (error "mjr_dsimp_get-data-attr: Invalid ATTR-NAME!")))
  (typecase data-index-or-name
    (integer (cond ((> 0 simplex-dimension)   (error "mjr_dsimp_get-data-attr: simplex-dimension must be positive!"))
                   ((< 4 simplex-dimension)   (error "mjr_dsimp_get-data-attr: simplex-dimension must be less than 4!")))
             (cdr (assoc attr-name (nth (+ 1 (* 2 data-index-or-name) (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp))) dsimp))))
    (string  (loop for smp-idx in (mjr_dsimp_simplex-indexes dsimp)
                   for num-dat in (mjr_dsimp_data-count dsimp)
                   for si from 0
                   do (loop for di from 0 upto (1- num-dat)
                            for dat-idx = (+ smp-idx 2) then (+ dat-idx 2)
                            when (string-equal data-index-or-name (cdr (assoc :ano-nam (nth (1- dat-idx) dsimp))))
                            do (return-from mjr_dsimp_get-data-attr (cdr (assoc attr-name (nth (1- dat-idx) dsimp)))))))
    (list  (if simplex-dimension
               (error "mjr_dsimp_get-data-attr: SIMPLEX-DIMENSION must not be provided if DATA-INDEX-OR-NAME is an data-index/simplex-dimension pair!")
               (mjr_dsimp_get-data-attr dsimp (first data-index-or-name) (second data-index-or-name))))
    (otherwise  (error "mjr_dsimp_get-data-attr: DATA-INDEX-OR-NAME must be a string or integer!"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_map (dsimp fun data simplex-dimension)
  "Evaluate fun on the data-arrays specified in DATA (by index).  

DATA may be a list of indexes or a single integer index.  An index of -1 (simplex-dimension is 0) means the 0-simplices themseves --
each 0-simplex will be passed to FUN as a 3 element vector."
  (let ((data (mjr_util_non-list-then-list data)))
    (cond ((null data)               (error "mjr_dsimp_map: DATA must be non-NIL"))
          ((> 0 simplex-dimension)   (error "mjr_dsimp_map: simplex-dimension must be positive!"))
          ((< 4 simplex-dimension)   (error "mjr_dsimp_map: simplex-dimension must be less than 4!")))
    (flet ((get-dat (data-index) 
             (if (> 0 data-index)
                 (if (= 0 simplex-dimension)
                     (mjr_dsimp_get-simplex-array dsimp simplex-dimension)
                     (error "mjr_dsimp_map: Data index of -1 only vlaid when simplex-dimension is 0"))
                 (mjr_dsimp_get-data-array dsimp data-index simplex-dimension))))
      (apply #'map 'vector fun (mapcar #'get-dat data)))))


;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_add-simplex-array (dsimp simplex-array simplex-dimension)
  "Add the simplex-array, possibly overwriting an existing array, to dsimp.  Return dsimp."
  (cond ((> 0 simplex-dimension)   (error "mjr_dsimp_add-simplex-array: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)   (error "mjr_dsimp_add-simplex-array:: simplex-dimension must be less than 4!")))
  (setf (nth (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp)) dsimp) simplex-array)
  dsimp)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_add-data (dsimp data-array simplex-dimension &key ano-nam ano-typ ano-colorspace) 
  ""
  (cond ((null ano-nam)                                                       (error "mjr_dsimp_add-data: :ano-nam must be non-NIL"))
        ((not (stringp ano-nam))                                              (error "mjr_dsimp_add-data: :ano-nam must be a string"))
        ((and ano-typ (not (member ano-typ '(:ano-typ-real :ano-typ-integer
                                           :ano-typ-complex :ano-typ-color  
                                           :ano-typ-ivec :ano-typ-rvec   
                                           :ano-typ-cvec))))                  (error "mjr_dsimp_add-data: Invalid ano-typ!"))
        ((and ano-colorspace (not (member ano-colorspace '(:cs-tru :cs-rgb 
                                                         :cs-hsv :cs-hsl)))) (error "mjr_dsimp_add-data: Invalid ano-typ!"))
        ((and (not (equalp ano-typ :ano-typ-color)) ano-colorspace)             (error "mjr_dsimp_add-data: :ano-colorspace invalid as :ano-typ is not :ano-typ-color!"))
        ((mjr_dsimp_get-data-array dsimp ano-nam)                             (error "mjr_dsimp_add-data: Duplicate :ano-nam!"))
        ((> 0 simplex-dimension)                                             (error "mjr_dsimp_add-data: simplex-dimension must be positive!"))
        ((< 4 simplex-dimension)                                             (error "mjr_dsimp_add-data: simplex-dimension must be less than 4!")))
  (let ((idx (+ 2 (* 2 (1- (mjr_dsimp_data-count dsimp simplex-dimension))) (nth simplex-dimension (mjr_dsimp_simplex-indexes dsimp))))
        (met (loop for da-tag in (list :ano-nam :ano-typ                  :ano-colorspace)
                   for da-val in (list ano-nam  (or ano-typ :ano-typ-real) ano-colorspace)
                   when da-val
                   collect (cons da-tag da-val))))
    (push met        (cdr (nthcdr idx      dsimp)))
    (push data-array (cdr (nthcdr (1+ idx) dsimp))))
  (incf (nth simplex-dimension (nth 0 dsimp)))
  dsimp)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_add-data-from-map (dsimp fun data simplex-dimension &key ano-nam ano-typ ano-colorspace) 
  ""
  (mjr_dsimp_add-data dsimp
                      (mjr_dsimp_map dsimp fun data simplex-dimension) 
                      simplex-dimension
                      :ano-nam ano-nam :ano-typ ano-typ :ano-colorspace ano-colorspace))

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_make-from-xyz (&key xdat ydat zdat connect-points connect-closed data-vectors data-vector-names data-vector-types)
  "Construct a dsimp list from vectors of point components and 0-simplex data.

At least one of them must be a vector of numbers.  Each non-nil xdat, ydat, and zdat should be a vector of the same length.
These will be the x, y, and z compoents of the new points elment in the new dsimp.

When connect-points is non-NIL, the resulting DSIMP will contain 1-simplices connecting the points -- see MJR_DSIMP_CONNECT-POINTS.
When CONNECT-POINTS is non-NIL, CONNECT-CLOSED is passed to MJR_DSIMP_CONNECT-POINTS.

DATA-VECTORS is a vector or list of vectors.  DATA-VECTOR-NAMES is a name or list of names for each vector in DATA-VECTORS.
Similarly DATA-VECTOR-TYPES is a :ano-typ or a list of same.

Note that the elements of DATA-VECTORS are refrenced in the new DSIMP -- no copies are made."
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_make-from-points (points &key connect-points connect-closed (point-columns '(0 1 2)) data-columns data-column-names data-column-types)
  "Return a new DSIMP list having 0-simplices taken from POINTS.

POINTS may be a list of point vectors, a vector of point vectors, or a 2D array.  The 0-simplex array will be a new object even when POINTS is a
vector of vectors.  The contents of POINTS is not validated -- it is simply copied and stuffed into a DSIMP object.

POINT-COLUMNS is used if POINTS is a 2D array, in which case it is the columns to use for the points.  If points has fewer than
three elements, the trailing vector elements will be zero -- i.e. if you provide one integer, then the y and z components of each
point will be zero.  If point-columns is just one column, it may be an integer; otherwise it must be a list.  Similarly
DATA-COLUMNS is used to specify columns that will be used as 0-simplex data arrays -- meta data for the arrays comes from
DATA-COLUMN-NAMES and DATA-COLUMN-TYPES.  If only one data column is used, then these last three arguments may be individual data
elements (an integer, string, and :ano-typ); otherwise, they must be lists.

When connect-points is non-NIL, the resulting DSIMP will contain 1-simplices connecting the points -- see MJR_DSIMP_CONNECT-POINTS.
When CONNECT-POINTS is non-NIL, CONNECT-CLOSED is passed to MJR_DSIMP_CONNECT-POINTS."
  (let* ((new-points (typecase points
                      (list      (make-array (length points) :initial-contents points))
                      (array     (case (array-rank points)
                                   (0         (error "mjr_dsimp_make-from-points: If an array, POINTS must have non-zero rank!"))
                                   (1         (copy-seq points))
                                   (2         (let ((new-points    (make-array (array-dimension points 0)))
                                                    (point-columns (loop with pc = (mjr_util_non-list-then-list point-columns)
                                                                         for i from 0 upto 2
                                                                         collect (nth i pc))))
                                                (dotimes (i (array-dimension points 0))
                                                  (setf (aref new-points i) (map 'vector (lambda (x) (if x (aref points i x) 0)) point-columns)))
                                                new-points))
                                   (otherwise (error "mjr_dsimp_make-from-points: If an array, POINTS must be of rank 1 or 2!"))))
                      (otherwise (error "mjr_dsimp_make-from-points: POINTS must be a list or array!"))))
        (new-dsimp  (list (list 0 0 0 0)
                          new-points
                          nil
                          nil
                          nil)))
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

;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_dsimp_make-from-func (dsimp ???
;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_dsimp_colorize (dsimp fun data simplex-dimension ???

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dsimp_make-from-dquad (dquad domain range &key data domain-data-names do-not-include-domain-data u-close v-close)
  "Create a dsimp list from a dquad list.  Note that new dsimp may contain refrences to arrays in dquad!

The idea is to create curves and surfaces data contained in a DQUAD list.  

The DOMAIN is an axis-index, axis-name, or list of same.  In the table below, the first element of the domain is the variable 'u'
whiel the second is the vairable 'v'.

DOMAIN-DATA-NAMES          -- The :ano-nam values used for the DOMAIN data placed in the new DQUAD list.
DO-NOT-INCLUDE-DOMAIN-DATA -- Set to non-NIL to suppress the inclusion of the domain-data in the new DQUAD list.
CLOSED-GEOMETRY            -- 't or nil or a list of same.

u-close -- the curve/surface is 'closed' in the u-direction (first compoent of domain)
v-close -- the surface is 'closed' in the v-direction (second compoent of domain)

i.e. Create polygonal data from a function sampled on a rectilinear grid.

     domain-rank range-rank Representation             ----------Points---------- domain-data simplices
                                                       x        y        z        
     1 u         1          Rectilinear curve          u        f(u)     N/A      u           1-simplices
     2 u v       1          Rectilinear Surface in 3D  u        v        f(u,v)   u v         2-simplices
     1 u         2          Parametric Curve in 2D     f_x(u)   f_y(u)   0        u           1-simplices
     1 u         3          Parametric Curve in 3D     f_x(u)   f_y(u)   f_z(u)   u           1-simplices
     2 u v       3          Parametric Surface in 3D   f_x(u,v) f_y(u,v) f_z(u,v) u v         2-simplices"

  (let* ((dom (mapcar (lambda (x) (mjr_dquad_get-axis-vector dquad x))
                      (or (mjr_util_non-list-then-list domain)
                          (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_axis-count dquad))))))
         (rng  (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x))
                       (or (mjr_util_non-list-then-list range)
                           (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_data-count dquad))))))
         (dom0  (first  dom))
         (dom1  (second dom))
         (u-nam (or (first  domain-data-names) "u"))
         (v-nam (or (second domain-data-names) "v"))
         (rng0  (first  rng))
         (rng1  (second rng))
         (rng2  (third  rng)))

    ;; Step 1: Create functions that will spit out u/v data as required.
    (destructuring-bind (u u-len v v-len dom-dim)
        (case (length dom)
          (1   (typecase (aref dom0 0)
                 (vector    (list          (lambda (i) (aref (aref dom0 i) 0)) (length dom0) (lambda (i) (aref (aref dom0 i) 1)) (length dom0) 2))
                 (otherwise (list          (lambda (i) (aref dom0 i))          (length dom0) nil                                 nil           1))))
          (2   (list                       (lambda (i) (aref dom0 i))          (length dom0) (lambda (i) (aref dom1 i))          (length dom1) 2))
          (otherwise (error "mjr_dsimp_make-from-dquad: Domain must be 1 or 2 dimensional")))

      ;; Step 1: Create functions that will spit out fx/fy/fz data as required.
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

        ;; Now we create the dsimp list
        (if (= 1 dom-dim)
            (let ((points (make-array u-len))
                  (u-data (make-array u-len)))
              (loop for i from 0 upto (1- u-len)
                    do (setf (aref points i) (vector (funcall fx i) (funcall fy i) (funcall fz i)))
                    do (setf (aref u-data i) (funcall u i)))
              (let ((new-dsimp (mjr_dsimp_make-from-points points)))
                ;; Add the u-data
                (if (not do-not-include-domain-data)                                             
                    (mjr_dsimp_add-data new-dsimp u-data 0 :ANO-NAM u-nam))
                ;; Add the lines for curve
                (let ((new-lines (make-array (if u-close u-len (1- u-len)))))           
                  (loop for ci from 1 upto (length new-lines)
                        for li from 0
                        do (setf (aref new-lines li) (vector li (mod ci u-len))))
                  (mjr_dsimp_add-simplex-array new-dsimp new-lines 1))
                ;; Add data
                (dolist (cur-dat data)
                  (mjr_dsimp_add-data new-dsimp 
                                      (mjr_dquad_get-data-array dquad cur-dat)
                                      0
                                      :ano-nam        (mjr_dquad_get-data-attr dquad cur-dat :ano-nam)
                                      :ano-typ        (mjr_dquad_get-data-attr dquad cur-dat :ano-typ)
                                      :ano-colorspace (mjr_dquad_get-data-attr dquad cur-dat :ano-colorspace)))
                new-dsimp))
            (let* ((plen   (* u-len v-len))
                   (points (make-array plen))
                   (u-data (make-array plen))
                   (v-data (make-array plen)))
              (loop with k = -1
                    for i from 0 upto (1- u-len)
                    do (loop for j from 0 upto (1- v-len)
                             do (setf (aref points (incf k)) (vector (funcall fx i j) (funcall fy i j) (funcall fz i j)))
                             do (setf (aref u-data k) (funcall u i))
                             do (setf (aref v-data k) (funcall v j))))
              (let ((new-dsimp (mjr_dsimp_make-from-points points)))
                ;; Add domain-data
                (if (not do-not-include-domain-data)
                    (mjr_dsimp_add-data new-dsimp u-data 0 :ANO-NAM u-nam))
                (if (not do-not-include-domain-data)                    
                    (mjr_dsimp_add-data new-dsimp v-data 0 :ANO-NAM v-nam))
                ;; Add the surface triangles
                (let* ((nx       (if u-close (1+ u-len) u-len))
                       (ny       (if v-close (1+ v-len) v-len))
                       (new-tris (make-array (* 2 (1- nx) (1- ny))))
                       (k        -1))
                  (dotimes (xi (1- nx))
                    (dotimes (yi (1- ny))
                      (let ((p00  (+ xi                  (* u-len yi)))
                            (p01  (+ xi                  (* u-len (mod (1+ yi) v-len))))
                            (p11  (+ (mod (1+ xi) u-len) (* u-len (mod (1+ yi) v-len))))
                            (p10  (+ (mod (1+ xi) u-len) (* u-len yi))))
                        (setf (aref new-tris (incf k)) (if (>= yi (1- v-len)) (vector p00 p11 p10) (vector p00 p10 p11)))
                        (setf (aref new-tris (incf k)) (if (>= yi (1- v-len)) (vector p00 p11 p01) (vector p00 p11 p01))))))
                  (mjr_dsimp_add-simplex-array new-dsimp new-tris 2))
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
                                        :ano-nam        (mjr_dquad_get-data-attr dquad cur-dat :ano-nam)
                                        :ano-typ        (mjr_dquad_get-data-attr dquad cur-dat :ano-typ)
                                        :ano-colorspace (mjr_dquad_get-data-attr dquad cur-dat :ano-colorspace))))
                new-dsimp)))))))
