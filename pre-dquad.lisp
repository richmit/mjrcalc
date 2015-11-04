;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      pre-dquad.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995,2013 by Mitch Richling.  All rights reserved.
;; @brief     Data sets on QUADrilateral (rectilinear) grids.@EOL
;; @Keywords  lisp interactive math Quadrilateral Rectilinear Grid Data mesh lattice
;; @Std       Common Lisp
;;
;;            Many packages in mjrcalc sample functions on a grid (:MJR_VTK, :MJR_PLOT, :MJR_POV, :MJR_IMG).  It is my hope that
;;            this library can form the kernel of most of the computations for those other libraries.  It is also my hope that by
;;            having an intermediate data structure for the data, the process of working with gridded data can be more flexible.
;;
;;            TODO:
;;              * Dump a DQUAD list to
;;                * POVRay File
;;                * VTK File
;;                * Image File
;;                * GNUPlot PIPE
;;              * Create DQUADs from functions (may be done with this bit)
;;              * mjr_dquad_add-data-from-colorize
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_DQUAD 
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_COMBC
        :MJR_ARR
        :MJR_UTIL
        :MJR_COLORIZE
        :MJR_ANNOT)
  (:DOCUMENTATION "Brief: Data sets on QUADrilateral (rectilinear) grids.;")
  (:EXPORT #:mjr_dquad_help
           ;; Create a dquad list from a set of axes
           #:mjr_dquad_make-from-axis
           ;; Create data arrays from dquad lists
           #:mjr_dquad_map                     #:mjr_dquad_colorize
           ;; Add data data to a dquad list
           #:mjr_dquad_add-data-from-map
           #:mjr_dquad_add-data
           ;; New dquad lists from old ones
           #:mjr_dquad_slab                    #:mjr_dquad_copy
           ;; Get size info for a dquad list
           #:mjr_dquad_data-array-size         #:mjr_dquad_axis-vector-lengths
           #:mjr_dquad_data-count              #:mjr_dquad_axis-count
           ;; Get size elements from a dquad list
           #:mjr_dquad_get-data-array          #:mjr_dquad_get-data-attr
           #:mjr_dquad_get-axis-vector         #:mjr_dquad_get-axis-attr
           ;; Persistence
           #:mjr_dquad_read-from-file          #:mjr_dquad_write-to-file
           ;; Internal use.  Not exported
           ;; #:mjr_dquad_get-first-data-idx   #:mjr_dquad_get-first-axis-idx
           ;;                                  #:mjr_dquad_get-all-axis
           ;; #:mjr_dquad_fast_map101          #:mjr_dquad_fast_map110
           ))

(in-package :MJR_DQUAD)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_help ()
  "Help for MJR_DQUAD:

Library for dealing with gridded data sets (think a very, very poor man's RAM-only HDF5 or rectilinear data sets in VTK).

The canonical example is geography: Regularly spaced latitude and longitude points define a 2D grid, and the land altitude, and
potential of the Earths magnetic field define the data points at each grid point.  Another canonical example is meteorological.
Here we have the same latitude and longitude grid, but we add a 3rd dimension of altitude above the geoid.  This forms a 3D grid.
The data points might be wind speed, wind direction, and temperature.

Formally, such data sets consist of a grid of points.  This grid is defined as the cross product of axis vectors -- i.e. by points
on the axis in each dimension.  At each grid point we may have several data values.  The data values may be vectors or numbers.  We
wish to assign meta data to axes and data arrays such as a 'name', measurement units, and data type.  I have chosen to implement the
data storage as a simple list instead of a complex, opaque object -- the wisdom of that decision has yet to be proven. :)

  (list axis-count
        axis-meta_1 axis-vector_1
                 ...
        axis-meta_k axis-vector_k
        data-meta_1 data-array_1
                 ...
        data-meta_n data-array_n
  )

I am committed to keeping this a list; however, the locations of various items within the list may change as this package evolves.
To that end, I have abstracted some access patterns: 1) External to the package, the GET-THINGY functions should be used. 2) The
axis-vector/data-array for an axis-meta/data-meta item will always be the very next thing in the list.  3) axis/data (meta and
vector/array pairs) will always be contiguous with all the axis items coming first and data items coming next. 4) Performance isn't
an issue for the GET-THINGY functions -- they will be O(length of dquad list).  5) The first element of the list is the number of
meta/axis pairs.  6) The last thing on the list are the meta/data pairs.  Essentially this means that we can insert new things after
the first element (after the axis count and before the first axis meta item).  Note that this has ZERO impact to code outside of
this package as code outside the package should use the GET-THINGY functions!!!

The axis-meta & data-meta objects are alists as described by MJR_ANNOT_HELP.  Each axis-vector MUST
sorted. 

Note that an axis-meta and data-meta alists MUST contain both the :ano-nam and :ano-typ attributes, and :ano-nam values should be
unique.

  Example: '(1
              ((:ano-nam . \"X Values\") (:ano-typ . :ano-typ-real))
              #(1 2 3)
              ((:ano-nam . \"Y Values\") (:ano-typ . :ano-typ-real))
              #(1 4 9)
              ((:ano-nam . \"C\") (:ano-typ . :ano-typ-color) (:ano-colorspace . :cs-rgb))
              #(#(0.1 0.2 0.3) #(0.4 0.5 0.6) #(0.7 0.8 1.0))))"
  (documentation 'mjr_dquad_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_make-from-axis (&rest rest)
  "Take axis-meta/axis-vvec pairs, and create a data-less a dquad list.

Note that the input axis-vectors may be VVECs in which case they will be expanded into NEW, LISP vectors.  Note that all vectors
provided will be COPIED into NEW vectors for the returned dquad list.  The values for the axis-meta may be lists or strings.  If
they are strings, then an alist will be constructed with the string the value for the :ano-nam attribute and :ano-typ-real the value
for the :ano-typ attribute.  If they are non-NIL lists, they will be placed into the returned dquad list as is -- the contents are
not validated.  If they are NIL, then an error is returned.  "
  (append (list (/ (length rest) 2))
          (loop for thingy in rest
                for i from 1
                if (oddp i)
                collect (cond ((stringp thingy)  (list (cons :ano-nam thingy) (cons :ano-typ :ano-typ-real)))
                              ((null thingy)     (error "mjr_dquad_make-from-axis: axis-meta must not be nil"))
                              ((listp thingy)    thingy)
                              ('t                (error "mjr_dquad_make-from-axis: axis-meta was not a supported type")))
                else
                collect (if (vectorp thingy)
                            (copy-seq thingy)
                            (mjr_vvec_to-vec-maybe thingy)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_axis-count (dquad)
  "Return the number of axis arrays in the dquad list"
  (first dquad))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-first-axis-idx (dquad)
  "Return the index of the first axis array in the dquad list

This function makes the assumption that axis-meta/axis-vector pairs start on index 2 -- need to update if this changes."
  (declare (ignore dquad))
  2)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-first-data-idx (dquad)
  "Return the index of the first data array in the dquad list"
  (+ (mjr_dquad_get-first-axis-idx dquad) (* 2 (mjr_dquad_axis-count dquad))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_data-count (dquad)
  "Return the number of data arrays in the dquad list"
  (/ (1+ (- (length dquad) (mjr_dquad_get-first-data-idx dquad))) 2))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-all-axis (dquad)
  "Return a list containing all the axis arrays in dquad -- not copies of the arrays!"
  (loop with axis-count = (mjr_dquad_axis-count dquad)
        for i from 1 upto axis-count
        for idx = (mjr_dquad_get-first-axis-idx dquad) then (+ idx 2)
        collect (elt dquad idx)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-axis-vector (dquad axis-index-or-name)
  "Return the the axis vector identified by AXIS-INDEX-OR-NAME."
  (typecase axis-index-or-name
    (integer   (let ((axis-count (mjr_dquad_axis-count dquad)))
                 (cond ((<  axis-index-or-name 0)          (error "mjr_dquad_get-axis-vector: Axis index too small!"))
                       ((>= axis-index-or-name axis-count) (error "mjr_dquad_get-axis-vector: Axis index too large!")))
                 (elt dquad (+ (mjr_dquad_get-first-axis-idx dquad) (* 2 axis-index-or-name)))))
    (string    (loop with axis-count = (mjr_dquad_axis-count dquad)
                     for i from 1 upto axis-count
                     for idx = (mjr_dquad_get-first-axis-idx dquad) then (+ idx 2)
                     when (string-equal axis-index-or-name (cdr (assoc :ano-nam (elt dquad (1- idx)))))
                     do (return (elt dquad idx))))
    (otherwise (error "mjr_dquad_get-axis-vector: Value for axis-index-or-name an integer or string!"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-data-array (dquad data-index-or-name)
  "Return the data array identified by DATA-INDEX-OR-NAME."
  (typecase data-index-or-name
    (integer   (let ((data-count (mjr_dquad_data-count dquad)))
                 (cond ((<  data-index-or-name 0)          (error "mjr_dquad_get-data-array: Data index too small!"))
                       ((>= data-index-or-name data-count) (error "mjr_dquad_get-data-array: Data index too large!")))
                 (elt dquad (+ (+ 2 (* (mjr_dquad_axis-count dquad) 2)) (* 2 data-index-or-name)))))
    (string    (loop with data-count = (mjr_dquad_data-count dquad)
                     for i from 1 upto data-count
                     for idx = (mjr_dquad_get-first-data-idx dquad) then (+ idx 2)
                     when (string-equal data-index-or-name (cdr (assoc :ano-nam (elt dquad (1- idx)))))
                     do (return (elt dquad idx))))
    (otherwise (error "mjr_dquad_get-data-array: Value for data-index-or-name an integer or string!"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_axis-vector-lengths (dquad)
  "Return the lengths (as a list) of the axis vectors"
  (mapcar #'length (mjr_dquad_get-all-axis dquad)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_data-array-size (dquad)
  "Return the size of the data arrays"
  (mjr_dquad_axis-vector-lengths dquad))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-axis-attr (dquad axis-index-or-name attr-name)
  "Return the value for the ATTR-NAME for the Nth axis in the DQUAD list"
  (typecase axis-index-or-name
    (integer   (let ((axis-count (mjr_dquad_axis-count dquad)))
                 (cond ((<  axis-index-or-name 0)                     (error "mjr_dquad_get-axis-attr: Axis index too small!"))
                       ((>= axis-index-or-name axis-count)            (error "mjr_dquad_get-axis-attr: Axis index too large!"))
                       ((not (member attr-name '(:ano-nam :ano-typ))) (error "mjr_dquad_get-axis-attr: Invalid attr-name!")))
                 (cdr (assoc attr-name (elt dquad (1- (+ (mjr_dquad_get-first-axis-idx dquad) (* 2 axis-index-or-name))))))))
    (string    (loop with axis-count = (mjr_dquad_axis-count dquad)
                     for i from 1 upto axis-count
                     for idx = (mjr_dquad_get-first-axis-idx dquad) then (+ idx 2)
                     when (string-equal axis-index-or-name (cdr (assoc :ano-nam (elt dquad (1- idx)))))
                     do (return (cdr (assoc attr-name (elt dquad (1- idx)))))))
    (otherwise (error "mjr_dquad_get-axis-attr: AXIS-INDEX-OR-NAME must be a string or integer"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-data-attr (dquad data-index-or-name attr-name)
  "Return the value for the ATTR-NAME for the Nth data array in the DQUAD list"
  (typecase data-index-or-name
    (integer   (let ((data-count (mjr_dquad_data-count dquad)))
    (cond ((<  data-index-or-name  0)                                    (error "mjr_dquad_get-data-attr: Data index too small!"))
          ((>= data-index-or-name  data-count)                           (error "mjr_dquad_get-data-attr: Data index too large!"))
          ((not (member attr-name '(:ano-nam :ano-colorspace :ano-typ))) (error "mjr_dquad_get-data-attr: Invalid attr-name!")))
    (cdr (assoc attr-name (elt dquad (1- (+ (+ 2 (* (mjr_dquad_axis-count dquad) 2)) (* 2 data-index-or-name ))))))))
    (string    (loop with data-count = (mjr_dquad_data-count dquad)
                     for i from 1 upto data-count
                     for idx = (mjr_dquad_get-first-data-idx dquad) then (+ idx 2)
                     when (string-equal data-index-or-name (cdr (assoc :ano-nam (elt dquad (1- idx)))))
                     do (return (cdr (assoc attr-name (elt dquad (1- idx)))))))
    (otherwise (error "mjr_dquad_get-data-attr: DATA-INDEX-OR-NAME must be a string or integer"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defmacro mjr_dquad_fast_map101 (fn am axis-vectors data-arrays)
  "Return an array produced by evaluateing FN on the elements of the cross product of the given vectors.  am is the argument mode."
  (let* ((num-axis (length axis-vectors))
         (arr-v   (gensym "arr-"))
         (vei-vl  (loop for i from 0 upto (1- num-axis)
                        collect (gensym (format 'nil "vei-~d-" i))))
         (vev-vl  (loop for i from 0 upto (1- num-axis)
                        collect (gensym (format 'nil "vev-~d-" i))))
         (dc      (case am
                    (:arg-vector `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (vector ,@vev-vl ,@(loop for dv in data-arrays collect `(aref ,dv ,@vei-vl)) ))))
                    (:arg-number `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vev-vl         ,@(loop for dv in data-arrays collect `(aref ,dv ,@vei-vl)) )))
                    (:arg-args   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vev-vl         ,@(loop for dv in data-arrays collect `(aref ,dv ,@vei-vl)) )))
                    (:arg-list   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (list ,@vev-vl   ,@(loop for dv in data-arrays collect `(aref ,dv ,@vei-vl)) )))))))
    (loop for i downfrom (1- num-axis) to 0
          do (setq dc `(loop for ,(elt vev-vl i) across ,(elt axis-vectors i)
                             for ,(elt vei-vl i) upfrom 0
                             do ,dc)))
    `(let ((,arr-v (make-array (list ,@(mapcar #'length axis-vectors)))))
       ,dc
       ,arr-v)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defmacro mjr_dquad_fast_map110 (fn am &rest vecs)
  "Return an array produced by evaluateing FN on the elements of the cross product of the given vectors.  am is the argument mode."
  (let* ((num-vec (length vecs))
         (arr-v   (gensym "arr-"))
         (vei-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vei-~d-" i))))
         (vev-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vev-~d-" i))))
         (dc      (case am
                    (:arg-vector `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (vector ,@vei-vl ,@vev-vl))))
                    (:arg-number `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn         ,@vei-vl ,@vev-vl)))
                    (:arg-args   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn         ,@vei-vl ,@vev-vl)))
                    (:arg-list   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (list   ,@vei-vl ,@vev-vl)))))))
    (loop for i downfrom (1- num-vec) to 0
          do (setq dc `(loop for ,(elt vev-vl i) across ,(elt vecs i)
                             for ,(elt vei-vl i) upfrom 0
                             do ,dc)))
    `(let ((,arr-v (make-array (list ,@(mapcar #'length vecs)))))
       ,dc
       ,arr-v)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_map (dquad f &key axes idxes data arg-mode)
  "This is a 'super-duper map for dquad lists.

We can evaluate the map function different pats of the given dquad list:
  * The cross product space of the vectors.  THINK: mjr_combc_gen-all-cross-product
  * One or more data arrays.  THINK: mjr_arr_map
  * The 'indexes' of the cross product space of the vectors.  THINK: mjr_combc_gen-all-cross-product on '(:len n) ...

The F function arguments are in the following order: idxes, axes, and data.

AXES & IDXES may only be NIL or 'T, but DATA may be:
  * NIL -- no data elements
  * 't -- all data elements
  * list of data indexes
  * a single integer data index

Supported combinations of axes, idxes, and data arguments:

              |------+-------+------+---------------------------------------------------|
              | axes | idexs | data | Supported                                         |
              |------+-------+------+---------------------------------------------------|
              |    0 |     0 |    0 | ERROR                                             |
              |------+-------+------+---------------------------------------------------|
              |    1 |     0 |    0 | Fully supported (mjr_combc_gen-all-cross-product) |
              |    0 |     1 |    0 | Fully supported (mjr_combc_gen-all-cross-product) |
              |    0 |     0 |    1 | Fully supported (mjr_arr_map)                     |
              |------+-------+------+---------------------------------------------------|
              |    1 |     1 |    0 | Fully supported (mjr_dquad_fast_map110)           |
              |------+-------+------+---------------------------------------------------|
              |    0 |     1 |    1 | Not Supported!                                    |
              |    1 |     0 |    1 | Fully supported (mjr_dquad_fast_map101)           |
              |    1 |     1 |    1 | Not Supported!                                    |
              |------+-------+------+---------------------------------------------------|"
  (let* ((arg-mode   (or arg-mode :arg-number))
         (axis-count (mjr_dquad_axis-count dquad))
         (data-count (mjr_dquad_data-count dquad))
         (data       (cond ((integerp data) (list data))
                            ((listp data)   data)
                            ('t            (if (< data-count 0)
                                               (concatenate 'list (mjr_vvec_to-vec-maybe data-count)))))))
    (if data
        (cond ((not (every #'numberp data))               (error "mjr_dquad_map: An element of DATA was non-numeric"))
              ((not (every #'integerp data))              (error "mjr_dquad_map: An element of DATA was not an integer"))
              ((some (lambda (x) (< x 0)) data)           (error "mjr_dquad_map: An element of DATA was negative"))
              ((some (lambda (x) (>= x axis-count)) data) (error "mjr_dquad_map: An element of DATA was too large"))))
    (cond ((not (or axes idxes data)) (error "mjr_dquad_map: At least one of AXES, DATA, IDXES must be non-NIL"))
          ((and idxes data)            (error "mjr_dquad_map: non-NIL DATA may not be combined with non-NIL IDXES")))
    (if (null data)
        (if (null axes)                                                                                                                                   ;; A I D
            (mjr_combc_gen-all-cross-product (mjr_dquad_axis-vector-lengths dquad) :collect-value f :result-type :array :arg-mode arg-mode)               ;; 0 1 0
            (if (null idxes)
                (mjr_combc_gen-all-cross-product (mjr_dquad_get-all-axis dquad)                   :collect-value f :result-type :array :arg-mode arg-mode);; 1 0 0
                (eval (macroexpand `(mjr_dquad_fast_map110 ,f ,arg-mode ,@(mjr_dquad_get-all-axis dquad))))))                                             ;; 1 1 0
        (let ((da-dats (mapcar (lambda (i) (elt dquad (+ (+ 2 (* (mjr_dquad_axis-count dquad) 2)) (* 2 i)))) data)))
          (if (null axes)
              (apply #'mjr_arr_map (mjr_util_fun-adapt-x2x f arg-mode :arg-number) da-dats)                                                               ;; 0 ? 1
              (eval (macroexpand `(mjr_dquad_fast_map101 ,f ,arg-mode (,@(mjr_dquad_get-all-axis dquad)) (,@da-dats)))))))))                              ;; 1 0 1

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_colorize (dquad &key axes idxes data color-method color-space max-color data-range auto-scale)
  "This is a helper function to create a colorization function and then apply it to DQUAD via MJR_DQUAD_MAP.

Note that if the axes (or idxes) are used, the dquad must be 2D (i.e. two axes).  If just DATA is used, then the data may be of
dimension greater than 2 in which case the result will not be a simple 2D image.  When the result is 2D, the result is compatible
with the MJR_IMG library using a packing function of #'identity."
  (cond ((and (or idxes axes)
              (not (= 2 (mjr_dquad_axis-count dquad)))) (error "mjr_dquad_colorize: dquad must have only two axes!!"))
        ((not (or axes idxes data))                     (error "mjr_dquad_colorize: At least one of AXES, IDXES, or DATA must be provided"))
        ((and idxes data)                               (error "mjr_dquad_colorize: non-NIL DATA may not be combined with non-NIL IDXES")))

  (let* ((data-count (mjr_dquad_data-count dquad))
         (data       (or (mjr_util_non-list-then-list data)
                         (concatenate 'list (mjr_vvec_to-vec-maybe (1- data-count)))))
         (data-range (if (and auto-scale (null data-range))
                         (apply #'concatenate 'vector (concatenate 'list
                                                                   (if idxes (mapcar (lambda (x) (list 0 (1- x)))
                                                                                     (mjr_dquad_axis-vector-lengths dquad)))
                                                                   (if axes  (mapcar #'mjr_arr_min-max
                                                                                     (mjr_dquad_get-all-axis dquad)))
                                                                   (mapcar (lambda (x) (multiple-value-list (mjr_arr_min-max x)))
                                                                           (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x)) data))))
                         data-range)))
    (mjr_dquad_map dquad (mjr_colorize_make-colorize-function color-method color-space max-color data-range auto-scale)
                   :idxes idxes :axes axes :data data)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_add-data (dquad data-array &key ano-nam ano-typ ano-colorspace)
  "Destructively add the given data-array and and a data-meta list to dquad"
  (cond ((not (equal (mjr_dquad_data-array-size dquad) (array-dimensions data-array))) (error "mjr_dquad_add-data: data-array of wrong size!!")))
  (let ((ano-typ (or ano-typ :ano-typ-real)))
    (nconc dquad
           (list (loop for da-tag in (list :ano-nam :ano-typ :ano-colorspace)
                       for da-val in (list ano-nam  ano-typ  ano-colorspace)
                       when da-val
                       collect (cons da-tag da-val)))
           (list data-array))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_add-data-from-map (dquad f &key axes idxes data ano-nam ano-typ ano-colorspace (arg-mode :arg-number))
  "Destructively add a new data-array and data-meta to dquad by applying MJR_DQUAD_MAP to DQUAD and F."
  (let ((arg-mode   (or arg-mode :arg-number)))
    (mjr_dquad_add-data dquad (mjr_dquad_map dquad f :axes axes :idxes idxes :data data :arg-mode arg-mode)
                        :ano-nam ano-nam :ano-typ ano-typ :ano-colorspace ano-colorspace)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_read-from-file (file-name)
  "Read a dquad list from a file.

Data in the file after the first DQUAD list will be ignored.  This space is traditionally used for free text commentary regarding
the data set."
  (with-open-file (stream file-name)
    (read stream)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_write-to-file (dquad file-name)
  "Write a dquad list from a file"
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (null (write  dquad :stream stream))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_slab (dquad  &rest subscripts)
  "Create a new DQUAD list from a slab of DQUAD.  See: mjr_arr_get-slab.

Note that all data-array elements in the new dquad list are new, but meta data and retained axis-vector elements are not copies."
  (let ((new-axis-count (count nil subscripts))
        (old-axis-count (mjr_dquad_axis-count dquad)))
    (cond ((<= new-axis-count 0)                                           (error "mjr_dquad_slab: Slab must contain at least one axis!!"))
          ((not (= old-axis-count (length subscripts)))                    (error "mjr_dquad_slab: Must have a subscript argument for each axis!!"))
          ((some (lambda (x) (and x (< x 0))) subscripts)                  (error "mjr_dquad_slab: One or more subscripts negative!!"))
          ((some #'identity (mapcar (lambda (x y) (and x y (<= x y)))
                                    (mjr_dquad_axis-vector-lengths dquad)
                                    subscripts))                           (error "mjr_dquad_slab: One or more subscripts too large!!")))
    (loop with first-axis-idx = (1- (mjr_dquad_get-first-axis-idx dquad))
        with first-data-idx = (1- (mjr_dquad_get-first-data-idx dquad))
        for idx from 0 upto (1- (length dquad))
        for thingy in dquad
        for is-meta = (if (>= idx first-axis-idx) (evenp (- idx first-axis-idx)))
        for axIdx   = 0 then (if (and (> idx first-axis-idx) is-meta) (1+ axIdx) axIdx)
        for keepAx  = (if (>= idx first-axis-idx) (null (elt subscripts axIdx)))
        for thingy2 = (cond ((= 0 idx)                               new-axis-count)                               ;;Fix the axis count
                            ((< idx first-axis-idx)                  thingy)                                       ;;Keep stuff between axis count and first axis
                            ((and (>= idx first-data-idx) is-meta)   thingy)                                       ;;Keep all data meta element
                            ((>= idx first-data-idx)                 (apply #'mjr_arr_get-slab thingy subscripts)) ;;Extract slab from data element
                            (keepAx                                  thingy))                                      ;;An axis.  Keep it
        when thingy2
        collect thingy2)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_copy (dquad)
  "Create a new DQUAD list from DQUAD -- all elements of the new dquad list will be NEW (copied) objects."
  (loop for thingy in dquad
        collect (typecase thingy
                  (integer thingy)
                  (list (copy-tree thingy))
                  (array (mjr_arr_copy thingy)))))


