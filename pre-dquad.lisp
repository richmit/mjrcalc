;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      pre-dquad.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995,2013 by Mitch Richling.  All rights reserved.
;; @brief     Data sets on QUADrilateral (rectilinear) grids.@EOL
;; @Keywords  lisp interactive math Quadrilateral Rectilinear Grid Data mesh lattice
;; @Std       Common Lisp
;;
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
           ;; Create a dquad list
           #:mjr_dquad_make-from-axis
           #:mjr_dquad_make-from-func-r123-r123
           #:mjr_dquad_make-from-func-c1-c1
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
           #:mjr_dquad_get-data-array          #:mjr_dquad_get-data-ano
           #:mjr_dquad_get-axis-vector         #:mjr_dquad_get-axis-ano
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

A typical 2D example is geographic data: Regularly spaced latitude and longitude points define a 2D grid while data points might
include atomosphphicric pressure (a scalar) and the force of gravity (a vector) both measured at the serface.  We can extend this
example by adding a 3rd dimension of altitude above the geoid.  In this 3D example, the data points might be wind velocity (a
vector) and temperature (a scalar).  Throughout the *MJRCALC* library such data sets are common.  Some examples include

   - Sampleing a functiosn on a regular grid across its domain is a common first step for visulization and graphing

   - Solving ODEs, systems of ODEs, and preforming univariate quadrature (1D grids)

   - Solving PDEs, arising from fluid mechanics for example, are frequently solved on 2D or 3D grids

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

The axis-meta & data-meta objects are alists as described by MJR_ANNOT_HELP.  Each axis-vector MUST sorted. 

Note that an axis-meta and data-meta alists MUST contain both the :ano-nam and :ano-typ keys, and :ano-nam values should be unique.

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
they are strings, then an alist will be constructed with the string the value for the :ano-nam key.  If they are non-NIL lists, they
will be placed into the returned dquad list as is -- the contents are not validated.  If they are NIL, then an error is returned.  "
  (append (list (/ (length rest) 2))
          (loop for thingy in rest
                for i from 1
                if (oddp i)
                collect (cond ((stringp thingy)  (list (cons :ano-nam thingy)))
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
                     when (string-equal axis-index-or-name (mjr_annot_get-value :ano-nam (elt dquad (1- idx))))
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
                     when (string-equal data-index-or-name (mjr_annot_get-value :ano-nam (elt dquad (1- idx))))
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
(defun mjr_dquad_get-axis-ano (dquad axis-index-or-name &optional ano-key)
  "Return the value for the ano-key for the Nth axis in the DQUAD list.  Return entire annotation alist if ano-key is NIL"
  (if ano-key (mjr_annot_check-ano-key ano-key))
  (let ((anno-alist (typecase axis-index-or-name
                      (integer   (let ((axis-count (mjr_dquad_axis-count dquad)))
                                   (cond ((<  axis-index-or-name 0)          (error "mjr_dquad_get-axis-ano: Axis index too small!"))
                                         ((>= axis-index-or-name axis-count) (error "mjr_dquad_get-axis-ano: Axis index too large!")))
                                   (elt dquad (1- (+ (mjr_dquad_get-first-axis-idx dquad) (* 2 axis-index-or-name))))))
                      (string    (loop with axis-count = (mjr_dquad_axis-count dquad)
                                       for i from 1 upto axis-count
                                       for idx = (mjr_dquad_get-first-axis-idx dquad) then (+ idx 2)
                                       when (string-equal axis-index-or-name (mjr_annot_get-value :ano-nam (elt dquad (1- idx))))
                                       do (return (elt dquad (1- idx)))))
                      (otherwise (error "mjr_dquad_get-axis-ano: AXIS-INDEX-OR-NAME must be a string or integer")))))
    (if ano-key
        (mjr_annot_get-value ano-key anno-alist)
        anno-alist)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_get-data-ano (dquad data-index-or-name &optional ano-key)
  "Return the value for the ano-key for the Nth data array in the DQUAD list.  Return entire annotation alist if ano-key is NIL"
  (if ano-key (mjr_annot_check-ano-key ano-key))
  (let ((anno-alist (typecase data-index-or-name
                      (integer   (let ((data-count (mjr_dquad_data-count dquad)))
                                   (cond ((<  data-index-or-name  0)          (error "mjr_dquad_get-data-ano: Data index too small!"))
                                         ((>= data-index-or-name  data-count) (error "mjr_dquad_get-data-ano: Data index too large!")))
                                   (elt dquad (1- (+ (+ 2 (* (mjr_dquad_axis-count dquad) 2)) (* 2 data-index-or-name))))))
                      (string    (loop with data-count = (mjr_dquad_data-count dquad)
                                       for i from 1 upto data-count
                                       for idx = (mjr_dquad_get-first-data-idx dquad) then (+ idx 2)
                                       when (string-equal data-index-or-name (mjr_annot_get-value :ano-nam (elt dquad (1- idx))))
                                       do (return (elt dquad (1- idx)))))
                      (otherwise (error "mjr_dquad_get-data-ano: DATA-INDEX-OR-NAME must be a string or integer")))))
    (if ano-key
        (mjr_annot_get-value ano-key anno-alist)
        anno-alist)))
        
;;----------------------------------------------------------------------------------------------------------------------------------
(defmacro mjr_dquad_fast_map101 (fn am axis-vectors data-arrays)
  "Return an array produced by evaluating FN on the elements of the cross product of the given vectors.  am is the argument mode."
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
  "Return an array produced by evaluating FN on the elements of the cross product of the given vectors.  am is the argument mode."
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
         (data-count (mjr_dquad_data-count dquad))
         (data       (typecase data
                       (integer   (list data))
                       (string    (list data))
                       (list      data)
                       (otherwise (if (not (zerop data-count)) (concatenate 'list (mjr_vvec_to-vec-maybe data-count)))))))
    (cond ((not (or axes idxes data)) (error "mjr_dquad_map: At least one of AXES, DATA, IDXES must be non-NIL"))
          ((and idxes data)           (error "mjr_dquad_map: non-NIL DATA may not be combined with non-NIL IDXES")))
    (if (null data)
        (if (null axes)                                                                                                                                   ;; A I D
            (mjr_combc_gen-all-cross-product (mjr_dquad_axis-vector-lengths dquad) :collect-value f :result-type :array :arg-mode arg-mode)               ;; 0 1 0
            (if (null idxes)
                (mjr_combc_gen-all-cross-product (mjr_dquad_get-all-axis dquad)                   :collect-value f :result-type :array :arg-mode arg-mode);; 1 0 0
                (eval (macroexpand `(mjr_dquad_fast_map110 ,f ,arg-mode ,@(mjr_dquad_get-all-axis dquad))))))                                             ;; 1 1 0
        (let ((da-dats (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x)) data)))          
          (if (null axes)
              (apply #'mjr_arr_map (mjr_util_fun-adapt-x2x f arg-mode :arg-number) da-dats)                                                               ;; 0 ? 1
              (eval (macroexpand `(mjr_dquad_fast_map101 ,f ,arg-mode (,@(mjr_dquad_get-all-axis dquad)) (,@da-dats)))))))))                              ;; 1 0 1

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_add-data (dquad data-array &key ano-nam ano-typ ano-colorspace ano-cpacking ano-units)
  "Destructively add the given data-array and and a data-meta list to dquad"
  (cond ((not (equal (mjr_dquad_data-array-size dquad) (array-dimensions data-array))) (error "mjr_dquad_add-data: data-array of wrong size!!"))
        ((mjr_dquad_get-data-ano dquad ano-nam :ano-nam)                               (error "mjr_dquad_add-data: data-array already exists :ano-nam!")))
  (nconc dquad
         (list (mjr_annot_make-alist ano-nam ano-typ ano-colorspace ano-cpacking ano-units))
         (list data-array)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_add-data-from-map (dquad f &key axes idxes data ano-nam ano-typ ano-colorspace ano-cpacking (arg-mode :arg-number))
  "Destructively add a new data-array and data-meta to dquad by applying MJR_DQUAD_MAP to DQUAD and F."
  (let ((arg-mode   (or arg-mode :arg-number)))
    (mjr_dquad_add-data dquad (mjr_dquad_map dquad f :axes axes :idxes idxes :data data :arg-mode arg-mode)
                        :ano-nam ano-nam :ano-typ ano-typ :ano-colorspace ano-colorspace :ano-cpacking ano-cpacking)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_colorize (dquad &key axes idxes data color-method (max-color 1) data-range (auto-scale 't) ano-nam (ano-colorspace :CS-RGB))
  "This is a helper function to create a colorization function and then apply it to DQUAD via MJR_DQUAD_MAP.

Arguments:
  - axes, idxes, data ................................... Same as in mjr_dquad_map
  - color-method, max-color, data-range, & auto-scale ... As in mjr_colorize_make-colorize-function
  - ano-colorspace, ano-nam ............................. As in mjr_dquad_add-data

When the result is 2D, it is compatible with the MJR_IMG library using a packing function of #'identity."
  (cond ((not (or axes idxes data)) (error "mjr_dquad_colorize: At least one of AXES, IDXES, or DATA must be provided"))
        ((and idxes data)           (error "mjr_dquad_colorize: non-NIL DATA may not be combined with non-NIL IDXES"))
        ((not ano-nam)              (error "mjr_dquad_colorize: ANO-NAM must be a non-NIL!"))
        ((not (stringp ano-nam))    (error "mjr_dquad_colorize: ANO-NAM must be a string!")))
; MJR SCM NOTE <2015-03-28 15:05:21 CDT> mjr_dquad_colorize: Better error checking on ano-nam argument.
  (let* ((data-count (mjr_dquad_data-count dquad))
         (data       (typecase data
                       (string    (list data))
                       (integer   (list data))
                       (list      data)
                       (otherwise (if (not (zerop data-count)) (concatenate 'list (mjr_vvec_to-vec-maybe data-count))))))
         (data-range (or data-range
                         (if auto-scale
                             (apply #'concatenate 'vector (concatenate 'list
                                                                       (if idxes (mapcar (lambda (x) (list 0 (1- x)))
                                                                                         (mjr_dquad_axis-vector-lengths dquad)))
                                                                       (if axes  (mapcar (lambda (x) (multiple-value-list (mjr_arr_min-max x)))
                                                                                         (mjr_dquad_get-all-axis dquad)))
                                                                       (mapcar (lambda (x) (multiple-value-list (mjr_arr_min-max x)))
                                                                               (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x)) data))))))))
    (mjr_dquad_add-data-from-map dquad
                                 (mjr_colorize_make-colorize-function color-method ano-colorspace max-color data-range auto-scale)
                                 :idxes idxes :axes axes :data data
                                 :ano-nam ano-nam :ano-typ :ano-typ-color :ano-colorspace ano-colorspace :ano-cpacking :CP-IDENTITY)))

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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_make-from-func-r123-r123 (func &key
                                                  (func-lab "f")
                                                  xdat ydat zdat (arg-mode :arg-number) (xlab "x") (ylab "y") (zlab "z")
                                                  ax-color-meth  (ax-color-max-color  1) (ax-color-lab  "c") (ax-color-auto-scale  't) (ax-color-colorspace  :cs-rgb)
                                                  f-color-meth   (f-color-max-color   1) (f-color-lab   "c") (f-color-auto-scale   't) (f-color-colorspace   :cs-rgb)
                                                  all-color-meth (all-color-max-color 1) (all-color-lab "c") (all-color-auto-scale 't) (all-color-colorspace :cs-rgb))
  "Transform one or more mathematical functions into a dquad or dsimp list
     Arguments:
       - func ................. A function or list of same
       - func-lab ............. Labels (names) for func
       - xdat ydat zdat ....... VVECs that define the domain
       - xlab ylab zlab ....... Labels (names) for domain axes (xdat, ydat, and zdat)
       - arg-mode ............. How args are passed to the functions.  May be a list
       - u-close v-close ...... For :dsimp output.  Close the curve or surface
       - X-color-lab .......... Labels (names) for *-color-meth
       - X-color-meth ......... color-method or list of same.  See: MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-colorspace ... colorspace or list of same.  See: MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-auto-scale ... auto-scale or list of same.  See: MJR_COLORIZE_MAKE-COLORIZE-FUNCTION
       - X-color-max-color .... max-color or list of same.  MJR_See: COLORIZE_MAKE-COLORIZE-FUNCTION
                           -  X=ax .... axis              -- color functions receive x, y, z
                           -  X=f ..... function          -- color functions receive f0, ..., n_{n-1}
                           -  X=all ... axis and function -- color functions receive x, y, z, f0, ..., n_{n-1})"
  (flet ((mkLabs (labs num) ;; Create NUM unique labels from list of labels in LABS
           (if (<= num (length labs))
               labs
               (let ((good-labs nil))
                 (loop for i from 0 upto (1- num)
                       for b = (mjr_util_elt-mod labs i)
                       do (loop for j from 0
                                for n = (format nil "~a~d" b j)
                                when (not (member n good-labs :test #'string=))
                                return (push n good-labs))
                       finally (return (reverse good-labs)))))))
    (let* ((list-of-funcs           (mjr_util_non-list-then-list func))
           (list-of-func-labs       (mkLabs (mjr_util_non-list-then-list func-lab) (length list-of-funcs)))
           (list-of-ax-color-meths  (mjr_util_non-list-then-list ax-color-meth))
           (list-of-ax-color-mc     (mjr_util_non-list-then-list ax-color-max-color))
           (list-of-ax-color-as     (mjr_util_non-list-then-list ax-color-auto-scale))
           (list-of-ax-color-cs     (mjr_util_non-list-then-list ax-color-colorspace))
           (list-of-ax-color-labs   (mkLabs (mjr_util_non-list-then-list ax-color-lab) (length list-of-ax-color-meths)))
           (list-of-f-color-meths   (mjr_util_non-list-then-list f-color-meth))
           (list-of-f-color-mc      (mjr_util_non-list-then-list f-color-max-color))
           (list-of-f-color-as      (mjr_util_non-list-then-list f-color-auto-scale))
           (list-of-f-color-cs      (mjr_util_non-list-then-list f-color-colorspace))
           (list-of-f-color-labs    (mkLabs (mjr_util_non-list-then-list f-color-lab) (length list-of-f-color-meths)))
           (list-of-all-color-meths (mjr_util_non-list-then-list all-color-meth))
           (list-of-all-color-mc    (mjr_util_non-list-then-list all-color-max-color))
           (list-of-all-color-as    (mjr_util_non-list-then-list all-color-auto-scale))
           (list-of-all-color-cs    (mjr_util_non-list-then-list all-color-colorspace))
           (list-of-all-color-labs  (mkLabs (mjr_util_non-list-then-list all-color-lab) (length list-of-all-color-meths))))
      (cond ((zerop (length list-of-funcs))         (error "mjr_dquad_make-from-func-r123-r123: FUNC must be provided!"))
            ((not xdat)                             (error "mjr_dquad_make-from-func-r123-r123: XDAT must be provided!"))
            ((and zdat (not ydat))                  (error "mjr_dquad_make-from-func-r123-r123: YDAT must be provided when ZDAT is provided!")))
      (let ((daDquad (apply #'mjr_dquad_make-from-axis (concatenate 'list (list xlab xdat) (if ydat (list ylab ydat)) (if zdat (list zlab zdat))))))
        (loop for i from 0
              for f in list-of-funcs
              for d-lab in list-of-func-labs
              for d = (mjr_dquad_map daDquad f :axes 't :arg-mode arg-mode)
              for d-val = (mjr_arr_aref-row-major d 0)
              for d-typ = (typecase d-val
                            (complex   :ano-typ-complex)
                            (number    :ano-typ-real)
                            (vector    (typecase (aref d-val 0)
                                         (complex   :ano-typ-cvec)
                                         (number    :ano-typ-rvec)
                                         (otherwise (error "mjr_dquad_make-from-func-r123-r123: Function return type not supported!"))))
                            (otherwise (error "mjr_dquad_make-from-func-r123-r123: Function return type not supported!")))
              do (mjr_dquad_add-data daDquad d :ano-nam d-lab :ano-typ d-typ))
        (if list-of-ax-color-meths
            (loop for i from 0
                  for c-cm  in list-of-ax-color-meths
                  for c-lab in list-of-ax-color-labs
                  for c-as  = (mjr_util_elt-mod list-of-ax-color-as i)
                  for c-cs  = (mjr_util_elt-mod list-of-ax-color-cs i)
                  for c-mc  = (mjr_util_elt-mod list-of-ax-color-mc i)                  
                  do (mjr_dquad_colorize daDquad :color-method c-cm :axes 't :auto-scale c-as :ano-nam c-lab :max-color c-mc :ano-colorspace c-cs)))
        (if list-of-f-color-meths
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec-maybe (length list-of-funcs)))))
              (loop for i from 0
                    for c-cm  in list-of-f-color-meths
                    for c-lab in list-of-f-color-labs
                    for c-as  = (mjr_util_elt-mod list-of-f-color-as i)
                    for c-cs  = (mjr_util_elt-mod list-of-f-color-cs i)
                    for c-mc  = (mjr_util_elt-mod list-of-f-color-mc i)                  
                    do (mjr_dquad_colorize daDquad :color-method c-cm :data data-idxs :auto-scale c-as :ano-nam c-lab :max-color c-mc :ano-colorspace c-cs))))
        (if list-of-all-color-meths
            (let ((data-idxs (concatenate 'list (mjr_vvec_to-vec-maybe (length list-of-funcs)))))
              (loop for i from 0
                    for c-cm  in list-of-all-color-meths
                    for c-lab in list-of-all-color-labs
                    for c-as  = (mjr_util_elt-mod list-of-all-color-as i)
                    for c-cs  = (mjr_util_elt-mod list-of-all-color-cs i)
                    for c-mc  = (mjr_util_elt-mod list-of-all-color-mc i)                  
                    do (mjr_dquad_colorize daDquad :color-method c-cm :axes 't :data data-idxs :auto-scale c-as :ano-nam c-lab :max-color c-mc :ano-colorspace c-cs))))
        daDquad))))

; MJR SCM NOTE <2015-02-28 13:36:22 CST> mjr_dquad_make-from-func-r123-r123: Support names for everything.  Super duper colorization.

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dquad_make-from-func-c1-c1 (f &key rdat idat f_color z_color zf_color)

  "Return a dquad list containing data useful for complex function visualization.

The returned dquad list will containing the following:
  - AXIS: real     -- Re(z)
  - AXIS: imag     -- Im(z)
  - DATA: z        -- A complex value in the domain.  When exported to VTK, this will yield two scalar values: z_real & z_imag
  - DATA: f        -- f(z)                            When exported to VTK, this will yield two scalar values: f_real & f_imag
  - DATA: f_abs    -- abs(f(z))
  - DATA: f_phase  -- arg(f(z))
  - DATA: z_color  -- z_color(z)                      Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp
  - DATA: f_color  -- f_color(f(z))                   Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp
  - DATA: zf_color -- zf_color(z, f(z))               Must return :cs-rgb colors See use-colorize.lisp and use-colorizer.lisp
Note: Some tools will only use one color scalar in a data set"
  (let* (;;(c-func  (mjr_mxp_string-or-func-to-lambda c-func "Z"))
         ;;(f       (mjr_mxp_string-or-func-to-lambda f      "Z"))
         (daDquad (mjr_dquad_make-from-axis "real" rdat
                                            "imag" idat)))
    (mjr_dquad_add-data-from-map daDquad #'complex    :axes 't     :ano-nam "z"        :ano-typ :ano-typ-complex)
    (mjr_dquad_add-data-from-map daDquad f            :data 0      :ano-nam "f"        :ano-typ :ano-typ-complex)
    (mjr_dquad_add-data-from-map daDquad #'abs        :data 1      :ano-nam "f_abs"    :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map daDquad #'phase      :data 1      :ano-nam "f_phase"  :ano-typ :ano-typ-real)
    (if z_color
        (mjr_dquad_add-data-from-map daDquad z_color  :data 0      :ano-nam "z_color"  :ano-typ :ano-typ-color :ano-colorspace :cs-rgb))
    (if zf_color
        (mjr_dquad_add-data-from-map daDquad zf_color :data '(0 1) :ano-nam "zf_color" :ano-typ :ano-typ-color :ano-colorspace :cs-rgb))
    (if f_color
        (mjr_dquad_add-data-from-map daDquad f_color  :data 1      :ano-nam "f_color"  :ano-typ :ano-typ-color :ano-colorspace :cs-rgb))
    daDquad))
