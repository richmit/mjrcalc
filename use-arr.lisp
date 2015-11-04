;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-arr.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2004,2010,2012,2013 by Mitch Richling.  All rights reserved.
;; @brief     Array utilities.@EOL
;; @Keywords  lisp interactive math arrays
;; @Std       Common Lisp
;;
;;            TODO: Make the "special array element indexing techniques" setf'able.
;;

;;----------------------------------------------------------------------------------------------------------------------------------

(defpackage :MJR_ARR
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Array utilities.;")
  (:EXPORT #:mjr_arr_help
           ;; Handy array dim query functions
           #:mjr_arr_rank-is #:mjr_arr_num-rows #:mjr_arr_num-cols
           ;; every and friends
           #:mjr_arr_every #:mjr_arr_some #:mjr_arr_notevery #:mjr_arr_notany

           ;; Vector indexing with out-of-bound behavior control
           #:mjr_arr_svref-mod #:mjr_arr_aref-vector-clip
           ;; General array indexing with out-of-bound behavior control
           #:mjr_arr_aref-mod #:mjr_arr_aref-clip
           ;; Addressing of general arrays as vectors
           #:mjr_arr_aref-as-vector
           ;; Addressing of general arrays as vectors with out-of-bound behavior control
           #:mjr_arr_aref-as-vector-mod #:mjr_arr_aref-cmo-as-vector-mod
           ;; Vector indexing with vectors (R-like)
           #:mjr_arr_svref-via-bit-seq #:mjr_arr_svref-via-bol-seq #:mjr_arr_svref-via-int-seq
           ;; Sub-array: General Case
           #:mjr_arr_get-subarray
           ;; Sub-array: Special Cases
           #:mjr_arr_get-subarray2 #:mjr_arr_set-subarray2
           ;; Slabs: General Case
           #:mjr_arr_get-slab
           ;; Slabs: Special Cases
           #:mjr_arr_get-row #:mjr_arr_get-rows #:mjr_arr_get-col #:mjr_arr_get-cols
           ;; Reshape
           #:mjr_arr_reflow
           ;; Copy: General case and a special case
           #:mjr_arr_copy #:mjr_arr_copy2
           ;; Reshape in-place
           #:mjr_arr_nreflow #:mjr_arr_nreflow-to-vec #:mjr_arr_nreflow-min-rank
           ;; Create & fill
           #:mjr_arr_make-and-fill
           ;; Map: General case
           #:mjr_arr_map
           ;; Map: Special cases
           #:mjr_arr_unary-map2 #:mjr_arr_binary-map2
           ;; Glue Slabs: Special Cases (R-like)
           #:mjr_arr_rbind #:mjr_arr_cbind
           ;; Some computations
           #:mjr_arr_min-max
           ))

(in-package :MJR_ARR)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_help ()
  "Help for MJR_ARR: ARRay tools

This package contains array functionality that is useful for other packages."
  (documentation 'mjr_arr_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_rank-is (rank obj)
  "Non-NIL if obj is an array of the given rank."
  (and (arrayp obj)
       (= (array-rank obj) rank)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_num-rows (an-array)
  "Number of rows in an array (i.e. 1st array dimension -- that is dim 0 in array-dimension speak)"
  (array-dimension an-array 0))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_num-cols (an-array)
  "Number of columns in an array (i.e. 2nd array dimension -- that is dim 1 in array-dimension speak)"
  (array-dimension an-array 1))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_nreflow (old-array new-dims)
  "Return new array (with possibly diffrent diminitions) offset to old-array -- NO DATA IS COPIED!"
  (make-array new-dims :displaced-to old-array))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_nreflow-to-vec (old-array)
  "Return new vector offset to old-array -- NO DATA IS COPIED!"
  (make-array (array-total-size old-array) :displaced-to old-array))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_nreflow-min-rank (an-array)
  "Return new array offset to old-array such that the new array has minimal rank -- NO DATA IS COPIED!"
  (mjr_arr_nreflow an-array (or (delete-if (lambda (x) (= 1 x)) (array-dimensions an-array)) '(1))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_reflow (old-array new-dims)
  "Create a new array (possibly with different dimensions), and copy the contents of old-array in row major order.

The number of dimensions of the arrays need not be the same (a matrix may be reflowed into a vector for example).  The arrays
need not even contain the same number of elements -- if old-array is bigger, then some data will simply not be copied; if
old-array is smaller, then some array elements in the new array will not be initialized."
  (cond ((some #'zerop new-dims)              (error "mjr_arr_reflow: ERROR: No dimension may be zero!"))
        ((some (lambda (x) (< x 0)) new-dims) (error "mjr_arr_reflow: ERROR: No dimension may be negative!"))
        ((zerop (reduce #'* new-dims))        (error "mjr_arr_reflow: ERROR: New array is of zero size")))
  (let* ((old-nelt   (array-total-size old-array))
         (new-nelt   (apply #'* new-dims)))
    (cond ((< new-nelt old-nelt) (warn  "mjr_arr_reflow: WARNING: New array is too small!"))
          ((> new-nelt old-nelt) (warn  "mjr_arr_reflow: WARNING: New array is too big!")))
    (let ((new-array (make-array new-dims)))
      (loop with old-array-i  = (make-array old-nelt :displaced-to old-array)
            with new-array-i  = (make-array new-nelt :displaced-to new-array)
            for i from 0 upto (1- (min new-nelt old-nelt))
            for ov across old-array-i
            do (setf (aref new-array-i i) ov)
            finally (return new-array)))))

;;------------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_map (func &rest arrays)
  "MAP for arrays.  Return array will have same dimensions as the first array.  Arrays need not have the same element count."
  (let* ((dims (array-dimensions (first arrays)))
         (nelt (apply #'* dims))
         (nvec (apply #'map 'vector func (mapcar #'mjr_arr_nreflow-to-vec arrays)))
         (vlen (length nvec)))
    (if (= nelt vlen)
        (mjr_arr_nreflow nvec dims)
        (mjr_arr_reflow  nvec dims))))

;;------------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_as-vec-map (func &rest arrays)
  "Like mjr_arr_map, but returns a vector -- (mjr_arr_nreflow-to-vec (mjr_arr_map func array ...)), but faster."
  (apply #'map 'vector func (mapcar #'mjr_arr_nreflow-to-vec arrays)))

;;------------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_binary-map2 (an-array another-array func)
  "MAP for a pair of 2D array -- NOTE: func is LAST argument, not the first.
An error will be signaled if the two arrays have different dimensions even if both arrays have the same number of elements."
  (let* ((dims      (array-dimensions an-array))
         (new-array (make-array dims)))
    (if (not (equal dims (array-dimensions another-array)))
        (error "mjr_arr_binary-map2: ERROR: Incompatible array dimensions!")
        (destructuring-bind (rows cols) dims
          (dotimes (row rows new-array)
            (dotimes (col cols)
              (setf (aref new-array row col) (funcall func (aref an-array row col) (aref another-array row col)))))))))

;;------------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_unary-map2 (an-array func)
  "MAP for a single 2D array -- NOTE: func is LAST argument, not the first."
  (let* ((dims      (array-dimensions an-array))
         (new-array (make-array dims)))
    (destructuring-bind (rows cols) dims
      (dotimes (row rows new-array)
        (dotimes (col cols)
          (setf (aref new-array row col) (funcall func (aref an-array row col))))))))

;;------------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_every (predicate &rest arrays)
  "Like 'every', but with arrays of arbitrary rank and dimensions (but they all should have the same number of elements)"
  (apply #'every predicate (mapcar #'mjr_arr_nreflow-to-vec arrays)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_some (predicate &rest arrays)
  "Like 'some', but with arrays of arbitrary rank and dimensions (but they all should have the same number of elements)"
  (apply #'some predicate (mapcar #'mjr_arr_nreflow-to-vec arrays)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_notevery (predicate &rest arrays)
  "Like 'notevery', but with arrays of arbitrary rank and dimensions (but they all should have the same number of elements)"
  (apply #'notevery predicate (mapcar #'mjr_arr_nreflow-to-vec arrays)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_notany (predicate &rest arrays)
  "Like 'notany', but with arrays of arbitrary rank and dimensions (but they all should have the same number of elements)"
  (apply #'notany predicate (mapcar #'mjr_arr_nreflow-to-vec arrays)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_copy (an-array)
  "Construct a new array and copy the elements from an-array into it."
  (mjr_arr_reflow an-array (array-dimensions an-array)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_copy2 (an-array)
  "Make a copy of the given 2D array.  Just like mjr_arr_copy, but faster for this special case."
  (let* ((dims      (array-dimensions an-array))
         (new-array (make-array dims)))
    (destructuring-bind (rows cols) dims
      (dotimes (row rows new-array)
        (dotimes (col cols)
          (setf (aref new-array row col) (aref an-array row col)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_svref-mod (an-array subscript &optional an-array-length)
  "Index AN-ARRAY, but modulous SUBSCRIPT to AN-ARRAY-LENGTH.  AN-ARRAY-LENGTH is computed if not provided."
  (aref an-array (mod subscript (or an-array-length (length an-array)))))

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (defsetf mjr_arr_svref-mod (an-array subscript &optional an-array-length) (val-to-store)
;;   `(setf (aref ,an-array (mod ,subscript (or ,an-array-length (length ,an-array)))) ,val-to-store))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_svref-clip (an-array subscript &optional an-array-length)
  "Index A, but clip SUBSCRIPT to [0,AN-ARRAY-LENGTH).  AN-ARRAY-LENGTH is computed if not provided."
  (aref an-array (max 0 (min subscript (1- (or an-array-length (length an-array)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_aref-mod (an-array &rest subscripts)
  "aref with recycle on each dimension."
  (apply #'aref an-array (mapcar #'mod subscripts (array-dimensions an-array))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_aref-clip (an-array &rest subscripts)
  "aref with bound clipping on each dimension."
  (apply #'aref an-array (mapcar (lambda (i m) (max 0 (min i (1- m)))) subscripts (array-dimensions an-array))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_aref-as-vector (an-array subscript)
  "Return the subscript'th element of an-array -- in row major order."
  (if (vectorp an-array)
      (svref an-array subscript)
      (aref (make-array (array-total-size an-array) :displaced-to an-array) subscript)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_aref-as-vector-mod (an-array subscript)
  "Return the subscript'th element of an-array -- in row major order.  Out of bounds subscripts are wrapped."
  (let ((dims (array-dimensions an-array)))
    (if (vectorp an-array)
        (aref an-array subscript)
        (let ((nelt (apply #'* dims)))
          (aref (make-array (apply #'* dims) :displaced-to an-array) (mod subscript nelt))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_aref-cmo-as-vector-mod (an-array subscript)
  "Return the subscript'th element of an-array -- in row major order.  Out of bounds subscripts are clipped."
  (apply #'aref an-array (loop for j from 0
                               for d-prod = 1 then (* d-prod d)
                               for d in (array-dimensions an-array)
                               collect (mod (truncate subscript d-prod) d))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_svref-via-bit-seq (an-array idx)
  "Index vector with an bitmask -- like R indexing with a boolean vector, but idx is NOT recycled"
  (let ((an-array  (if (vectorp an-array)
                       an-array
                       (make-array (array-total-size an-array) :displaced-to an-array)))
        (new-array (make-array (count-if-not #'zerop idx))))
    (loop with nidx = -1
          for b across idx
          for e across an-array
          when (not (zerop b))
          do (setf (aref new-array (incf nidx)) e)
          finally (return new-array))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_svref-via-bol-seq (an-array idx)
  "Index vector with an boolean vector -- like R, but idx is NOT recycled"
  (let ((an-array  (if (vectorp an-array)
                       an-array
                       (make-array (array-total-size an-array) :displaced-to an-array)))
        (new-array (make-array (count-if #'identity idx))))
    (if (vectorp idx)
        (loop with nidx = -1
              for b across idx
              for e across an-array
              when b
              do (setf (aref new-array (incf nidx)) e)
              finally (return new-array))
        (loop with nidx = -1
              for b in idx
              for e across an-array
              when b
              do (setf (aref new-array (incf nidx)) e)
              finally (return new-array)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_svref-via-int-seq (an-array idx)
  "Index vector with an integer vector -- like R, but an-array and idx are NOT recycled"
  (let ((an-array  (if (vectorp an-array)
                                an-array
                                (make-array (array-total-size an-array) :displaced-to an-array)))
        (new-array (make-array (length idx))))
    (if (vectorp idx)
        (loop with nidx = -1
              for i across idx
              do (setf (aref new-array (incf nidx)) (aref an-array i))
              finally (return new-array))
        (loop with nidx = -1
              for i in idx
              do (setf (aref new-array (incf nidx)) (aref an-array i))
              finally (return new-array)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defmacro mjr_arr_fill-sub-array (da-arr da-sub-ranges)
  "Used by mjr_arr_get-subarray to fill the subarray from the origonal array"
  (let* ((num-vec (length da-sub-ranges))
         (dims    (array-dimensions da-arr))
         (ranges  (loop for lv in da-sub-ranges
                        for dv in dims
                        for dv-1 = (1- dv)
                        for ll = (typecase lv
                                   (number lv)
                                   (null   0)
                                   (list   (or (first lv) 0)))
                        for ul = (typecase lv
                                   (number lv)
                                   (null   dv-1)
                                   (list   (or (second lv) dv-1)))
                        collect (list ll ul)))
         (ovdim   (loop for lv in ranges
                        collect (1+ (- (second lv) (first lv)))))
         (ini-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "ini-~d-" i))))
         (outi-vl (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "outi-~d-" i))))
         (ov-v    (gensym "ov-"))
         (dc      `(setf (aref ,ov-v ,@outi-vl) (aref ,da-arr ,@ini-vl))))
    (loop for i from 0 upto (1- num-vec)
          for lv in ranges
          for ov in ovdim
          do (setq dc `(loop for ,(elt ini-vl i) from ,(first lv) upto ,(second lv)
                             for ,(elt outi-vl i) upfrom 0
                             do ,dc)))
    `(let ((,ov-v (make-array (list ,@ovdim))))
       ,dc
       ,ov-v)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-subarray (an-array &rest subscript-ranges)
  "Return a copy of the subarray.

An argument is required for each dimension.  A list will be interpreted as '(min-index max-index), a nil will be interpreted as
the entire range for that dimension, and a number will be interpreted as a single index value.

Examples:
  (defvar a #2a((11 12 13 14)(21 22 23 24)(31 32 33 34)(41 42 43 44)))
  (defvar b #3a(((11 12 13)(14 15 16)(17 18 19))((21 22 23)(24 25 26)(27 28 29))((31 32 33)(34 35 36)(37 38 39))))
  (mjr_arr_get-subarray a '(1 2) '(1 2))     => #2A((22 23) (32 33))
  (mjr_arr_get-subarray a '(1 2) '(1 1))     => #2A((22) (32))
  (mjr_arr_get-subarray a '(1 1) '(1 2))     => #2A((22 23))
  (mjr_arr_get-subarray a '(1 1) '(1 1))     => #2A((22))
  (mjr_arr_get-subarray b    nil    nil nil) => b
  (mjr_arr_get-subarray b      1    nil nil) => #3A(((21 22 23) (24 25 26) (27 28 29)))
  (mjr_arr_get-subarray b      1    nil   1) => #3A(((22) (25) (28)))"
    (eval `(mjr_arr_fill-sub-array ,an-array ,subscript-ranges)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-subarray2 (an-array row0 col0 row1 col1)
  "Return a copy of the subarray -- like mjr_arr_get-subarray, but faster for this special case.

row1 and col1 will be set to the maximum value if they are nil or -1."
  (destructuring-bind (rows cols) (array-dimensions an-array)
    (let* ((row1 (if (or (null row1) (> 0 row1)) (1- rows) row1))
           (col1 (if (or (null col1) (> 0 col1)) (1- cols) col1))
           (nrows    (1+ (- row1 row0)))
           (ncols    (1+ (- col1 col0))))
      (cond ((< row1 row0)      (error "mjr_arr_get-subarray2: ROW1 may not be larger than ROW0!"))
            ((< col1 col0)      (error "mjr_arr_get-subarray2: COL1 may not be larger than COL0!"))
            ((< row0 0)         (error "mjr_arr_get-subarray2: ROW0 must be positive!"))
            ((< col0 0)         (error "mjr_arr_get-subarray2: COL0 must be positive!"))
            ((< rows 1)         (error "mjr_arr_get-subarray2: AN-ARRAY must not be empty!"))
            ((< cols 1)         (error "mjr_arr_get-subarray2: AN-ARRAY must not be empty!"))
            ((< (1- rows) row0) (error "mjr_arr_get-subarray2: ROW0 too large!"))
            ((< (1- cols) col0) (error "mjr_arr_get-subarray2: COL0 too large!"))
            ((< (1- rows) row1) (error "mjr_arr_get-subarray2: ROW1 too large!"))
            ((< (1- cols) col1) (error "mjr_arr_get-subarray2: COL1 too large!")))
      (let ((new-array (make-array (list nrows ncols))))
        (dotimes (row nrows new-array)
          (dotimes (col ncols)
            (setf (aref new-array row col) (aref an-array (+ row0 row) (+ col0 col)))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_set-subarray2 (an-array row0 col0 submat)
  "Place the matrix submat at row0 by col0 in matrix.  DEPRECATED

DEPRECATED: In the future this function will be replaced by a setf expansion for mjr_arr_get-subarray2."
  (destructuring-bind (nrows ncols) (array-dimensions submat)
    (dotimes (row nrows an-array)
      (dotimes (col ncols)
        (setf (aref an-array (+ row0 row) (+ col0 col)) (aref submat row col))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-slab (an-array &rest subscripts)
  "Extract the slab (reduced rank sub-array).

Slabs are obtained by extracting a sub-array such that every dimension is of length 1 or the same length as the original array,
and then removing all dimensions of length 1 (i.e. use mjr_arr_nreflow-min-rank).  For example, rank 2 arrays only have two
kinds of slabs: rows & columns.

An argument is required for each dimension.  Each argument must be an integer or nil -- an integer picks the index for that
dimension while a nil indicates that the entire dimension will be selected.

Examples:
  (defvar a #2a((11 12 13 14)(21 22 23 24)(31 32 33 34)(41 42 43 44)))
  (defvar b #3a(((11 12 13)(14 15 16)(17 18 19))((21 22 23)(24 25 26)(27 28 29))((31 32 33)(34 35 36)(37 38 39))))
  (mjr_arr_get-slab a   1 nil)     => #(21 22 23 24)
  (mjr_arr_get-slab a nil   1)     => #(12 22 32 42)
  (mjr_arr_get-slab b nil nil nil) => b
  (mjr_arr_get-slab b   1 nil nil) => #2A((21 22 23) (24 25 26) (27 28 29))
  (mjr_arr_get-slab b   1 nil   1) => #(22 25 28)"
  (if (notevery (lambda (x) (or (integerp x) (null x))) subscripts)
      (error "mjr_arr_get-slab: Subscripts must be integers and/or NIL")
      (mjr_arr_nreflow-min-rank (apply #'mjr_arr_get-subarray an-array subscripts))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-row (an-array the-row)
  "Extract a row (as a vector) of a 2D array -- see mjr_arr_get-slab for the general case."
  (let* ((cols      (array-dimension an-array 1))
         (new-array (make-array cols)))
    (dotimes (col cols new-array)
      (setf (aref new-array col) (aref an-array the-row col)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-rows (an-array)
  "Return a list of the rows (as vectors) of a 2D array"
  (loop for i from 0 upto (1- (array-dimension an-array 0))
        collect (mjr_arr_get-row an-array i)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-col (an-array the-col)
  "Extract a col (as a vector) of a 2D array -- see mjr_arr_get-slab for the general case."
  (let* ((rows      (array-dimension an-array 0))
         (new-array (make-array rows)))
    (dotimes (row rows new-array)
      (setf (aref new-array row) (aref an-array row the-col)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_get-cols (an-array &optional the-cols)
  "Return a list of the cols (as vectors) of a 2D array.  

The value for THE-COLS the column number, a list of column numbers, or NIL (all columns)"
  (if the-cols
      (typecase the-cols
        (integer (list (mjr_arr_get-col an-array the-cols)))
        (list    (loop for i in the-cols
                       collect (mjr_arr_get-col an-array i)))
        (otherwise (error "mjr_arr_get-cols: THE-COLS must be an integer or list of integers")))
      (loop for i from 0 upto (1- (array-dimension an-array 1))
            collect (mjr_arr_get-col an-array i))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_cbind (&rest arrays)
  "Creates an array from the columns in a list of 1D & 2D arrays (vectors are assumed to be column vectors)!

If one argument is provided and it is a list, then it is assumed that it is a list of arrays to be bound."
  (let ((first-a   (first arrays)))
    (if (and arrays (listp first-a) (null (cdr arrays)))
        (apply #'mjr_arr_cbind first-a)
        (let* ((cols-l    (mapcar (lambda (a) (if (vectorp a) 1 (array-dimension a 1))) arrays))
               (rows      (if (vectorp first-a) (length first-a) (array-dimension first-a 0)))
               (new-array (make-array (list rows (apply #'+ cols-l)))))
          (loop with tcol = 0
                for cols in cols-l
                for sarr in arrays
                do (if (vectorp sarr)
                       (progn
                         (dotimes (row rows)
                           (setf (aref new-array row tcol) (aref sarr row)))
                         (incf tcol))
                       (dotimes (scol cols)
                         (dotimes (row rows)
                           (setf (aref new-array row tcol) (aref sarr row scol)))
                         (incf tcol)))
                finally (return new-array))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_rbind (&rest arrays)
  "Creates an array from the rows in a list of 1D & 2D arrays (vectors are assumed to be row vectors)!

If one argument is provided and it is a list, then it is assumed that it is a list of arrays to be bound."
  (let ((first-a   (first arrays)))
    (if (and arrays (listp first-a) (null (cdr arrays)))
        (apply #'mjr_arr_rbind first-a)
        (let* ((rows-l    (mapcar (lambda (a) (if (vectorp a) 1 (array-dimension a 0))) arrays))
               (cols      (if (vectorp first-a) (length first-a) (array-dimension first-a 1)))
               (new-array (make-array (list (apply #'+ rows-l) cols))))
          (loop with trow = 0
                for rows in rows-l
                for sarr in arrays
                do (if (vectorp sarr)
                       (progn
                         (dotimes (col cols)
                           (setf (aref new-array trow col) (aref sarr col)))
                         (incf trow))
                       (dotimes (srow rows)
                         (dotimes (col cols)
                           (setf (aref new-array trow col) (aref sarr srow col)))
                         (incf trow)))
                finally (return new-array))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_make-and-fill (dims &rest data)
  "Create a new matrix and fill it with the data in the given sequence arguments (numbers, lists, vectors, and/or arrays)."
  (let* ((new-array   (make-array dims))
         (new-array-v (mjr_arr_nreflow-to-vec new-array))
         (idx         -1))
    (dolist (obj data new-array)
      (typecase obj
        (vector    (loop for d across obj
                         do (setf (aref new-array-v (incf idx)) d)))
        (list      (loop for d in obj
                         do (setf (aref new-array-v (incf idx)) d)))
        (array     (loop for d across (mjr_arr_nreflow-to-vec obj)
                         do (setf (aref new-array-v (incf idx)) d)))
        (otherwise (setf (aref new-array-v (incf idx)) obj))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_arr_min-max (an-arr &optional map-fun)
  "Compute the max/min of all the values in an-arr."
  (loop for z across (make-array (apply #'* (array-dimensions an-arr)) :displaced-to an-arr)
        for zf = (if map-fun (funcall map-fun z) z)
        maximize zf into zmax
        minimize zf into zmin
        finally (return (values zmin zmax))))
