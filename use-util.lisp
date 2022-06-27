;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-util.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Utilities.@EOL
;; @std       Common Lisp
;; @see       tst-util.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2004,2010,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_UTIL
  (:USE :COMMON-LISP
        :MJR_STRING)
  (:DOCUMENTATION "Brief: Utilities.;")
  (:EXPORT #:mjr_util_help
           #:mjr_util_read-csv-file #:mjr_util_write-csv-file
           #:mjr_util_read-file #:mjr_util_write-file
           #:mjr_util_max-print-width
           #:mjr_util_check-thing-and-contents
           #:mjr_util_get-all-elements-or-args
           #:mjr_util_get-kwarg-supplied #:mjr_util_get-kwarg-vals #:mjr_util_strip-kwarg #:mjr_util_strip-nil-val-kwarg
           #:mjr_util_partition-list-if
           #:mjr_util_split-seq-if #:mjr_util_split-seq-on-elt
           #:mjr_util_super-concatenate
           #:mjr_util_random-permute-seq
           #:mjr_util_non-empty-seqp
           #:mjr_util_fun-adapt-eval-v #:mjr_util_func-args-to-vector #:mjr_util_fun-adapt-strap
           #:mjr_util_fun-adapt-x2x    #:mjr_util_fun-hostile-eval    #:mjr_util_fun-adapt-hostile-eval-v
           #:mjr_util_funcall-many-if #:mjr_util_funcall-one-if
           #:mjr_util_func-domain-rect-to-unit
           #:mjr_util_elt-mod
           #:mjr_util_non-list-then-list
           ))

(in-package :MJR_UTIL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_help ()
  "Help for MJR_UTIL:

This packages is a home for all the code that didn't have a better place to live -- just like the stuff in that extra drawer in the kitchen that holds all the
odds and ends.  That's right.  Sometimes things in this package get factored when similar code reaches a critical mass here -- that is how :mjr_part
and :mjr_arr started."
  (documentation 'mjr_util_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_get-all-elements-or-args (first-or-all &rest rest)
  "If (null (car rest)) then the elements of FIRST-OR-ALL are returned.  Otherwise, all the arguments are returned."
  (if (null (car rest))
      (typecase first-or-all
        (list    (values-list first-or-all))
        (vector  (values-list (concatenate 'list first-or-all)))
        (complex (values (realpart first-or-all) (imagpart first-or-all)))
        (t       (values first-or-all)))
      (values-list (append (list first-or-all) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_non-empty-seqp (x)
  "If X is a non-empty sequence (list or vector), then return X's length.  Otherwise, return nil"
  (and (or (listp x) (vectorp x))
       (let ((len (length x)))
         (if (> len 0)
             len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_strip-kwarg (kw-arg &key strip-list keep-list)
  "Given a list of KW arguments, strip off the pairs in strip-list and keep only the ones in keep-list.
If both :STRIP-LIST & :KEEP-LIST are nil, then KW-ARG is returned."
  (if (oddp (length kw-arg))
      (error "mjr_util_strip-kwarg: KW-ARG of odd length -- impossible for key-word/value pairs!"))
  (if (or strip-list keep-list)
      (loop for (s v) in (mapcar 'list (loop for a in kw-arg by #'cddr collect a) (loop for b in (cdr kw-arg) by #'cddr collect b))
            when (not (symbolp s))
            do (error "mjr_util_strip-kwarg: KW-ARG contained a non-keyword argument!")
            when (or (and strip-list (not (member s strip-list)))
                     (and keep-list  (member s keep-list)))
            append (list s v))
      kw-arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_strip-nil-val-kwarg (kw-arg)
  "Given a list of KW arguments, strip off the pairs with a NIL value"
  (if (oddp (length kw-arg))
      (error "mjr_util_strip-nil-val-kwarg: KW-ARG of odd length -- impossible for key-word/value pairs!"))
  (loop for (s v) in (mapcar 'list (loop for a in kw-arg by #'cddr collect a) (loop for b in (cdr kw-arg) by #'cddr collect b))
        when (not (symbolp s))
        do (error "mjr_util_strip-nil-val-kwarg: KW-ARG contained a non-keyword argument!")
        when v
        append (list s v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_get-kwarg-supplied (args-to-find kw-args)
"Like mjr_util_get-kwarg-vals, but instead of returning the value returns 't or NIL depending on if the keyword argument was supplied in the list."
  (values-list (mapcar (lambda (x) (and (member x kw-args) 't)) args-to-find)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_get-kwarg-vals (args-to-return kw-args &optional (error-on-extra-kw-args-with-non-nil-values nil))
  "Given a list of KW arguments, return the values for the arguments named by the rest of the parameters.
Useful for multiple value bind'n argument values."
  (let ((len (length kw-args)))
    (if (oddp len)
        (error "mjr_util_get-kwarg-vals: KW-ARG of odd length -- impossible for key-word/value pairs!")
        (let ((kvp (loop for i from 1 upto (/ len 2)
                         for s = (pop kw-args)
                         for v = (pop kw-args)
                         when (not (symbolp s))
                         do (error "mjr_util_get-kwarg-vals: KW-ARGS contained a non-keyword argument!")
                         if (member s args-to-return)
                         collect (cons s v)
                         else
                         when (and error-on-extra-kw-args-with-non-nil-values v)
                         do (error "mjr_util_get-kwarg-vals: Extra keyword args found with non-nil values (~s)!" s))))
          (values-list (loop for arg in args-to-return
                             collect (cdr (assoc arg kvp))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_max-print-width (in-seq &optional (fmt-str "~a"))
  "Return maximum format (print) width for any of the elements in IN-SEQ using the given FMT-STR."
  (reduce 'max (map 'vector (lambda (x) (length (format nil fmt-str x))) in-seq)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_partition-list-if (the-list the-predicate)
  "Return two lists with the first containing the elements for which the-predicate is true, and the rest in the second."
  (loop for x in the-list
        finally (return (values true-list false-list))
        if (funcall the-predicate x)
        collect x into true-list
        else
        collect x into false-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_split-seq-if (the-seq the-predicate &key show-progress)
  "Split the sequence up on the elements where the predicate is true.  Returns a list of sequences"
  (loop with cur-pos = 0
        with str-len = (length the-seq)
        with max-pos = (1- str-len)
        for nxt-pos = (position-if the-predicate the-seq :start cur-pos)
        do (if show-progress (format 't "~d ~d ~%" (min max-pos cur-pos) nxt-pos))
        collect (subseq the-seq cur-pos (or nxt-pos str-len))
        until (null nxt-pos)
        do (setq cur-pos (1+ nxt-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_split-seq-on-elt (the-seq split-elt &key show-progress)
  "Split the sequence up on the given element.  Returns a list of sequences"
  (mjr_util_split-seq-if the-seq (lambda (x) (equal split-elt x)) :show-progress show-progress))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_read-csv-file (file-name &key
                                         (header-lines-to-skip 1) (max-lines nil) (max-data-lines nil)
                                         (return-as-array nil) (check-field-count 't) (filter-func nil)
                                         (delimiter-char #\,))
  "Read the given CSV file into a list of lists.

Only 'simple CSV' is supported -- no fancy quoting.

Optional keyword arguments:
   :header-lines-to-skip -- The number of lines to skip at the beginning of the file
   :max-lines            -- The maximum number of lines to read -- including header lines
   :max-data-lines       -- The maximum number of lines to read -- excluding header lines
   :check-field-count    -- Error out if a non-header line has a number of fields different from any previous line
   :return-as-array      -- Return as an array instead of a list
   :filter-func          -- Apply to each field before storing in the return list"
  (with-open-file (stream file-name)
    (dotimes (i (or header-lines-to-skip 0))
      (read-line stream nil))
    (let* ((dat-lines       0)
           (fields-per-line nil)
           (the-data       (loop for line = (read-line stream nil)
                                 for line-number from (1+ (or header-lines-to-skip 0))
                                 until (null line)
                                 until (and max-lines (> line-number max-lines))
                                 until (and max-data-lines (> (1+ dat-lines) max-data-lines))
                                 do (incf dat-lines)
                                 collect (let* ((tmp-list (mjr_string_split line delimiter-char))
                                                (list-len (length tmp-list)))
                                           (if (and check-field-count fields-per-line (not (= list-len fields-per-line)))
                                               (error "mjr_util_read-csv-file: Line ~d has ~d fields, but previous lines had ~d fields!"
                                                      line-number list-len fields-per-line)
                                               (setq fields-per-line list-len))
                                           (if filter-func
                                               (mapcar filter-func tmp-list)
                                               tmp-list)))))
      (if return-as-array
          (make-array (list dat-lines fields-per-line) :initial-contents the-data)
          the-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_write-csv-file (data file-name &key (titles nil) (filter-func #'identity) (check-field-count 't))
  "Write the given data into the given file as CSV data.

Only 'simple CSV' is supported -- no fancy quoting.

Optional keyword arguments:
   :titles       -- The titles (a sequence) to print
   :check-field-count    -- Error out if a non-header line has a number of fields different from any previous line
   :filter-func  -- Apply to each DATA fields before writing to the file"
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (if titles
        (let ((num-titles-1 (1- (length titles))))
          (dotimes (i num-titles-1)
            (format stream "~a," (elt titles i)))
          (format stream "~a~%" (elt titles num-titles-1))))
    (let* ((dat-is-matrix   (not (or (vectorp data) (listp data))))
           (dat-lines       (if dat-is-matrix (array-dimension data 0) (length data)))
           (fields-per-line (if dat-is-matrix (array-dimension data 1) (length (elt data 0)))))
      (dotimes (j dat-lines)
        (if (and (not dat-is-matrix) check-field-count (not (= fields-per-line (length (elt data j)))))
            (error "mjr_util_write-csv-file: Element ~d has ~d fields, but a previous element had ~d fields!"
                   j (length (elt data j)) fields-per-line))
        (dotimes (i fields-per-line)
          (format stream "~a~a"
                  (funcall filter-func (if dat-is-matrix (aref data j i) (elt (elt data j) i)))
                  (if (= i (1- fields-per-line)) "" ",")))
        (format stream "~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_super-concatenate (output-type-spec &rest x)
  "Assemble a group of sequences (lists and vectors), and other objects into one big list.  Arrays are flattened.
Example: (mjr_util_super-concatenate 'list 1 '(2) #(3) #2a((5 6)(7 8)) 9) => '(1 2 3 5 6 7 8 9)"
  (apply #'concatenate output-type-spec (mapcar (lambda (y) (typecase y
                                                              (vector y)
                                                              (list  y)
                                                              (array (make-array (apply #'* (array-dimensions y)) :displaced-to y))
                                                              (otherwise (list y))))
                                                x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_random-permute-seq (the-seq &optional (num-swaps nil))
  "Randomly permute the input sequence.  "
  (let* ((new-seq   (concatenate 'vector the-seq))
         (seq-len   (length the-seq))
         (num-swaps (or num-swaps (* 3 seq-len))))
    (dotimes (i num-swaps)
      (rotatef (aref new-seq (random seq-len)) (aref new-seq (random seq-len))))
    (if (listp the-seq)
        (concatenate 'list new-seq)
        new-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_func-args-to-vector (arg-mode &rest rest)
  "Take the args after the first and return a vector.  Used to transform arguments to vector form.

:arg-mode: determines how the vector is formed from rest:
  * :arg-number . Takes one or more distinct numeric arguments and stuff them into a vector
  * :arg-args ... Same as :arg-number
  * :arg-vector . Takes a single vector containing one or more numbers, and returns that vector
  * :arg-list ... Takes a single list containing one or more numbers and stuffs the numbers into a vector
  * :arg-concat . Takes one or more numbers, numeric vectors, & numeric lists and stuffs all the numbers into a single vector"
  (cond ((eq arg-mode :arg-vector)   (first rest))
        ((eq arg-mode :arg-number)   (apply #'vector rest))
        ((eq arg-mode :arg-args)     (apply #'vector rest))
        ((eq arg-mode :arg-list)     (apply #'vector (first rest)))
        ((eq arg-mode :arg-concat)   (apply #'mjr_util_super-concatenate 'vector rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_funcall-one-if (f x)
  "If F is non-NIL, then FUNCALL F on X.  Otherwise return X."
  (if f
      (funcall f x)
      x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_funcall-many-if (f &rest rest)
  "If F is non-NIL, then APPLY F to the rest of the arguments.  Otherwise return arguments after F."
  (if f
      (apply f rest)
      (values-list rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_fun-adapt-eval-v (f x arg-mode)
  "Evaluate the function F (R^N->A) on the vector x.  Usually N>1, but this is not a requirement.

The function specified in the F argument takes a N numeric values.  :ARG-MODE: determines how the values are provided:
  * (arg-mode = :arg-number)  F takes N distinct arguments (each a number) DEFAULT
  * (arg-mode = :arg-args)    Saem as :arg-number
  * (arg-mode = :arg-vector)  F takes a single vector containing N numbers
  * (arg-mode = :arg-list)    F takes a single list of N numbers"
  (let ((arg-mode (or arg-mode :arg-number)))
    (cond ((eq arg-mode :arg-vector)   (funcall f x))
          ((eq arg-mode :arg-number)   (apply   f (concatenate 'list x)))
          ((eq arg-mode :arg-args)     (apply   f (concatenate 'list x)))
          ((eq arg-mode :arg-list)     (funcall f (concatenate 'list x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_check-thing-and-contents (obj obj-pred &optional obj-elt-pred)
  "Return non-NIL if object is non-NIL, OBJ-PRED applied to OBJ is non-NIL, and OBJ-ELT-PRED (when provided) is non-NIL when applied to every element of OBJ.

Ex: Test that obj is a vector of real numbers: (mjr_util_check-type obj #'vectorp #'realp)"
  (and obj
       (funcall obj-pred obj)
       (or (null obj-elt-pred)
          (every obj-elt-pred obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_fun-adapt-hostile-eval-v (f v arg-mode valid-return-pred)
  "Rather like a combination of MJR_UTIL_FUN-HOSTILE-EVAL combined with MJR_UTIL_FUN-ADAPT-EVAL-V."
  (let* ((arg-mode (or arg-mode :arg-number))
         (ret-val  (cond ((eq arg-mode :arg-vector)   (ignore-errors (funcall f v)))
                         ((eq arg-mode :arg-number)   (ignore-errors (apply   f (concatenate 'list v))))
                         ((eq arg-mode :arg-args)     (ignore-errors (apply   f (concatenate 'list v))))
                         ((eq arg-mode :arg-list)     (ignore-errors (funcall f (concatenate 'list v)))))))
    (if (or (not valid-return-pred) (funcall valid-return-pred ret-val))
        ret-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_fun-hostile-eval (valid-return-pred func &rest x)
  "Return value of FUNC applied to X, or NIL when function errors or return valid-return-pred returns NIL on function result."
  (let ((ret-val (ignore-errors (apply func x))))
    (if (or (not valid-return-pred) (funcall valid-return-pred ret-val))
        ret-val)))

  ;; * Real Number           #'realp
  ;; * Real Vector           (lambda (x) (mjr_util_check-thing-and-contents x #'vectorp #'realp))
  ;; * Any Type              nil
  ;; * Number                #'numberp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_fun-adapt-x2x (f from-arg-mode to-arg-mode)
  "Convert a function from one :arg-mode to another."
  (let ((from-arg-mode (or from-arg-mode :arg-number)))
    (cond ((eq to-arg-mode :arg-vector)  (cond ((eq from-arg-mode :arg-vector)   f)
                                               ((eq from-arg-mode :arg-number)   (lambda (v) (apply   f (concatenate 'list v))))
                                               ((eq from-arg-mode :arg-args)     (lambda (v) (apply   f (concatenate 'list v))))
                                               ((eq from-arg-mode :arg-list)     (lambda (v) (funcall f (concatenate 'list v))))))
          ((eq to-arg-mode :arg-list)    (cond ((eq from-arg-mode :arg-vector)   (lambda (L) (funcall f (concatenate 'vector L))))
                                               ((eq from-arg-mode :arg-number)   (lambda (L) (apply   f L)))
                                               ((eq from-arg-mode :arg-args)     (lambda (L) (apply   f L)))
                                               ((eq from-arg-mode :arg-list)     f)))
          ((eq to-arg-mode :arg-number)  (cond ((eq from-arg-mode :arg-vector)   (lambda (&rest rest) (funcall f (concatenate 'vector rest))))
                                               ((eq from-arg-mode :arg-number)   f)
                                               ((eq from-arg-mode :arg-args)     f)
                                               ((eq from-arg-mode :arg-list)     (lambda (&rest rest) (funcall f (concatenate 'list   rest))))))
          ((eq to-arg-mode :arg-args)    (cond ((eq from-arg-mode :arg-vector)   (lambda (&rest rest) (funcall f (concatenate 'vector rest))))
                                               ((eq from-arg-mode :arg-number)   f)
                                               ((eq from-arg-mode :arg-args)     f)
                                               ((eq from-arg-mode :arg-list)     (lambda (&rest rest) (funcall f (concatenate 'list   rest)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_fun-adapt-strap (result-type &rest rest)
  "Take several functions, and return a single function that returns the results from calling all the input functions.

result-type must be one of 'vector, 'list, or 'values

Ex: (mjr_util_fun-adapt-strap 'vector #'sin #'cos) ==> (lambda (x) (vector (cos x) (sin x)))"
  (cond ((eq result-type 'list)    (lambda (&rest fun-args)
                                     (loop for f in rest
                                           collect (apply f fun-args))))
        ((eq result-type 'vector)  (lambda (&rest fun-args)
                                     (loop with r = (make-array (length rest))
                                           for f in rest
                                           for i from 0
                                           do (setf (aref r i) (apply f fun-args))
                                           finally (return r))))
        ((eq result-type 'values)  (lambda (&rest fun-args)
                                     (values-list (loop for f in rest
                                                        collect (apply f fun-args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_elt-mod (the-seq idx &optional seq-len)
  (elt the-seq (mod idx (or seq-len (length the-seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_func-domain-rect-to-unit (f v-min v-max arg-mode)
  "Shift function input domain to the unit interval, square, cube, etc... Useful for plotting."
  (lambda (&rest rest)
    (mjr_util_fun-adapt-eval-v f
                               (map 'vector #'/
                                    (map 'vector #'- (apply #'mjr_util_func-args-to-vector arg-mode rest)  v-min)
                                    (map 'vector #'- v-max                                                 v-min))
                               arg-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_read-file (file-name)
  "Read the entire content of the file into a string."
  (with-open-file (stream file-name)
    (let ((da-string (make-string (file-length stream))))
      (read-sequence da-string stream)
      da-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_write-file (file-name da-string)
  "Write the given string into the given file as CSV data."
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence da-string stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_util_non-list-then-list (thingy)
  "If THINGY is a list, then return it.  Otherwise return (LIST THINGY)"
  (if (listp thingy)
      thingy
      (list thingy)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun mjr_util_elt-to-str (expr)
;;   "Recursively find every non-list thing in EXPR, and convert it to a string"
;;   (typecase expr
;;     (symbol     (format nil "~a" expr))
;;     (list       (mapcar #'mjr_mxp_elt-to-str expr))
;;     (otherwise  (format nil "~a" expr))))

