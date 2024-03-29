;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-combc.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Constructive Combinatorics: Generating combinatorial objects.@EOL
;; @std       Common Lisp
;; @see       tst-combc.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2011,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Better test coverage.@EOL@EOL
;; @warning   While mjr_combc_gen-all-cross-product is qute well tested tested, the rest of the library has quite poor coverage.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_COMBC
  (:USE :COMMON-LISP
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Constructive Combinatorics: Generating combinatorial objects.;")
  (:EXPORT #:mjr_combc_help
           ;; Permutations
           #:mjr_combc_gen-all-permutations-gry   #:mjr_combc_gen-rand-permutation     #:mjr_combc_gen-all-permutations-lex
           ;; Cross Powers
           #:mjr_combc_gen-all-cross-power        #:mjr_combc_gen-rand-cross-power
           ;; Cross Products
           #:mjr_combc_gen-all-cross-product      #:mjr_combc_gen-rand-cross-product
           ;; Combinations
           #:mjr_combc_gen-all-combinations       #:mjr_combc_gen-rand-combinations
           ;; Subsets
           #:mjr_combc_gen-all-subsets            #:mjr_combc_gen-rand-subsets
           ;; Integer Partitions
           #:mjr_combc_gen-all-partitions-revlex
           #:mjr_combc_gen-all-k-partitions-colex
           ;; Not Exported:
           ;; #:mjr_combc_gen-all-cross-product-array
           ;; #:mjr_combc_gen-all-cross-product-i-array
           ;; #:mjr_combc_gen-all-cross-product-table
           ))

(in-package :MJR_COMBC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_help ()
  "Help for MJR_COMBC: Constructive Combinatorics

Two types of generating functions: mjr_combc_gen-all-* & mjr_combc_gen-rand-*

The first will generate all objects of a particular class (permutations, cross product elements, combinations, and subsets) while
the second will generate random objects of a particular class such that any object in the class is equally likely to be generated.

Base spaces are specified directly via sequences or as integers ($n$ represents the set $[n] = \{0,1,\cdots,n\}$).

Generated objects can take one of three forms:
  * Vectors of elements from base spaces specified as vectors
  * Vectors of integers from base spaces specified as integers (i.e. base spaces are of the form $Z_n$)
  * Bit vectors representing bitmasks (representing subsets)

All generating functions have some setup required at the start, but the generation loops are generally quite tight.  It is best to
make use of :collect-value, :collect-if, :exit-if, & :func arguments to process the generated objects internal to the generating
function rather than calling the generating function repeatedly or processing after generating all objects.

Processing loop
   1) Object is generated
   2) If :func is non-NIL, then it is evaluated on object.  The :arg-mode always applies to :func.
   3) If :pre-if-filter is non-NIL, then it is evaluated on object.  The :arg-mode always applies to :pre-if-filter
   4) If :collect-if is non-NIL, it is evaluated on the :pre-if-filter result or object if :pre-if-filter was nil
      If :pre-if-filter is nil, then :arg-mode applies to :collect-if
   5) If :collect-if was non-NIL and returned non-NIL, or if :collect-if was nil and :collect-value was non-NIL
      then :collect-value is applied to the object and the return value is stored for later return
      The :arg-mode always applies to :collect-value.
   6) If :exit-if was non-NIL, it is evaluated on the :pre-if-filter result or object if :pre-if-filter was nil.
      If :pre-if-filter is nil, then :arg-mode applies to :exit-if
      If the return from :exit-if was non-nil, then the function returns.  If :collect-value was non-nil, then
      the collected objects are returned.  Otherwise the last object generated is returned.

Other args:
  * :arg-mode      -- How are args given to funcs
  * :show-progress -- show progress as objects are generated

Note: :collect-if REQUIRES the :collect-value -- otherwise the objects collected will be temporaries."
  (documentation 'mjr_combc_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-permutations-gry (length-or-seq &key
                                       (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                       (arg-mode :arg-vector) (show-progress nil))
  "Generate all permutations.  See: mjr_combc_help.

Example: Find all permutations of #(0 1 2 3) that have a 1 in the 3rd position.
  (mjr_combc_gen-all-permutations-gry 4
                                  :collect-value #'copy-seq
                                  :collect-if (lambda (v) (= (aref v 2) 1)))
Example: Same thing, but with a :pre-if-filter
  (mjr_combc_gen-all-permutations-gry 4
                                  :collect-value #'copy-seq
                                  :pre-if-filter (lambda (v) (aref v 2))
                                  :collect-if (lambda (x) (= x 1)))

References:
  Richard A. Brualdi (1999); Introductory Combinatorics 3rd; pp 86"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-all-permutations-gry: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((n       (if (numberp length-or-seq) length-or-seq (length length-or-seq)))
         (perm-i (make-array n :initial-contents (loop for i from 0 upto (1- n) collect i)))
         (perm-e (if (not (numberp length-or-seq))
                     (concatenate 'vector length-or-seq)))
         (perm-d (make-array n :initial-element 0 :element-type 'bit))
         (perms  nil))
    (loop with maxi2 = nil
          with maxv = -1
          with maxi1 = nil
          for j from 0
          finally (return perms)
          do (if show-progress (format 't "mjr_combc_gen-all-permutations-gry: ~10s ~s ~s~%" j perm-i perm-e))
          do (if func
                 (mjr_util_fun-adapt-eval-v func (or perm-e perm-i) arg-mode))
          do (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or perm-e perm-i) arg-mode))))
               (if (if collect-if
                       (if pre-if-filter
                           (funcall collect-if pf)
                           (mjr_util_fun-adapt-eval-v collect-if (or perm-e perm-i) arg-mode))
                       collect-value)
                   (push (mjr_util_fun-adapt-eval-v collect-value (or perm-e perm-i) arg-mode) perms))
               (if (and exit-if (if pre-if-filter
                                    (funcall exit-if pf)
                                    (mjr_util_fun-adapt-eval-v exit-if (or perm-e perm-i) arg-mode)))
                   (if (or collect-value collect-if)
                       (return perms)
                       (return (or perm-e perm-i)))))
          do (setf maxv -1
                   maxi2 nil
                   maxi1 0)
          until (null (loop for curv across perm-i
                            for cur from 0
                            for nxt = (if (zerop (bit perm-d cur))
                                          (if (< 0      cur) (1- cur))
                                          (if (> (1- n) cur) (1+ cur)))
                            for nxtv = (and nxt (aref perm-i nxt))
                            finally (return maxi2)
                            when (and nxt (> curv nxtv) (< maxv curv))
                            do (setf maxi2 nxt
                                     maxi1 cur
                                     maxv  curv)))
          do (if perm-e
                 (rotatef (aref perm-e maxi1) (aref perm-e maxi2)))
          do (rotatef     (aref perm-i maxi1) (aref perm-i maxi2))
          do (rotatef     (aref perm-d maxi1) (aref perm-d maxi2))
          do (loop for curv across perm-i
                   for cur from 0
                   when (< maxv curv)
                   do (setf (bit perm-d cur) (if (zerop (bit perm-d cur)) 1 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-permutations-lex (length-or-seq
                                           &key
                                           (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                           (arg-mode :arg-vector) (show-progress nil))
  "Generate and process permutations in lexicographic order.  See: mjr_combc_gen-all-permutations-gry & mjr_combc_help.

Works like mjr_combc_gen-all-permutations-gry with the following exception:

   * If a sequence is provided for length-or-seq, then it MUST
     * Only contain only FIXNUM values
     * The elements must be ordered with respect to #'<
   * Works on multi-sets: Example: #(1 2 2 3) will generate
       #(1 2 2 3) #(1 2 3 2) #(1 3 2 2) #(2 1 2 3) #(2 1 3 2) #(2 2 1 3)
       #(2 2 3 1) #(2 3 1 2) #(2 3 2 1) #(3 1 2 2) #(3 2 1 2) #(3 2 2 1)

References:
  Knuth; The Art of Computer Programming, Volume 4A, Combinatorial Algorithms, Part 1; Section 7.2.1.2; page 319"
  (flet ((nextLexPerm (daPerm)
           (let* ((n (length daPerm))
                  (j (loop for j from (- n 2) downto 0
                           when (< (aref daPerm j) (aref daPerm (1+ j)))
                           do (return j))))
             (if j (let ((l (loop for l downfrom (1- n)
                                  when (< (aref daPerm j) (aref daPerm l))
                                  do (return l))))
                     (rotatef (aref daPerm j) (aref daPerm l))
                     (loop for k from (1+ j)
                           for l downfrom (1- n)
                           until (>= k l)
                           do (rotatef (aref daPerm k) (aref daPerm l)))
                     daPerm)))))
    (if (and collect-if (not collect-value))
        (error "mjr_combc_gen-all-permutations-lex: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
    (let* ((n       (if (numberp length-or-seq) length-or-seq (length length-or-seq)))
           (perm-ioe (if (not (numberp length-or-seq))
                        (concatenate 'vector length-or-seq)
                        (make-array n :initial-contents (loop for i from 0 upto (1- n) collect i))))
           (perms   nil))
      (loop for j from 0
            finally (return perms)
            do (if show-progress (format 't "mjr_combc_gen-all-permutations-lex: ~10s ~s~%" j perm-ioe))
            do (if func
                   (mjr_util_fun-adapt-eval-v func perm-ioe arg-mode))
            do (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter perm-ioe arg-mode))))
                 (if (if collect-if
                         (if pre-if-filter
                             (funcall collect-if pf)
                             (mjr_util_fun-adapt-eval-v collect-if perm-ioe arg-mode))
                         collect-value)
                     (push (mjr_util_fun-adapt-eval-v collect-value perm-ioe arg-mode) perms))
                 (if (and exit-if (if pre-if-filter
                                      (funcall exit-if pf)
                                      (mjr_util_fun-adapt-eval-v exit-if perm-ioe arg-mode)))
                     (if (or collect-value collect-if)
                         (return perms)
                         (return perm-ioe))))
            while (nextLexPerm perm-ioe)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_combc_gen-all-cross-product-array (fn am &rest vecs)
  "Return an array produced by evaluating FN on the elements of the cross product of the given vectors.  am is the argument mode."
  (let* ((num-vec (length vecs))
         (arr-v   (gensym "arr-"))
         (vei-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vei-~d-" i))))
         (vev-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vev-~d-" i))))
         (dc      (case am
                    (:arg-vector `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (vector ,@vev-vl))))
                    (:arg-number `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vev-vl)))
                    (:arg-args   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vev-vl)))
                    (:arg-list   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (list ,@vev-vl)))))))
    (loop for i downfrom (1- num-vec) to 0
          do (setq dc `(loop for ,(elt vev-vl i) across ,(elt vecs i)
                             for ,(elt vei-vl i) upfrom 0
                             do ,dc)))
    `(let ((,arr-v (make-array (list ,@(mapcar #'length vecs)))))
       ,dc
       ,arr-v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_combc_gen-all-cross-product-i-array (fn am &rest lengths)
  "Instead explicit vectors, this macro takes integers representing the lengths of each vector -- i.e. VVEC '(:len length)"
  (let* ((num-vec (length lengths))
         (arr-v   (gensym "arr-"))
         (vei-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vei-~d-" i))))
         (dc      (case am
                    (:arg-vector `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (vector ,@vei-vl))))
                    (:arg-number `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vei-vl)))
                    (:arg-args   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn ,@vei-vl)))
                    (:arg-list   `(setf (aref ,arr-v ,@vei-vl) (funcall ,fn (list ,@vei-vl)))))))
    (loop for i downfrom (1- num-vec) to 0
          do (setq dc `(loop for ,(elt vei-vl i) upfrom 0 to (1- ,(elt lengths i))
                             do ,dc)))
    `(let ((,arr-v (make-array ',lengths)))
       ,dc
       ,arr-v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_combc_gen-all-cross-product-table (fn am &rest vecs)
  (let* ((num-vec (length vecs))
         (table-v (gensym "table-"))
         (row-v   (gensym "row-"  ))
         (fvseq-v (gensym "fvseq-"))
         (fval-v  (gensym "fval-" ))
         (fev-v   (gensym "fev-"  ))
         (fei-v   (gensym "fei-"  ))
         (vei-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vei-~d-" i))))
         (vev-vl  (loop for i from 0 upto (1- num-vec)
                        collect (gensym (format 'nil "vev-~d-" i))))
         (vu      (loop for i from 0 upto (1- num-vec)
                        collect `(setf (aref ,table-v ,row-v ,i) ,(elt vev-vl i))))
         (fc      (case am
                    (:arg-vector `(funcall ,fn (vector ,@vev-vl)))
                    (:arg-number `(funcall ,fn ,@vev-vl))
                    (:arg-args   `(funcall ,fn ,@vev-vl))
                    (:arg-list   `(funcall ,fn (list ,@vev-vl)))))
         (dc      `(let ((,fval-v ,fc))
                     (if (null ,table-v)
                         (progn (setf ,fvseq-v (not (numberp ,fval-v)))
                                (setf ,table-v (make-array (list (reduce #'* (mapcar #'length ',vecs)) (+ ,num-vec (if ,fvseq-v (length ,fval-v) 1)))))))
                     ,@vu
                     (if ,fvseq-v
                         (if (vectorp ,fval-v)
                             (loop for ,fev-v across ,fval-v
                                   for ,fei-v from ,num-vec
                                   do (setf (aref ,table-v ,row-v ,fei-v) ,fev-v))
                             (loop for ,fev-v in ,fval-v
                                   for ,fei-v from ,num-vec
                                   do (setf (aref ,table-v ,row-v ,fei-v) ,fev-v)))
                         (setf (aref ,table-v ,row-v ,num-vec) ,fval-v))
                     (incf ,row-v))))
    (loop for i downfrom (1- num-vec) to 0
          do (setq dc `(loop for ,(elt vev-vl i) across ,(elt vecs i)
                             for ,(elt vei-vl i) upfrom 0
                             do ,dc)))
    `(let ((,table-v nil)
           (,fvseq-v nil)
           (,row-v   0))
       ,dc
       ,table-v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-cross-product (list-of-lengths-or-seqs &key
                                        (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                        (result-type :list)
                                        (arg-mode :arg-vector) (show-progress nil))
  "Generate all tuples.  See: mjr_combc_help.

The value of :result-type determines the type of the results returnd from :collect-value & :collect-if:
  * :list  -- list of :collect-value returns
  * :array -- array containing :collect-value returns
  * :table -- 2D array of tuples and :collect-value returns

Example: Find members of (1 2 3)x(4 4)x(6 7 8 9) such that the tuple elements sum to 15:
  (mjr_combc_gen-all-cross-product (list (list 1 2 3) (list 4 5) (list 6 7 8 9))
                                   :collect-value #'copy-seq
                                   :collect-if (lambda (v) (= (reduce #'+ v) 15)))

Example: Find members of (0 1 2)x(0 1)x(0 1 2 3) such that the tuple elements sum to 4:
  (mjr_combc_gen-all-cross-product (list 3 2 4)
                                   :collect-value #'copy-seq
                                   :collect-if (lambda (v) (= (reduce #'+ v) 4)))"
  (cond ((and collect-if (not collect-value))
         (error "mjr_combc_gen-all-cross-product: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
        ((and (eq result-type :table) (not collect-value))
         (error "mjr_combc_gen-all-cross-product: For a :result-type of :table, :collect-value must be provided"))
        ((and (eq result-type :array) (not collect-value))
         (error "mjr_combc_gen-all-cross-product: For a :result-type of :array, :collect-value must be provided"))
        ((and (eq result-type :table) (or func pre-if-filter collect-if exit-if))
         (error "mjr_combc_gen-all-cross-product: For a :result-type of :table the :func, :pre-if-filter, :collect-if, :exit-if arguments must all be nil"))
        ((not (member result-type '(:table :array :list nil)))
         (error "mjr_combc_gen-all-cross-product: :result-type must be one of :table, :array, :list, or nil"))
        ((and collect-value (not result-type))
         (error "mjr_combc_gen-all-cross-product: :result-type must be one of :table, :array, or :list when :collect-value is provided")))

  (if (and (member result-type '(:table :array)) (not (or func pre-if-filter collect-if exit-if)))
      (if (and (every #'integerp list-of-lengths-or-seqs) (equalp result-type :array))
          (progn (if show-progress (format 't "mjr_combc_gen-all-cross-product: Using optimized array macro (i).~%"))
                 (eval (macroexpand `(mjr_combc_gen-all-cross-product-i-array ,collect-value ,arg-mode ,@list-of-lengths-or-seqs))))
          (let ((list-o-vecs (mapcar (lambda (v) (if (numberp v)
                                                     (let ((a (make-array v :element-type 'fixnum)))
                                                       (dotimes (i v)
                                                         (setf (aref a i) i))
                                                       a)
                                                     (concatenate 'vector v)))
                                     list-of-lengths-or-seqs)))
            (case result-type
              (:array
               (if show-progress (format 't "mjr_combc_gen-all-cross-product: Using optimized array macro.~%"))
               (eval (macroexpand `(mjr_combc_gen-all-cross-product-array ,collect-value ,arg-mode ,@list-o-vecs))))
              (:table
               (if show-progress (format 't "mjr_combc_gen-all-cross-product: Using optimized table macro.~%"))
               (eval `(mjr_combc_gen-all-cross-product-table ,collect-value ,arg-mode ,@list-of-lengths-or-seqs))))))
      (let* ((return-array (eq result-type :array))
             (nvecs        (if (not (numberp (first list-of-lengths-or-seqs)))
                               (map 'vector
                                    (lambda (x)
                                      (if (vectorp x)
                                          x
                                          (concatenate 'vector x)))
                                    list-of-lengths-or-seqs)))
             (ns           (if nvecs
                               (map 'vector #'length nvecs)
                               (concatenate 'vector list-of-lengths-or-seqs)))
             (tl           (length ns))
             (tuple-i      (make-array tl :initial-element 0))
             (tuple-e      (if nvecs (make-array tl :initial-contents (map 'list (lambda (x) (aref x 0)) nvecs))))
             (tuples       (if return-array (make-array (concatenate 'list ns)))))
        (if show-progress (format 't "mjr_combc_gen-all-cross-product: Using unoptimized code.~%"))
        (dotimes (j (reduce #'* ns) tuples)
          (if show-progress (format 't "mjr_combc_gen-all-cross-product: ~s ~s~%" tuple-i tuple-e))
          (if func
              (mjr_util_fun-adapt-eval-v func (or tuple-e tuple-i) arg-mode))
          (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or tuple-e tuple-i) arg-mode))))
            (if (if collect-if
                    (if pre-if-filter
                        (funcall collect-if pf)
                        (mjr_util_fun-adapt-eval-v collect-if (or tuple-e tuple-i) arg-mode))
                    collect-value)
                (let ((cv (mjr_util_fun-adapt-eval-v collect-value (or tuple-e tuple-i) arg-mode)))
                  (if return-array
                      (setf (apply #'aref tuples (concatenate 'list tuple-i)) cv)
                      (push cv tuples))))
            (if (and exit-if (if pre-if-filter
                                 (funcall exit-if pf)
                                 (mjr_util_fun-adapt-eval-v exit-if (or tuple-e tuple-i) arg-mode)))
                (if (or collect-value collect-if)
                    (return tuples)
                    (return (or tuple-e tuple-i)))))
          (loop for i from (1- tl) downto 0
                when (= (1- (aref ns i)) (aref tuple-i i))
                do (progn (setf (aref tuple-i i)  0)
                          (if tuple-e
                              (setf (aref tuple-e i) (aref (aref nvecs i) 0))))
                else
                do (progn (incf (aref tuple-i i))
                          (if tuple-e
                              (setf (aref tuple-e i) (aref (aref nvecs i) (aref tuple-i i))))
                          (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-cross-power (integer-power length-or-seq &key
                                      (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                      (arg-mode :arg-vector) (show-progress nil))
  "Generate all tuples.  See: mjr_combc_help.

Example: Find all tuples of #(0 1 2 3)^3 that sum to 4.
  (mjr_combc_gen-all-cross-power 3
                                 4
                                 :collect-value #'copy-seq
                                 :collect-if (lambda (v) (= (reduce #'+ v) 4)))"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-all-cross-power: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((nvec    (if (not (numberp length-or-seq))
                      (if (vectorp length-or-seq)
                          length-or-seq
                          (concatenate 'vector length-or-seq))))
         (n       (if nvec (length nvec) length-or-seq))
         (tuple-i (make-array integer-power :initial-element 0))
         (tuple-e (if nvec (make-array integer-power :initial-element (aref nvec 0))))
         (tuples  nil))
    (dotimes (j (expt n integer-power) tuples)
      (if show-progress (format 't "mjr_combc_gen-all-cross-power: ~s ~s~%" tuple-i tuple-e))
      (if func
          (mjr_util_fun-adapt-eval-v func (or tuple-e tuple-i) arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or tuple-e tuple-i) arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if (or tuple-e tuple-i) arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value (or tuple-e tuple-i) arg-mode) tuples))
        (if (and exit-if (if pre-if-filter
                             (funcall exit-if pf)
                             (mjr_util_fun-adapt-eval-v exit-if (or tuple-e tuple-i) arg-mode)))
            (if (or collect-value collect-if)
                (return tuples)
                (return (or tuple-e tuple-i)))))
      (loop for i from (1- integer-power) downto 0
            when (= (1- n) (aref tuple-i i))
            do (progn (setf (aref tuple-i i)  0)
                      (if tuple-e
                          (setf (aref tuple-e i) (aref nvec 0))))
            else
            do (progn (incf (aref tuple-i i))
                      (if tuple-e
                          (setf (aref tuple-e i) (aref nvec (aref tuple-i i))))
                      (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-combinations (length-or-seq comb-len &key
                                       (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                       (bitmask nil)
                                       (arg-mode :arg-vector) (show-progress nil))
  "Generate all combinations of length COMB-LEN.  See: mjr_combc_help.

Example: Find all combinations of length 3 of #(0 1 2 3) that are sorted:
  (mjr_combc_gen-all-combinations 4
                                  3
                                  :collect-value #'copy-seq
                                  :collect-if (lambda (v) (equalp v (sort v #'<))))

References:
  P. Eades & B. McKay (1984); An algorithm for generating subsets of fixed size with a strong minimal change property; Inform. Process. Lett. 19 , no. 3, 131-133."
  (if (and :collect-if (not collect-value))
      (error "mjr_combc_gen-all-combinations: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((nvec     (if (not (numberp length-or-seq)) (concatenate 'vector length-or-seq)))
         (n        (if nvec (length nvec) length-or-seq))
         (subsets  nil))
    (loop for k in (if (listp comb-len) comb-len (list comb-len))
          do (let* ((subset-i (make-array k :initial-contents (loop for i from 0 upto (1- k)
                                                                    collect i)))
                    (subset-b (if bitmask (make-array n :element-type 'bit :initial-contents (loop for i from 0 upto (1- n)
                                                                                                   collect (if (< i k) 1 0)))))
                    (subset-e (if nvec (make-array k :initial-contents (concatenate 'list (subseq nvec 0 k))))))
               (labels ((warg () (cond (bitmask subset-b)
                                       (nvec    subset-e)
                                       ('t      subset-i)))
                        (prc-act (pos newv)
                          (if (and (> pos 0) (<= pos k))
                              (progn (if bitmask
                                         (setf (aref subset-b (aref subset-i (1- pos))) 0
                                               (aref subset-b (1- newv))                1))
                                     (if nvec
                                         (setf (aref subset-e (1- pos))                 (aref nvec (1- newv))))
                                     (setf     (aref subset-i (1- pos))                 (1- newv))))
                          (if show-progress (format 't "mjr_combc_gen-all-combinations: ~s ~s ~s~%" subset-i subset-b subset-e))
                          (if func
                              (mjr_util_fun-adapt-eval-v func (warg) arg-mode))
                          (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (warg) arg-mode))))
                            (if (if collect-if
                                    (if pre-if-filter
                                        (funcall collect-if pf)
                                        (mjr_util_fun-adapt-eval-v collect-if (warg) arg-mode))
                                    collect-value)
                                (push (mjr_util_fun-adapt-eval-v collect-value (warg) arg-mode) subsets))
                            (if (and exit-if
                                     (if pre-if-filter
                                         (funcall exit-if pf)
                                         (mjr_util_fun-adapt-eval-v exit-if (warg) arg-mode)))
                                (if (or collect-value collect-if)
                                    (return-from mjr_combc_gen-all-combinations subsets)
                                    (return-from mjr_combc_gen-all-combinations (warg))))))
                        (fwd-act (ptr dif)
                          (cond ((and (< ptr k) (< (- dif ptr) (- n k 1)))
                                 (fwd-act (+ ptr 2) (+ dif 2))
                                 (prc-act (+ ptr 1) (- (+ ptr n 1) k))
                                 (rev-act (+ ptr 1) (+ dif 2))
                                 (prc-act ptr       (+ dif 2))
                                 (fwd-act ptr       (+ dif 1)))
                                ((= ptr k)
                                 (loop for lst-n-arry from (+ dif 2) upto n
                                       do (prc-act k lst-n-arry)))))
                        (rev-act (ptr dif)
                          (cond ((and (< ptr k) (< (- dif ptr) (- n k 1)))
                                 (rev-act ptr       (+ dif 1))
                                 (prc-act ptr       (+ dif 1))
                                 (fwd-act (+ ptr 1) (+ dif 2))
                                 (prc-act (+ ptr 1) (+ dif 2))
                                 (rev-act (+ ptr 2) (+ dif 2)))
                                ((= ptr k)
                                 (loop for lst-n-arry from (- n 1) downto (+ dif 1)
                                       do (prc-act k lst-n-arry))))))
                 (prc-act 0 0)
                 (fwd-act 1 0))))
    subsets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-subsets (length-or-seq &key
                                  (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                  (bitmask nil)
                                  (arg-mode :arg-vector) (show-progress nil))
  "Generate all subsets.  See: mjr_combc_help.

Example: Find all subsets of length 3 of #(0 1 2 3):
  (mjr_combc_gen-all-subsets 4
                             :collect-value #'copy-seq
                             :collect-if (lambda (v) (= 3 (length v))))"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-all-subsets: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (mjr_combc_gen-all-combinations length-or-seq
                                  (loop for k from 0 upto (if (numberp length-or-seq) length-or-seq (length length-or-seq))
                                        collect k)
                                  :func func :collect-value collect-value :pre-if-filter pre-if-filter :collect-if collect-if :exit-if exit-if
                                  :bitmask bitmask
                                  :show-progress show-progress :arg-mode arg-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-rand-permutation (length-or-seq num-perms &key
                                       (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                       (arg-mode :arg-vector) (show-progress nil))
  "Generate NUM-PERMS random permutations.  See mjr_combc_gen-all-permutations-gry.

The algorithm is the Fisher-Yates shuffle.
References:. T
  Knuth; The Art of Computer Programming, Volume 2, Seminumerical Algorithms; Section 3.4.2; page 145"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-rand-permutation: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((n        (if (numberp length-or-seq) length-or-seq (length length-or-seq)))
         (perm-ioe (if (not (numberp length-or-seq))
                       (concatenate 'vector length-or-seq)
                       (make-array n :initial-contents (loop for i from 0 upto (1- n) collect i))))
         (perms    nil))
    (dotimes (j num-perms perms)
      (loop for i from (1- n) downto 1
            for rnd = (random (1+ i))
            do (rotatef (aref perm-ioe i) (aref perm-ioe rnd)))
      (if show-progress (format 't "mjr_combc_gen-rand-permutation: ~s~%" perm-ioe))
      do (if func
             (mjr_util_fun-adapt-eval-v func perm-ioe arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter perm-ioe arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if perm-ioe arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value perm-ioe arg-mode) perms))
        (if (and exit-if (if pre-if-filter
                             (funcall exit-if pf)
                             (mjr_util_fun-adapt-eval-v exit-if perm-ioe arg-mode)))
            (if (or collect-value collect-if)
                (return perms)
                (return perm-ioe)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-rand-cross-power (integer-power length-or-seq num-tuples &key
                                       (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                       (arg-mode :arg-vector) (show-progress nil))
  "Generate NUM-TUPLES random tuples.  See: mjr_combc_gen-all-cross-power)"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-rand-cross-power: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((nvec    (if (not (numberp length-or-seq))
                      (if (vectorp length-or-seq)
                          length-or-seq
                          (concatenate 'vector length-or-seq))))
         (n       (if nvec (length nvec) length-or-seq))
         (tuple-i (make-array integer-power))
         (tuple-e (if nvec (make-array integer-power)))
         (tuples  nil))
    (dotimes (j num-tuples tuples)
      (dotimes (i integer-power)
        (let ((ri (random n)))
          (setf (aref tuple-i i) ri)
          (if tuple-e
              (setf (aref tuple-e i) (aref nvec ri)))))
      (if show-progress (format 't "mjr_combc_gen-rand-cross-power: ~s ~s~%" tuple-i tuple-e))
      (if func
          (mjr_util_fun-adapt-eval-v func (or tuple-e tuple-i) arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or tuple-e tuple-i) arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if (or tuple-e tuple-i) arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value (or tuple-e tuple-i) arg-mode) tuples))
        (if (and exit-if (if pre-if-filter
                             (funcall exit-if pf)
                             (mjr_util_fun-adapt-eval-v exit-if (or tuple-e tuple-i) arg-mode)))
            (if (or collect-value collect-if)
                (return tuples)
                (return (or tuple-e tuple-i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-rand-cross-product (list-of-lengths-or-seqs num-tuples &key
                                         (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                         (arg-mode :arg-vector) (show-progress nil))
  "Generate NUM-TUPLES random tuples.  See: mjr_combc_gen-all-cross-product"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-rand-cross-product: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((nvecs   (if (not (numberp (first list-of-lengths-or-seqs)))
                      (map 'vector
                           (lambda (x)
                             (if (vectorp x)
                                 x
                                 (concatenate 'vector x)))
                           list-of-lengths-or-seqs)))
         (ns      (if nvecs
                      (map 'vector #'length nvecs)
                      (concatenate 'vector list-of-lengths-or-seqs)))
         (tl      (length ns))
         (tuple-i (make-array tl :initial-element 0))
         (tuple-e (if nvecs (make-array tl :initial-contents (map 'list (lambda (x) (aref x 0)) nvecs))))
         (tuples  nil))
    (dotimes (j num-tuples tuples)
      (dotimes (i tl)
        (let ((ri (random (aref ns i))))
          (setf (aref tuple-i i) ri)
          (if tuple-e
              (setf (aref tuple-e i) (aref (aref nvecs i) ri)))))
      (if show-progress (format 't "mjr_combc_gen-rand-cross-product: ~s ~s~%" tuple-i tuple-e))
      (if func
          (mjr_util_fun-adapt-eval-v func (or tuple-e tuple-i) arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or tuple-e tuple-i) arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if (or tuple-e tuple-i) arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value (or tuple-e tuple-i) arg-mode) tuples))
        (if (and exit-if (if pre-if-filter
                             (funcall exit-if pf)
                             (mjr_util_fun-adapt-eval-v exit-if (or tuple-e tuple-i) arg-mode)))
            (if (or collect-value collect-if)
                (return tuples)
                (return (or tuple-e tuple-i)))))
      (loop for i from (1- tl) downto 0
            when (= (1- (aref ns i)) (aref tuple-i i))
              do (progn (setf (aref tuple-i i)  0)
                        (if tuple-e
                            (setf (aref tuple-e i) (aref (aref nvecs i) 0))))
            else
              do (progn (incf (aref tuple-i i))
                        (if tuple-e
                            (setf (aref tuple-e i) (aref (aref nvecs i) (aref tuple-i i))))
                        (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-rand-combinations (length-or-seq comb-len  num-combs &key
                                        (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                        (arg-mode :arg-vector) (show-progress nil))
  "Generate NUM-COMBS random combinations.  See: mjr_combc_gen-all-combinations"
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-rand-combinations: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((n      (if (numberp length-or-seq) length-or-seq (length length-or-seq)))
         (perm-i (make-array n :initial-contents (loop for i from 0 upto (1- n) collect i)))
         (perm-e (if (not (numberp length-or-seq))
                     (concatenate 'vector length-or-seq)))
         (comb-i (make-array comb-len :displaced-to perm-i))
         (comb-e (if perm-e (make-array comb-len :displaced-to perm-e)))
         (combs  nil))
    (dotimes (j num-combs combs)
      (dotimes (i n)
        (let ((ri (+ i (random (- n i)))))
          (if perm-e
              (rotatef (aref perm-e i) (aref perm-e ri)))
          (rotatef (aref perm-i i) (aref perm-i ri))))
      (if show-progress (format 't "mjr_combc_gen-rand-combinations: ~s ~s~%" comb-i comb-e))
      (if func
          (mjr_util_fun-adapt-eval-v func (or comb-e comb-i) arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter (or comb-e comb-i) arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if (or comb-e comb-i) arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value (or comb-e comb-i) arg-mode) combs))
        (if (and exit-if
                 (if pre-if-filter
                     (funcall exit-if pf)
                     (mjr_util_fun-adapt-eval-v exit-if (or comb-e comb-i) arg-mode)))
            (if (or collect-value collect-if)
                (return combs)
                (return (or comb-e comb-i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-rand-subsets (length-or-seq num-sets &key
                                   (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                   (bitmask nil)
                                   (arg-mode :arg-vector) (show-progress nil))
  "Generate NUM-SETS random subsets.  See: MJR_COMBC_GEN-ALL-SUBSETS."
  (if (and collect-if (not collect-value))
      (error "mjr_combc_gen-rand-subsets: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)"))
  (let* ((n        (if (numberp length-or-seq) length-or-seq (length length-or-seq)))
         (subsets  nil)
         (perm-X   (cond ((numberp length-or-seq) (make-array n :initial-contents (loop for i from 0 upto (1- n) collect i)))
                         (bitmask                 nil)
                         ('t                      (concatenate 'vector length-or-seq))))
         (subset-X (if bitmask (make-array n :element-type 'bit))))
    (dotimes (j num-sets subsets)
      (if bitmask
          (dotimes (i n)
            (setf (aref subset-X i) (random 2)))
          (progn
            (setq subset-X (make-array (random (1+ n)) :displaced-to perm-X))
            (dotimes (i n)
                (rotatef (aref perm-X i) (aref perm-X (+ i (random (- n i))))))))
      (if show-progress (format 't "mjr_combc_gen-rand-subsets: ~s~%" subset-X))
      (if func
          (mjr_util_fun-adapt-eval-v func subset-X arg-mode))
      (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter subset-X arg-mode))))
        (if (if collect-if
                (if pre-if-filter
                    (funcall collect-if pf)
                    (mjr_util_fun-adapt-eval-v collect-if subset-X arg-mode))
                collect-value)
            (push (mjr_util_fun-adapt-eval-v collect-value subset-X arg-mode) subsets))
        (if (and exit-if (if pre-if-filter
                             (funcall exit-if pf)
                             (mjr_util_fun-adapt-eval-v exit-if subset-X arg-mode)))
            (if (or collect-value collect-if)
                (return subsets)
                (return subset-X)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-partitions-revlex (n &key
                                                 (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                                 (arg-mode :arg-vector) (show-progress nil))
  "Generate and process integer partitions in reverse lexicographic order.  See: mjr_combc_help.

Find the partitions of 3
  - (mjr_combc_gen-all-partitions-revlex 3  :collect-value #'copy-seq)
    '(#(3) #(2 1) #(1 1 1))
Find partitions of 12 with only even numbers:
  - (mjr_combc_gen-all-partitions-revlex 12 :collect-value #'copy-seq :collect-if (lambda (x) (every #'evenp x)))
    '(#(2 2 2 2 2 2) #(4 2 2 2 2) #(4 4 2 2) #(4 4 4) #(6 2 2 2) #(6 4 2) #(6 6) #(8 2 2) #(8 4) #(10 2) #(12))
Find partitions of 15 with only prime numbers:
  - (mjr_combc_gen-all-partitions-revlex 15 :collect-value #'copy-seq :collect-if (lambda (x) (every #'mjr_prime_primep x)))
    '(#(3 2 2 2 2 2 2) #(3 3 3 2 2 2) #(3 3 3 3 3) #(5 2 2 2 2 2) #(5 3 3 2 2) #(5 5 3 2) #(5 5 5) #(7 2 2 2 2) #(7 3 3 2) #(7 5 3) #(11 2 2) #(13 2))
Find partitions of 21 with only odd, prime numbers:
  - (mjr_combc_gen-all-partitions-revlex 21 :collect-value #'copy-seq :collect-if (lambda (x) (and (every #'oddp x) (every #'mjr_prime_primep x))))
    '(#(3 3 3 3 3 3 3) #(5 5 5 3 3) #(7 5 3 3 3) #(7 7 7) #(11 5 5) #(11 7 3) #(13 5 3))

References:
  Knuth; The Art of Computer Programming, Volume 4A, Combinatorial Algorithms, Part 1; Section 7.2.1.4; page 392"
  (cond ((not (integerp n))        (error "mjr_combc_gen-all-k-partitions-colex: First argument must be an integer!"))
        ((< n 0)                   (error "mjr_combc_gen-all-k-partitions-colex: First argument must be non-negative!"))
        ((and collect-if
              (not collect-value)) (error "mjr_combc_gen-all-partitions-revlex: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)")))
  (let* ((a (make-array (1+ n) :initial-element 1))
         (m      1)
         (q      (if (= n 1) 0 1))
         (x      0)
         (parts  nil))
    (setf (aref a 0) 0
          (aref a 1) n)
    (loop for j from 0
          for daPart = (subseq a 1 (1+ m))
          do (if show-progress (format 't "mjr_combc_gen-all-partitions-revlex: ~10s ~s~%" j daPart))
          do (if func
                 (mjr_util_fun-adapt-eval-v func daPart arg-mode))
          do (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter daPart arg-mode))))
               (if (if collect-if
                       (if pre-if-filter
                           (funcall collect-if pf)
                           (mjr_util_fun-adapt-eval-v collect-if daPart arg-mode))
                       collect-value)
                   (push (mjr_util_fun-adapt-eval-v collect-value daPart arg-mode) parts))
               (if (and exit-if (if pre-if-filter
                                    (funcall exit-if pf)
                                    (mjr_util_fun-adapt-eval-v exit-if daPart arg-mode)))
                   (if (or collect-value collect-if)
                       (return parts)
                       (return daPart))))
          do (if (not (= (aref a q) 2))
                 (progn (if (zerop q)
                            (return parts)
                            (setf x          (- (aref a q) 1)
                                  (aref a q) x
                                  n          (+ (- m q) 1)
                                  m          (+ q 1)))
                        (loop do (if (<= n x)
                                     (return (setf (aref a m) n
                                                   q          (- m (if (= n 1) 1 0))))
                                     (progn (setf (aref a m) x)
                                            (incf m)
                                            (decf n x)))))
                 (progn (setf (aref a q) 1)
                        (decf q)
                        (incf m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combc_gen-all-k-partitions-colex (n k &key
                                                   (func nil) (collect-value nil) (pre-if-filter nil) (collect-if nil) (exit-if nil)
                                                   (arg-mode :arg-vector) (show-progress nil))
  "Generate and process integer k-partitions in colexicographic order.  See: mjr_combc_help.

Find the length 3 partitions of 11
  - (mjr_combc_gen-all-k-partitions-colex 11 3 :collect-value #'copy-seq)
    (#(4 4 3) #(5 3 3) #(5 4 2) #(6 3 2) #(7 2 2) #(5 5 1) #(6 4 1) #(7 3 1) #(8 2 1) #(9 1 1))
Find the length 4 partitions of 12 with only even numbers:
  - (mjr_combc_gen-all-k-partitions-colex 12 4 :collect-value #'copy-seq :collect-if (lambda (x) (every #'evenp x)))
    (#(4 4 2 2) #(6 2 2 2))
Find the length 5 partitions of 15 with only prime numbers:
  - (mjr_combc_gen-all-k-partitions-colex 15 5 :collect-value #'copy-seq :collect-if (lambda (x) (every #'mjr_prime_primep x)))
    (#(3 3 3 3 3) #(5 3 3 2 2) #(7 2 2 2 2))
Find the length 5  partitions of 21 with only odd, prime numbers:
  - (mjr_combc_gen-all-k-partitions-colex 21 5 :collect-value #'copy-seq :collect-if (lambda (x) (and (every #'oddp x) (every #'mjr_prime_primep x))))
    (#(5 5 5 3 3) #(7 5 3 3 3))

References:
  Knuth; The Art of Computer Programming, Volume 4A, Combinatorial Algorithms, Part 1; Section 7.2.1.4; page 392"
  (cond ((not (integerp n))        (error "mjr_combc_gen-all-k-partitions-colex: First argument must be an integer!"))
        ((< n 0)                   (error "mjr_combc_gen-all-k-partitions-colex: First argument must be non-negative!"))
        ((not (integerp k))        (error "mjr_combc_gen-all-k-partitions-colex: Second argument must be an integer!"))
        ((< k 0)                   (error "mjr_combc_gen-all-k-partitions-colex: Second argument must be non-negative!"))
        ((and collect-if
              (not collect-value)) (error "mjr_combc_gen-all-k-partitions-colex: :collect-if requires :collect-value (use #'copy-seq to collect generated objects)")))
  (let* ((a (make-array (+ 1 k) :initial-element 1))
         (daPart (make-array k :displaced-to a))
         (j      0)
         (x      0)
         (s      0)
         (parts  nil))
    (setf (aref a 0) (1+ (- n k)))
    (setf (aref a k) -1)
    (loop for i from 0
          do (if show-progress (format 't "mjr_combc_gen-all-k-partitions-colex: ~10s ~s~%" i daPart))
          do (if func
                 (mjr_util_fun-adapt-eval-v func daPart arg-mode))
          do (let* ((pf (and pre-if-filter (mjr_util_fun-adapt-eval-v pre-if-filter daPart arg-mode))))
               (if (if collect-if
                       (if pre-if-filter
                           (funcall collect-if pf)
                           (mjr_util_fun-adapt-eval-v collect-if daPart arg-mode))
                       collect-value)
                   (push (mjr_util_fun-adapt-eval-v collect-value daPart arg-mode) parts))
               (if (and exit-if (if pre-if-filter
                                    (funcall exit-if pf)
                                    (mjr_util_fun-adapt-eval-v exit-if daPart arg-mode)))
                   (if (or collect-value collect-if)
                       (return parts)
                       (return daPart))))
          do (if (= k 1)
                 (return parts))
          do (if (< (aref a 1) (1- (aref a 0)))
                 (progn (decf (aref a 0))
                        (incf (aref a 1)))
                 (progn (loop initially (setf j 2
                                              s (1- (+ (aref a 0) (aref a 1))))
                              while (>= (aref a j) (1- (aref a 0)))
                              do (incf s (aref a j))
                              do (incf j))
                        (if (> (1+ j) k)
                            (return parts)
                            (progn (setf x (1+ (aref a j)))
                                   (setf (aref a j) x)
                                   (decf j)))
                        (loop while (> j 0)
                              do (setf (aref a j) x)
                              do (decf s x)
                              do (decf j))
                        (setf (aref a 0) s))))))
