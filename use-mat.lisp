;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-mat.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Matrix math library.@EOL
;; @std       Common Lisp
;; @see       tst-mat.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1995-2010,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_mat_make-from-func: Use vvec iterators instead of materializing vectors.@EOL@EOL
;; @todo      Better behavior when vectors are used -- i.e. A*x should return a vector when x is a vector.@EOL@EOL
;; @todo      Refactor into several packages out of the basic matrix object stuff
;;             * mjr_mat  -- basic matrix stuff
;;             * mjr_matc -- matrix computations (equations, eigensystems, determinants
;;             * mjr_mats -- special matrixes@EOL@EOL
;; @todo      Stationary methods: Gauss Seidel method.@EOL@EOL
;; @todo      Stationary methods: Successive Over-Relaxation method (SOR).@EOL@EOL
;; @todo      Stationary methods: Jacobi Iteration method.@EOL@EOL
;; @todo      Krylov subspace methods: Conjugate Gradient method (CG).@EOL@EOL
;; @todo      Krylov subspace methods: Generalized minimal residual method (GMRES).@EOL@EOL
;; @todo      Krylov subspace methods: Biconjugate gradient method (BiCG).@EOL@EOL
;; @todo      Eigenvalues & vectors.@EOL@EOL
;; @todo      Condition number.@EOL@EOL
;; @todo      Currently this code uses a system of "special matrix" symbols for
;;               1) generate special matrices,
;;               2) computing determinants of special (test) matrices, and
;;               3) testing matrix properties.
;;             Extend this system to general matrix computation routines so that they may
;;             exploit special structure or properties for increased efficiently.  Examples: determinants and inverse for
;;             upper triangular matrices, determinants for tridiagonal matrices, or inverses for orthogonal matrices..@EOL@EOL
;; @todo      SVD factorization.@EOL@EOL
;; @todo      LU Factorization.@EOL@EOL
;; @todo      Iterative system solution.@EOL@EOL
;; @todo      Add mjr_mat_solve-sys-tridiag -- for tridiagonal systems..@EOL@EOL
;; @todo      Add some better error checking for *-special functions.  In particular, a clean way to specify what arguments.@EOL@EOL
;;            are required by each special case with useful error messages.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_MAT
  (:USE :COMMON-LISP
        :MJR_VEC
        :MJR_CMP
        :MJR_EPS
        :MJR_NUMU
        :MJR_UTIL
        :MJR_POLY
        :MJR_PERM
        :MJR_VVEC
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Matrix math library.;")
  (:EXPORT #:mjr_mat_help
           #:mjr_mat_cv2m #:mjr_mat_m2cv
           #:mjr_mat_matp #:mjr_mat_cols #:mjr_mat_rows
           #:mjr_mat_make-and-fill #:mjr_mat_make-const #:mjr_mat_make-zero #:mjr_mat_make-identity
           #:mjr_mat_make-diag #:mjr_mat_make-from-func #:mjr_mat_make-table #:mjr_mat_make-poly-companion
           #:mjr_mat_print #:mjr_mat_code
           #:mjr_mat_transpose
           #:mjr_mat_* #:mjr_mat_expt #:mjr_mat_+ #:mjr_mat_- #:mjr_mat_ew* #:mjr_mat_ew/ #:mjr_mat_s/
           #:mjr_mat_apply-linear-tform
           #:mjr_mat_every-idx #:mjr_mat_test-property-struct
           #:mjr_mat_fill-stats
           #:mjr_mat_float #:mjr_mat_rationalize
           #:mjr_mat_diag
           #:mjr_mat_minor
           #:mjr_mat_trace #:mjr_mat_diag-prod
           #:mjr_mat_rowop-swap #:mjr_mat_rowop-mult #:mjr_mat_rowop-div #:mjr_mat_rowop-mult-add
           ;; Low level computation (used by other routines, but rarely by humans)
           #:mjr_mat_orthogonal-zero-pair #:mjr_mat_householder-many
           #:mjr_mat_apply-householder-2!! #:mjr_mat_apply-givens-2!! #:mjr_mat_apply-householder-many!!
           #:mjr_mat_apply-gauss-1!! #:mjr_mat_apply-gauss-2!! #:mjr_mat_apply-gauss-3!!
           ;; Linear Algebra Computation
           #:mjr_mat_factor-qdr
           #:mjr_mat_solve-sys-sub
           #:mjr_mat_eliminate #:mjr_mat_det-ge #:mjr_mat_inv-sge #:mjr_mat_solve-sys-sge #:mjr_mat_rank-sge
           #:mjr_mat_inv-fl #:mjr_mat_inv-small #:mjr_mat_inv
           #:mjr_mat_cpoly-eval #:mjr_mat_cpoly #:mjr_mat_cpoly-det #:mjr_mat_eigen-rational #:mjr_mat_cpoly-fl #:mjr_mat_eigen-float
           #:mjr_mat_det-small #:mjr_mat_det-minor-expand #:mjr_mat_det
           #:mjr_mat_norm
           #:mjr_mat_test-property-math
           ;; EXPERIMENTAL:
           #:mjr_mat_krylov-sequence #:mjr_mat_eigen-power-method
           #:mjr_mat_factor-ldlt
           #:mjr_mat_tridiagonalize-householder
           ;; EVEN MORE EXPERIMENTAL:
           #:mjr_mat_sis-jacobi
           #:mjr_mat_sis-sor
           #:mjr_mat_sis-gauss-seidel
           #:mjr_mat_solve-sys-itr
           ))

(in-package :MJR_MAT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_help ()
  "Help for MJR_MAT:  MATrix math

This package emphasizes interactive use with special attention given to theoretical and textbook'esq computation.

 * Many functions are provided to assist manual computations (ex: row operations).
 * Several 'numerical' algorithms directly correspond with methods associated more with 'hand computation'.  Some of them are even able to provide 'partial
   results' useful for human inspection when things go wrong -- just like one might do 'on paper'. (ex: Gaussian elimination followed by a visual inspection
   to determine matrix rank or solution structure)
 * Ease of use over storage efficiency. (ex: No special matrix storage formats for things like sparse, tridiagonal or triangular matrices)
 * Usability wins over raw performance; however, reasonable interactive performance is still maintained.
 * Functions organized in a way that promotes rapid numerical prototyping and experimental algorithm development.
 * Options for theoretically useful, but computationally inefficient, things like constructing Givens or Householder matrices explicitly.
 * Preservation of rational numbers whenever possible even when that choice might lead to finite floating point instability. (ex: Square root free
   Cholesky-like decomposition)

About vectors:

  An m element vector argument will be interpreted by all functions, except MAT_MATP, as as a mx1 matrix (a column vector).  While such vectors may be
  provided as input when matrices are expected, functions generally return properly formed matrices (2D arrays). MJR_MAT_APPLY-LINEAR-TFORM &
  MJR_MAT_ELIMINATE (and some functions that use them) are exceptions."
  (documentation 'mjr_mat_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_matp (obj &optional vec-is-a-mat)
  "Non-nil if obj is a matrix (2D array).
If VEC-IS-A-MAT, then non-nil will be returned for 1D arrays too."
  (and (arrayp obj) (or (= (array-rank obj) 2) (and vec-is-a-mat (= (array-rank obj) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cols (mat-or-cv)
  "Number of columns in matrix or column vector.  NIL if MAT-OR-CV is not a matrix or vector."
  (if (mjr_mat_matp mat-or-cv)
      (array-dimension mat-or-cv 1)
      (if (mjr_mat_matp mat-or-cv 't)
          1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rows (matrix)
  "Number of rows in matrix or column vector.  NIL if MAT-OR-CV is not a matrix or vector."
  (if (mjr_mat_matp matrix 't) (array-dimension matrix 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-const (rows cols &optional (constant 0))
  "Create a constant matrix.  If missing, constant=0."
  (cond ((minusp rows) (error "mjr_mat_make-const: ROWS must not be negative"))
        ((minusp cols) (error "mjr_mat_make-const: COLS must not be negative")))
  (make-array (list rows cols) :initial-element constant))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-zero (rows-or-matrix &optional cols)
  "Create a zero matrix/vector.

If ROWS-OR-MATRIX is a matrix, return a zero matrix of the same size. If ROWS-OR-MATRIX is not a matrix, then a zero matrix of ROWS-OR-MATRIX rows will be
produced.  In this case, the matrix will have COLS columns, or will be square if COLS is missing."
  (if (mjr_mat_matp rows-or-matrix 't)
      (mjr_mat_make-zero (mjr_mat_rows rows-or-matrix) (mjr_mat_cols rows-or-matrix))
    (if cols
        (mjr_mat_make-const rows-or-matrix cols)
      (mjr_mat_make-const rows-or-matrix rows-or-matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cv2m (vec)
  "Convert a Column Vector to a Matrix.  Returns VEC if conversion impossible or not required.

Doesn't create new object if VEC is NOT a vector."
  (if (vectorp vec)
      (let* ((rows (array-dimension vec 0))
             (newmat (mjr_mat_make-zero rows 1)))
        (dotimes (row rows newmat)
          (setf (aref newmat row 0) (aref vec row))))
      vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_m2cv (matrix)
  "Convert a 1 column Matrix into a vector.  Returns MATRIX if conversion impossible or not required."
  (if (and (mjr_mat_matp matrix) (= 1 (mjr_mat_cols matrix)))
      (mjr_arr_get-col matrix 0)
    matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-from-func (func &key rpoints rstart rend rstep rlen
                                         cpoints cstart cend cstep clen
                                         points  start  end  step  len
                                         rows cols
                                         (rfunc #'identity) (cfunc #'identity)
                                         arg-mode)
  "Generate a matrix such that M(ri,cj)=FUNC(RFUNC(IR(ri)),CFUNC(IC(cj))) with ci & rj being arithmetic sequences determined by MJR_VVEC_KW-NORMALIZE using
the arguments: RPOINTS, RSTART, REND, RSTEP, RLEN, CPOINTS, CSTART, CEND, CSTEP, CLEN, POINTS, START, END, STEP, & LEN.  The arguments RPOINTS, RSTART, REND,
RSTEP, RLEN determine ri if they exist, while the C* arguments determine ci.  If any of them is missing while the corresponding undifferentiated argument
exists, it is used.  For example, if RPOINTS is missing and POINTS is provided, it will be used for RPOINTS.  One special case, if CLEN, RLEN, or LEN is a
matrix, then the number of rows or cols will be used as appropriate.  Note that COLS may be used for CLEN, and ROWS may be used for RLEN.  If only COLS (ROWS)
is given, then it will be used for ROWS (COLS).

The :ARG-MODE: argument determines how the values are provided to func (See: MJR_UTIL_FUN-ADAPT-EVAL-V).

Typical examples (defun ftgr (i j) (sin (+ (* i i) (* j j)))):
  * All equivalent:
  * (mjr_mat_make-from-func #'ftgr :rlen 10                     :clen 8)
  * (mjr_mat_make-from-func #'ftgr :rlen 100 :rstart -8 :rend 8 :clen 100  :cstart -8 :cend 8)
  * (mjr_mat_make-from-func #'ftgr :len  100 :start  -8 :end  8)
  * (mjr_mat_make-from-func #'ftgr :rlen 10  :rstart -8 :rend 8 :cpoints #(-8 -7 -3 0 1 2 6 8) )
  * (mjr_mat_make-from-func #'ftgr :rlen 100 :rstart  1 :rend 8 :clen 100  :cstart 1  :cend 8 :rfunc #'log :cfunc #'log)"
  (let* ((rlen (or (and (mjr_mat_matp (or rlen len rows cols)) (mjr_mat_rows (or rlen len rows cols))) (or rlen len rows cols)))
         (clen (or (and (mjr_mat_matp (or clen len cols rows)) (mjr_mat_cols (or clen len cols rows))) (or clen len cols rows)))
         (rvec (mjr_vvec_to-vec (mjr_util_strip-nil-val-kwarg
                                 (list :points (or rpoints points) :start (or rstart start) :end (or rend end) :step (or rstep step) :len rlen))))
         (cvec (mjr_vvec_to-vec (mjr_util_strip-nil-val-kwarg
                                 (list :points (or cpoints points) :start (or cstart start) :end (or cend end) :step (or cstep step) :len clen)))))
    (let ((rlen (length rvec))
          (clen (length cvec)))
      (let ((newmat (mjr_mat_make-zero rlen clen)))
        (loop for row from 0 upto (1- rlen)
              for ri across rvec
              finally (return newmat)
              do (loop for col from 0 upto (1- clen)
                       for ci across cvec
                       do (setf (aref newmat row col)
                                (ignore-errors (mjr_util_fun-adapt-eval-v func (vector (funcall rfunc ri) (funcall cfunc ci)) arg-mode)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-and-fill (matrix-or-rows &optional data-or-cols missing-or-data)
  "Create a new matrix and fill it with the data in the given sequence (vector or list).  If not enough data is provided, then the matrix will be padded with
zeros.  Extra data will be discarded.  If data-or-cols is a number, and matrix-or-rows is a matrix, then the size of the matrix wins (i.e. data-or-cols is
ignored)."
  (let* ((mism   (mjr_mat_matp matrix-or-rows))
         (rows   (if mism (mjr_mat_rows matrix-or-rows) matrix-or-rows))
         (cols   (if mism (mjr_mat_cols matrix-or-rows) (if (numberp data-or-cols) data-or-cols rows)))
         (data   (if (or (vectorp data-or-cols) (listp data-or-cols)) data-or-cols missing-or-data))
         (len    (length data))
         (newmat (mjr_mat_make-zero rows cols)))
    (dotimes (row rows newmat)
      (dotimes (col cols)
        (let ((i (+ col (* row cols))))
          (if (< i len)
              (setf (aref newmat row col) (elt data i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-diag (diag &optional (diag-offset 0))
  "Make a matrix with the given diagonal at the given location."
  (let ((diag (if (vectorp diag) diag (map 'vector #'identity diag))))
    (mjr_mat_make-from-func (lambda (i j) (if (= 0 (+ diag-offset (- i j))) (aref diag (if (>= diag-offset 0) i j)) 0))
                            :rows (+ (length diag) (abs diag-offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-poly-companion (poly &key (form :standard))
  "Construct the companion matrix for the given polynomial.
Set :form to one of
 * :matlab    -- coefficients on along the top
 * :mattran   -- coefficients on along the left
 * :standard  -- coefficients along the right
 * :transpose -- coefficients along the bottom"
  (let* ((len (length poly))
         (c   (aref poly 0)))
    (cond ((< len 2)
           (error "mjr_mat_make-poly-companion: Polynomial must be degree 1 or higher!"))
          ((not (or (eq form :matlab) (eq form :standard) (eq form :transpose) (eq form :mattran)))
           (error "mjr_mat_make-poly-companion: :form must be one of :matlab, :standard, :transpose, or :mattran"))
          ((= len 2)
           (make-array (list 1 1) :initial-element (- (/ (aref poly 1) c))))
          ('t
           (let ((newmat (mjr_mat_make-diag (mjr_vec_make-const (- len 2) 1) (if (or (eq form :transpose) (eq form :mattran)) 1 -1))))
             (dotimes (i (1- len) newmat)
               do (setf (aref newmat
                              (case form
                                (:matlab    0)
                                (:mattran   (- len i 2))
                                (:standard  i)
                                (:transpose (- len 2))
                                (otherwise  0))
                              (case form
                                (:matlab    (- len i 2))
                                (:mattran   0)
                                (:standard  (- len 2))
                                (:transpose i)
                                (otherwise  0))) (- (/ (aref poly (- len i 1)) c)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-identity (rows-or-matrix &optional (unit 1))
  "Make an identity matrix with the given unit (default unit is 1).

If rows-or-matrix is a matrix, a an matrix of the number of rows found in ROWS-OR-MATRIX is returned. If ROWS-OR-MATRIX is a not a matrix, then an identity
matrix with ROWS-OR-MATRIX rows will be produced."
  (if (mjr_mat_matp rows-or-matrix 't)
      (mjr_mat_make-identity (mjr_mat_rows rows-or-matrix) unit)
      (mjr_mat_make-from-func (lambda (i j) (if (= i j) unit 0))
                              :rows (or (mjr_mat_rows rows-or-matrix) rows-or-matrix)
                              :cols (or (mjr_mat_cols rows-or-matrix) rows-or-matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_make-table (func &key points start end step len n arg-mode)
  "Produce a table of values from the given function (FUNC).

The function specified in the FUNC argument takes a N numeric values.  The :ARG-MODE: argument determines how the values are provided.  The values
of :ARG-AUTO have the same meaning as in MJR_UTIL_FUN-ADAPT-EVAL-V.

The value of N is the maximum of:
  * 1
  * N (if it is provided)
  * The number of rows in :POINTS if :POINTS is a matrix
  * The lengths of :START, :END, :STEP, and :LEN if any of them are a sequence
  * The length of :POINTS if it is a list

The function specified in the :FUNC argument returns M elements in the form of a VECTOR, LIST, or single number (when M=1). The function must return the same
type, of the same length, EVERY time it is called.  The value of M need not be provided as it will be deduced from the return value the first time FUNC is
evaluated.

N sequences are generated from :POINTS, :START, :END, :STEP, and :LEN.  If any of them are lists, they are used as is. If
:POINTS is a matrix, then it is converted into a list of the rows of the matrix.  If :POINTS is a vector, it is converted into a list containing one vector.
If :START, :END, :STEP, or :LEN are scalars, then they are converted into one element lists.  Any lists that are less than N elements long will be
'recycled' (as in R) to be as N elements long.  Finally N sequences are then created with the i'th sequence constructed from the i'th elements
of :POINTS, :START, :END, :STEP, and :LEN as described in the documentation for MJR_VVEC_KW-NORMALIZE.  We call these sequences: C_1, C_2, ..., C_N

The :FUNC function will be evaluated on every element of C_1 X C_2 X ... X C_N -- i.e. evaluated #(C_1)*#(C_2)*..*#(C_N) times.  The return value from this
function is a matrix with (+ M N) columns, and D_1*D_2*...*D_N rows, constructed such that (the first N columns are the N-tuples the function is evaluated on,
and the last M columns are the function values).  The rows of this matrix are ordered with left most most columns of the N-tuples (first N columns) varying
most slowly."
  (flet ((a2l  (a)   (cond ((and (listp a) (< 0 (length a))) a)                                                ;; Make lists
                           ((vectorp a)                      (concatenate 'list a))
                           ('t                               (list a))))
         (mnth (i l) (nth (mod i (length l)) l)))                                                              ;; Get the nth mod length
    (let* ((points    (if (mjr_mat_matp points)                                                                ;; Convert to list
                          (mjr_arr_get-rows points)
                          (if (and points (listp points) (or (vectorp (first points)) (listp (first points))))
                              points                                                                           ;; It was already a list of sequences
                              (list points))))                                                                 ;; It is probably a sequence of numbers...
           (start     (a2l start))                                                                             ;; Convert to list
           (end       (a2l end))                                                                               ;; Convert to list
           (step      (a2l step))                                                                              ;; Convert to list
           (len       (a2l len))                                                                               ;; Convert to list
           (n         (max (or n 1) (length points) (length start) (length end) (length step) (length len)))   ;; Compute n (num vars FUNC takes)
           (seqs      (loop for i from 0 upto (1- n)                                                           ;; Compute sequences
                            collect (mjr_vvec_to-vec (mjr_util_strip-nil-val-kwarg (list :points (mnth i points)
                                                                                         :start  (mnth i start)
                                                                                         :end    (mnth i end)
                                                                                         :step   (mnth i step)
                                                                                         :len    (mnth i len))))))
           (slen      (map 'list #'length seqs))                                                               ;; Length of sequences
           (r         (reduce #'* slen))                                                                       ;; Number of tuples we will generate
           (x         (map 'vector (lambda (ts) (aref ts 0)) seqs)))                                           ;; First X
      (let* ((y0 (mjr_util_fun-adapt-eval-v func (map 'vector (lambda (ts) (aref ts 0)) seqs) arg-mode))       ;; Evaluate function on first tuple
             (m  (if (numberp y0) 1 (length y0)))                                                              ;; Compute the size of FUNC return
             (ar (make-array (list r (+ n m)))))                                                               ;; The final return matrix
        (flet ((v2v (val)       (cond ((vectorp val) val)                                                      ;; Convert FUNC return to vector
                                      ((listp   val) (make-array m :initial-contents val))
                                      ((numberp val) (make-array m :initial-element  val)))))
          (flet ((ur  (row xv yv) (loop for i from 0                                                           ;; Update ROW of ar
                                        for el across (concatenate 'vector xv (v2v yv))
                                        do (setf (aref ar row i) el))))
            (ur 0 x y0)                                                                                        ;; Update row one
            (loop with xn = (make-array n :initial-element 0)                                                  ;; Hold the index vector
                  for j from 1 upto (1- r)                                                                     ;; The rest of the rows
                  finally (return ar)                                                                          ;; We return the table. :)
                  do (loop for i from (1- n) downto 0                                                          ;; Increment N-tuple
                           do (if (= (aref xn i) (1- (nth i slen)))
                                  (setf (aref xn i) 0                                                          ;; digit max -> zero it, next
                                        (aref x i)  (aref (nth i seqs) 0))
                                  (return (setf (aref xn i) (1+ (aref xn i))                                   ;; digit not max -> increment, done
                                                (aref  x i) (aref (nth i seqs) (aref xn i))))))
                  do (ur j x (v2v (mjr_util_fun-adapt-eval-v func x arg-mode))))))))))                         ;; Update row

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_code (matrix &key (lang :lang-matlab))
  "Return a string representing the matrix in syntax of the selected programming language or computational environment."
  (let ((matrix (mjr_mat_cv2m matrix)))
    (if (not (mjr_mat_matp matrix))
        (error "mjr_mat_code: The MATRIX argument must be a matrix or vector"))
    (let* ((rows   (mjr_mat_rows matrix))
           (cols   (mjr_mat_cols matrix))
           (rows-1 (1- rows))
           (cols-1 (1- cols))
           (bams  (case lang            ;;    0           1    2  3  4      5    6   7
                    ((:lang-matlab
                      :lang-scilab
                      :lang-octave
                      :lang-euler
                      :lang-gp
                      :lang-pari
                      :lang-pari/gp)    (list "["         ""   "" "" "]"    ";"  "," "," ))
                    (:lang-mathematica  (list "{{"        "{"  "" "" "}};"  "}," "," "," ))
                    ((:lang-idl
                      :lang-gap)        (list "[["        "["  "" "" "]];"  "]," "," "," ))
                    (:lang-hp48         (list "[["        "["  "" "" "]]"   "]"  "," "," ))
                    (:lang-python       (list "(("        "("  "" "" "))"   ")"  "," "," ))
                    (:lang-ruby         (list "Matrix[["  "["  "" "" "]]"   "]," "," "," ))
                    ((:lang-axiom
                      :lang-open-axiom) (list "matrix[["  "["  "" "" "]]"   "]," "," "," ))
                    (:lang-maple        (list "matrix([[" "["  "" "" "]]);" "]," "," "," ))
                    (:lang-maxima       (list "matrix(["  "["  "" "" "]);"  "]," "," "," ))
                    (:lang-M2           (list "matrix{{"  "{"  "" "" "}}"   "}," "," "," ))
                    (:lang-r            (list "rbind(c("  "c(" "" "" "))"   ")," "," "," ))
                    (:lang-lisp         (list "#2a(("     "("  "" "" "))"   ")"  " " " " ))
                    ((:lang-csv
                      :lang-csvl)       (list ""          ""   "" "" "~%"   "~%"  "," ","))
                    (:lang-povray       (list "matrix <"  ""   "" "" ">"    ","   "," ","))
                    ('t                 nil                                               ))))
      (cond ((or (< rows 1) (< cols 1))  (error "mjr_mat_code: An empty matrix may not be coded into a string!"))
            ((null bams)                 (error "mjr_mat_code: Requested language is not supported!")))
      (with-output-to-string (str-out)
                             (dotimes (row rows)
                               (dotimes (col cols)
                                 (format str-out (cond ((and (= col 0) (= row 0))         (nth 0 bams))
                                                       ((= col 0)                         (nth 1 bams))
                                                       ((= row 0)                         (nth 2 bams))
                                                       ('t                                (nth 3 bams))))
                                 (if (numberp (aref matrix row col))
                                     (format str-out (mjr_numu_code (aref matrix row col) :lang lang))
                                     (error "mjr_mat_code: Matrix element was not a number!"))
                                 (format str-out (cond ((and (= col cols-1) (= row rows-1)) (nth 4 bams))
                                                       ((= col cols-1)                      (nth 5 bams))
                                                       ((= row rows-1)                      (nth 6 bams))
                                                       ('t                                  (nth 7 bams))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_transpose (matrix &optional (hermitian 't))
  "Hermitian (or not if HERMITIAN is NIL) Transpose of the given matrix"
  (let ((matrix (mjr_mat_cv2m matrix)))
    (mjr_mat_make-from-func (lambda (i j) (let ((v (aref matrix j i))) (if hermitian (conjugate v) v)))
                            :rows (mjr_mat_cols matrix) :cols (mjr_mat_rows matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_* (&rest mats)
  "Multiply given matrices and/or scalars."
  (if (< 2 (length mats))
      (reduce #'mjr_mat_* mats)
      (let* ((arg1 (mjr_mat_cv2m (first mats)))
             (arg2 (mjr_mat_cv2m (second mats))))
        (if (numberp arg1)
            (if (numberp arg2)
                (* arg1 arg2)                                     ; Both are numbers
                (mjr_arr_unary-map2 arg2 (lambda (x) (* x arg1))))      ; First is number, second is matrix
            (if (numberp arg2)
                (mjr_arr_unary-map2 arg1 (lambda (x) (* x arg2)))       ; Second is number, first is matrix
                (let* ((rows1  (mjr_mat_rows arg1))               ; Both are NOT-numbers (vectors and/or matrices hopefully)
                       (cols1  (mjr_mat_cols arg1))
                       (cols2  (mjr_mat_cols arg2))
                       (rows2  (mjr_mat_rows arg2))
                       (rowlst (mjr_arr_get-rows arg1))
                       (collst (mjr_arr_get-cols arg2))
                       (newmat (mjr_mat_make-zero rows1 cols2)))
                  (if (not (= cols1 rows2))
                      (error "mjr_mat_*: Incompatible matrix dimensions.")
                      (loop for row from 0
                            for the-row in rowlst
                            finally (return newmat) do
                            (loop for col from 0
                                  for the-col in collst do
                                  (setf (aref newmat row col) (mjr_vec_dot the-row the-col nil)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-linear-tform (mat vec)
  "Apply the linear transform (MAT) to the vector (VEC).
Very fast.  NO ERROR CHECKING"
  (let* ((m (length vec))
         (n (array-dimension mat 1))
         (v (make-array m :initial-element 0)))
    (dotimes (r m v)
      (dotimes (c n v)
        (incf (aref v r) (* (aref mat r c) (aref vec c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_expt (matrix an-integer)
  "Compute the positive integer power of MATRIX.  Returns NIL if the computation can't be performed."
  (if (and (integerp an-integer) (< 0 an-integer))
      (let ((newmat (mjr_arr_copy2 matrix)))
        (dotimes (i (1- an-integer) newmat)
          (setf newmat (mjr_mat_* newmat matrix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_+ (&rest mats)
  "Add matrices and/or scalars.
If a matrix is added to a scalar, then the scalar is added to each element of the matrix."
  (if (< 2 (length mats))
      (reduce #'mjr_mat_+ mats)
      (let ((arg1 (first mats))
            (arg2 (second mats)))
        (if (numberp arg1)
            (if (numberp arg2)
                (+ arg1 arg2)                                      ; Both are numbers
                (mjr_arr_unary-map2 arg2 (lambda (x) (+ arg1 x))))       ; First is number, second is matrix
            (if (numberp arg2)
                (mjr_arr_unary-map2 arg1 (lambda (x) (+ x arg2)))        ; Second is number, first is matrix
                (mjr_arr_binary-map2 (first mats) (second mats) '+))))))  ; Both are matrices

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_ew* (&rest mats)
  "Element-wise multiplication of matrices"
  (if (< 2 (length mats))
      (reduce #'mjr_mat_ew* mats)
    (mjr_arr_binary-map2 (first mats) (second mats) '*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_ew/ (&rest mats)
  "Element-wise division of matrices"
  (if (< 2 (length mats))
      (reduce #'mjr_mat_ew/ mats)
    (mjr_arr_binary-map2 (first mats) (second mats) '/)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_- (&rest mats)
  "Subtract matrices and/or scalars or negate a single matrix or scalar"
  (if (< 2 (length mats))
      (reduce #'mjr_mat_- mats)
      (let ((arg1 (first mats))
            (arg2 (second mats)))
            (if (numberp arg1)
                (if arg2
                    (if (numberp arg2)
                        (- arg1 arg2)                                      ; Both are numbers
                        (mjr_arr_unary-map2 arg2 (lambda (x) (- arg1 x))))       ; First is number, second is matrix
                    (- arg1))                                              ; Single numeric argument
                (if arg2
                    (if (numberp arg2)
                        (mjr_arr_unary-map2 arg1 (lambda (x) (- x arg2)))        ; Second is number, first is matrix
                        (mjr_arr_binary-map2 arg1 arg2 '-))                       ; Both are matrices
                    (mjr_arr_unary-map2 arg1 '-))))))                            ; Single matrix argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_s/ (arg1 arg2)
  "Scalar division -- divide each element of the matrix by the scalar.  The argument order is irrelevant."
  (if (numberp arg1)
      (mjr_arr_unary-map2 (mjr_mat_cv2m arg2) (lambda (x) (/ x arg1)))
    (mjr_arr_unary-map2 (mjr_mat_cv2m arg1) (lambda (x) (/ x arg2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_float (matrix)
  "Convert matrix elements from rational/complex rational float/complex float"
  (mjr_arr_unary-map2 (mjr_mat_cv2m matrix)
                      (lambda (x) (if (complexp x)
                                      (complex (float (realpart x)) (float (imagpart x)))
                                      (if (numberp x) (float x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rationalize (matrix)
  "Convert matrix elements from float/float complex into rational/complex rational"
  (mjr_arr_unary-map2 (mjr_mat_cv2m matrix)
                      (lambda (x) (if (complexp x)
                                      (complex (rationalize (realpart x)) (rationalize (imagpart x)))
                                      (if (numberp x) (rationalize x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_diag (matrix)
  "Extract the diagonal of the given matrix.  Return it as a 1D array (vector)"
  (let* ((matrix (mjr_mat_cv2m matrix))
         (psiz   (min (mjr_mat_rows matrix) (mjr_mat_cols matrix)))
         (newvec (mjr_vec_make-const psiz)))
    (dotimes (i psiz newvec)
      (setf (aref newvec i) (aref matrix i i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_every-idx (pred rows-or-matrix &optional cols-or-missing)
  "Non-nil if PRED is 't for every tuple (i,j).  Note difference from built in EVERY function."
  (let* ((rows-or-matrix (mjr_mat_cv2m rows-or-matrix))
         (rows  (if cols-or-missing rows-or-matrix  (mjr_mat_rows rows-or-matrix)))
         (cols  (if cols-or-missing cols-or-missing (mjr_mat_cols rows-or-matrix))))
    (dotimes (row rows (not nil))
      (dotimes (col cols)
        (if (not (funcall pred row col)) (return-from mjr_mat_every-idx nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_test-property-struct (matrix &rest mp-list)
  "Return non-nil if the given matrix has all of the specified matrix properties.
See mjr_mat_test-property-math for non-structural properties.

  Symbol                Description                                            elt_comp type
  :mp-number          - All elements are numbers                               (N/A)    Homogeneous on a_{ij}
  :mp-real            - All elements are real                                  (N/A)    Homogeneous on a_{ij}
  :mp-integer         - All elements are integers                              (N/A)    Homogeneous on a_{ij}
  :mp-rational        - All elements are rational                              (N/A)    Homogeneous on a_{ij}
  :mp-complex         - All elements are complex                               (N/A)    Homogeneous on a_{ij}
  :mp-positive        - All elements numeric and positive                      (fuzzy)  Homogeneous on a_{ij}
  :mp-zero            - All elements numeric and zero                          (fuzzy)  Homogeneous on a_{ij}
  :mp-constant        - All elements numeric and constant                      (fuzzy)  Homogeneous on a_{ij}
  :mp-binary          - All elements are 0 or 1                                (fuzzy)  Homogeneous on a_{ij}
  :mp-identity        - 1's on the diagonal, 0 elsewhere                       (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diagonal        - off diagonal elements zero                             (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diagonal-fr     - On diagonal !=0, off diagonal elements 0 (full rank)   (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-abs-unit   - Diagonal elements are all 1 or -1                      (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-unit       - Diagonal elements are all 1                            (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-const      - Diagonal elements are all equal                        (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-non-zero   - Diagonal elements are all non-zero                     (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-zero       - Diagonal elements are all zero                         (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-diag-pos        - Diagonal elements are positive                         (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-signature       - :mp-diagonal & :mp-diag-abs-unit                       (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-m-diagonal      - Generalized mult-diagonal                              * CAN'T TEST * CAN'T TEST * CAN'T TEST *
  :mp-tri-diagonal    - 0's off the sub/main/super-diagonal                    (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-u-triangular    - only 0's below the diagonal                            (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-l-triangular    - only 0's above the diagonal                            (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-u-hessenberg    - only 0's below the sub-diagonal                        (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-l-hessenberg    - only 0's above the super-diagonal                      (fuzzy)  Homogeneous on (a_{ij}, i, j)
  :mp-symmetric       - A^t = A                                                (fuzzy)  Homogeneous on (a_{ij}, a_{ji}, i, j)
  :mp-hermitian       - A^* = A                                                (fuzzy)  Homogeneous on (a_{ij}, a_{ji}, i, j)
  :mp-antisymmetric   - A^t = -A   'skew-symmetric'                            (fuzzy)  Homogeneous on (a_{ij}, a_{ji}, i, j)
  :mp-antihermitian   - A^* = -A   'skew-Hermitian'                            (fuzzy)  Homogeneous on (a_{ij}, a_{ji}, i, j)
  :mp-hankel          - Constant skew-diagonals <=> A[i,j]=A[i-1,j+1]          (fuzzy)
  :mp-persymmetric    - A[i,j]=A[n-j+1, n-i+1]                                 (fuzzy)
  :mp-toeplitz        - A[i,j]=A[i+1, i+1]                                     (fuzzy)
  :mp-shape-empty     - A has zero rows (and thus columns)                     (N/A)    shape
  :mp-shape-nonempty  - A has more than 0 rows (and thus more than 0 columns)  (N/A)    shape
  :mp-shape-square    - rows = cols                                            (N/A)    shape
  :mp-shape-tall      - rows > cols                                            (N/A)    shape
  :mp-shape-wide      - rows < cols                                            (N/A)    shape
  :mp-sqr-perm        - a SQUARE permutation matrix                            (fuzzy)  Non-homogeneous
  :mp-sqr-perm-c      - A product of a :mp-sqr-perm & :mp-diagonal-fr matrix   (fuzzy)  Non-homogeneous
  :mp-diag-dom-row    - diagonally dominant by rows                            (fuzzy)  Non-homogeneous
  :mp-diag-dom-col    - diagonally dominant by columns                         (fuzzy)  Non-homogeneous
  :mp-diag-dom-row-st - Strictly diagonally dominant by rows                   (fuzzy)  Non-homogeneous
  :mp-diag-dom-col-st - Strictly diagonally dominant by columns                (fuzzy)  Non-homogeneous
  :mp-hyperbolic      - 2x2, real, symmetric, constant diagonal, det=1         (fuzzy)  Non-homogeneous
  :mp-rotation        - 2x2, real, a_{01}=-a_{10}, constant diagonal, det=1    (fuzzy)  Non-homogeneous"
  (if (mjr_mat_matp matrix 't)
      (if (cdr mp-list)
          (every (lambda (mp) (mjr_mat_test-property-struct matrix mp)) mp-list)
          (let ((the-mp (first mp-list))
                (rows   (mjr_mat_rows matrix))
                (cols   (mjr_mat_cols matrix)))
            (case the-mp
              (:mp-identity           (mjr_mat_every-idx (lambda (i j) (if (= i j) (mjr_cmp_= 1 (aref matrix i j)) (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-hankel             (mjr_mat_every-idx (lambda (i j) (or (= 0 i) (= (- rows 1) j) (mjr_cmp_= (aref matrix i j) (aref matrix (1- i) (1+ j))))) matrix))
              (:mp-toeplitz           (mjr_mat_every-idx (lambda (i j) (or (= i (1- rows)) (= j (1- cols)) (mjr_cmp_= (aref matrix i j) (aref matrix (1+ i) (1+ j)))))  matrix))
              (:mp-persymmetric       (mjr_mat_every-idx (lambda (i j) (mjr_cmp_= (aref matrix i j)                (aref matrix (- cols j 1) (- rows i 1))))  matrix))
              (:mp-perhermitian       (mjr_mat_every-idx (lambda (i j) (mjr_cmp_= (aref matrix i j)     (conjugate (aref matrix (- cols j 1) (- rows i 1))))) matrix))
              (:mp-symmetric          (mjr_mat_every-idx (lambda (i j) (or (> i j) (mjr_cmp_= (aref matrix i j)                (aref matrix j i))))           matrix))
              (:mp-hermitian          (mjr_mat_every-idx (lambda (i j) (or (> i j) (mjr_cmp_= (aref matrix i j)     (conjugate (aref matrix j i)))))          matrix))
              (:mp-antisymmetric      (mjr_mat_every-idx (lambda (i j) (or (> i j) (mjr_cmp_= (- (aref matrix i j))            (aref matrix j i))))           matrix))
              (:mp-antihermitian      (mjr_mat_every-idx (lambda (i j) (or (> i j) (mjr_cmp_= (- (aref matrix i j)) (conjugate (aref matrix j i)))))          matrix))
              (:mp-diagonal           (mjr_mat_every-idx (lambda (i j) (or (= i j) (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-diagonal-fr        (mjr_mat_every-idx (lambda (i j) (if (= i j) (mjr_cmp_!=0 (aref matrix i j)) (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-signature          (mjr_mat_every-idx (lambda (i j) (if (= i j) (mjr_cmp_= 1 (abs (aref matrix i j))) (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-diag-abs-unit      (dotimes (i (min rows cols) 't) (if (mjr_cmp_!= 1 (abs (aref matrix i i))) (return-from mjr_mat_test-property-struct nil))))
              (:mp-diag-unit          (dotimes (i (min rows cols) 't) (if (mjr_cmp_!= 1      (aref matrix i i))  (return-from mjr_mat_test-property-struct nil))))
              (:mp-diag-pos           (dotimes (i (min rows cols) 't) (if (mjr_cmp_>= 0      (aref matrix i i))  (return-from mjr_mat_test-property-struct nil))))
              (:mp-diag-non-zero      (dotimes (i (min rows cols) 't) (if (mjr_cmp_=0        (aref matrix i i))  (return-from mjr_mat_test-property-struct nil))))
              (:mp-diag-zero          (dotimes (i (min rows cols) 't) (if (mjr_cmp_!=0       (aref matrix i i))  (return-from mjr_mat_test-property-struct nil))))
              (:mp-diag-const         (dotimes (i (min rows cols) 't) (if (mjr_cmp_!=        (aref matrix i i) (aref matrix 0 0)) (return-from mjr_mat_test-property-struct nil))))
              (:mp-tri-diagonal       (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_>= 1 (mjr_numu_absdif i j)) (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-u-triangular       (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_<= i j)                     (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-l-triangular       (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_>= i j)                     (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-u-hessenberg       (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_<= i (1+ j))                (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-l-hessenberg       (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_>= i (1- j))                (mjr_cmp_=0 (aref matrix i j)))) matrix))
              (:mp-number             (mjr_mat_every-idx (lambda (i j) (numberp     (aref matrix i j))) matrix))
              (:mp-real               (mjr_mat_every-idx (lambda (i j) (realp       (aref matrix i j))) matrix))
              (:mp-integer            (mjr_mat_every-idx (lambda (i j) (integerp    (aref matrix i j))) matrix))
              (:mp-rational           (mjr_mat_every-idx (lambda (i j) (rationalp   (aref matrix i j))) matrix))
              (:mp-complex            (mjr_mat_every-idx (lambda (i j) (complexp    (aref matrix i j))) matrix))
              (:mp-positive           (mjr_mat_every-idx (lambda (i j) (mjr_cmp_< 0 (aref matrix i j))) matrix))
              (:mp-zero               (mjr_mat_every-idx (lambda (i j) (mjr_cmp_=0  (aref matrix i j))) matrix))
              (:mp-binary             (mjr_mat_every-idx (lambda (i j) (or (mjr_cmp_=0 (aref matrix i j)) (mjr_cmp_= 1 (aref matrix i j)))) matrix))
              (:mp-constant           (mjr_mat_every-idx (lambda (i j) (mjr_cmp_=0 (aref matrix 0 0) (aref matrix i j))) matrix))
              (:mp-shape-empty        (and (= 0 rows) (= 0 cols)))
              (:mp-shape-nonempty     (and (< 0 rows) (< 0 cols)))
              (:mp-shape-square       (= rows cols))
              (:mp-shape-tall         (> rows cols))
              (:mp-shape-wide         (< rows cols))
              (:mp-sqr-perm           (mjr_mat_test-property-struct matrix :mp-binary :mp-sqr-perm-c)) ; Sorta a slow way to do it..
              (:mp-sqr-perm-c         (and (mjr_mat_test-property-struct matrix :mp-shape-square)
                                           (if (= 1 rows)
                                               (mjr_cmp_!=0 (aref matrix 0 0))
                                               (let ((row-fnd (make-array rows :initial-element nil))
                                                     (col-fnd (make-array rows :initial-element nil)))
                                                 (dotimes (i rows)
                                                   (dotimes (j cols)
                                                     (if (mjr_cmp_!=0 (aref matrix i j))
                                                         (if (or (aref row-fnd i) (aref col-fnd j))
                                                             (return-from mjr_mat_test-property-struct nil)  ;; Just found too many non-zeros. Yikes!!
                                                             (setf (aref row-fnd i) 't
                                                                   (aref col-fnd j) 't)))))
                                                 (and (every #'identity row-fnd) (every #'identity col-fnd)))))) ;; Should find exactly 1 !=0 in each row and col?
              ((:mp-diag-dom-row-st
                :mp-diag-dom-col-st
                :mp-diag-dom-row
                :mp-diag-dom-col)     (and (mjr_mat_test-property-struct matrix :mp-shape-square) ;; We don't do this much, so it can be slow...
                                           (dotimes (row rows 't)
                                             (if (funcall (if (or (eq the-mp :mp-diag-dom-row)
                                                                  (eq the-mp :mp-diag-dom-col))
                                                              #'mjr_cmp_<
                                                              #'mjr_cmp_<=)
                                                          (abs (aref matrix row row))
                                                          (loop for col from 0 upto (1- cols)
                                                                when (not (= col row))
                                                                sum (abs (if (eq the-mp :mp-diag-dom-row)
                                                                             (aref matrix row col)
                                                                             (aref matrix col row)))))
                                                 (return nil)))))
            (:mp-hyperbolic         (and (= rows 2) (= cols 2)
                                         (mjr_cmp_= (aref matrix 0 0) (aref matrix 1 1))
                                         (mjr_cmp_= (aref matrix 0 1) (aref matrix 1 0))
                                         (mjr_cmp_= 1 (- (* (aref matrix 0 0) (aref matrix 1 1))
                                                         (* (aref matrix 0 1) (aref matrix 1 0))))))
            (:mp-rotation           (and (= rows 2) (= cols 2)
                                         (mjr_cmp_= (aref matrix 0 0) (aref matrix 1 1))
                                         (mjr_cmp_= (aref matrix 0 1) (- (aref matrix 1 0)))
                                         (mjr_cmp_= 1 (- (* (aref matrix 0 0) (aref matrix 1 1))
                                                         (* (aref matrix 0 1) (aref matrix 1 0))))))
            (otherwise              (error "mjr_mat_test-property-struct: Matrix Property Unknown")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_fill-stats (matrix &optional eps)
  "Count non-zero matrix elements.

Two values are returned. The first is a list with counts for various parts of the matrix: above the super-diagonal, on the super-diagonal, on the main
diagonal, on the sub-diagonal, below the sub-diagonal.  The second is the number of non-zero elements in the matrix.

This function uses MJR_CMP_!= to identify non-zero elements."
  (let ((rows               (mjr_mat_rows matrix))
        (cols               (mjr_mat_cols matrix))
        (non-zero-diag-zero 0)
        (non-zero-diag-up   0)
        (non-zero-diag-dn   0)
        (non-zero-lower     0)
        (non-zero-upper     0))
    (dotimes (row rows (values
                        (/ (+ non-zero-upper non-zero-diag-up non-zero-diag-zero non-zero-diag-dn non-zero-lower) (* rows cols))
                        (list non-zero-upper non-zero-diag-up non-zero-diag-zero non-zero-diag-dn non-zero-lower)
                        (+ non-zero-upper non-zero-diag-up non-zero-diag-zero non-zero-diag-dn non-zero-lower)))
      (dotimes (col cols)
        (if (mjr_cmp_!=0 (aref matrix row col) eps)
            (cond ((= row col)      (incf non-zero-diag-zero))
                  ((= (1+ row) col) (incf non-zero-diag-up))
                  ((= row (1+ col)) (incf non-zero-diag-dn))
                  ((> row col)      (incf non-zero-lower))
                  ((< row col)      (incf non-zero-upper))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_print (matrix &key fmt-str (out-file nil) (fmt-type "~A") ele-pad filter-func eps)
  "Print a matrix.  Return matrix -- so this may be inserted inside nested function calls.

If a is not a matrix, then it will be printed out with ~A~&.

Each element of the matrix is transformed, via :filter-func, formatted according to :fmt-str or :ele-pad and :fmt-type, and then printed (to STDOUT,
when :out-file is nil; or a file, when :out-file is a filename string).

The transformation argument, :filter-func, is used to specify a transformation that should be applied to each element.  It may be a function, a symbol bound
to a function, or one of :zp (zero pattern), :zbp (zero binary pattern), or :pnp (positive, negative pattern).  These last three are used to see an overview
of the the zero structure of the matrix -- they all set
:ele-pad to 1 if it is not provided.

The format string used for each matrix element is :fmt-str, if provided, or it will be derived from :fmt-type and :ele-pad.  When deriving :fmt-str
from :fmt-type and :ele-pad, the value of :fmt-type provides the basic 'type' (i.e. ~A, ~S, etc...), a minimum width using that format type is computed,
and :ele-pad is then added to that width to produce the final :fmt-str.  For example, ~4A will be the result of 3 character wide elements with :fmt-type ~A
and :ele-pad of 1.  When both FMT-STR and FMT-TYPE are provided, then FMT-STR wins.  The default value of :ele-pad is 3 unless :filter-func is one
of :zp, :zbp, or :pnp."
  (let ((dest (if out-file (open out-file :direction :output :if-exists :supersede :if-does-not-exist :create) 't)))
    (if (and (mjr_mat_matp matrix 't) (mjr_mat_test-property-struct matrix :mp-shape-nonempty))
        (let* ((ele-pad     (or ele-pad
                                (cond ((position filter-func (list :zp :zbp :pnp)) 1)
                                      ('t                                          3))))
               (filter-func (cond ((null filter-func)    (lambda (x) x))
                                  ((eq filter-func :zp)  (lambda (x) (if (mjr_cmp_!=0 x eps) "#" ".")))
                                  ((eq filter-func :zbp) (lambda (x) (if (mjr_cmp_!=0 x eps) "1" "0")))
                                  ((eq filter-func :pnp) (lambda (x) (if (mjr_cmp_!=0 x eps) (if (< x 0) "-" "+") "0")))
                                  ('t                    filter-func)))
               (newmat      (mjr_arr_unary-map2 (mjr_mat_cv2m matrix) filter-func))
               (rows        (mjr_mat_rows newmat))
               (cols        (mjr_mat_cols newmat))
               (fmt-str     (or fmt-str
                                (concatenate
                                 'string
                                 "~"
                                 (format nil "~d" (+ ele-pad (mjr_util_max-print-width
                                                              (map 'vector filter-func (mjr_arr_nreflow-to-vec matrix)) fmt-type)))
                                 "<" fmt-type "~>"))))
          (dotimes (row rows)
            (format dest "~%")
            (dotimes (col cols)
              (format dest fmt-str (aref newmat row col))))
          (format dest "~%"))
        (format dest "~S~&" matrix))
    (if out-file (close dest)))
  matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_factor-ldlt (matrix &optional (return-d-as-vector nil))
  "Return the LDLT decomposition (nil on failure) of a square, symetric, positive definite matrix.  L is lower triangular, and D is diagonal.

If return-d-as-vector is 't, then D is a vector containing the diagonal elements.
If the input is not symmetric or square, this function may fail silently returning invalid results.  

Recover MATRIX with:
   * return-d-as-vector is nil:      (mjr_mat_* L D                     (mjr_mat_transpose L)) 
   * return-d-as-vector is not nil:  (mjr_mat_* L (mjr_mat_make-diag D) (mjr_mat_transpose L))

This LTLT factorization is closely related to the Cholesky factorization C*t(C) in that C = L*sqrt(D).  Note that, unlike the Cholesky factorization, this
algorithm has the charming characteristic of not taking square roots.  If the input matrix is rational, then the output will be rational as well.

If the matrix is positive definite, this function will succeed; however, success doesn't imply that MATRIX is positive definite.  This function may be used to
determine if MATRIX is positive definite.  If this function fails, then MATRIX is not positive.  If it success and: 1) all the diagonal elements of D must be
positive, then MATRIX is positive definite.  2) if all the diagonal elements of D are non-negative with some being zero, then MATRIX is positive
semi-definite."
;; This function is performance sensitive as it may be used in tight loops in other functions.  Thus we have some oddities. 1) note the two almost identical
;; loops selected by a gigantic if statement -- putting the if in side the loops would be much less code, but much slower.  Also note the use of loop to add
;; things up instead of mjr_numu_sum
  (if return-d-as-vector
      (let* ((rows (mjr_mat_rows matrix))
             (L    (mjr_mat_make-identity rows))
             (D    (mjr_vec_make-const rows 1)))
        (dotimes (row rows (values L D))
          (dotimes (col rows)
            (cond ((> row col) (let ((Dcol (aref D col)))
                                 (if (mjr_cmp_=0 Dcol)
                                     (return-from mjr_mat_factor-ldlt nil)
                                     (setf (aref L row col)
                                           (* (/ Dcol) (- (aref matrix row col)
                                                          (loop for k from 0 upto (1- row)
                                                                sum (* (aref L row k) (conjugate (aref L col k)) (aref D k)))))))))
                  ((= row col) (setf (aref D row)
                                     (- (aref matrix row col)
                                        (if (= 0 col)
                                            0
                                            (loop for k from 0 upto (1- col)
                                                  for lrk = (aref L row k)
                                                  sum (* lrk (conjugate lrk) (aref D k)))))))))))
      (let* ((rows (mjr_mat_rows matrix))
             (L    (mjr_mat_make-identity rows))
             (D    (mjr_mat_make-identity rows)))
        (dotimes (row rows (values L D))
          (dotimes (col rows)
            (cond ((> row col) (let ((Dcol (aref D col col)))
                                 (if (mjr_cmp_=0 Dcol)
                                     (return-from mjr_mat_factor-ldlt nil)
                                     (setf (aref L row col)
                                           (* (/ Dcol) (- (aref matrix row col)
                                                          (loop for k from 0 upto (1- row)
                                                                sum (* (aref L row k) (conjugate (aref L col k)) (aref D k k)))))))))
                  ((= row col) (setf (aref D row row)
                                     (- (aref matrix row col)
                                        (if (= 0 col)
                                            0
                                            (loop for k from 0 upto (1- col)
                                                  for lrk = (aref L row k)
                                                  sum (* lrk (conjugate lrk) (aref D k k)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rowop-swap (matrix row1 row2)
  "Exchange row1 and row2 of a matrix.
This function is designed for interactive use at the REPL:  It is not fast, and it performs extra error checking."
  (let* ((matrix (mjr_mat_cv2m  matrix))
         (cols   (mjr_mat_cols  matrix))
         (rows   (mjr_mat_rows  matrix))
         (newmat (mjr_arr_copy2 matrix)))
    (cond ((or (>= row1 rows) (>= row2 rows) (> 0 row1) (> 0 row2)) (error "mjr_mat_rowop-swap: Row argument out of range!"))
          ((= row1 row2)                                            (error "mjr_mat_rowop-swap: Row argument are equal!"))
          ((>= 0 rows)                                              (error "mjr_mat_rowop-swap: Matrix is empty!")))
    (dotimes (col cols newmat)
      (rotatef (aref newmat row1 col) (aref newmat row2 col)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rowop-mult (matrix row &optional factor)
  "Multiply row by factor.  If missing, factor will be set to unitize the leading non-zero element.
This function is designed for interactive use at the REPL:  It is not fast, and it performs extra error checking.
This function uses MJR_CMP_!= to identify non-zero elements when FACTOR is not provided."
  (let* ((matrix (mjr_mat_cv2m  matrix))
         (cols   (mjr_mat_cols  matrix))
         (rows   (mjr_mat_rows  matrix))
         (newmat (mjr_arr_copy2 matrix)))
    (cond ((or (>= row rows) (> 0 row))  (error "mjr_mat_rowop-mult: Row argument out of range!"))
          ((>= 0 rows)                   (error "mjr_mat_rowop-mult: Matrix is empty!")))
    (if (not factor)  ;; Set factor to reciprocal of first non-zero element of row, or 1
        (setq factor (/ (or (find-if 'mjr_cmp_!=0 (mjr_arr_get-row newmat row)) 1))))
    (dotimes (col cols newmat)
      (setf (aref newmat row col) (* factor (aref newmat row col))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rowop-div (matrix row &optional factor)
  "Divide row by factor.  If missing, factor will be set to unitize the leading non-zero element.
This function is designed for interactive use at the REPL:  It is not fast, and it performs extra error checking.
Returns nil if factor is zero. This function uses MJR_CMP_!= to identify non-zero elements when FACTOR is not provided."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (cols   (mjr_mat_cols  matrix))
         (rows   (mjr_mat_rows  matrix))
         (newmat (mjr_arr_copy2 matrix)))
    (cond ((and factor (mjr_cmp_=0 factor)) (error "mjr_mat_rowop-div: Factor is zero!"))
          ((or (>= row rows) (> 0 row))     (error "mjr_mat_rowop-div: Row argument out of range!"))
          ((>= 0 rows)                      (error "mjr_mat_rowop-div: Matrix is empty!")))
    (if (not factor)  ;; Set factor to first non-zero element of row, or 1
        (setq factor (or (find-if 'mjr_cmp_!=0 (mjr_arr_get-row newmat row)) 1)))
    (dotimes (col cols newmat)
      (setf (aref newmat row col) (/ (aref newmat row col) factor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rowop-mult-add (matrix row1 row2 &optional factor)
  "Multiply row1 by factor and add it to row2.
If missing, factor is set to eliminate the element of row2 directly above/below the leading non-zero element of row1.
This function is designed for interactive use at the REPL:  It is not fast, and it performs extra error checking.
This function uses MJR_CMP_!= to identify non-zero elements when FACTOR is not provided."
  (let* ((matrix (mjr_mat_cv2m  matrix))
         (cols   (mjr_mat_cols  matrix))
         (rows   (mjr_mat_rows  matrix))
         (newmat (mjr_arr_copy2 matrix)))
    (cond ((>= 0 rows)                                               (error "mjr_mat_rowop-mult-add: Matrix is empty!"))
          ((or (>= row1 rows) (>= row2 rows) (> 0 row1) (> 0 row2))  (error "mjr_mat_rowop-mult-add: Row argument out of range!"))
          ((= row1 row2)                                             (error "mjr_mat_rowop-mult-add: Row argument are equal!")))
    (if (not factor) (let ((maxi (position-if 'mjr_cmp_!=0 (mjr_arr_get-row newmat row1))))
                       (setq factor (- (/ (aref newmat row2 maxi) (aref newmat row1 maxi))))))
    (dotimes (col cols newmat)
      (setf (aref newmat row2 col) (+ (aref newmat row2 col) (* factor (aref newmat row1 col)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_orthogonal-zero-pair (x y d)
  "Compute c and s for a 2x2 orthogonal (Givens or Householder) transformation.

                  [ c   s ]                       [ c   s ]
     Givens(d=1): [ _   _ ]    Householder(d=-1): [ _   _ ]
                  [-s   c ]                       [ s  -c ]

The computation is careful to insure that the determinants are real -- 1 for Givens and -1 for Householder.  Overflow is avoided in the computation of the
length by using the MJR_NUMU_HYPOT function.

NOTE: NOT EXPORTED!!!!"
  (if (mjr_cmp_=0 y)
      (values 1 0)
      (if (mjr_cmp_=0 x)
          (values 0 1)
          (let* ((r (mjr_numu_hypot x y))
                 (d (* 1d0 (mjr_numu_signum-pos d)))
                 (c (* d (/ x r)))
                 (s (* d (/ y r))))
            (values c s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_householder-many (x y &optional p)
  "Compute the Householder transformation, call it H, that will map X onto Y.

Any elements of Y that are nil, are ignored -- i.e. if y_i is nil, then (Hx)_i=x_i.
If Y is nil, then is set equal to X.  This is only useful when P is given.

p is nil
   Then compute H such that (Hx)_i=y_i for all non-NIL y_i
p is an integer:
   Then compute H such that (Hx)_i=0 for all non-NIL y_i and i!=p and
                            |(Hx)_p|=||x||
   This is effectively the same as the p=NIL case if |y_p|=||x|| and y_i=0 for non-NIL y_i.

References:
  David S. Watkins (2002); Fundamentals of Matrix Computations
  Golub & Van Loan (1996); Matrix Computations

NOTE: This function is similar to the 'house' function found in Matlab/Octave or Golub&Van Loan, but MJR_MAT_HOUSEHOLDER-MANY is significantly more
sophisticated.

NOTE: NOT EXPORTED!!!!"
  (let* ((len   (length x))
         (y     (or y x))
         (mag-x nil)
         (u     (make-array len :initial-element nil)))                             ;; Fill u with nil's
    (if p
        (let ((max-i (mjr_vvec_map-maxi
                      (list :end (1- len)                                                 ;; Compute largest element of x
                            :map-fun (lambda (i)                                          ;;   so we can get ||x|| using
                                       (and (or (aref y i) (and p (= i p)))               ;;   a stable algorithm..
                                            (abs (aref x i))))))))
              (if (mjr_cmp_!=0 (aref x max-i))
              (setf mag-x
                    (* (abs (aref x max-i))
                       (sqrt (1+ (loop for xi across x                             ;; Compute the 1-norm of x
                                       for yi across y                             ;;   using a stable algorithm..
                                       for i from 0
                                       when (and (or yi (and p (= i p)))
                                                 (not (= max-i i)))
                                       sum (expt (abs (/ xi (aref x max-i))) 2))))))
               (return-from mjr_mat_householder-many (values 1 u)))))               ;; If x=0, then return identity
    (loop for xi across x                                                           ;; Set elements of u
          for yi across y
          for i from 0
          when (or yi (and p (= i p)))
          do (setf (aref u i) (if p                                                 ;; Set according to pivot mode
                                  (* 1d0
                                     (if (= i p)                                    ;; Pivot mode case
                                         (let ((yp (* mag-x
                                                      (mjr_numu_signum-pos xi))))
                                           (mjr_cmp_abs-max (- xi yp) (+ xi yp)))   ;; if on pivot, then set to magnitude
                                         xi))                                       ;; else just xi
                                  (- xi yi))))                                      ;; non-pivot mode case
    (values (/ 2 (mjr_vvec_map-sum (list :points u                                      ;;  Return gamma and u
                                         :map-fun (lambda (ui) (and ui (expt (abs ui) 2))))))
            u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_norm  (matrix &key (norm :infinity))
  "Return the specified (:infinity by default) matrix norm.
Norms supported:
  :infinity     -- The infinity-norm (max row sum)
  :one          -- The 1-norm (max column sum)
  1             -- Same as :one
  :frobenius    -- The Frobenius-norm (square root of the sum of the squares)
  :frobenius-sq -- The Frobenius-norm squared"
  (case norm
    (:infinity        (reduce #'max (loop for arow in (mjr_arr_get-rows (mjr_mat_cv2m matrix))
                                          collect (loop for aele across arow sum (abs aele)))))
    ((1 :one)         (reduce #'max (loop for acol in (mjr_arr_get-cols (mjr_mat_cv2m matrix))
                                          collect (loop for aele across acol sum (abs aele)))))
    (:frobenius-sq    (loop for aele across (mjr_arr_nreflow-to-vec matrix) sum (* aele aele)))
    (:frobenius       (sqrt (mjr_mat_norm matrix :norm :frobenius-sq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_minor (matrix del-row del-col)
  "Return a new matrix with the given row and column deleted.  If del-row is nil, then no row is deleted at all. If del-col is nil, then no column is deleted.
If both are nil, then a copy of the input matrix is returned.  When both are NOT nil, then this function returns the sub-matrix associated with the
del-row/del-col'th minor as normally defined in linear algebra texts -- hence the choice of function name.  Note this function doesn't modify element signs,
it just deletes rows and/or columns."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (cols   (mjr_mat_cols matrix))
         (nrows  (if del-row (1- rows) rows))
         (ncols  (if del-col (1- cols) cols)))
    (if (or (< nrows 1) (< ncols 1))
        (error "mjr_mat_minor: Result is an empty matrix!"))
    (let ((newmat (mjr_mat_make-zero nrows ncols)))
      (dotimes (row nrows newmat)
        (dotimes (col ncols)
          (setf (aref newmat row col) (aref matrix
                                            (if (and del-row (>= row del-row)) (1+ row) row)
                                            (if (and del-col (>= col del-col)) (1+ col) col))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det-small (matrix)
  "Compute the determinant of small matrices (n<5) using explicit formulas. If the matrix is not square, is too big, or is empty,
then the result is nil -- NOT 0!!  For small matrices, this function is hundreds of times faster than the alternatives."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (cols   (mjr_mat_cols matrix)))
    (if (and (= rows cols) (< 0 rows) (> 5 rows))
        (case rows
          (1      (aref matrix 0 0))
          (2      (- (* (aref matrix 0 0) (aref matrix 1 1))
                     (* (aref matrix 0 1) (aref matrix 1 0))))
          (3      (- (+ (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2))
                        (* (aref matrix 0 1) (aref matrix 1 2) (aref matrix 2 0))
                        (* (aref matrix 0 2) (aref matrix 1 0) (aref matrix 2 1)))
                     (* (aref matrix 0 0) (aref matrix 1 2) (aref matrix 2 1))
                     (* (aref matrix 0 1) (aref matrix 1 0) (aref matrix 2 2))
                     (* (aref matrix 0 2) (aref matrix 1 1) (aref matrix 2 0))))
          (4      (- (+ (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2) (aref matrix 3 3))
                        (* (aref matrix 0 0) (aref matrix 1 2) (aref matrix 2 3) (aref matrix 3 1))
                        (* (aref matrix 0 0) (aref matrix 1 3) (aref matrix 2 1) (aref matrix 3 2))
                        (* (aref matrix 0 1) (aref matrix 1 0) (aref matrix 2 3) (aref matrix 3 2))
                        (* (aref matrix 0 1) (aref matrix 1 2) (aref matrix 2 0) (aref matrix 3 3))
                        (* (aref matrix 0 1) (aref matrix 1 3) (aref matrix 2 2) (aref matrix 3 0))
                        (* (aref matrix 0 2) (aref matrix 1 0) (aref matrix 2 1) (aref matrix 3 3))
                        (* (aref matrix 0 2) (aref matrix 1 1) (aref matrix 2 3) (aref matrix 3 0))
                        (* (aref matrix 0 2) (aref matrix 1 3) (aref matrix 2 0) (aref matrix 3 1))
                        (* (aref matrix 0 3) (aref matrix 1 0) (aref matrix 2 2) (aref matrix 3 1))
                        (* (aref matrix 0 3) (aref matrix 1 1) (aref matrix 2 0) (aref matrix 3 2))
                        (* (aref matrix 0 3) (aref matrix 1 2) (aref matrix 2 1) (aref matrix 3 0)))
                     (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 3) (aref matrix 3 2))
                     (* (aref matrix 0 0) (aref matrix 1 2) (aref matrix 2 1) (aref matrix 3 3))
                     (* (aref matrix 0 0) (aref matrix 1 3) (aref matrix 2 2) (aref matrix 3 1))
                     (* (aref matrix 0 1) (aref matrix 1 0) (aref matrix 2 2) (aref matrix 3 3))
                     (* (aref matrix 0 1) (aref matrix 1 2) (aref matrix 2 3) (aref matrix 3 0))
                     (* (aref matrix 0 1) (aref matrix 1 3) (aref matrix 2 0) (aref matrix 3 2))
                     (* (aref matrix 0 2) (aref matrix 1 0) (aref matrix 2 3) (aref matrix 3 1))
                     (* (aref matrix 0 2) (aref matrix 1 1) (aref matrix 2 0) (aref matrix 3 3))
                     (* (aref matrix 0 2) (aref matrix 1 3) (aref matrix 2 1) (aref matrix 3 0))
                     (* (aref matrix 0 3) (aref matrix 1 0) (aref matrix 2 1) (aref matrix 3 2))
                     (* (aref matrix 0 3) (aref matrix 1 1) (aref matrix 2 2) (aref matrix 3 0))
                     (* (aref matrix 0 3) (aref matrix 1 2) (aref matrix 2 0) (aref matrix 3 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det-minor-expand (matrix &key (ex-row 0))
  "Compute the determinant using C's rule.

This function is impractical for matrices over about 10x10, but it it never uses division and thus avoids much arithmetical ugliness.  The keyword argument
EX-ROW specifies the row that used for co-factor expansion for larger matrices.  If the matrix is not square or is empty, then the result is nil -- NOT 0!!"
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (cols   (mjr_mat_cols matrix)))
    (if (<= rows 4)
        (mjr_mat_det-small matrix)
        (if (and (= rows cols) (< 0 rows))
            (loop for i from 0 upto (1- cols)
                  sum (* (aref matrix ex-row i)
                         (if (evenp (+ i ex-row)) 1 -1)
                         (mjr_mat_det-minor-expand (mjr_mat_minor matrix ex-row i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_factor-qdr (matrix)
  "Returns the qdr factorization of MATRIX, or nil if it doesn't exist.

The matrix q is orthogonal'ish (q'q=d^-1), d is diagonal, and r is upper triangular.  MATRIX=q*d*r.

This nonstandard factorization is related to QR factorization in that Q=q*sqrt(d) and R=sqrt(d)*r.  The d matrix has the reciprocal of the length squared of
the i'th column of q at location (i,i) . The reason behind this strange factorization is that it provides some of the useful features of the QR factorization
without requiring square roots.  This means that rational numbers in, produce rational number results.

This is NOT a high performance algorithm.
This function uses MJR_CMP_!=0 to identify non-zero elements."
  (let* ((matrix    (mjr_mat_cv2m matrix))
         (q-hat-vec (mjr_vec_orthogonalize (mjr_arr_get-cols matrix) :method :cgs))
         (vec-norms (map 'vector #'mjr_vec_norm-two-squared q-hat-vec)))
    (if (not (every #'mjr_cmp_!=0 vec-norms))
        (error "mjr_mat_factor-qdr: QDR factorization not possible!")
        (let* ((q-hat-mat (apply #'mjr_arr_cbind q-hat-vec))
               (d-hat-mat (mjr_mat_make-diag (mjr_vec_/ vec-norms)))
               (r-hat-mat (mjr_mat_* (mjr_mat_transpose q-hat-mat) matrix)))
          (values q-hat-mat d-hat-mat r-hat-mat)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_solve-sys-sub (matrix b &key row-ord show-progress return-partial-results)
  "Solve the linear system by substitution.

Should always work on a square, full rank, permutation of a triangular system.

The ROW-ORD function can be used to specify the order in which rows should be examined for solutions.  See the MJR_MAT_SOLVE-SYS-SGE function for a good
example of how to use this argument.

Return

  If all variables are solved, then return is a vector or matrix with the solutions.  If no variables are isolated, then the return is nil.  If some, but not
  all, variables are found, then the return depends upon the value of :RETURN-PARTIAL-RESULTS like so:
    * RETURN-PARTIAL-RESULTS is NIL: the return is nil
    * RETURN-PARTIAL-RESULTS is non-NIL: three values are returned:
          1) The answer matrix/vector which will have some elements set to NIL for unsolved variables
          2) A vector containing an element for each row of the system.  The elements indicate
               non-negative integer : indicates variable we got from this row
               nil                  : indicates this row was consistent
               -1                   : indicates this row was inconstant
               -2                   : indicates this row was not verified
          3) A 'T if all answers are consistent, or a NIL if any are inconsistent or unverifiable"
  (let* ((matrix  (mjr_mat_cv2m  matrix))
         (bm      (mjr_arr_copy2 (mjr_mat_cv2m b)))
         (colsb   (mjr_mat_cols  bm))
         (rowsb   (mjr_mat_rows  bm))
         (rows    (mjr_mat_rows  matrix))
         (cols    (mjr_mat_cols  matrix))
         (goodans 't)
         (var-fnd 0)
         (row-ord (or (and (every #'integerp row-ord) row-ord)
                      (mjr_vec_make-seq :start 0 :end (1- rows))))
         (row2var (make-array rows :initial-element nil))
         (var2row (make-array cols :initial-element nil)))
    (cond ((or (>= 0 rows)  (>= 0 cols))  (error "mjr_mat_solve-sys-sub: Left hand side matrix was empty!"))
          ((or (>= 0 rowsb) (>= 0 colsb)) (error "mjr_mat_solve-sys-sub: Right hand side matrix was empty!"))
          ((not (= rows rowsb))           (error "mjr_mat_solve-sys-sub: Number of rows differ on right and left hand side!")))
    (loop for found-this-iter = nil
          for iter from 0 upto rows
          until (= cols var-fnd)
          do (loop for row across row-ord
                   for cur-var = nil
                   when (= 1 (loop for col from 0 upto (1- cols)
                                   for ccof = (aref matrix row col)
                                   count (if (and (mjr_cmp_!=0 ccof) (null (aref var2row col)))
                                             (progn (setf cur-var col) (aref matrix row col)))))
                   do (let ((cur-coef (aref matrix row cur-var)))                            ; Found one to solve for....
                        (loop for col from 0 upto (1- cols)                                  ; Substitute vars, and subtract
                              for ccof = (aref matrix row col)
                              when (and (mjr_cmp_!=0 ccof) (aref var2row col))
                              do (dotimes (rhsi colsb)
                                   (decf (aref bm row rhsi)
                                         (* ccof (aref bm (aref var2row col) rhsi)))))
                        (dotimes (rhsi colsb)                                                ; divide to solve for var
                          (setf (aref bm row rhsi) (/ (aref bm row rhsi) cur-coef)))
                        (setf (aref var2row cur-var) row)                                    ; mark var as found
                        (setf (aref row2var row) cur-var)                                    ; mark row as used
                        (setf found-this-iter 't)                                            ; We found one..
                        (incf var-fnd))                                                      ; Increment solved counter
                   do (if show-progress
                          (format 't "PROGRESS: mjr_mat_solve-sys-sub: I: ~d R: ~d F: ~d~%" iter row var-fnd)))
          until (null found-this-iter))
    (dotimes (row rows)                                                                      ; Process unused rows
      (if (null (aref row2var row))
          (loop for rhsi from 0 upto (1- colsb)
                for lhsv = (loop for col from 0 upto (1- cols)
                                 when (and (mjr_cmp_!=0 (aref matrix row col)) (aref var2row col))
                                 sum  (* (aref matrix row col) (aref bm (aref var2row col) rhsi))
                                 when (and (mjr_cmp_!=0 (aref matrix row col)) (null (aref var2row col)))
                                 do   (return nil))
                when (null lhsv)
                do   (progn
                       (if show-progress
                           (format 't "WARNING: mjr_mat_solve-sys-sub: Unused row: ~d can not be verified as consistent!~%" row))
                       (if return-partial-results
                           (setf (aref row2var row) -2
                                 goodans            nil)
                           (return-from mjr_mat_solve-sys-sub nil)))
                when (numberp lhsv)
                do   (if (= lhsv (aref bm row rhsi))
                         (if show-progress
                             (format 't "INFO: mjr_mat_solve-sys-sub: Unused row: ~d is consistent!~%" row))
                         (progn
                           (if show-progress
                               (format 't "WARNING: mjr_mat_solve-sys-sub: Unused row: ~d is inconsistent!~%" row))
                           (if return-partial-results
                               (setf (aref row2var row) -1
                                     goodans            nil)
                               (return-from mjr_mat_solve-sys-sub nil)))))))
    (let ((ansm    (make-array (list cols colsb) :initial-element nil))                     ; Construct answer matrix
          (ans-cnt 0))
      (dotimes (col cols)
        (dotimes (rhsi colsb)
          (if (aref var2row col)
              (progn
                (setf (aref ansm col rhsi) (aref bm (aref var2row col) rhsi))
                (incf ans-cnt)))))
      (if (and goodans (= ans-cnt (* cols colsb)))                                          ; Got'em all and they are consistent => return them now
          (if (vectorp b) (mjr_mat_m2cv ansm) ansm)
          (if return-partial-results                                                        ; Didn't get them all, but had return-partial-results set..
              (if (not (zerop ans-cnt))                                                     ; We did get at least one answer
                  (values (if (vectorp b) (mjr_mat_m2cv ansm) ansm) row2var goodans)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-gauss-3!! (a f p w)
  "Apply Gaussian transformation IN-PLACE: a[w,] <- f*a[p,]+a[w,], and return A.  (Add a factor times one row to another row)
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (dotimes (i (array-dimension a 1) a)
    (incf (aref a w i) (* f (aref a p i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-gauss-2!! (a f w)
  "Apply Gaussian transformation IN-PLACE: a[w,] <- f*a[w,], and return A. (scale one row)
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (dotimes (i (array-dimension a 1) a)
    (setf (aref a w i)  (* f (aref a w i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-gauss-1!! (a p w)
  "Apply Gaussian transformation IN-PLACE: a[w,1:nc], a[p,] <- a[p,], a[w,], and return A. (swap rows)
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (dotimes (i (array-dimension a 1) a)
    (rotatef (aref a w i) (aref a p i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-givens-2!! (a c s p w &optional on-right)
  "Apply Givens rotation defined by c & s ([[c,s],[-s,c]]) to rows p & w of a, and return A.
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (let ((cc (conjugate c))
        (sc (conjugate s)))
    (if on-right
        (dotimes (i (array-dimension a 0) a)
          (psetf (aref a i p) (+ (* cc (aref a i p)) (* sc (aref a i w)))
                 (aref a i w) (- (* c  (aref a i w)) (* s  (aref a i p)))))
        (dotimes (i (array-dimension a 1) a)
          (psetf (aref a p i) (+ (* cc (aref a p i)) (* sc (aref a w i)))
                 (aref a w i) (- (* c  (aref a w i)) (* s  (aref a p i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-householder-2!! (a c s p w &optional on-right)
  "Apply Householder reflection defined by c & s ([[c,s],[s,-c]]) to rows p & w of A, and return A.
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (let ((cc (conjugate c))
        (sc (conjugate s)))
    (if on-right
        (dotimes (i (array-dimension a 0) a)
          (psetf (aref a i p) (+ (* cc (aref a i p)) (* sc (aref a i w)))
                 (aref a i w) (- (* s  (aref a i p)) (* c  (aref a i w)))))
        (dotimes (i (array-dimension a 1) a)
          (psetf (aref a p i) (+ (* cc (aref a p i)) (* sc (aref a w i)))
                 (aref a w i) (- (* s  (aref a p i)) (* c  (aref a w i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_apply-householder-many!! (a gamma u &optional on-right)
  "Apply multi-dimensional Householder reflection defined by gamma & u to A, and return A.
NOTE: THE ARRAY A IS MODIFIED DIRECTLY.  THIS FUNCTION HAS SIDE EFFECTS!!!!
NOTE: NO ERROR CHECKING!!!!
NOTE: NOT EXPORTED!!!!"
  (let* ((cols  (array-dimension a 1))
         (rows  (array-dimension a 0))
         (w     (make-array cols :initial-element nil)))
    (if on-right
        (progn (dotimes (row rows)
                 (setf (aref w row)
                       (loop for j from 0
                             for uj across u
                             when uj
                             sum (* uj (aref a row j)))))
               (dotimes (col cols a)
                 (if (aref u col)
                     (dotimes (row rows)
                       (decf (aref a row col)
                             (* gamma (conjugate (aref u col)) (aref w row)))))))
        (progn (dotimes (col cols)
                 (setf (aref w col)
                       (loop for j from 0
                             for uj across u
                             when uj
                             sum (* (conjugate uj) (aref a j col)))))
               (dotimes (row rows a)
                 (if (aref u row)
                     (dotimes (col cols)
                       (decf (aref a row col)
                             (* gamma (aref u row) (aref w col))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_eliminate (matrix &key augment show-progress full-elim elim-method pivot-row pivot-col max-cols return-xform unitize-pivot sort-echelon)
  "Eliminate (make zero) elements of MATRIX and :augment via Gaussian, Givens, & Householder transformations.

Return values (five of them):
  1 A copy of MATRIX that has been eliminated -- we call it MATRIXE
  2 A permutation vector (P[i] indicates that the MATRIX[i,i] element was used as the pivot to eliminate elements column i
  3 nil if :AUGMENT is nil.  Otherwise, a copy of the transformed :AUGMENT matrix is returned -- we call it AUGMENTE
    If AUGMENT is a vector, then AUGMENTE will be a vector too.
    The typical use case is the RHS of a matrix equation -- i.e. the bit that would be 'augmented on' when solving by hand.
  4 nil if :RETURN-XFORM is nil.  Otherwise, the 'transform matrix' -- we will call it T
    Note: T*MATRIX=MATRIXE and that T*AUGMENT=AUGMENTE
  5 The determinant of the transform matrix T
    NOTE: If :SORT-ECHELON & :UNITIZE-PIVOT are nil, then
          |T|=1      when :ELIM-METHOD is :MP-GIVENS-2 or :ELIM-GAUSS-3 -- so |MATRIX|=|MATIXE|)
          abs(|T|)=1 when :ELIM-METHOD is :MP-HOUSEHOLDER-2 or :MP-HOUSEHOLDER-M -- so abs(|MATRIX|)=abs(|MATIXE|)
    NOTE: The option :SORT-ECHELON can change the sign of |T|
    NOTE: The option :UNITIZE-PIVOT can change the value of |T| to just about anything.
    NOTE: |T|*|MATRIX|=|MATRIXE|.  It is normally easy to compute |MATRIXE|.

The algorithm:

  The algorithm traverses across the columns of MATRIX in an order specified by :PIVOT-COL.  For each column, a pivot element is identified according to the
  rule specified by :PIVOT-ROW.  This pivot element is used to construct a linear transformation that will eliminate other elements in that column (which
  elements are determined by :FULL-ELIM).

Which elements are eliminated in each column:

  :FULL-ELIM non-NIL  All elements in the column other than the pivot will be reduced to zero
  :FULL-ELIM is NIL   Only elements in rows that have not previously provided pivot will be eliminated
                      Fastest and highest floating point stability.  Full rank square matrix => permuted triangular matrix

One of three types of transformations may be used to permutation the elimination via :ELIM-METHOD:

  :ELIM-GAUSS-3     Gaussian Type 3 (a*R_1+R_2->R_2)
  :MP-GIVENS-2      2D Givens
  :MP-HOUSEHOLDER-2 2D Householder transformations
  :MP-HOUSEHOLDER-m Multi-dimensional Householder transformations

The column pivoting strategy is selected via :PIVOT-COL:

  :LEFT-RIGHT         March along left to right
                      Fastest option.  Compatible with :SORT-ECHELON = :PIVOT-UP
  :RIGHT-LEFT         March along right to left
                      Compatible with :SORT-ECHELON = :PIVOT-DOWN
  :MAX-NORM-INF       Select the column with the largest element (largest infinity norm)
  :MAX-NORM-TWO       Select the column with the largest Euclidean length
                      Yields the highest floating point stability -- especially good for nearly singular least squares problems
  sequence            A list/vector of integers explicitly specifies the column order (elt i determines the column for step i)

The row pivoting strategy is selected via :PIVOT-ROW:

  :MAX-NON-ZERO       Select the largest non-zero element in absolute value of each column
                      Yields the highest floating point stability, and is the default option.
  :FIRST-NON-ZERO     Select the first non-zero element in each column
                      Fastest option that always reduces non-singular matrices.
                      Results in the simplest possible row permutation vector -- no swaps if the diagonal has no zeros.
  :DIAG-ELEMENT       Select the diagonal element of each column
                      Fastest option.
                      May lead to an error for :ELIM-GAUSS-3.
                      Not a bad option for positive definite matrices.
  :MIN-NON-ZERO-TOP   Select the non-zero element with minimal numerator
                      Only works on rational matrices
                      Tends to produce fewer fractions and smaller denominators

Various row sorting schemes may be used. Gaussian transforms (type 1) are used to achieve the requested row sorting.  The sign of |T| may be changed. Note
also that this option will mean the transform matrix T will generally NOT be unitary when using Givens or Householder transformations.  The sorting is
controlled by the :SORT-ECHELON argument:

  :PIVOT-UP           Move pivot elements up as far as possible.  Good for :PIVOT-COL values of :RIGHT-LEFT
  :PIVOT-DOWN         Move pivot elements down as far as possible.  Good for :PIVOT-COL values of :LEFT-RIGHT
  :PIVOT-DIAG         Move the pivot to the diagonal.  Generally compatible with any :PIVOT-COL ordering scheme.

Misc options:

  :MAX-COLS           Maximum number of columns to eliminate.  If not provided, then the number of columns to eliminate is determined by
                      the size of the matrix or the :PIVOT-COL/:PIVOT-ROW options.

  :SHOW-PROGRESS      Set to 't for progress reports printed during the computation.

  :UNITIZE-PIVOT      Will use Gaussian (type 2) transforms to unitize the pivot elements.  This will change the eigenvalues, eigenvectors, and the
                      determinant. Note also that this option will mean the transform matrix T will generally NOT be unitary when using Givens or Householder
                      transformations.

Some recipes:
  :ELIM-GAUSS-3, :MAX-NORM-INF, :MAX-NON-ZERO
       Gaussian elimination with full pivoting
       Probably the most numerically stable for full rank, square systems
  :ELIM-GAUSS-3, :LEFT-RIGHT, :MAX-NON-ZERO
       Gaussian elimination with partial pivoting
       Normally numerically stable enough for full rank, square systems
  :ELIM-GAUSS-3, :LEFT-RIGHT, :MAX-NON-ZERO, :FULL-ELIM
       Gaussian-Jordan elimination with partial pivoting
  :MP-HOUSEHOLDER-M, :LEFT-RIGHT
       Classical QR factorization with Householder
       Normally good enough for well conditioned least squares problems
  :MP-HOUSEHOLDER-M, :MAX-NORM-TWO
       Classical QR factorization with column pivoting and Householder
       The most numerically stable option for nearly singular least squares problems
  :MP-GIVENS-2, :LEFT-RIGHT
       Classical QR factorization with Givens
       Faster than householder for sparse least squares problems
  :ELIM-GAUSS-3, :LEFT-RIGHT, :MAX-NON-ZERO, :PIVOT-UP
       Variation of Gaussian elimination with partial pivoting that renders row echelon form
       Produces upper triangular results from full rank, square matrices
  :ELIM-GAUSS-3, :LEFT-RIGHT, :MAX-NON-ZERO, :PIVOT-UP, :FULL-ELIM, :UNITIZE-PIVOT
       Variation of Gaussian-Jordan elimination with partial pivoting that renders reduced row echelon form

This function uses MJR_CMP_ functions."
  (let* ((matrix      (mjr_mat_cv2m matrix))
         (cols        (mjr_mat_cols matrix))
         (rows        (mjr_mat_rows matrix))
         (amatrix     (mjr_mat_cv2m augment))
         (arows       (mjr_mat_rows amatrix))
         (rows-left   (mjr_vec_make-const rows 1))
         (cols-left   (mjr_vec_make-const cols 1))
         (row-perm    (mjr_vec_make-const cols nil))
         (newmat      (mjr_arr_copy2 matrix))
         (anewmat     (if amatrix (mjr_arr_copy2 amatrix)))
         (atfrm       (if return-xform (mjr_mat_make-identity rows)))
         (xfrm-det    1)
         (top-row     0)
         (pivot-col   (or pivot-col :left-right))
         (elim-method (or elim-method :elim-gauss-3))
         (pivot-row   (or pivot-row :max-non-zero)))
    (cond ((not (or (member pivot-col '(:right-left :left-right :max-norm-inf :max-norm-two)) (listp pivot-col) (vectorp pivot-col)))
           (error "mjr_mat_eliminate: Unknown column pivot strategy"))
          ((not (or (member pivot-row '(:max-non-zero :min-non-zero-top :first-non-zero :diag-element)) (listp pivot-row) (vectorp pivot-row)))
           (error "mjr_mat_eliminate: Unknown row pivot strategy"))
          ((not (or (null sort-echelon) (member sort-echelon '(:pivot-diag :pivot-up :pivot-down))))
           (error "mjr_mat_eliminate: Unknown sort-echelon option."))
          ((and (not (eq elim-method :elim-gauss-3)) full-elim)
           (error "mjr_mat_eliminate: :FULL-ELIM only compatible with :ELIM-GAUSS-3!"))
          ((and amatrix (not (= rows arows)))
           (error "mjr_mat_eliminate: MATRIX and :AUGMENT have different row counts"))
          ((not (member elim-method '(:mp-givens-2 :elim-gauss-3 :mp-householder-m :mp-householder-2)))
           (error "mjr_mat_eliminate: Unknown elimination strategy")))
    (dotimes (col-iter (or max-cols
                           (min (or (mjr_util_non-empty-seqp pivot-col) cols) (or (mjr_util_non-empty-seqp pivot-row) cols))))
      (let ((rpividx nil)
            (rpivval nil)
            (cpividx nil))
        (cond ((eq pivot-col :left-right)         (setf cpividx col-iter))                                               ; Col pivot
              ((eq pivot-col :right-left)         (setf cpividx (1- (- cols col-iter))))
              ((eq pivot-col :max-norm-two)       (setf cpividx
                                                        (mjr_vvec_map-maxi
                                                         (list :points (map 'list (lambda (x y) (and x y))                     ; Constructing the list of
                                                                            cols-left                                          ; cols is expensive, but
                                                                            (mjr_arr_get-cols newmat))                         ; the code for stable two norm
                                                               :map-fun (lambda (x) (and x (mjr_vec_norm-two x)))))))           ; is painful. :)
              ((eq pivot-col :max-norm-inf)       (loop with maxv = nil
                                                        for j from 0 upto (1- rows)                                      ; We used this construct instead
                                                        when (aref rows-left j)                                          ; of using the previous one
                                                        do (loop for i from 0 upto (1- cols)                             ; because this option corresponds
                                                        for eltv = (aref newmat j i)                                     ; to 'full pivot' when used with
                                                        when (and (aref cols-left i) (mjr_cmp_!=0 eltv)                  ; :max-non-zero for :pivot-row.
                                                                  (or (null maxv) (mjr_cmp_abs> eltv maxv)))             ; It needs to be fast.
                                                        do (progn (format 't "~a ~a~%" i eltv) (setq cpividx i
                                                                 maxv eltv)))))
              ((listp pivot-col)                  (setf cpividx (nth col-iter pivot-col)))
              ((vectorp pivot-col)                (setf cpividx (aref pivot-col col-iter))))
        (setf (aref cols-left cpividx) nil)                                                                              ; Update cols-left vector
        (cond ((eq pivot-row :max-non-zero)       (loop for i from 0 upto (1- rows)                                      ; Row pivot
                                                        for eltv = (aref newmat i cpividx)
                                                        when (and (aref rows-left i) (mjr_cmp_!=0 eltv)
                                                                  (or (not rpivval) (mjr_cmp_abs> eltv rpivval)))
                                                        do (setq rpividx i
                                                                 rpivval eltv)))
              ((eq pivot-row :min-non-zero-top)   (loop with mint = nil
                                                        for i from 0 upto (1- rows)
                                                        for eltv = (aref newmat i cpividx)
                                                        for eltt = (numerator eltv)
                                                        when (and (aref rows-left i) (mjr_cmp_!=0 eltt)
                                                                  (or (not mint) (mjr_cmp_abs< eltt mint)))
                                                        do (setq rpividx i
                                                                 rpivval eltv
                                                                 mint   eltt)))
              ((eq pivot-row :first-non-zero)     (loop for i from 0 upto (1- rows)
                                                        when (and (aref rows-left i) (mjr_cmp_!=0 (aref newmat i cpividx)))
                                                        do (return (setf rpivval (aref newmat i cpividx) rpividx i))))
              ((eq pivot-row :diag-element)       (if (< cpividx rows)
                                                      (setf rpivval (aref newmat cpividx cpividx)
                                                            rpividx cpividx)))
              ((listp pivot-row)                  (setf rpividx (nth col-iter pivot-row)
                                                        rpivval (aref newmat rpividx cpividx)))
              ((vectorp pivot-row)                (setf rpividx (aref pivot-row col-iter)
                                                        rpivval (aref newmat rpividx cpividx))))
        (if show-progress (format 't "~%PROGRESS: mjr_mat_eliminate: elm col ~7d with row ~7d ~%" cpividx rpividx))
        (if rpividx
            (progn
              (if (and sort-echelon top-row                                                                     ;; If we are sorting
                       (setf top-row
                             (case sort-echelon
                               (:pivot-diag (and (< cpividx rows)                                               ;; Sort to diag position
                                                 (aref rows-left cpividx)
                                                 cpividx))
                               (:pivot-up   (position-if #'identity rows-left))                                 ;; Sort up as much as possible
                               (:pivot-down (position-if #'identity rows-left                                   ;; Sort to down as much as possible
                                                         :from-end 't)))))
                  (if (not (= rpividx top-row))                                                                 ;; If the sort location!=current location
                      (progn
                        (setf xfrm-det (- xfrm-det))                                                            ;; Gauss type two changes det
                        (mjr_mat_apply-gauss-1!!             newmat  rpividx top-row)                           ;; apply the xform
                        (if atfrm (mjr_mat_apply-gauss-1!!   atfrm   rpividx top-row))                          ;; apply the xform to atfrm
                        (if amatrix (mjr_mat_apply-gauss-1!! anewmat rpividx top-row))                          ;; apply the xform to amatrix too
                        (setf rpividx top-row))))
              (setf (aref rows-left rpividx) nil                                                                ;; Mark the row as used (so we don't reuse it later)
                    (aref row-perm cpividx)  rpividx)                                                           ;; Update the row permutation vector
              (if (eq elim-method :mp-householder-m)                                                            ;; Householder-many case
                  (let ((x (mjr_arr_get-col newmat cpividx)))
                    (multiple-value-bind (gamma u) (mjr_mat_householder-many x rows-left rpividx)               ;; Compute Householder vector
                      (setf xfrm-det (- xfrm-det))                                                              ;; Update transformation determinant
                      (mjr_mat_apply-householder-many!! newmat gamma u)))                                       ;; Eliminate entire column
                  (dotimes (row rows)                                                                           ;; eliminate each row
                    (if (and (mjr_cmp_!=0 (aref newmat row cpividx))                                            ;; Skip if element is already zero
                             (if full-elim
                                 (not (= row rpividx))                                                          ;; Skip current pivot row only
                                 (aref rows-left row)))                                                         ;; Skip current pivot row and all past pivot rows
                        (if (eq elim-method :elim-gauss-3)                                                      ;; Eliminate via gauss
                            (if (mjr_cmp_!=0 rpivval)
                                (let ((fac (- (/ (aref newmat row cpividx) rpivval))))
                                  (mjr_mat_apply-gauss-3!!             newmat  fac rpividx row)                 ;; apply the xform
                                  (if atfrm (mjr_mat_apply-gauss-3!!   atfrm   fac rpividx row))                ;; apply the xform to atfrm
                                  (if amatrix (mjr_mat_apply-gauss-3!! anewmat fac rpividx row)))               ;; apply the xform to amatrix too
                                (error "mjr_mat_eliminate: Bad pivot"))
                            (let ((xv (aref newmat rpividx cpividx))                                            ;; Eliminate via orthogonal transform
                                  (yv (aref newmat row     cpividx)))
                              (if (eq elim-method :mp-givens-2)
                                  (multiple-value-bind (c s) (mjr_mat_orthogonal-zero-pair xv yv 1)             ;; ortho == givens
                                    (mjr_mat_apply-givens-2!!             newmat  c s rpividx row)              ;; apply the xform
                                    (if atfrm (mjr_mat_apply-givens-2!!   atfrm   c s rpividx row))             ;; apply the xform to atfrm
                                    (if amatrix (mjr_mat_apply-givens-2!! anewmat c s rpividx row)))            ;; apply the xform to amatrix too
                                  (multiple-value-bind (c s) (mjr_mat_orthogonal-zero-pair xv yv -1)            ;; ortho == householder
                                    (setf xfrm-det (- xfrm-det))                                                ;; Update transformation determinant
                                    (mjr_mat_apply-householder-2!!             newmat  c s rpividx row)         ;; apply the xform
                                    (if atfrm (mjr_mat_apply-householder-2!!   atfrm   c s rpividx row))        ;; apply the xform to atfrm
                                    (if amatrix (mjr_mat_apply-householder-2!! anewmat c s rpividx row))))))))) ;; apply the xform to amatrix too
              (if unitize-pivot
                  (let ((fac  (/ (aref newmat rpividx cpividx))))                                               ;; Gauss type two factor
                        (setf xfrm-det (* xfrm-det fac))                                                        ;; Gauss type two changes det
                        (mjr_mat_apply-gauss-2!!             newmat  fac (aref row-perm cpividx))               ;; apply the xform
                        (if atfrm (mjr_mat_apply-gauss-2!!   atfrm   fac (aref row-perm cpividx)))              ;; apply the xform to atfrm
                        (if amatrix (mjr_mat_apply-gauss-2!! anewmat fac (aref row-perm cpividx)))))))          ;; apply the xform to amatrix too
        (if (and show-progress (< rows 50)) (mjr_mat_print newmat :filter-func :zp))))
    (values newmat row-perm (if (vectorp augment) (mjr_mat_m2cv anewmat) anewmat) atfrm xfrm-det)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det-ge (matrix &key show-progress pivot-row)
  "Return the determinant of a matrix or NIL if the matrix is not square or empty.
Any arguments beyond MATRIX will be passed on to MJR_MAT_ELIMINATE."
  (if (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
      (multiple-value-bind (newmat row-perm) (mjr_mat_eliminate matrix :show-progress show-progress :pivot-row pivot-row)
        (if (not (every #'numberp row-perm))
            0                                     ;; Had a nil => matrix is singular => det=0
            (let* ((rows (length row-perm)))      ;; No nil's
              (* (mjr_perm_sgn row-perm)          ;; Note: matrix is square => row-perm is a mathematical permutation
                 (mjr_numu_prod :start 0 :end (1- rows) :seq-fun (lambda (i) (aref newmat (aref row-perm i) i)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_solve-sys-sge (matrix b &rest rest)
  "Solve the linear system.  See MJR_MAT_SOLVE-SYS-SUB for details on return values."
  (multiple-value-bind (newmat row-perm newb)
      (apply #'mjr_mat_eliminate matrix :augment b rest)
    (apply #'mjr_mat_solve-sys-sub newmat newb :row-ord (reverse row-perm) (mjr_util_strip-kwarg rest :keep-list (list :show-progress)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_inv-sge (matrix &key pivot-row show-progress)
  "Compute matrix inverse using MJR_MAT_ELIMINATE via MJR_MAT_SOLVE-SYS-SGE."
  (let ((matrix (mjr_mat_cv2m matrix)))
    (if (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
        (let ((imatrix (mjr_mat_solve-sys-sge matrix (mjr_mat_make-identity matrix)
                                              :pivot-row pivot-row
                                              :show-progress show-progress)))
          imatrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_rank-sge (matrix &key eps0 pivot-row show-progress)
  "Compute matrix rank.  Second return value is the larger of the column count or row count.
This function uses MJR_EPS_=0 and EPS0 recognize zeros vectors."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (cols   (mjr_mat_cols matrix)))
    (if (or (>= 0 rows) (>= 0 cols)) (error "mjr_mat_rows: matrix was empty!"))
    (values
     (min rows cols (loop for r in (mapcar (lambda (x) (mjr_eps_=0 x eps0))
                                           (mjr_arr_get-rows (mjr_mat_eliminate matrix
                                                                                :pivot-row pivot-row
                                                                                :show-progress show-progress)))
                          when (not r) count 1))
     (max rows cols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cpoly-eval (matrix x &key pivot-row show-progress)
  "Compute the value of the characteristic polynomial, |I*x-M|, at x.
This function requires a matrix copy and a determinant computation.
Error if matrix is empty or not square."
  (let* ((matrix (mjr_mat_cv2m  matrix))
         (rows   (mjr_mat_rows  matrix))
         (cols   (mjr_mat_cols  matrix))
         (newmat (mjr_arr_copy2 matrix)))
    (cond ((or (>= 0 rows) (>= 0 cols))   (error "mjr_mat_cpoly-eval: Matrix was empty!"))
          ((not (= rows cols))            (error "mjr_mat_cpoly-eval: Matrix was not square!")))
    (* (if (oddp rows) -1 1)
       (mjr_mat_det-ge (dotimes (i rows newmat)
                                (setf (aref newmat i i) (- (aref matrix i i) x)))
                              :pivot-row pivot-row :show-progress show-progress))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_trace (matrix)
  "Compute trace of the given matrix"
  (reduce '+ (mjr_mat_diag matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_diag-prod (matrix)
  "Compute product of the diagonal elements"
  (reduce '* (mjr_mat_diag matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_det (matrix &optional the-mp)
  "Compute the determinant using with an algorithm heuristically selected for speed.

Current algorithm selection (first matching rule is used): 
   * 4x4 and smaller                => MJR_MAT_DET-SMALL
   * the-mp = :mp-diagonal          => MJR_MAT_DIAG-PROD
   * the-mp = :mp-u-triangular      => MJR_MAT_DIAG-PROD 
   * the-mp = :mp-l-triangular      => MJR_MAT_DIAG-PROD
   * the-mp = :mp-pos-def           => Use MJR_MAT_FACTOR-LDLT to reduce to :MP-DIAGONAL cases
   * the-mp = :mp-tri-diagonal      => Use recursive algorithm of complexity O(n)
   * otherwise                      => MJR_MAT_DET-GE (which effectively reduces to the :MP-L-TRIANGULAR case)"
  (let ((rows (mjr_mat_rows matrix)))
    (if (<= rows 4)
        (mjr_mat_det-small matrix)
        (if (null the-mp)
            (mjr_mat_det-ge matrix)
            (case the-mp
              (nil                    (print "GOG NIL"))
              ((:mp-diagonal
                :mp-u-triangular     
                :mp-l-triangular)     (mjr_mat_diag-prod matrix))
              (:mp-pos-def            (multiple-value-bind (l d) (mjr_mat_factor-ldlt matrix 't)
                                        (if (null l)
                                            (progn (warn "mjr_mat_det: THE-MP was invalid for given MATRIX!  Falling back to general methods!")
                                                   (mjr_mat_det matrix))
                                            (reduce #'* d))))
               (:mp-tri-diagonal      (loop for n from 2 upto rows
                                            for an   = (aref matrix (- n 1) (- n 1))
                                            for bn-1 = (aref matrix (- n 2) (- n 1))
                                            for cn-1 = (aref matrix (- n 1) (- n 2))
                                            for fn-2 = 1 then fn-1
                                            for fn-1 = (aref matrix 0 0) then fn
                                            for fn   = (- (* an fn-1) (* cn-1 bn-1 fn-2))
                                            finally (return fn)))
              (otherwise              (progn (warn "mjr_mat_det: THE-MP was unusable!  Falling back to general methods!")
                                             (mjr_mat_det matrix))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cpoly-fl (matrix)
  "Compute the characteristic polynomial via the Faddeev-Leverrier algorithm"
  (let ((rows   (mjr_mat_rows matrix))
        (cols   (mjr_mat_cols matrix)))
    (cond ((or (>= 0 rows) (>= 0 cols))   (error "mjr_mat_cpoly-eval: Matrix was empty!"))
          ((not (= rows cols))            (error "mjr_mat_cpoly-eval: Matrix was not square!")))
    (let* ((idn   (mjr_mat_make-identity matrix))
           (cpoly (mjr_vec_make-const (1+ rows) 1)))
      (loop for i from 0 upto rows
            for tr = 1 then (mjr_mat_trace b)
            for p = -1 then (/ tr i)
            for b = (mjr_arr_copy2 matrix) then (mjr_mat_* matrix (mjr_mat_- b (mjr_mat_* p idn)))
            do (setf (aref cpoly i) (- p)))
      cpoly)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cpoly (matrix)
  "Compute the characteristic polynomial using with an algorithm heuristically selected for speed.

Current algorithm selection: mjr_mat_cpoly-fl -- the heuristically is a bit simplistic. :)"
  (mjr_mat_cpoly-fl matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_inv-fl (matrix)
  "Compute the matrix inverse via the Faddeev-Leverrier algorithm.

Note that this function is slower than MJR_MAT_INV for floating point matrices, but may be significantly faster for integer ones.  Returns NIL if matrix is
singular, empty, or not square.  Error for non-real matrices."
  (if (mjr_mat_matp matrix 't)
      (let* ((matrix (mjr_mat_cv2m matrix))
             (rows   (mjr_mat_rows matrix))
             (cols   (mjr_mat_cols matrix)))
        (if (and (< 0 rows) (< 0 cols) (= rows cols))
            (if (not (mjr_mat_test-property-struct matrix :mp-real))
                (error "mjr_mat_inv-fl: This function only supports real matrices!")
                (if (= rows 1)
                    (let ((ele (aref matrix 0 0)))
                      (if (mjr_cmp_!=0 ele)
                          (mjr_mat_make-const rows cols (/ ele))))
                    (let* ((im   nil)
                           (idn  (mjr_mat_make-identity matrix))
                           (ctrm nil))
                      (loop for i from 0 upto rows
                            for tr = 1 then (mjr_mat_trace b)
                            for p = -1 then (/ tr i)
                            for it = nil                  then (mjr_mat_- b (mjr_mat_* p idn))
                            for b = (mjr_arr_copy2 matrix) then (mjr_mat_* matrix it)
                            when (= i (- rows 1)) do (setf im it)
                            when (= i (- rows 0)) do (setf im (if (zerop p) nil (mjr_mat_* im (/ p))))
                        do (setf ctrm (- p)))
                  (if (mjr_cmp_!=0 ctrm) im))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_eigen-rational (matrix)
  "Find the rational eigenvalues of MATRIX.

This is not very efficient as it first computes the characteristic polynomial with mjr_mat_cpoly, and then looks for rational roots with
mjr_poly_root-solve-rational -- both functions are not optimized. :)"
  (mjr_poly_root-solve-rational (mjr_mat_cpoly matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_eigen-float (matrix)
  "Find floating point approximations for the eigenvalues of MATRIX.

This is not very efficient as it first computes the characteristic polynomial with mjr_mat_cpoly, and then looks for roots with
mjr_poly_root-solve-search-deflate -- both functions are not optimized. :)"
  (mjr_poly_root-solve-search-deflate (mjr_mat_cpoly matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_cpoly-det (matrix &key show-progress)
  "Compute the characteristic polynomial: |I*x-M|.

For matrices 4x4 and smaller, a direct computation is used.  For larger matrices, polynomial interpolation is used."
  (let ((rows   (mjr_mat_rows matrix))
        (cols   (mjr_mat_cols matrix)))
    (cond ((or (>= 0 rows) (>= 0 cols))   (error "mjr_mat_cpoly-eval: Matrix was empty!"))
          ((not (= rows cols))            (error "mjr_mat_cpoly-eval: Matrix was not square!"))
          ((= 1 rows)    (vector 1
                                     (- (aref matrix 0 0))))
          ((= 2 rows)    (vector 1
                                 (- (mjr_mat_trace matrix))
                                 (mjr_mat_det-minor-expand matrix)))
          ((= 3 rows)    (vector 1
                                 (- (mjr_mat_trace matrix))
                                 (/ (- (mjr_mat_trace (mjr_mat_expt matrix 2)) (expt (mjr_mat_trace matrix) 2)) -2)
                                 (- (mjr_mat_det-minor-expand matrix))))
          ((= 4 rows)    (vector 1
                                 (- (mjr_mat_trace matrix))
                                 (- (+ (* (aref matrix 0 0) (aref matrix 1 1))
                                       (* (aref matrix 0 0) (aref matrix 2 2))
                                       (* (aref matrix 0 0) (aref matrix 3 3))
                                       (* (aref matrix 1 1) (aref matrix 2 2))
                                       (* (aref matrix 1 1) (aref matrix 3 3))
                                       (* (aref matrix 2 2) (aref matrix 3 3)))
                                    (* (aref matrix 0 1) (aref matrix 1 0))
                                    (* (aref matrix 0 2) (aref matrix 2 0))
                                    (* (aref matrix 0 3) (aref matrix 3 0))
                                    (* (aref matrix 1 2) (aref matrix 2 1))
                                    (* (aref matrix 1 3) (aref matrix 3 1))
                                    (* (aref matrix 2 3) (aref matrix 3 2)))
                                 (- (+ (* (aref matrix 0 0) (aref matrix 1 2) (aref matrix 2 1))
                                       (* (aref matrix 0 0) (aref matrix 1 3) (aref matrix 3 1))
                                       (* (aref matrix 0 0) (aref matrix 2 3) (aref matrix 3 2))
                                       (* (aref matrix 0 1) (aref matrix 1 0) (aref matrix 2 2))
                                       (* (aref matrix 0 1) (aref matrix 1 0) (aref matrix 3 3))
                                       (* (aref matrix 0 2) (aref matrix 1 1) (aref matrix 2 0))
                                       (* (aref matrix 0 2) (aref matrix 2 0) (aref matrix 3 3))
                                       (* (aref matrix 0 3) (aref matrix 1 1) (aref matrix 3 0))
                                       (* (aref matrix 0 3) (aref matrix 2 2) (aref matrix 3 0))
                                       (* (aref matrix 1 1) (aref matrix 2 3) (aref matrix 3 2))
                                       (* (aref matrix 1 2) (aref matrix 2 1) (aref matrix 3 3))
                                       (* (aref matrix 1 3) (aref matrix 2 2) (aref matrix 3 1)))
                                    (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2))
                                    (* (aref matrix 0 0) (aref matrix 1 1) (aref matrix 3 3))
                                    (* (aref matrix 0 0) (aref matrix 2 2) (aref matrix 3 3))
                                    (* (aref matrix 0 1) (aref matrix 1 2) (aref matrix 2 0))
                                    (* (aref matrix 0 1) (aref matrix 1 3) (aref matrix 3 0))
                                    (* (aref matrix 0 2) (aref matrix 1 0) (aref matrix 2 1))
                                    (* (aref matrix 0 2) (aref matrix 2 3) (aref matrix 3 0))
                                    (* (aref matrix 0 3) (aref matrix 1 0) (aref matrix 3 1))
                                    (* (aref matrix 0 3) (aref matrix 2 0) (aref matrix 3 2))
                                    (* (aref matrix 1 1) (aref matrix 2 2) (aref matrix 3 3))
                                    (* (aref matrix 1 2) (aref matrix 2 3) (aref matrix 3 1))
                                    (* (aref matrix 1 3) (aref matrix 2 1) (aref matrix 3 2)))
                                 (mjr_mat_det-minor-expand matrix)))
                         ;; Use polynomial interpolation -- by solving the Vandermonde system.
          ('t            (mjr_mat_solve-sys-sge
                          (mjr_mat_make-from-func (lambda (i j) (expt (1+ i) (- rows j)))         :rows (1+ rows))
                          (mjr_vec_make-from-func (lambda (i) (mjr_mat_cpoly-eval matrix (1+ i))) :len  (1+ rows))
                          :show-progress show-progress)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_test-property-math (matrix &rest mp-list)
  "Return non-nil if the given matrix has all of the specified matrix properties.
See mjr_mat_test-property-struct for structural properties.

Matrix properties
      * :mp-involutory        - Q^2=I
      * :mp-orthogonal        - Q * Q^* = I
      * :mp-orthogonal-rev    - Q^* * Q = I
      * :mp-unitary           - Q * Q^* = Q^* * Q = I
      * :mp-special           - det(Q)=1
      * :mp-singular          - Has no inverse (typical reasons: not square, empty, or rank deficient)
      * :mp-invertible        - Is square, non-empty, and has an inverse
Properties that may be added later (the todo list):
      * :mp-pos-def           - Re(x * Q * x^*) >  0 for all x
      * :mp-pos-semi-def      - Re(x * Q * x^*) >= 0 for all x"
  (if (mjr_mat_matp matrix 't)
      (if (cdr mp-list)
          (every (lambda (mp) (mjr_mat_test-property-math matrix mp)) mp-list)
          (let ((the-mp (first mp-list)))
            (case the-mp
              (:mp-involutary     (and (mjr_mat_test-property-struct matrix :mp-shape-nonempty)
                                       (mjr_mat_test-property-struct (mjr_mat_* matrix matrix) :mp-identity)))
              (:mp-orthogonal     (and (mjr_mat_test-property-struct matrix :mp-shape-nonempty)
                                       (mjr_mat_test-property-struct (mjr_mat_* matrix (mjr_mat_transpose matrix)) :mp-identity)))
              (:mp-orthogonal-rev (and (mjr_mat_test-property-struct matrix :mp-shape-nonempty)
                                       (mjr_mat_test-property-struct (mjr_mat_* (mjr_mat_transpose matrix) matrix) :mp-identity)))
              (:mp-unitary        (and (mjr_mat_test-property-struct matrix :mp-shape-nonempty)
                                       (mjr_mat_test-property-struct (mjr_mat_* matrix (mjr_mat_transpose matrix)) :mp-identity)
                                       (mjr_mat_test-property-struct (mjr_mat_* (mjr_mat_transpose matrix) matrix) :mp-identity)))
              (:mp-special        (and (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
                                       (mjr_cmp_= 1 (mjr_mat_det matrix))))
              (:mp-invertible     (and (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
                                       (mjr_mat_test-property-struct (mjr_mat_eliminate matrix) :mp-sqr-perm-c)))
              (:mp-singular       (not (mjr_mat_test-property-math matrix :mp-invertible)))
              ('t                 (error "mjr_mat_test-property-math: Matrix Property Unknown")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_inv-small (matrix)
  "Compute the inverses of small matrices (n<4) using explicit formulas. If the matrix is not square, is too big, or is empty,
then the result is nil!!  For small matrices, this function is hundreds of times faster than the alternatives."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (cols   (mjr_mat_cols matrix)))
    (if (and (= rows cols) (< 0 rows) (> 4 rows))
        (let ((detval (mjr_mat_det-small matrix)))
          (if (mjr_cmp_!=0 detval)
              (let ((newmat (mjr_mat_make-zero rows cols)))
                (case rows
                  (1      (setf (aref newmat 0 0) 1))
                  (2      (setf (aref newmat 0 0)    (aref matrix 1 1)
                                (aref newmat 0 1) (- (aref matrix 0 1))
                                (aref newmat 1 0) (- (aref matrix 1 0))
                                (aref newmat 1 1)    (aref matrix 0 0)))
                  (3      (setf (aref newmat 0 0) (- (* (aref matrix 1 1) (aref matrix 2 2)) (* (aref matrix 1 2) (aref matrix 2 1)))
                                (aref newmat 0 1) (- (* (aref matrix 0 2) (aref matrix 2 1)) (* (aref matrix 0 1) (aref matrix 2 2)))
                                (aref newmat 0 2) (- (* (aref matrix 0 1) (aref matrix 1 2)) (* (aref matrix 0 2) (aref matrix 1 1)))
                                (aref newmat 1 0) (- (* (aref matrix 1 2) (aref matrix 2 0)) (* (aref matrix 1 0) (aref matrix 2 2)))
                                (aref newmat 1 1) (- (* (aref matrix 0 0) (aref matrix 2 2)) (* (aref matrix 0 2) (aref matrix 2 0)))
                                (aref newmat 1 2) (- (* (aref matrix 0 2) (aref matrix 1 0)) (* (aref matrix 0 0) (aref matrix 1 2)))
                                (aref newmat 2 0) (- (* (aref matrix 1 0) (aref matrix 2 1)) (* (aref matrix 1 1) (aref matrix 2 0)))
                                (aref newmat 2 1) (- (* (aref matrix 0 1) (aref matrix 2 0)) (* (aref matrix 0 0) (aref matrix 2 1)))
                                (aref newmat 2 2) (- (* (aref matrix 0 0) (aref matrix 1 1)) (* (aref matrix 0 1) (aref matrix 1 0))))))
                (dotimes (i rows newmat)
                  (dotimes (j cols)
                    (setf (aref newmat i j) (/ (aref newmat i j) detval))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_inv (matrix)
  "Compute matrix inverse using the most appropriate algorithm."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix)))
    (if (> 4 rows)
        (mjr_mat_inv-small matrix)
        (mjr_mat_inv-sge matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_krylov-sequence (matrix &key v k)
  "Compute a Krylov sequence {MATRIX^i * v | i=1..k}.
If missing, K=rows.  If V is a number, then it is taken to be e_V.  If missing all together, then V is taken to be e_1.
If last-vec-only is non-nill, only the k'th vector will be returned -- not the whole list."
  (let* ((matrix (mjr_mat_cv2m matrix))
         (rows   (mjr_mat_rows matrix))
         (k      (or k rows))
         (v      (or (and v (numberp v) (mjr_vec_make-e (1- v) :len rows)) v (mjr_vec_make-e 0 :len rows))))
    (loop for cur-v = (concatenate 'vector v) then (mjr_mat_m2cv (mjr_mat_* matrix cur-v))
          repeat k
          collect cur-v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPERIMENTAL CODE BELOW THIS LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_eigen-power-method (matrix &key v (max-itr 1000) eps show-progress)
  "Return eigenvector, associated eigenvalue, and an accuracy estimate.

Why?  While considered primitive by many modern numerical analysts, the power method can exhibit miraculous levels of performance on very well-conditioned
matrices.

If the accuracy estimate is not nearly zero, then the eigenvalue/eigenvector are almost certainly wrong.  Uses mjr_cmp_=0 to tell when the iteration should
end, the EPS argument is passed directly to mjr_cmp_=0.  MAX-ITR determines a maximal number of iterations to try before giving up."
  (if (mjr_mat_test-property-struct matrix :mp-shape-square :mp-shape-nonempty)
      (let* ((matrix (mjr_mat_cv2m matrix))
             (rows   (mjr_mat_rows matrix))
             (v      (or (and v (numberp v) (mjr_vec_make-e (1- v) :len rows)) v (mjr_vec_make-e 0 :len rows)))
             (evec   (loop for prv-v = nil then cur-v
                           for cur-v = (mjr_vec_normalize v) then (mjr_vec_normalize (mjr_mat_m2cv (mjr_mat_* matrix cur-v)))
                           ;;for cur-v = (mjr_vec_normalize v #'mjr_vec_norm-infinity) then (mjr_vec_normalize (mjr_mat_m2cv (mjr_mat_* matrix cur-v)) #'mjr_vec_norm-infinity)
                           for cur-e = (and prv-v (mjr_vec_norm-two-squared (mjr_vec_- cur-v prv-v)))
                           for i from 1 upto max-itr
                           do (if (and show-progress cur-e)
                                  (format 't "~7d ~20fd ~a~%" i cur-e cur-v))
                           when (= i max-itr)
                           return cur-v
                           when (and cur-e (mjr_cmp_=0 cur-e eps))
                           return cur-v))
             (evl    (mjr_vec_/ (mjr_mat_m2cv (mjr_mat_* matrix evec)) evec))
             (mnl    (reduce #'mjr_cmp_abs-min evl))
             (mxl    (reduce #'mjr_cmp_abs-max evl)))
        (values evec (/ (+ mnl mxl) 2) (abs (- mxl mnl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_eliminate-similarity (matrix &key show-progress)
  "Eliminate via similarity transforms: tridiagonalize (symmetric case) or reduce to upper-Heisenberg form (non-symmetric case).

Returns NIL if the matrix is empty or non-square.  Otherwise it will always return a matrix -- partial results when the computation fails.  It is important to
check the structure of the resulting matrix as it may not be tridiagonal or upper-Heisenberg.

References:
  Householder (1958); Unitary Triangularization of a Nonsymmetric Matrix; Journal of the ACM
  Morrison (1960); Remarks on the Unitary Triangularization of a Nonsymmetric Matrix; Journal of the ACM
  David S. Watkins (2002); Fundamentals of Matrix Computations
  Golub & Van Loan (1996); Matrix Computations"
  (if (mjr_mat_test-property-struct  matrix :mp-shape-square :mp-shape-nonempty)
      (let* ((matrix  (mjr_mat_cv2m  matrix))
             (newmat  (mjr_arr_copy2 matrix))
             (cols    (mjr_mat_cols  matrix))
             (rows    (mjr_mat_rows  matrix))
             (xform   (mjr_mat_make-identity cols)))
        (loop for k from 0 upto (- cols 3)
              for x = (mjr_arr_get-col newmat k)
              for y = (mjr_vec_make-from-func (lambda (i) (> i (+ 1 k))) :len rows)
              do (multiple-value-bind (gamma u) (mjr_mat_householder-many x y (+ 1 k))
                   (mjr_mat_apply-householder-many!! xform  gamma u)
                   (mjr_mat_apply-householder-many!! newmat gamma u)
                   (mjr_mat_apply-householder-many!! newmat gamma u 't))
              do (if show-progress (mjr_mat_print newmat :filter-func :zp)))
        (values newmat xform))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_sis-jacobi (matrix b x0 x1)
"Jacobi Iteration
WARNING: The X1 array is changed IN PLACE!!!!!

$x_1 = D^{-1}\cdot(b-R\cdot x_0)$.  Here $A=D+R$ where $D$ a diagonal matrix and $R$ has a zero diagonal.

Refrences:
  http://en.wikipedia.org/wiki/Jacobi_method
  R Varga (2000); Matrix Iterative Analysis 2ed; ISBN: 9783540663218; p63
  Yousef Saad (1996); Iterative Methods for Sparse Linear Systems; ch4, p105
  Allaire & Kaber (2008); Numerical Linear Algebra; ISBN: 9780387341590; p147
  Golub & Van Loan (2013); Matrix Computations 4ed; ISBN: 9781421407944; p611"
  (let ((x-len (length x0)))
    (dotimes (i x-len)
      (setf (aref x1 i)
            (* (/ (aref matrix i i))
               (- (aref b i)
                  (loop for j from 0 upto (1- x-len)
                        when (not (= i j))
                        sum (* (aref matrix i j)
                               (aref x0 j)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_sis-sor (matrix b x0 x1 w)
  "Successive Over-Relaxation (SOR) Iteration 
WARNING: The X1 array is changed IN PLACE!!!!!

$$A x=b$$
$$x^{(k+1)}_i  = (1-\omega)x^{(k)}_i + \frac{\omega}{a_{ii}} \left(b_i - \sum_{j>i} a_{ij}x^{(k)}_j - \sum_{j<i} a_{ij}x^{(k+1)}_j \right),\quad i=1,2,\ldots,n$$

Notes:
  * When w=1, mjr_mat_sis-sor == mjr_mat_sis-gauss-seidel
Converges if:
  * MATRIX is symmetric positive-definite and 0<w<2
Fails to converge if (see Young 1971):
  * w<=0
  * w>=2
Refrences:
  http://en.wikipedia.org/wiki/Successive_over-relaxation
  D.M. Young (1971); Iterative Solution of Large Linear Systems
  R Varga (2000); Matrix Iterative Analysis 2ed; ISBN: 9783540663218
  Yousef Saad (1996); Iterative Methods for Sparse Linear Systems
  Allaire & Kaber (2008); Numerical Linear Algebra; ISBN: 9780387341590
  Golub & Van Loan (2013); Matrix Computations 4ed; ISBN: 9781421407944; p619"
  (let ((x-len (length x0)))
    (dotimes (i x-len)
      (setf (aref x1 i)
            (+
             (* (- 1 w) (aref x0 i))
             (* (/ w (aref matrix i i))
                (- (aref b i)
                   (loop for j from 0 upto (1- x-len)
                         when (not (= i j))
                         sum (* (aref matrix i j)
                                (if (> j i) 
                                    (aref x0 j)
                                    (aref x1 j)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_sis-gauss-seidel (matrix b x0 x1)
  "Gauss Seidel Iteration
WARNING: The X1 array is changed IN PLACE!!!!!

Converges if MATRIX is:
  * Symmetric positive-definite
  * Strictly diagonally dominant
  * Irreducibly diagonally dominant
Refrences:
  http://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method
  TODO: Add refs"
  (let ((x-len (length x0)))
    (dotimes (i x-len)
      (setf (aref x1 i)
            (* (/ (aref matrix i i))
               (- (aref b i)
                  (loop for j from 0 upto (1- x-len)
                        when (not (= i j))
                        sum (* (aref matrix i j)
                               (if (> j i) 
                                   (aref x0 j)
                                   (aref x1 j))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mat_solve-sys-itr (matrix b x0 &key (sim-fun #'mjr_mat_sis-jacobi) (xeps 0.0001) (yeps 0.0001) (max-itr 1000) (show-progress nil) sim-fun-parm)
  "Solve the linear system via a stationary iterative process
The argument :sim-fun must be one of:
  * #'mjr_mat_sis-jacobi
  * #'mjr_mat_sis-sor
  * #'mjr_mat_sis-gauss seidel"
  (cond ((not (mjr_mat_test-property-struct matrix :mp-diag-non-zero)) (error "mjr_mat_solve-sys-jacobi: Zero on the diagonal!!")))
  (loop with x-cur = (map 'vector (lambda (x) (float x 1d0)) x0)
        with x-old = nil
        with y-cur = nil
        for i-cur from 0
        do (progn
             (setf y-cur (mjr_mat_m2cv (mjr_mat_* matrix x-cur)))
             (if (mjr_eps_= y-cur b yeps)                              (return (values     x-cur y-cur i-cur "Y=0")))
             (if (and x-old (mjr_eps_= x-cur x-old xeps))              (return (values nil x-cur y-cur i-cur "X-DELTA=0")))
             (if show-progress
                 (format 't "~5d ~80@s ~80@s ~%" i-cur x-cur y-cur))
             (setf x-old (copy-seq x-cur))
             (apply sim-fun matrix b x-old x-cur (if (listp sim-fun-parm) sim-fun-parm (list sim-fun-parm)))
             (if (>= i-cur max-itr)                                    (return (values nil x-cur y-cur i-cur "MAX-ITR"))))))
