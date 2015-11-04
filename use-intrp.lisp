;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-intrp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Polynomial interpolation.@EOL
;; @std       Common Lisp
;; @see       tst-intrp.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_INTRP
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_CMP
        :MJR_VVEC)
  (:DOCUMENTATION "Brief: Polynomial interpolation.;")
  (:EXPORT #:mjr_intrp_help
           #:mjr_intrp_poly-val
           #:mjr_intrp_poly
           #:mjr_intrp_lfip
           ;; Not exported
           ;; #:mjr_intrp_poly-val-neville
           ;; #:mjr_intrp_poly-newton
           ))

(in-package :MJR_INTRP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_help ()
  "Help for MJR_INTRP:  Polynomial interpolation"
  (documentation 'mjr_intrp_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_poly-val-neville (x x-data y-data)
  "Evaluate the polynomial interpolating X-DATA/Y-DATA at the point X.

NOTE: This is a raw algorithmic function intended for internal use by other functions -- not interactively.  Not Exported.

Implementation notes:
  Based based upon Neville's algorithm with minor enhancements suggested by Stoer & Bulirsch (2002). Storage and CPU consumption are both O(1).

Reference:
  Josef Stoer & Roland Bulirsch (2002); Introduction to numerical analysis; Springer; pp 42"
  (let ((tmp (copy-seq y-data))
        (len (length y-data)))
    (loop for i from 1 upto (1- len)
          finally (return (aref tmp 0))
          do (loop for j from (1- i) downto 0
                   for tj1 = (aref tmp (1+ j))
                   for xi  = (aref x-data i)
                   do (setf (aref tmp j) (+ tj1 (/ (* (- tj1 (aref tmp j)) (- x xi))
                                                   (- xi (elt x-data j)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_poly-val (x x-data y-data &optional use-points)
  "Return interpolating polynomial value at X.

If the number of data points in Y-DATA outnumber the X-DATA points, then X-DATA is assumed to contain only TWO data points representing the minimum x data
value and the maximum x data value, any remaining elements of X-DATA are ignored, and the x data used in the computation is COMPUTED on a regular grid the
length Y-DATA.  USE-POINTS is used to specify the number of data points to use for the interpolation.  If it is less than the length of Y-DATA, then the
closest USE-POINTS points to X will be selected in order to perform the interpolation.

NOTE: If :use-points is 2, then this can be used to produce the piecewise linear interpolation function."
  (let* ((y-data (if (vectorp y-data) y-data (concatenate 'vector y-data)))
         (leny   (length y-data))
         (x-data (if (< (length x-data) leny)
                     (mjr_vvec_to-vec (list :start (elt x-data 0) :end (elt x-data 1) :len leny))
                     (if (vectorp x-data) x-data (concatenate 'vector x-data))))
         (lenx   (length x-data)))
    (cond ((< leny lenx)                        (error "mjr_intrp_poly-val: Not enough Y data points!"))
          ((< leny 1)                           (error "mjr_intrp_poly-val: No Y data points!"))
          ((and use-points (< use-points 1))    (error "mjr_intrp_poly-val: use-points value must be positive!"))
          ((and use-points (< use-points 1))    (error "mjr_intrp_poly-val: use-points value must be positive!"))
          ((and use-points (< leny use-points)) (error "mjr_intrp_poly-val: use-points value is too large!")))
    (if (= 1 leny)
        (aref y-data 0)
        (if (and use-points (< use-points leny))
            (let* ((xmini (min (or (mjr_vvec_map-mini (list :points x-data :map-fun (lambda (xi) (and (mjr_cmp_> x xi) (mjr_numu_absdif xi x))))) 0)
                               (- leny use-points)))
                   (xminx (aref x-data xmini)))
              (if (mjr_cmp_= xminx x)
                  (aref y-data xmini)
                  (mjr_intrp_poly-val-neville x (subseq x-data xmini (+ xmini use-points)) (subseq y-data xmini (+ xmini use-points)))))
            (mjr_intrp_poly-val-neville x x-data y-data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_poly-newton (x-data y-data)
  "Compute the interpolating polynomial for the given data.

NOTE: This is a raw algorithmic function intended for internal use by other functions -- not interactively.  Not Exported.

Implementation notes:
  The implementation of this function first computes the Newton polynomial via the solution of a lower-triangular, Vandermonde-like system, and then
  transforms the Newton polynomial into the interpolating polynomial in the standard power basis.  Storage and CPU consumption are O(n^2)."
  (let* ((len (length x-data))
         (np  (copy-seq y-data)))
    ;; Compute the newton polynomial (->np)
    (let ((a (make-array (list len len))))
      ;; Construct Vandermonde-like matrix against the basis for the Newton form of interpolating polynomial (->a)
      ;; The upper triangular part of A is never used, so we never initialize it.
      ;; We don't use mjr_mat_make-from-func: 1) don't want to depend on it, 2) for speed
      (loop for row from 0 upto (1- len)
            do (setf (aref a row 0) 1))
      (loop for col from 1 upto (1- len)
            for xc = (aref x-data (1- col))
            do (loop for row from col upto (1- len)
                     for x = (aref x-data row)
                     do (setf (aref a row col) (* (aref a row (1- col)) (- x xc)))))
      ;; Solve a*np=y with substitution (->np)
      ;; We don't use mjr_mat_solve-sys-sub: 1) don't want to depend on it, 2) for speed
      (loop for row from 0 upto (1- len)
            do (loop for col from 0 upto (1- row)
                     do (decf (aref np row) (* (aref np col) (aref a row col))))
            do (setf (aref np row) (/ (aref np row) (aref a row row)))))
    ;; Compute and return the interpolating polynomial in the power basis form
    (let ((b (make-array len :initial-element 0))
          (p   (make-array len :initial-element 0)))
      ;; Iteratively compute the Newton basis polynomials (->b), and the power basis form (->p)
      ;; We don't use mjr_poly_* & mjr_poly_+: 1) for speed
      (loop for i from 0 upto (1- len)
            for ii from (1- len) downto 1
            ;; Set the first b & p
            initially (setf (aref b (1- len)) 1
                            (aref p (1- len)) (* (aref np 0)))
            ;; Compute next b
            do (setf (aref b (- ii 1)) (aref b ii))
            do (loop for jj from ii upto (- len 2)
                     do (setf (aref b jj) (- (aref b (1+ jj)) (* (aref x-data i) (aref b jj)))))
            do (setf (aref b (1- len)) (* (aref b (1- len)) (- (aref x-data i))))
            ;; Update p
            do (loop for j from 0 upto (1- len)
                     do (incf (aref p j) (* (aref np (1+ i)) (aref b j)))))
      p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_poly (x-data y-data &key start end)
  "Compute the interpolating polynomial for the given data."
  (let* ((len    (length y-data))
         (x-data (if (vectorp x-data)
                     (if (= (length x-data) len)
                         x-data
                         (mjr_vvec_to-vec (list :start (elt x-data 0) :end (elt x-data 1) :len len)))
                     (concatenate 'vector x-data)))
         (y-data (if (vectorp y-data) y-data (concatenate 'vector y-data)))
         (poly   (if (or start end)
                     (let ((start (or start 0))
                           (end   (or end (1- (length y-data)))))
                       (mjr_intrp_poly-newton (subseq x-data start (1+ end)) (subseq y-data start (1+ end))))
                     (mjr_intrp_poly-newton x-data y-data))))
    (subseq poly (or (position-if #'mjr_cmp_!=0 poly) 0)))) ;; remove leading zeros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intrp_lfip (i x-data)
  "Compute the i'th (0-based) LFIP (Lagrange Fundamental Interpolating Polynomial)
The LFIP is 0 on all the x values except the i'th one -- it is 1 on that x value."
  (let* ((y-data (make-array (length x-data) :initial-element 0)))
    (setf (svref y-data i) 1)
    (mjr_intrp_poly x-data y-data)))
