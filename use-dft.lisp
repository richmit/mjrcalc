;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-dft.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     DFT and inverse DFT.@EOL
;; @Keywords  lisp interactive dft processing
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_DFT
  (:USE :COMMON-LISP
        :MJR_ARR)
  (:DOCUMENTATION "Brief: DFT and inverse DFT.;")
  (:EXPORT #:mjr_dft_help
           #:mjr_dft_transform
           #:mjr_dft_dft
           #:mjr_dft_idft
           ))

(in-package :MJR_DFT)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dft_help ()
  "Compute DFT and Inverse DFT of arrays" 
  (documentation 'mjr_dft_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dft_transform (x ttype &optional (offset 0) (stride 1) (maxelt nil) (out nil) (ooffset 0) (ostride 1))
  "Compute DFT/IDFT a subset of the data in the array X (a 2D or 1D array)

This function forms the computational kernel of the MJR_DFT package.  One normally uses the MJR_DFT_DFT and MJR_DFT_IDFT
functions, but this one is exported in order to support more sophisticated computations.

The dimension(s) of X should be power(s) of two for maximum performance.

If X is a matrix (2D array), then the 2D DFT is returned for the entire array.  If X is a vector, then 1) subsets of the vector
may be processed, and 2) the output may be placed in a provided array instead of one allocated by this function.  This
additional functionality when X is a vector is provided via several optional arguments:
    * offset  -- index to begin with
    * stride  -- distance between elements
    * maxelt  -- Maximum number of elements to process from x
    * out     -- vector into which output will be placed
    * ooffset -- index to begin with (only valid when OUT is non-NIL)
    * ostride -- distance between elements (only valid when OUT is non-NIL)

References:
  Cooley, Lewis, & Welch (1969); The finite Fourier transform; IEEE Trans. Audio Electroacoustics 17"
  (if (vectorp x)
      (let* ((mi   (array-dimension x 0))                                                         ;; Vector case
             (mo   (or maxelt (ceiling (- mi offset) stride)))
             (y    (or out (make-array mo :initial-element 0)))
             (prdp (* (if (equal :dft ttype) -1 1) #C(0 2) pi (/ mo)))
             (scl  (if (equal :dft ttype) 1 (/ mo))))
        (if (and (> mo 4) (evenp mo))
            (let ((edft (mjr_dft_transform x ttype offset            (* 2 stride) (/ mo 2)))      ;;;; Recursive Split odd/even
                  (odft (mjr_dft_transform x ttype (+ offset stride) (* 2 stride) (/ mo 2)))
                  (mo/2  (/ mo 2)))
              (loop for kl from 0 upto (1- mo/2)                                                  ;;;;;; construct DFT from 
                    for ku from mo/2                                                              ;;;;;; odd and even parts
                    for lhs = (aref edft kl)
                    for rhs = (* (exp (* prdp kl)) (aref odft kl))
                    finally (return y)
                    do (setf (aref y (+ ooffset (* ostride kl))) (* (if (equal :dft ttype) 1 1/2) (+ lhs rhs)))
                    do (setf (aref y (+ ooffset (* ostride ku))) (* (if (equal :dft ttype) 1 1/2) (- lhs rhs)))))
            (loop for io from 0 upto (1- mo)                                                      ;;;; Direct DFT case
                  for ii from offset upto (1- mi) by stride
                  for ia from ooffset by ostride
                  finally (return y)
                  do (setf (aref y ia)                                                            ;;;;;; Eval formula
                           (* scl (loop for ko from 0 upto (1- mo)
                                        for ki from offset upto (1- mi) by stride
                                        sum (* (aref x ki) (exp (* prdp ko io)))))))))
      (let* ((dims    (array-dimensions x))                                                       ;; Matrix case
             (rows    (first dims))
             (cols    (second dims))
             (num-ele (* rows cols))
             (workn-x (mjr_arr_nreflow-to-vec x))                                                 ;;;; Displaced arrays let
             (workn-y1 (make-array num-ele))                                                      ;;;; us use the vector code
             (workn-y2 (make-array num-ele)))
        (loop for r from 0 upto (1- rows)                                                         ;;;; Row DFTs
              do (mjr_dft_transform workn-x ttype (* r cols) 1 cols workn-y1 (* r cols) 1))
        (loop for c from 0 upto (1- cols)                                                         ;;;; Col DFTs
              do (mjr_dft_transform workn-y1 ttype c cols rows workn-y2 c cols))
        (make-array dims :displaced-to workn-y2))))                                               ;;;; Reshape for return

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dft_dft (x)
  "Compute DFT of the data in the X (a matrix for 2D DFT or vector for 1D DFT)"
  (mjr_dft_transform x :dft))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_dft_idft (x)
  "Compute IDFT of the data in the X (a matrix for 2D IDFT or vector for 1D IDFT)"
  (mjr_dft_transform x :idft))
