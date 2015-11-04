;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-intrp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_intrp.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_INTRP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_POLY :MJR_MAT :MJR_CMP :MJR_INTRP :MJR_NUMU :MJR_PRNG :MJR_EPS))

(in-package :MJR_INTRP-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intrp_poly-val-lagrange (x x-data y-data)
  "Evaluate the polynomial interpolating X-DATA/Y-DATA at the point X.

NOTE: This is a raw algorithmic function intended for internal use by other functions -- not interactively.  Not Exported.

Implementation notes: 
  Based directly on the Lagrange polynomial definition.  Very slow, but O(0) on storage.
"
 (let ((len (length y-data)))
   (mjr_numu_sum :end (1- len)
                 :seq-fun (lambda (i) (* (aref y-data i)
                                         (mjr_numu_prod :end (1- len)
                                                        :seq-fun (lambda (j) (if (not (= i j))
                                                                                 (let ((xj (aref x-data j))
                                                                                       (xi (aref x-data i)))
                                                                                   (/ (- x xj) (- xi xj)))))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intrp_poly-lagrange (x-data y-data)
  "Compute the interpolating polynomial for the given data.

NOTE: This is a raw algorithmic function intended for internal use by other functions -- not interactively.  Not Exported.

Implementation notes: 
  Directly compute the interpolating polynomial via polynomial arithmetic and the Lagrange formula.  Quite slow, but it always
  works -- and is memory efficient."
  (let ((len-1 (1- (length y-data))))
    (apply #'mjr_poly_+ (loop for k from 0 upto len-1
                              for yk = (aref y-data k)
                              for xk = (aref x-data k)
                              collect (apply #'mjr_poly_* yk (loop for i from 0 upto len-1
                                                                   for xi = (aref x-data i)
                                                                   when (not (= k i))
                                                                   collect (vector (/ (- xk xi)) (/ xi (- xi xk)))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intrp_poly-vandermonde (x-data y-data)
  "Compute the interpolating polynomial for the given data.

NOTE: This is a raw algorithmic function intended for internal use by other functions -- not interactively.  Not Exported.

Implementation notes: 
  Compute the interpolating polynomial via the Vandermonde matrix.  Uses a bit more memory than one would like, and is not
  terribly efficient -- but it works.  always works -- and is memory efficient.  Things work fastest if the input sequences are
  vectors."
  (let ((poly (reverse (mjr_mat_solve-sys-sge (mjr_mat_make-from-func (lambda (i j) (expt (aref x-data i) j)) :rows (length y-data)) 
                                              y-data))))
    (subseq poly (or (position-if #'mjr_cmp_!=0 poly) 0))))

(defvar x1-pts) (setq x1-pts (list (* -2/3 pi) (* -1/3 pi) (* 1/3 pi) (* 2/3 pi)))
(defvar y1-pts) (setq y1-pts (mapcar #'sin x1-pts))

(defvar x2-pts) (setq x2-pts (list 2 5/2 4))
(defvar y2-pts) (setq y2-pts (mapcar #'/ x2-pts))

(defvar x3-pts) (setq x3-pts (list 1 2 5/2 4 5))
(defvar y3-pts) (setq y3-pts (mapcar #'/ x3-pts))

(defvar x4-pts) (setq x4-pts (list 1 2 5/2 4))
(defvar y4-pts) (setq y4-pts (mapcar #'/ x4-pts))

(defvar x5-pts) (setq x5-pts (list 2 5/2 4 5))
(defvar y5-pts) (setq y5-pts (mapcar #'/ x5-pts))

(defvar x6-pts) (setq x6-pts '(0 24 36 48 72 144))
(defvar y6-pts) (setq y6-pts '(10 20 9 21 22 14))

(defvar x6-vec) (setq x6-vec #(0 24 36 48 72 144))
(defvar y6-vec) (setq y6-vec #(10 20 9 21 22 14))

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_intrp_poly-val
  ;; Make sure that mjr_intrp_poly-val evaluated on one of the x-data points always returns the appropriate y-data point
  (dotimes (i 50)
    (let* ((x (delete-duplicates (loop for j from 0 upto (mjr_prng_int-cc 1 15) collect j)))
           (y (mapcar (lambda (z) z (mjr_prng_int-cc -10 10)) x)))
      (if (> (length x) 0)
          (loop for cx in x
                for cy in y
                do (assert-equal cy (mjr_intrp_poly-val cx x y))))))
  ;; Normal cases
  (assert-equalp 3923508365/2579890176    (mjr_intrp_poly-val -1  x6-pts   y6-pts))
  (assert-equalp 10                       (mjr_intrp_poly-val 0   x6-pts   y6-pts))
  (assert-equalp 1267867/110592           (mjr_intrp_poly-val 30  x6-pts   y6-pts))
  (assert-equalp 27342639853/2579890176   (mjr_intrp_poly-val 31  x6-pts   y6-pts))
  (assert-equalp 14                       (mjr_intrp_poly-val 144 x6-pts   y6-pts))
  (assert-equalp 222731658379/2579890176  (mjr_intrp_poly-val 145 x6-pts   y6-pts))
  ;; Normal cases with points=2                                            
  (assert-equalp 115/12                   (mjr_intrp_poly-val -1  x6-pts   y6-pts    2))
  (assert-equalp 10                       (mjr_intrp_poly-val 0   x6-pts   y6-pts    2))
  (assert-equalp 29/2                     (mjr_intrp_poly-val 30  x6-pts   y6-pts    2))
  (assert-equalp 163/12                   (mjr_intrp_poly-val 31  x6-pts   y6-pts    2))
  (assert-equalp 14                       (mjr_intrp_poly-val 144 x6-pts   y6-pts    2))
  (assert-equalp 125/9                    (mjr_intrp_poly-val 145 x6-pts   y6-pts    2))
  ; Make sure things work with vectors too                                           
  (assert-equalp 125/9                    (mjr_intrp_poly-val 145 x6-vec   y6-pts    2))
  (assert-equalp 125/9                    (mjr_intrp_poly-val 145 x6-pts   y6-vec    2))
  (assert-equalp 125/9                    (mjr_intrp_poly-val 145 x6-vec   y6-vec    2))
  ;; One data point means a constant function.                             
  (assert-equalp 0                        (mjr_intrp_poly-val 1.5 '(0)     '(0)))
  (assert-equalp 10                       (mjr_intrp_poly-val 1.5 '(10)    '(10)))
  (assert-equalp 14                       (mjr_intrp_poly-val 145 x6-pts   y6-vec    1))
  (assert-equalp 14                       (mjr_intrp_poly-val 145 x6-vec   y6-vec    1))
  ; Some error cases
  (assert-error  'error                   (mjr_intrp_poly-val 1   #(0 1 2) #(0 1 2)  4))
  (assert-error  'error                   (mjr_intrp_poly-val 1   '(0 1 2) '(0 1 2)  4))
  (assert-error  'error                   (mjr_intrp_poly-val 1.5 '(0 1 2) '(0 1 2)  4))
  (assert-error  'error                   (mjr_intrp_poly-val 1   #(0 1 2) #(0 1 2)  0))
  (assert-error  'error                   (mjr_intrp_poly-val 1   '(0 1 2) '(0 1 2)  0))
  (assert-error  'error                   (mjr_intrp_poly-val 1.5 '(0 1 2) '(0 1 2)  0))
  (assert-error  'error                   (mjr_intrp_poly-val 1.5 '()      '()))
  (assert-error  'error                   (mjr_intrp_poly-val 1.5 #()      #()))
  ;; Make sure mjr_intrp_poly-val, mjr_intrp_poly-val-neville, & mjr_intrp_poly-val-lagrange have constant values
  (dotimes (i 20)
    (let* ((x-dat (map 'vector #'identity (delete-duplicates (loop for j from 0 upto (mjr_prng_int-cc 1 25) collect j))))
           (y-dat (map 'vector (lambda (z) z (mjr_prng_int-cc -10 10)) x-dat))
           (p     (mjr_intrp_poly x-dat y-dat)))
      (dotimes (i 200)
        (let* ((x (mjr_prng_int-cc -10 10))
               (y (mjr_poly_eval p x)))
          (assert-equalp (mjr_intrp_poly-val                     x x-dat y-dat) y)
          (assert-equalp (mjr_intrp::mjr_intrp_poly-val-neville  x x-dat y-dat) y)
          (assert-equalp (mjr_intrp_poly-val-lagrange x x-dat y-dat) y)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_intrp_poly
  (assert-true (mjr_eps_= #(-0.1256879165858069d0 -5.551115123125783d-17 
                            0.9648255669881362d0 -1.1102230246251565d-16)                      (mjr_intrp_poly x1-pts y1-pts)))
  (assert-equalp #(1 2 3)                                                                      (mjr_intrp_poly #(1 2 3 4 5 6 7 8) #(6 11 18 27 38 51 66 83)))
  (assert-equalp #(1/20 -17/40 23/20)                                                          (mjr_intrp_poly x2-pts y2-pts))
  (assert-equalp #(1/20 -17/40 23/20)                                                          (mjr_intrp_poly x3-pts y3-pts :start 1 :end 3))
  (assert-equalp #(1/20 -17/40 23/20)                                                          (mjr_intrp_poly x4-pts y4-pts :start 1))
  (assert-equalp #(1/20 -17/40 23/20)                                                          (mjr_intrp_poly x5-pts y5-pts :end 2))
  (assert-equalp #(1 0)                                                                        (mjr_intrp_poly '(1 2 3)  '(1 2 3)))
  (assert-equalp #(1 0 1)                                                                      (mjr_intrp_poly '(-1 0 1) '(2 1 2)))
  ;; Make sure that the poly evaluated on the x-data points always return the appropriate y-data point
  (dotimes (i 50)
    (let* ((x-dat (delete-duplicates (loop for j from 0 upto (mjr_prng_int-cc 1 15) collect j)))
           (y-dat (mapcar (lambda (z) z (mjr_prng_int-cc -10 10)) x-dat))
           (p     (mjr_intrp_poly x-dat y-dat)))
      (if (> (length x-dat) 0)
          (loop for x in x-dat
                for y in y-dat
                for pv = (mjr_poly_eval p x)
                do (assert-equal y pv)))))
  ;; Make sure mjr_intrp_poly == mjr_intrp_poly-newton == mjr_intrp_poly-vandermonde == mjr_intrp_poly-lagrange
  (dotimes (i 150)
    (let* ((x-dat (map 'vector #'identity (delete-duplicates (loop for j from 0 upto (mjr_prng_int-cc 1 25) collect j))))
           (y-dat (map 'vector (lambda (z) z (mjr_prng_int-cc -10 10)) x-dat))
           (px    (mjr_intrp_poly x-dat y-dat))
           (pl    (mjr_poly_simplify (mjr_intrp_poly-lagrange    x-dat y-dat)))
           (pn    (mjr_poly_simplify (mjr_intrp::mjr_intrp_poly-newton      x-dat y-dat)))
           (pv    (mjr_poly_simplify (mjr_intrp_poly-vandermonde x-dat y-dat))))
      (assert-equalp px pl "L")
      (assert-equalp px pn "N")
      (assert-equalp px pv "V")))
  ;; MJR TODO NOTE mjr_intrp_poly: Rethink the error handling of this function, and add some error case checks here.
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_intrp_lfip
  (dotimes (k 10)
    (let* ((x0  (mjr_prng_int-cc -100 100))  ;; Minimum x value
           (len (mjr_prng_int-cc 5 10))      ;; Number of intrp nodes
           (xd  (mjr_prng_int-cc 1 3))       ;; Distance between intrp nodes
           (x-dat (loop for i from 0 upto len             ;; Construct intrp nodes
                        collect (+ x0 (* xd i)))))
      (dotimes (i (1+ len))                               ;; Construct each of the lfip polys
        (let ((li (mjr_intrp_lfip i x-dat)))
          (dotimes (j (1+ len))                           ;; Check that it has correct values
            (assert-equal (if (= j i) 1 0) (mjr_poly_eval li (elt x-dat j))))))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
