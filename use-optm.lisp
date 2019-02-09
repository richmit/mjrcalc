;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-optm.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Multivariate function OPTimization.@EOL
;; @std       Common Lisp
;; @see       tst-optm.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Simplex based search (see silver book and Kelly book).@EOL@EOL
;; @todo      BFGS (Broyden-Fletcher-Goldfarb-Shanno).@EOL@EOL
;; @todo      Generic pattern based search optimization algorithm.@EOL@EOL
;; @todo      Newton's method.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_OPTM
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_COMBC
        :MJR_VEC
        :MJR_MAT
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Multivariate function OPTimization.;")
  (:EXPORT #:mjr_optm_help

           #:mjr_optm_minimize-mjr-descent  #:mjr_optm_minimize-random-delta
           #:mjr_optm_minimize-hooke-jeeves #:mjr_optm_minimize-comb-search

           #:mjr_optm_maximize-adapter
           ))

(in-package :MJR_OPTM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_help ()
  "Multivariate optimization

      * Derivative free minimization
        * mjr_optm_minimize-mjr-descent   -- Steepest descent-like
        * mjr_optm_minimize-random-delta  -- Pure random step search
        * mjr_optm_minimize-hooke-jeeves  -- Classic Hooke & Jeeves
        * mjr_optm_minimize-comb-search   -- Exhaustive search of finite search space
      * Maximize
        * mjr_optm_maximize-adapter       --  Maximize a function using one of the minimize functions above"
  (documentation 'mjr_optm_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_minimize-mjr-descent (f x0 &key
                                             (delta-max 1) (delta-factor 1/2) (delta-min 1e-5)
                                             (dirs-min 8)  (dirs-slope 16)    (dirs-max 256)
                                             (max-itr 1000) arg-mode (show-progress nil))
  "Optimize the function F, from R^2->R, via a direct search analog of the steepest descent algorithm.

This algorithm is analogous to the steepest decent algorithm, except that the steepest direction is discovered at each step via a direct search.  The
algorithm:

   : Set x to x0, DIRS-NUM to DIRS-MIN
   : If DELTA-FACTOR<DELTA-MIN, then exit
  1: Compute DIRS-NUM evenly spaced points, (x_i) on a circle of radius DELTA-CUR around (x)
   : If min(f(x_i))<f(x), then {
   :   set (x) to the minimum point and goto 1
   : } ELSE { alternately shrink DELTA-CUR by multiplying by DELTA-FACTOR or grow DIRS-NUM by adding DIRS-SLOPE. }
   : If more than MAX-ITR iterations, then exit
  5: goto 1

F must take two arguments (x & y) -- not a vector!!!"
  (let* ((x-cur      x0)
         (f-cur      (mjr_util_fun-adapt-eval-v f x0 arg-mode))
         (shrinka    nil)
         (delta-cur  delta-max)
         (dirs-num   (- dirs-min dirs-slope))
         (f-nxt      nil)
         (x-nxt      nil)
         (x-del      nil)
         (rm         nil))
    (dotimes (itr max-itr (values x-cur f-cur itr "ITER"))
      (if (< delta-cur delta-min)
          (return-from mjr_optm_minimize-mjr-descent (values x-cur f-cur itr "X-EPS")))
      ;; Update *-nxt or shrink angle or delta
      (if f-nxt
          (setf f-cur f-nxt
                x-cur x-nxt)
          (if (and (setf shrinka (not shrinka)) (< dirs-num dirs-max))
              (let* ((a  (/ (* 2 pi) (- dirs-num 1)))
                     (ca (cos a))
                     (sa (sin a)))
                (setf dirs-num (min dirs-max (+ dirs-num dirs-slope))
                      rm       (make-array '(2 2) :initial-contents (list (list ca sa) (list (- sa) ca)))))
              (setf delta-cur (* delta-cur delta-factor))))
      (if show-progress (format 't "| ~65a | ~35f | ~5d | ~35f |~%" x-cur f-cur dirs-num delta-cur))
      ;; Look for better x-cur.
      (setf x-del (vector delta-cur 0)
            x-nxt #(0 0)
            f-nxt nil)
      (dotimes (j dirs-num)
        (setq x-del (mjr_mat_apply-linear-tform rm x-del))
        (let* ((x-explor (mjr_vec_+ x-cur x-del))
               (f-explor (mjr_util_fun-adapt-eval-v f x-explor arg-mode)))
          (if (< f-explor (or f-nxt f-cur))
              (setf f-nxt f-explor
                    x-nxt x-explor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_minimize-random-delta (f x0 &key (delta-max 1.0) (delta-factor 0.5) (delta-min 1e-5)
                                                 (num-guess 20)
                                                 (max-itr 1000) arg-mode (show-progress nil))
  "Optimize the function f, from R^n->R, via a randomized direct search algorithm.

The algorithm:
   : Set x to x0, DELTA-CUR to DELTA-MAX
  1: Compute a random perturbation, XP, of X with each component of the perpetration vector less than DELTA-CUR
   : if f(XP)<f(X), then { X=XP, goto 1
   : } ELSE { if we have tried fewer than NUM-GUESS times without an X update, then goto 1 }
   : If no new x has been found in NUM-GUESS tries, then shrink DELTA-CUR by DELTA-FACTOR
   : If DELTA-CUR is less than DELTA-MIN, then return
   : If more than MAX-ITR iterations occur, then return
   : goto 1

F must take a single argument -- a vector of n elements."
  (let* ((x-curent   x0)
         (f-curent   (mjr_util_fun-adapt-eval-v f x0 arg-mode))
         (num-vars   (length x0))
         (delta      delta-max)
         (x-explor   (copy-seq x-curent))
         (got-explor nil)
         (f-explor   f-curent))
    (dotimes (itr max-itr (values x-curent f-curent itr "ITER"))
      (if (< delta delta-min)
          (return-from mjr_optm_minimize-random-delta (values x-curent f-curent itr "X-EPS")))
      (if show-progress (format 't "~65a | ~20f | ~10f~%" x-curent f-curent delta))
      ; Look for better x-curent.
      (setf got-explor nil)
      (dotimes (j num-guess)
        (dotimes (i num-vars)
          (setf (aref x-explor i)
                (if (evenp (random 2))
                    (- (aref x-curent i) (random delta))
                    (+ (aref x-curent i) (random delta)))))
        (setf f-explor (mjr_util_fun-adapt-eval-v f x-explor arg-mode))
        (if (< f-explor f-curent)
            (progn (setf got-explor 't)
                   (setf f-curent f-explor)
                   (dotimes (i num-vars)
                     (setf (aref x-curent i) (aref x-explor i)))
                   (return))))
      ; Update delta if we couldn't find a better point
      (if (null got-explor)
          (setf delta (* delta delta-factor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_minimize-hooke-jeeves (f x0 &key (delta-min 1e-5) (delta-max 1) (delta-factor 1/2)
                                                 (max-itr 1000) arg-mode (show-progress nil))
  "Optimized F, from R^n->R, using the classical Hooke-Jeeves optimization algorithm.

Optimize a real function of n variables using a modified version of the Hooke & Jeeves method.  Hooke & Jeeves is a charming algorithm that is trivial to
code, easy to use, and quick to succeed or fail.  As optimization algorithms go, it may not be particularly reliable; however, it's reliability to simplicity
ratio is higher than most.

References:
    R. Hooke and T. A. Jeeves (1961); Direct Search Solution of Numerical and Statistical Problems; Journal of the ACM; Vol. 8; FILE: p212-hooke.pdf (Original reference)
    Arthur F. Kaupe (1963); Algorithm 178: Direct Search; Communications of the ACM; Vol 6; FILE: p313-wegstein.pdf (Better reference)
    Bell and Pike (1966); Remark on Algorithm 178: Direct Search; Communications of the ACM; Vol 9; FILE: p498-bell (Refinement)
    Tomlin and Smith (1969); Remark on Algorithm 178; Communications of the ACM; Vol 12; FILE: p637-tomlin.pdf (Refinement)
    Quarteroni, Sacco, and Saleri (2006); Numerical Mathematics; Springer; (Text Reference)
    C.T.Kelley (1999); Iterative Methods for Optimization; SIAM; (Better Text Reference); FILE: kelly-opt.pdf

This implementation is relatively fast, but it can reevaluate the function multiple times at the same coordinate.  F is expected to take a vector argument."
  (let* ((x-curent   x0)
         (f-curent   (mjr_util_fun-adapt-eval-v f x0 arg-mode))
         (num-vars   (length x0))
         (x-explor   (copy-seq x-curent))
         (x-backup   (make-array num-vars :initial-element 0))
         (delta      delta-max)
         (got-backup nil)
         (got-explor nil)
         (f-explor   f-curent)
         (f-backup   nil)
         (f-tmp      nil))
    (flet ((setv (a b)
             (dotimes (i num-vars)
               (setf (aref a i) (aref b i))))
           (testIndex (i s)
             (setf (aref x-explor i) (+ (aref x-curent i) (if (= s 1) delta (- delta))))
             (setf f-tmp (mjr_util_fun-adapt-eval-v f x-explor arg-mode))
             (if (<= f-tmp f-explor)
                 (setf f-explor f-tmp
                       got-explor 't))))
      (dotimes (itr max-itr (values x-curent f-curent itr "ITER"))
        (if (< delta delta-min)
            (return-from mjr_optm_minimize-hooke-jeeves (values x-curent f-curent itr "X-EPS")))
        (if show-progress (format 't "~65a | ~20f | ~10f~%" x-curent f-curent delta))
        ;; Find exploration point
        (setf got-explor nil)
        (dotimes (i num-vars)
          (if (not (testIndex i 1))
              (if (not (testIndex i -1))
                  (setf (aref x-explor i) (aref x-curent i)))))
        ;; Now see what we do next
        (if got-explor
            (if got-backup
                (if (<= f-explor f-backup)
                    (progn
                      (setf got-backup nil)
                      (setv x-curent x-explor)
                      (setf f-curent f-explor))
                    (progn
                      (setv x-curent x-backup)
                      (setf f-curent f-backup)
                      (setv x-explor x-backup)
                      (setf f-explor f-backup)))
                (progn
                  (setf got-backup 't)
                  (setv x-backup x-explor)
                  (setf f-backup f-explor)
                  (dotimes (i num-vars)
                    (setf (aref x-curent i) (- (* 2 (aref x-explor i)) (aref x-curent i))))
                  (setf f-curent (mjr_util_fun-adapt-eval-v f x-curent arg-mode))
                  (setv x-explor x-curent)
                  (setf f-explor f-curent)))
            (if got-backup
                (progn
                  (setf got-backup nil)
                  (setv x-curent x-backup)
                  (setf f-curent f-backup)
                  (setf f-explor f-curent)
                  (setv x-explor x-curent))
                (progn
                  (setf delta (* delta delta-factor)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_minimize-comb-search (f s &key  arg-mode (show-progress nil))
  "Minimize F, from $(S_1 \times S_2 \times \cdots \times S_n)\rightarrow R$, using the an exhaustive search algorithm.

Return a list of values (as vectors) minimizing F and the minimum value found.

This is a combinatorial optimization function that checks every possible combination of values, and may become quite slow.

Example: Find members of (1 2 3)x(4 4)x(6 7 8 9) such that the tuple elements sum to 15:
    (mjr_optm_minimize-comb-search (lambda (v) (abs (- (reduce #'+ v) 15)))
                                  (list (list 1 2 3) (list 4 5) (list 6 7 8 9)))"
  (let ((minv nil)
        (minx nil))
    (flet ((m (v)
             (let ((fv (mjr_util_fun-adapt-eval-v f v arg-mode)))
               (cond ((or (null minv) (< fv minv)) (setf minv fv
                                                         minx (list (copy-seq v))))
                     ((mjr_cmp_= fv minv)          (nconc minx (list (copy-seq v))))))))
      (mjr_combc_gen-all-cross-product s :func #'m :show-progress show-progress))
    (values minx minv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_optm_maximize-adapter (f minimizer-function &rest rest)
  "Maximize F by minimizing -F via minimizer-function"
  (let ((ret (multiple-value-list (apply minimizer-function (lambda (x) (- (funcall f x))) rest))))
    (values-list (apply #'list (first ret) (- (second ret)) (cddr ret)))))
