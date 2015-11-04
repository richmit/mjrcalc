;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-nleqm.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Multiple Non-linear EQuation root location.@EOL
;; @std       Common Lisp
;; @see       tst-nleqm.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1995-2010,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      Add newton/chord hybrid. Parameter, m: Max number of 'chord-steps' per 'newton iteration' (m=1 => algorithm is Newton's algorithm). Pick
;;            factorization function (QR, SVD, LU, Cholesky, etc...).@EOL@EOL
;; @todo      Add Newton-Krylov: GMRES.@EOL@EOL
;; @todo      Build hybrid algorithms from parts.  Provide the solvers for linear system solution, and how to switch between them. Provide logic for
;;            how to switch from chord <-> newton <-> quazi-newtion.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_NLEQM
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_EPS
        :MJR_VEC
        :MJR_MAT
        :MJR_UTIL
        :MJR_COMBC)
  (:DOCUMENTATION "Brief: Multiple Non-linear EQuation root location.;")
  (:EXPORT #:mjr_nleqm_help
           #:mjr_nleqm_root-newton
           #:mjr_nleqm_fixed-point-itr
           #:mjr_nleqm_root-comb-search
           ))

(in-package :MJR_NLEQM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleqm_help ()
  "Help for MJR_NLEQM:  Multiple Non-linear EQuation root location."
  (documentation 'mjr_nleqm_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleqm_root-newton (fdf x0 &key (df-is-inverse nil) (xeps 0.0001) (yeps 0.0001) (max-itr 1000) arg-mode (show-progress nil))
  "Use newton's method to localize a root of f near (hopefully anyhow) X0.  
The first return of FDF is the value of f, and the second return depends upon DF-IS-INVERSE:
  * DF-IS-INVERSE is NIL:      df/dx
  * DF-IS-INVERSE is non-NIL:  (df/dx)^(-1) (or NIL if df/dx is singular)
If a root is found (i.e. an x such that f(x) is nearly zero, then the return is: x, f(x), iteration count, WHY-STRING.
If MJR_NLEQM_ROOT-NEWTON exits for any other reason, then the return is: nil, x, f(x), iteration count, WHY-STRING.
WHY-STRING is a string description of why the function returned."
  (loop with x-cur = x0
        with x-old = nil
        with y-old = nil
        with x-del = nil
        for i-cur from 0
        do (multiple-value-bind (y-cur df) 
               (mjr_util_fun-adapt-eval-v fdf x-cur arg-mode)
             (if (mjr_eps_=0 y-cur yeps)                                           (return (values     x-cur y-cur i-cur "Y=0")))
             (if (and x-old (mjr_eps_= x-cur x-old xeps))                          (return (values nil x-cur y-cur i-cur "X-DELTA=0")))
             (if show-progress (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
             (setf x-old x-cur)
             (setf x-del (if df-is-inverse
                             (and df (mjr_mat_* df y-cur))
                             (mjr_mat_solve-sys-sge df y-cur)))
             (if (null x-del)                                                      (return (values nil x-cur y-cur i-cur "dy=0")))
             (setf x-cur (mjr_vec_- x-cur (mjr_mat_m2cv x-del)))
             (if (>= i-cur max-itr)                                                (return (values nil x-cur y-cur i-cur "MAX-ITR")))
             (if (and y-old (mjr_eps_= y-cur y-old yeps))                          (return (values nil x-cur y-cur i-cur "Y-DELTA=0")))
             (setf y-old y-cur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleqm_fixed-point-itr (f x0 &key (xeps 0.0001) (yeps 0.0001) (max-itr 1000) arg-mode (show-progress nil))
  "Use fixed point iteration to localize a root f(x)-x near (hopefully anyhow) X0.
If a root is found (i.e. an x such that f(x)-x is nearly zero, then the return is: x, f(x), iteration count, WHY-STRING.
If MJR_NLEQM_ROOT-NEWTON exits for any other reason, then the return is: nil, x, f(x), iteration count, WHY-STRING.
WHY-STRING is a string description of why the function returned."
  (loop with x-cur = x0
        with x-old = nil
        with y-old = nil
        for i-cur from 0
        do (let ((y-cur (mjr_util_fun-adapt-eval-v f x-cur arg-mode)))
             (if (mjr_eps_=0 y-cur yeps)                                           (return (values     x-cur y-cur i-cur "Y=X")))
             (if (and x-old (mjr_eps_= x-cur x-old xeps))                          (return (values nil x-cur y-cur i-cur "X-DELTA=0")))
             (if show-progress (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
             (setf x-old x-cur)
             (setf x-cur (copy-seq y-cur))
             (if (>= i-cur max-itr)                                                (return (values nil x-cur y-cur i-cur "MAX-ITR")))
             (if (and y-old (mjr_eps_= y-cur y-old yeps))                          (return (values nil x-cur y-cur i-cur "Y-DELTA=0")))
             (setf y-old y-cur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleqm_root-comb-search (f s &key (target 0) arg-mode show-progress)
  "Find best solutions to F=TARGET, from $(S_1 \times S_2 \times \cdots \times S_n)$, using the an exhaustive search algorithm.

Return a list of values (as vectors) minimizing the error and the error.

This is a combinatorial search function that checks every possible combination of values, and may become quite slow.

Example: Find members of (1 2 3)x(4 4)x(6 7 8 9) such that the tuple elements sum to 15:
    (mjr_nleqm_root-comb-search (lambda (v) (reduce #'+ v))
                                (list (list 1 2 3) (list 4 5) (list 6 7 8 9))
                                :target 15)"
  (let ((minv nil)
        (minx nil))
    (flet ((m (v)
             (let ((fv (abs (- target (mjr_util_fun-adapt-eval-v f v arg-mode)))))
               (cond ((or (null minv) (< fv minv)) (setf minv fv
                                                         minx (list (copy-seq v))))
                     ((mjr_cmp_= fv minv)          (nconc minx (list (copy-seq v))))))))
      (mjr_combc_gen-all-cross-product s :func #'m :show-progress show-progress))
    (values minx minv)))
