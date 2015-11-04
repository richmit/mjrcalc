;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-nleq.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Non-linear equation root finding.@EOL
;; @Std       Common Lisp
;;
;;            TODO: I want to rethink the way the functions in this package exit.  No more "errors"!  Just return enough data to the
;;                  caller so that it can figure out what happened, and behave appropriately.  This will transform these functions
;;                  into something that works better as a library.  I still want this to work well interactively, so the returns
;;                  must be meaningful and human readable.  Perhaps (values WHY best-x-guess y-value-at-best-x-guess ..)
;;
;;            TODO: Create a function like mjr_nleq_infix-string-to-diff-lambda, but that works with use-ndiff.lisp to compute
;;                  approximate derivatives.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_NLEQ
  (:USE :COMMON-LISP
        :MJR_EPS
        :MJR_CHK
        :MJR_CMP
        :MJR_MXP
        :MJR_CAS)
  (:DOCUMENTATION "Brief: Non-linear equation root finding.;")
  (:EXPORT #:mjr_nleq_help

           #:mjr_nleq_root-bsect 
           #:mjr_nleq_root-newton
           #:mjr_nleq_root-laguerre

           #:mjr_nleq_fixed-point-itr

           ;; Helpers
           ;; #:mjr_nleq_infix-string-to-diff-lambda
           ))

(in-package :MJR_NLEQ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_help ()
  "Functions in this package attempt to localize a solution/root of a given function.

The functions return the following values: x-best-guess fun-value-at-x-best-guess reason-for-search-ended

If a solution/root was found (i.e. fun-value-at-x-best-guess was within :YEPS of zero), then reason-for-search-ended will NIL.  Any other
value (which will be an informative string) requires a closer examination of x-best-guess fun-value-at-x-best-guess before one can
determine if they are acceptable."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_infix-string-to-diff-lambda (the-string var num-diff)
  "Take an MXP infix string, and return a lambda acceptable for MJR_NLEQ_ROOT-NEWTON or MJR_NLEQ_ROOT-LAGUERRE.
If THE-STRING is not a string, then THE-STRING is returned as is.
If THE-STRING contains the sub-string 'values(', it is assumed that the expression returns the required derivatives.  Otherwise
it is assumed to be F only, and symbolic derivatives are taken before the lambda is constructed."
  (if (stringp the-string)  
      (if (search "values(" the-string)
          (mjr_mxp_tree-to-lambda (mjr_mxp_infix-to-tree the-string) var)
          (mjr_mxp_trees-to-values-lambda (mjr_cas_diff-list (mjr_mxp_infix-to-tree the-string) var num-diff) (list var)))
      the-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_root-bsect (f x0 x1 &key (xeps 0.0001) (yeps 0.0001) (max-itr 1000) (show-progress nil) (use-false-position nil))
  "Use bisection, or false position if :USE-FALSE-POSITION is non-NIL, to localize a root of FUN between X0 and X1.

See mjr_nleq_help for a description of the three value return from this function.

While :USE-FALSE-POSITION is normally faster than regular bisection, it can fail to make progress in cases where regular bisection would have worked.  This is
because the new x value can be too close to a boundary and trigger an exit on XEPS or YEPS."
;; Maxima code for computing false position.
;; (%i69) eq:y=m*x+b;
;; (%o69) y = m*x+b
;; (%i70) solve(subst(y=0, subst(solve([subst([x=x1, y=y1], eq), subst([x=x2,y=y2], eq)], [m, b]), eq)), x);
;; (%o70) [x = (x1*y2-x2*y1)/(y2-y1)]
  (let ((f (mjr_nleq_infix-string-to-diff-lambda f "x" 0)))
    (flet ((nxt-x (x0 x1 y0 y1) (let ((fpc (if (and use-false-position (mjr_chk_!= y1 y0))
                                               (/ (- (* x0 y1) (* x1 y0)) (- y1 y0)))))
                                  (if (and fpc (not (mjr_eps_= x0 fpc xeps)) (not (mjr_eps_= x1 fpc xeps)))
                                      fpc
                                      (/ (+ x0 x1) 2)))))
      (loop with y0 = (funcall f x0)
            with y1 = (funcall f x1)
            with x-cur = (nxt-x x0 x1 y0 y1)
            with x-old = nil
            with y-old = nil
            for i-cur from 0
            do (let ((y-cur (funcall f x-cur)))
                 (if (mjr_eps_=0 y-cur yeps)                                (return (values x-cur y-cur nil)))
                 (if (and x-old (mjr_eps_= x-cur x-old xeps))               (return (values x-cur y-cur "X-DELTA=0")))
                 (if (mjr_eps_= x0 x1    xeps)                              (return (values x-cur y-cur "X0=X1")))
                 (if (mjr_eps_= x0 x-cur xeps)                              (return (values x-cur y-cur "X0=X")))
                 (if (mjr_eps_= x1 x-cur xeps)                              (return (values x-cur y-cur "X1=X")))
                 (if (mjr_eps_= y0 y1    yeps)                              (return (values x-cur y-cur "Y0=Y1")))
                 (if (mjr_eps_= y0 y-cur yeps)                              (return (values x-cur y-cur "Y0=Y")))
                 (if (mjr_eps_= y1 y-cur yeps)                              (return (values x-cur y-cur "Y1=Y")))
                 (if show-progress
                     (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
                 (setf x-old x-cur)
                 (cond ((mjr_cmp_< y0 y1) (if (mjr_cmp_< y-cur 0)
                                              (setq x0 x-cur y0 y-cur)
                                              (setq x1 x-cur y1 y-cur)))
                       ((mjr_cmp_> y0 y1) (if (mjr_cmp_< y-cur 0)
                                              (setq x1 x-cur y1 y-cur)
                                              (setq x0 x-cur y0 y-cur)))
                       ('t                                                  (return (values x-cur y-cur "Y0=Y1?"))))
                 (setf x-cur (nxt-x x0 x1 y0 y1))
                 (if (>= i-cur max-itr)                                     (return (values x-cur y-cur "MAX-ITR")))
                 (if (and y-old (mjr_eps_= y-cur y-old yeps))               (return (values x-cur y-cur "Y-DELTA=0")))
                 (setf y-old y-cur))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_root-newton (fdf x0 &key (xeps 0.0001) (yeps 0.0001) (max-itr 1000) (show-progress nil))
  "Use newton's method to localize a root of the f near (hopefully anyhow) X0.  fdf returns the value of f and df/dx.

See mjr_nleq_help for a description of the three value return from this function."
  (let ((fdf (mjr_nleq_infix-string-to-diff-lambda fdf "x" 1)))
    (loop with x-cur = x0
          with x-old = nil
          with y-old = nil
          for i-cur from 0
          do (multiple-value-bind (y-cur df) (funcall fdf x-cur)
               (if (mjr_eps_=0 y-cur yeps)                                      (return (values x-cur y-cur nil)))
               (if (mjr_eps_=0 df yeps)                                         (return (values x-cur y-cur "dy=0")))
               (if (mjr_chk_!=0 df)
                   (progn 
                     (if (and x-old (mjr_eps_= x-cur x-old xeps))               (return (values x-cur y-cur "X-DELTA=0")))
                     (if show-progress
                         (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
                     (setf x-old x-cur)
                     (setf x-cur (- x-cur (/ y-cur df)))
                     (if (>= i-cur max-itr)                                     (return (values x-cur y-cur "MAX-ITR")))
                     (if (and y-old (mjr_eps_= y-cur y-old yeps))               (return (values x-cur y-cur "Y-DELTA=0")))
                     (setf y-old y-cur))
                   (error "ERROR: mjr_nleq_root-newton: division by zero (dy) detected. Value of YEPS may be too small."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_root-laguerre (fdfddf n x0 &key (xeps 0.0001) (yeps 0.0001) (max-itr 1000) (show-progress nil))
  "Use laguerre's method to localize a root of f near (hopefully anyhow) X0. fdfddf returns (values f df/dx ddf/dx)

N is the degree of the polynomial FUN -- or just some number bigger than 1 if FUN is not a polynomial. :)

See mjr_nleq_help for a description of the three value return from this function."
  (let ((fdfddf (mjr_nleq_infix-string-to-diff-lambda fdfddf "x" 2)))
    (loop with x-cur = x0
          with x-old = nil
          with y-old = nil
          for i-cur from 0
          do (multiple-value-bind (y-cur df ddf) (funcall fdfddf x-cur)
               (if (mjr_eps_=0 y-cur yeps)                                  (return (values x-cur y-cur nil)))
               (if (mjr_eps_=0 df yeps)                                     (return (values x-cur y-cur "dy=0")))
               (if (and x-old (mjr_eps_= x-cur x-old xeps))                 (return (values x-cur y-cur "X-DELTA=0")))
               (if show-progress
                   (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
               (setf x-old x-cur)
               (setf x-cur
                     (let* ((G    (/ df y-cur))
                            (Gsq  (* G G))
                            (H    (- Gsq (/ ddf y-cur)))
                            (dsbp (sqrt (* (- n 1) (- (* n H) Gsq))))
                            (Am   (mjr_cmp_abs-max (- G dsbp) (+ G dsbp))))
                       (- x-cur (/ n Am))))
               (if (>= i-cur max-itr)                                       (return (values x-cur y-cur "MAX-ITR")))
               (if (and y-old (mjr_eps_= y-cur y-old yeps))                 (return (values x-cur y-cur "Y-DELTA=0")))
               (setf y-old y-cur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_nleq_fixed-point-itr (f x0 &key (xeps 0.0001) (yeps 0.0001) (max-itr 1000) (show-progress nil))
  "Use fixed point iteration to localize a a fixed point (a root of f(x)-x) near (hopefully anyhow) X0.  

See mjr_nleq_help for a description of the three value return from this function."
  (let ((f (mjr_nleq_infix-string-to-diff-lambda f "x" 0)))
    (loop with x-cur = x0
          with x-old = nil
          with y-old = nil
          for i-cur from 0
          do (let ((y-cur (funcall f x-cur)))
               (if (mjr_eps_= x-cur y-cur yeps)                           (return (values x-cur y-cur nil)))
               (if (and x-old (mjr_eps_= x-cur x-old xeps))               (return (values x-cur y-cur "X-DELTA=0")))
               (if show-progress
                   (format 't "~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
               (setf x-old x-cur)
               (setf x-cur y-cur)
               (if (>= i-cur max-itr)                                     (return (values x-cur y-cur "MAX-ITR")))
               (if (and y-old (mjr_eps_= y-cur y-old yeps))               (return (values x-cur y-cur "Y-DELTA=0")))
               (setf y-old y-cur)))))
