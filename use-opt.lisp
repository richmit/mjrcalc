;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-opt.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Univariate function optimization.@EOL
;; @Std       Common Lisp
;;
;;            TODO: * mxp support
;;            TODO: ** For functions to be optimized
;;            TODO: ** TO compute dy/dx for functions given as an infix-string
;;            TODO: * New 1D optimization
;;            TODO: ** 1D Newton
;;            TODO: ** steepest descent
;;            TODO: ** random search
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_OPT
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_PRNG)
  (:DOCUMENTATION "Brief: Univariate function optimization.;")
  (:EXPORT #:mjr_opt_minimize-bsect
           ))

(in-package :MJR_OPT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_opt_minimize-bsect (f x0 x1 xm &key (xeps 0.0001) (max-itr 1000) (show-progress nil) (use-random-position nil))
  "Optimize the function F, from R->R, via a bisection-like algorithm (minimum found will be X0 and X1).

XM should be between X0 and X1, and f(XM) should be less than f(X1) and f(X0) -- it is an approximation to the actual minimum.  If XM is NIL, then random
guesses between X0 and X1 will be used until a suitable XM is found or the maximum number of iterations is reached.

:USE-RANDOM-POSITION determines how new approximations are selected
  * if non-NIL, then a random point on the left or right of XM will be selected until X0 and X1 are too close, and then bisection except that the left right
    choice is made at random.
  * if NIL, then bisection is used (on the right of XM, and then alternating left & right)

If a minimum is found (i.e. an x1 nearly equal to X0), then the return is: x, (fun x), iteration count.  If MJR_NLOP_MIN-BSECT exits for any other
reason (perhaps a minimum/perhaps not), then the return is: nil, x, (fun x), iteration count.

References:
  Jones, Maillardet, & Robinson (2009); Introduction to Scientific Programming and Simulation Using R; ISBN:1420068725; pp 204"
;;
;; Def: \\ Given an element of $R^3$ denoted by $(x_0, x_m, x_1)$, we call this vector admissible with respect to a function
;; $f:R\to R$ if and only if $x_0<x_m<x_1$ and $f(x_0), f(x_1)\ge f(x_m)$.
;;
;; Thm: 
;; \begin{description}
;;   \item[$x_p\in(x_0,x_m)$ ::]
;;      If $f(x_p)<f(x_m)$, then $(x_0, x_p, x_m)$ is admissible, otherwise $(x_p, x_m, x_1)$ is admissible.
;;   \item[$x_p\in(x_m,x_1)$ ::]
;;        If $f(x_p)<f(x_m)$, then $(x_m, x_p, x_1)$ is admissible, otherwise $(x_0, x_m, x_p)$ is admissible.
;; \end{description}
;;
;; By picking new admissible vectors such that $|x_0-x_1|$ gets smaller, we find will eventually narrow down the interval onto a minimizer.
;;
  (let* ((y0 (funcall f x0))
         (y1 (funcall f x1))
         (xm (or xm
                 (loop for x-cur = (mjr_prng_float-oo x0 x1)
                       for y-cur = (funcall f x-cur)
                       for i-cur from 0
                       do (if show-progress
                              (format 't "XM: ~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
                       when (>= i-cur max-itr)
                       do (return nil)
                       when (and (mjr_cmp_< y-cur y0) (mjr_cmp_< y-cur y1))
                       return x-cur)))
         (ym (and xm (funcall f xm))))
    (if (null xm)
        (values nil nil nil nil "NO-MX")
        (loop with x-old = nil
              with y-old = nil
              for i-cur from 0
              for right = (if use-random-position
                              (mjr_prng_boolean)
                              (evenp i-cur))
              for x-cur = (if (and use-random-position (> (- x1 xm) (* 2 xeps)) (> (- xm x0) (* 2 xeps)))
                              (if right
                                  (mjr_prng_float-oo (+ xm xeps) x1)
                                  (mjr_prng_float-oo x0          (- xm xeps)))
                              (/ (+ xm (if right x1 x0)) 2))
              do (let ((y-cur (funcall f x-cur)))
                   (if (mjr_cmp_= x0 x1    xeps)                              (return (values x-cur x-cur y-cur i-cur "X0=X1")))
                   (if show-progress
                       (format 't "MN: ~5d ~60@s ~60@s ~%" i-cur x-cur y-cur))
                   (setf x-old x-cur)
                   (if right
                       (if (mjr_cmp_< y-cur ym)
                           (setq x0 xm y0 ym xm x-cur ym y-cur)
                           (setq x1 x-cur y1 y-cur))
                       (if (mjr_cmp_< y-cur ym)
                           (setq x1 xm y1 ym xm x-cur ym y-cur)
                           (setq x0 x-cur y0 y-cur)))
                   (if (>= i-cur max-itr)                                     (return (values nil x-cur y-cur i-cur "MAX-ITR")))
                   (setf y-old y-cur))))))
