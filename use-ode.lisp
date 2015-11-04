;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-ode.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @date      2015-MM-DD
;; @version   VERSION
;; @brief     ODE (Ordinary Differential Equation) IVP (Initial Value Problem) solvers.@EOL
;; @std       Common Lisp
;; @see       tst-ode.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997, 1998, 2004, 2013, 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo Put results in dquad.@EOL@EOL
;; @todo Don't print too many warnings. Ex: WARNING(mjr_ode_slv-ivp-erk-interval): Violated Y-ERR-ABS-MAX or Y-DELTA-ABS-MAX constraint!.@EOL@EOL
;; @todo mjr_ode_slv-ivp-erk-interval: Zero x-delta-min may lead to infinite loop.@EOL@EOL
;; @todo mjr_ode_slv-ivp-erk-interval: Add exit for total path length beyond a threshold.@EOL@EOL
;; @todo mjr_ode_slv-ivp-erk-interval: Add exit for length of y beyond a threshold.@EOL@EOL
;; @todo mjr_ode_slv-ivp-erk-interval: Add min y-delta -- a "get progress" parameter.@EOL@EOL
;; @todo mjr_ode_slv-ivp-erk-interval: Starting step size computation .@EOL@EOL
;; @todo mjr_ode_erk-step-kernel: Add option to provide traditional error computation.@EOL@EOL
;; @todo Add support EQ list with some elements returning vectors and others return numbers.@EOL@EOL
;; @todo Better handling when tolerances are not given: i.e. don't compute y2-delta, traverse lists unnecessarily.@EOL@EOL
;; @todo FASTER.@EOL@EOL
;; @todo Richardson Extrapolation for 'err' tolerances.@EOL@EOL
;; @todo Add high order Verner methods.@EOL@EOL
;; @todo Add additional fehlberg methods.@EOL@EOL
;; @todo Add optimized version of Dormand-Price method (double, formula based, FAST).@EOL@EOL
;; @todo Add embedded methods for all order 1, 2, 3, & 4 methods.@EOL@EOL
;; @todo Add NAG methods.@EOL@EOL
;; @todo Do everything in float-double.@EOL@EOL
;; @todo Add better support for stiff problems.@EOL@EOL
;; @todo Figure out a way to do everything with optimized doubles.  Could use macros to create optimized step functions..@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_ODE
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_VVEC
        :MJR_CHK
        :MJR_VEC
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: ODE (Ordinary Differential Equation) IVP (Initial Value Problem) solvers.;")
  (:EXPORT #:mjr_ode_help

           #:mjr_ode_slv-ivp-erk-mesh #:mjr_ode_slv-ivp-erk-interval

           #:mjr_ode_erk-step-kernel

           #:mjr_ode_erk-step-euler-1 #:mjr_ode_erk-step-heun-2 #:mjr_ode_erk-step-mid-point-2 #:mjr_ode_erk-step-runge-kutta-4
           #:mjr_ode_erk-step-kutta-three-eight-4

           #:mjr_ode_erk-step-heun-euler-1-2 #:mjr_ode_erk-step-zonneveld-4-3 #:mjr_ode_erk-step-merson-4-5
           #:mjr_ode_erk-step-fehlberg-4-5 #:mjr_ode_erk-step-fehlberg-7-8 #:mjr_ode_erk-step-bogackia-shampine-3-2
           #:mjr_ode_erk-step-cash-karp-5-4 #:mjr_ode_erk-step-dormand-prince-5-4 #:mjr_ode_erk-step-verner-6-5

           #:mjr_ode_erk-step-euler-1-direct #:mjr_ode_erk-step-heun-2-direct #:mjr_ode_erk-step-runge-kutta-4-direct

           ;; Experimental -- not exported
           ;;#:mjr_ode_erk-step-Prince-Dormand-7-8
           ))

(in-package :MJR_ODE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_help ()
"Help for MJR_ODE:
A system of ODEs is defined to be:

$$
\\frac{\\mathrm{d}\\vec{\\mathbf{y}}}{\\mathrm{d}x} =  \\mathbf{f}(x, \\mathbf{y})
=
\\left[\\begin{array}{c}
 \\frac{\\mathrm{d}y_1}{\\mathrm{d}x} \\\\
 \\vdots                            \\\\
 \\frac{\\mathrm{d}y_n}{\\mathrm{d}x} \\\\
\\end{array}\\right]
=
\\left[\\begin{array}{c}
 f_1(x, \\mathbf{y}) \\\\
 \\vdots             \\\\
 f_n(x, \\mathbf{y}) \\\\
\\end{array}\\right]
=
\\left[\\begin{array}{c}
 f_1(x, [y_1, \\cdots, y_n]^\\mathrm{T}) \\\\
 \\vdots                                \\\\
 f_n(x, [y_1, \\cdots, y_n]^\\mathrm{T}) \\\\
\\end{array}\\right]
$$

While such systems may be the natural result of the physical system under study, it is more common for them to be the result of transforming a single
$n^\\mathrm{th}$ order differential equation into a group of $n$ first order equations -- which facilitates the solution of the original equation via
numerical methods.  When multiple higher order ODEs are to be solved, the natural result is several such systems to be simultaneously solved.  This package
solves such groups of first order ODE systems via various Runge-Kutta methods.

A Runge-Kutta method is defined by a set of coefficients that are most frequently organized into a ``Butcher tableau'':

$$\\begin{array}{c|cccc}
c_1              & a_{11}      & a_{12}      & \\dots  & a_{1s}      \\\\
c_2              & a_{21}      & a_{22}      & \\dots  & a_{2s}      \\\\
\\vdots          & \\vdots     & \\vdots     & \\ddots & \\vdots     \\\\
c_s              & a_{s1}      & a_{s2}      & \\dots  & a_{ss}      \\\\
\\hline
\\rule{0pt}{12pt} & \\check{b}_1 & \\check{b}_2 & \\dots  & \\check{b}_s \\\\
                 &   \\hat{b}_1 &   \\hat{b}_2 & \\dots  &   \\hat{b}_s \\\\
\\end{array}$$

Given initial conditions, $x_0$ and $\\mathbf{y_0}$, we may approximate the value of $\\mathbf{y}(x_0+\\Delta{x})$ by
$\\mathbf{y_0}+\\mathbf{\\Delta\\check{y}}$ with $\\mathbf{\\Delta\\check{y}}$ computed as below.  Note that the
$\\mathbf{\\hat{b}}$ vector, if it is defined, is used to construct an alternate solution for the purpose of estimating the error of
the approximation generated from $\\mathbf{\\check{b}}$.

$$\\mathbf{\\hat{k}}_i   = \\mathbf{f}\\left(x + c_i \\Delta{x},\\, \\mathbf{y} + \\Delta{x} \\sum_{j=1}^{s} a_{ij} \\mathbf{\\hat{k}}_j\\right)
     \\,\\,\\,\\,\\,\\,\\,\\,\\,\\,\\,\\,
  \\mathbf{\\check{k}}_i = \\mathbf{f}\\left(x + c_i \\Delta{x},\\, \\mathbf{y} + \\Delta{x} \\sum_{j=1}^{s} a_{ij} \\mathbf{\\check{k}}_j\\right)$$
$$\\mathbf{\\Delta\\check{y}} = \\Delta{x}\\sum_{i=1}^s \\check{b}_i \\mathbf{k}_i \\,\\,\\,\\,\\,\\,\\,\\,\\,\\,\\,\\,
  \\mathbf{\\Delta\\hat{y}}   = \\Delta{x}\\sum_{i=1}^s \\hat{b}_i   \\mathbf{k}_i$$

The primary 'Human Interface' functions are:

  * MJR_ODE_SLV-IVP-ERK-MESH     -- Solve ODE IVP via an explicit RK method at specified grid points
  * MJR_ODE_SLV-IVP-ERK-INTERVAL -- Solve ODE IVP via explicit RK method interval endpoint (with an adaptive mesh or single jump)

The primary emphasis of this code base is to enable one to easily compare various RK methods and experiment with new ones.  The functions above implement the
overall structure common to all RK algorithms, but leave the actual RK step to plugin functions.

Some vocabulary:
   * ERK      Explicit Runge-Kutta
               The matrix $[a_{i,}]$ is lower triangular.
   * EERK     Embedded Explicit Runge-Kutta
               The $\\mathbf{\\hat{b}}$ vector is defined.  
   * EERKLE   Embedded Explicit Runge-Kutta with Local Extrapolation
               The $\\mathbf{\\check{b}}$ method has is higher order than the $\\mathbf{\\hat{b}}$ method 
   * ODE      Ordinary Differential Equation
   * IVP      Initial Value Problem

The following functions each implement a the single step RK method suitable for use with the MJR_ODE_SLV-IVP-ERK-MESH and MJR_ODE_SLV-IVP-ERK-INTERVAL
functions:

  * mjr_ode_erk-step-euler-1               --  order 1      ERK
  * mjr_ode_erk-step-euler-1-direct        --  order 1      ERK - as above, but a faster direct formula
  * mjr_ode_erk-step-heun-2                --  order 2      ERK
  * mjr_ode_erk-step-heun-2-direct         --  order 2      ERK - as above, but a faster direct formula
  * mjr_ode_erk-step-mid-point-2           --  order 2      ERK
  * mjr_ode_erk-step-runge-kutta-4         --  order 4      ERK
  * mjr_ode_erk-step-runge-kutta-4-direct  --  order 4      ERK - as above, but a faster direct formula
  * mjr_ode_erk-step-kutta-three-eight-4   --  order 4      ERK
  * mjr_ode_erk-step-heun-euler-1-2        --  order 1(2)   EERK
  * mjr_ode_erk-step-bogackia-shampine-3-2 --  order 3(2)   EERKLE
  * mjr_ode_erk-step-zonneveld-4-3         --  order 4(3)   EERK
  * mjr_ode_erk-step-merson-4-5            --  order 4('5') EERK
  * mjr_ode_erk-step-fehlberg-4-5          --  order 4(5)   EERK
  * mjr_ode_erk-step-cash-karp-5-4         --  order 5(4)   EERKLE
  * mjr_ode_erk-step-dormand-prince-5-4    --  order 5(4)   EERKLE
  * mjr_ode_erk-step-verner-6-5            --  order 6(5)   EERKLE
  * mjr_ode_erk-step-fehlberg-7-8          --  order 7(8)   EERK

See MJR_ODE_ERK-STEP-KERNEL and MJR_ODE_ERK-STEP-HEUN-EULER-1-2 for the interface guidelines for a RK step function, and how to construct one from the Butcher
Tableau.  See MJR_ODE_ERK-STEP-EULER-1-DIRECT for an example of directly implementing a non-embedded RK step function."
  (documentation 'mjr_ode_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-kernel (eq x y xdelta
                                a c b1 b2 p1 p2
                                y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Perform one step of an Explicit Runge-Kutta method (with or without local extrapolation).

Return is two values:

  * y1-delta -- the delta approximation computed from B1.
  * error    -- approximation of the error via  B2 or NIL if B2 was not defined

Arguments:
  A, C, B1, & B2 specify the Butcher Tableau (Butcher 1964b) for the Runge-Kutta method
     A - The matrix
     C - The left vector
     B1 - First bottom vector (Used to compute Y-NEXT approximation)
     B2 - Second bottom vector (Used to compute ERR)."
  (declare (ignore p2))
  (let* ((nstage (array-dimension a 0))
         (k-lst  (mapcar (lambda (cur-eq cur-y)
                           (let ((k (make-array nstage)))
                             (loop for i from 0 upto (1- nstage)
                                   finally (return k)
                                   do (setf (aref k i)
                                            (funcall cur-eq (+ x (* (aref c i) xdelta))
                                                     (mjr_vec_+ cur-y
                                                                (mjr_vec_* xdelta
                                                                           (loop with vs = 0
                                                                                 for j from 0 upto (1- i)
                                                                                 do (setf vs (mjr_vec_+ vs (mjr_vec_* (aref a i j)
                                                                                                                      (aref k j))))
                                                                                 finally (return vs)))))))))
                         eq
                         y))
         (y1-delta (mapcar (lambda (k)
                             (mjr_vec_* xdelta (reduce #'mjr_vec_+ (map 'vector #'mjr_vec_* b1 k))))
                           k-lst))
         (y2-delta (if b2
                       (mapcar (lambda (k)
                                 (mjr_vec_* xdelta (reduce #'mjr_vec_+ (map 'vector #'mjr_vec_* b2 k))))
                               k-lst)))
         (err      nil)
         (y-rat    nil))
    (if (or y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max) ;; Compute err and y-rat
        (loop for cur-y1-delta        in y1-delta
              for cur-y2-delta        in y2-delta
              for cur-y               in y
              for cur-y-err-abs-max   in y-err-abs-max
              for cur-y-err-rel-max   in y-err-rel-max
              for cur-y-delta-abs-max in y-delta-abs-max
              for cur-y-delta-rel-max in y-delta-rel-max
              do (loop for i-cur-y1-delta        across cur-y1-delta
                       for i-cur-y2-delta        across cur-y2-delta
                       for i-cur-y               across cur-y
                       for i-cur-y-err-abs-max   across cur-y-err-abs-max
                       for i-cur-y-err-rel-max   across cur-y-err-rel-max
                       for i-cur-y-delta-abs-max across cur-y-delta-abs-max
                       for i-cur-y-delta-rel-max across cur-y-delta-rel-max
                       for y-mag = (+ (abs i-cur-y) (mjr_numu_max-nil i-cur-y1-delta i-cur-y2-delta))
                       for i-s   = (mjr_numu_max-nil i-cur-y-err-abs-max (if i-cur-y-err-rel-max (* i-cur-y-err-rel-max y-mag)))
                       when (mjr_chk_!=0 i-cur-y1-delta)
                       do (progn (if i-cur-y-delta-abs-max
                                     (setf y-rat (mjr_numu_min-nil y-rat (abs (/ i-cur-y-delta-abs-max i-cur-y1-delta)))))
                                 (if i-cur-y-delta-rel-max
                                     (setf y-rat (mjr_numu_min-nil y-rat (abs (/ (* y-mag i-cur-y-delta-rel-max) i-cur-y1-delta))))))
                       ;; We don't check for /0 here as i-s can be very small....
                       when i-s
                       do (setf err (mjr_numu_max-nil err (abs (/ (- i-cur-y1-delta i-cur-y2-delta) i-s)))))))
    (values y1-delta
            (mjr_numu_min-nil (if err
                                  (if (mjr_chk_!=0 err)
                                      ;; We use p1+1 instead of 1+max(p1,p2) as is normally used.  The normal algorithm might
                                      ;; use b1 or b2 to compute y-delta, and we always use b1 here -- so we can just use p1+1.
                                      (expt (/ err) (/ (+ 1 p1)))
                                      1))
                              y-rat))))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-euler-1 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 1.
References:
  Euler (1768); Institutionum Calculi Integralis. Volumen Primum, Opera Omnia, Vol XI. p424
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p35"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0))
                           #(0)
                           #(1)
                           nil
                           1 nil
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-heun-2 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 2.
References:
  Butcher (2008); Numerical Methods for Ordinary Differential Equations; p98
Corresponds to the trapezoidal rule."
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0 0)
                               (1 0))
                           #(0 1)
                           #(1/2 1/2)
                           nil
                           2 nil
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-mid-point-2 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 2.
References:
  Butcher (2008); Numerical Methods for Ordinary Differential Equations; p98
Corresponds to the mid-point rule."
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0   0)
                               (1/2 0))
                           #(0 1/2)
                           #(0 1)
                           nil
                           2 nil
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-runge-kutta-4 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 4.
References:
  Kutta (1901); Beitrag zur n\"herungsweisen Integration totaler Differentialgleichungen; Z. Math. Phys. 46; p435-453. 
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p138
In the literature, this method is frequently called 'RK4'.  It is considered by many to be 'the' Runge-Kutta method."
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0   0   0 0)
                               (1/2 0   0 0)
                               (0   1/2 0 0)
                               (0   0   1 0))
                           #(0   1/2 1/2 1)
                           #(1/6 1/3 1/3 1/6)
                           nil
                           4 nil
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-kutta-three-eight-4 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 4.  More precise than RK4.  Sometimes called the '3/8-rule'.
References:
  Kutta (1901), Beitrag zur n\"herungsweisen Integration totaler Differentialgleichungen; Z. Math. Phys. 46; p435-453. 
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p138"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a(( 0   0 0 0)
                               ( 1/3 0 0 0)
                               (-1/3 1 0 0)
                               ( 0  -1 1 0))
                           #(0   1/3 2/3 1)
                           #(1/8 3/8 3/8 1/8)
                           nil
                           4 nil
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-heun-euler-1-2 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0 0)
                               (1 0))
                           #(0 1)
                           #(1 0)
                           #(1/2 1/2)
                           1 2
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-verner-6-5 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 6(5).
References:
  J.H. Verner (1978); Explicit Runge-Kutta methods with estimates of the local truncation error; SIAM J. Numer. Anal. 15; pp. 772
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p181"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0             0           0             0        0           0 0          0)
                               ( 1/6          0           0             0        0           0 0          0)
                               ( 4/75         16/75       0             0        0           0 0          0)
                               ( 5/6         -8/3         5/2           0        0           0 0          0)
                               (-165/64       55/6       -425/64        85/96    0           0 0          0)
                               ( 12/5        -8           4015/612     -11/36    88/255      0 0          0)
                               (-8263/15000   124/75     -643/680      -81/250   2484/10625  0 0          0)
                               ( 3501/1720   -300/43      297275/52632 -319/2322 24068/84065 0 3850/26703 0))
                           #(0 1/6 4/15 2/3 5/6 1 1/15 1)
                           #(13/160 0 2375/5984 5/16  12/85    3/44 0         0)
                           #(3/40   0 875/2244  23/72 264/1955 0    125/11592 43/616)
                           6 5
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-merson-4-5 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 4('5').

The '5' is fake.  This method is really 4th order.

This method is frequently called 'Kutta-Merson' or 'Runge-Kutta-Merson' (as in the NAG library).

References:
  R.H. Merson (1957); An operational method for the study of integration processes; Proc. Symp. Data Processing; pp 110-125
  Butcher (2008); Numerical Methods for Ordinary Differential Equations; p201
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p167"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a(( 0   0    0   0)
                               ( 1/3 0    0   0)
                               ( 1/6 1/6  0   0)
                               ( 1/8 3/8  0   0)
                               ( 1/2 0   -3/2 2))
                           #(0    1/3  1/3 1/2 1)
                           #(1/2  0   -3/2 2   0)
                           #(1/6  0    0   2/3 1/6)
                           4 4
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-zonneveld-4-3 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 4(3).

References:
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p167
  Zonneveld (1963); Automatic Integration of Ordinary Differential Equations; Report R743, Mathematisch Centrum"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a(( 0    0    0      0)
                               ( 1/2  0    0      0)
                               ( 0    1/2  0      0)
                               ( 0    0    1      0)
                               ( 5/32 7/32 13/32 -1/32))
                           #(0     1/2  1/2 1     3/4)
                           #( 1/6  1/3  1/3 1/6   0)
                           #(-1/2  7/3  7/3 13/6 -16/3)
                           4 4
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

(defun mjr_ode_erk-step-fehlberg-4-5 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 4(5).
This method is not designed for local extrapolation, and performs quite poorly when it is used with local extrapolation.
References:
  Erwin Fehlberg (1969); Klassische Runge-Kutta-Formeln f\"nfter and siebenter Ordnung mit Schrittweiten-Kontrolle; Computing (Arch. Elektron. Rechnen) 4
  Erwin Fehlberg (1969); Low-order Classical Runge-Kutta Formulas with Stepsize Control; NASA Technical Report R-315
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p177"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0         0          0          0         0      0)
                               (1/4       0          0          0         0      0)
                               (3/32      9/32       0          0         0      0)
                               (1932/2197 -7200/2197 7296/2197  0         0      0)
                               (439/216   -8         3680/513   -845/4104 0      0)
                               (-8/27     2          -3544/2565 1859/4104 -11/40 0))
                           #(0 1/4 3/8 12/13 1 1/2)
                           #(25/216 0 1408/2565  2197/4104    -1/5  0)
                           #(16/135 0 6656/12825 28561/56430  -9/50 2/55)
                           4 5
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-fehlberg-7-8 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 7(8).
This method is not designed for local extrapolation, and performs quite poorly when it is used with local extrapolation.
References:
  Erwin Fehlberg (1972); Classical eight- and lower-order Runge-Kutta-Nystroem formulas with stepsize control for special second-order differential equations; NASA Technical Report M-533
  Butcher (2008); Numerical Methods for Ordinary Differential Equations; p209
  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p180"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a(( 0         0    0      0         0          0        0         0      0       0      0 0  0)
                               ( 2/27      0    0      0         0          0        0         0      0       0      0 0  0)
                               ( 1/36      1/12 0      0         0          0        0         0      0       0      0 0  0)
                               ( 1/24      0    1/8    0         0          0        0         0      0       0      0 0  0)
                               ( 5/12      0   -25/16  25/16     0          0        0         0      0       0      0 0  0)
                               ( 1/20      0    0      1/4       1/5        0        0         0      0       0      0 0  0)
                               (-25/108    0    0      125/108  -65/27      125/54   0         0      0       0      0 0  0)
                               ( 31/300    0    0      0         61/225    -2/9      13/900    0      0       0      0 0  0)
                               ( 2         0    0     -53/6      704/45    -107/9    67/90     3      0       0      0 0  0)
                               (-91/108    0    0      23/108   -976/135    311/54  -19/60     17/6  -1/12    0      0 0  0)
                               ( 2383/4100 0    0     -341/164   4496/1025 -301/82   2133/4100 45/82  45/164  18/41  0 0  0)
                               ( 3/205     0    0      0         0         -6/41    -3/205    -3/41   3/41    6/41   0 0  0)
                               (-1777/4100 0    0     -341/164   4496/1025 -289/82   2193/4100 51/82  33/164  12/41  0 1  0))
                           #(0 2/27 1/9 1/6 5/12 1/2 5/6 1/6 2/3 1/3 1 0 1)
                           #(41/840 0 0 0 0 34/105 9/35 9/35 9/280 9/280 41/840 0      0)
                           #(0      0 0 0 0 34/105 9/35 9/35 9/280 9/280 0      41/840 41/840)
                           7 8
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-bogackia-shampine-3-2 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 3(2).
References:
  Bogacki & Shampine (1989); A 3(2) pair of Runge-Kutta formulas; Applied Mathematics Letters."
  (mjr_ode_erk-step-kernel eq x y xdelta
                          #2a((0   0   0   0)
                              (1/2 0   0   0)
                              (0   3/4 0   0)
                              (2/9 1/3 4/9 0))
                          #(0 1/2 3/4 1)
                          #(2/9  1/3 4/9 0)
                          #(7/24 1/4 1/3 1/8)
                          3 2
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-cash-karp-5-4 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 5(4).
References:
  Cash & Karp(1990);  A variable order Runge-Kutta method for initial value problems with rapidly varying right-hand sides; ACM Transactions on Mathematical Software 16"
  (mjr_ode_erk-step-kernel eq x y xdelta
                          #2a((0          0       0         0            0        0)
                              (1/5        0       0         0            0        0)
                              (3/40       9/40    0         0            0        0)
                              (3/10       -9/10   6/5       0            0        0)
                              (-11/54     5/2     -70/27    35/27        0        0)
                              (1631/55296 175/512 575/13824 44275/110592 253/4096 0))
                          #(0 1/5 3/10 3/5 1 7/8)
                          #(37/378     0 250/621     125/594     0         512/1771)
                          #(2825/27648 0 18575/48384 13525/55296 277/14336 1/4)
                          5 4
                          y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-dormand-prince-5-4 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 5(4).
References:
  Dormand & Prince (1980); A family of embedded Runge-Kutta formulae; J. Comput. Appl. Math. 6, no. 1"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2a((0          0           0          0        0           0     0)
                               (1/5        0           0          0        0           0     0)
                               (3/40       9/40        0          0        0           0     0)
                               (44/45      -56/15      32/9       0        0           0     0)
                              (19372/6561 -25360/2187 64448/6561 -212/729 0           0     0)
                               (9017/3168  -355/33     46732/5247 49/176   -5103/18656 0     0)
                               (35/384     0           500/1113   125/192  -2187/6784  11/84 0))
                           #(0 1/5 3/10 4/5 8/9 1 1)
                           #(35/384     0  500/1113   125/192 -2187/6784    11/84    0)
                           #(5179/57600 0  7571/16695 393/640 -92097/339200 187/2100 1/40)
                           5 4
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-Prince-Dormand-7-8 (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Order 7(8).  Not as well tested as mjr_ode_erk-step-fehlberg-7-8.  Use with care"
  (mjr_ode_erk-step-kernel eq x y xdelta
                           #2A((0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (1/18 0 0 0 0 0 0 0 0 0 0 0 0)
                               (1/48 1/16 0 0 0 0 0 0 0 0 0 0 0)
                               (1/32 0 3/32 0 0 0 0 0 0 0 0 0 0)
                               (5/16 0 -75/64 75/64 0 0 0 0 0 0 0 0 0)
                               (3/80 0 0 3/16 3/20 0 0 0 0 0 0 0 0)
                               (215595617/4500000000 0 0 202047683/1800000000 -28693883/1125000000 23124283/1800000000 0 0 0 0 0 0 0)
                               (14873762658037143/879168438156250000 0 0 3467633544794897/8940695981250000 1474287494383247/40978189914062500 26709270507070017/135600555715625000 -14591655588284/84484570233063 0 0 0 0 0 0)
                               (7586331039021946882049083502441337664277676907617750536566352/109794461601491217860220353338581031394059220336451160078730445 0 0 -236057339412812449835946465344221735535939129430991059693568/372184615598275314780407977418918750488336340123563254504171 -3299739166368883603096250588167927276977533790499480498577408/20470153857905142312922438758040531276858498706795978997729405 4695919603694846215470554638065271273971468502369170235542016/33868800019443053645017125945121606294438606951244256159879561 291851811898394201384602939640627532330843113837053004434432000000/310174233778061645620360730197195350622945922304711702829528117367 6992959981041103840944260661352231159203510904000000/33042342481018810238716485165383193327572243242031481 0 0 0 0 0)
                               (99299034813490800741867453179778547/540971123539151162906952826011200000 0 0 -2493835259080554724582/1010153717930905426875 -48550347897506146536052/166675363458599395434375 -24871192635697392099560348960246/939492072180864357472739828818125 478776089216929482237673925052922000/168119099731629344552415590032785027 6560308981643238155096750/23314158982833116227901307 1586281686644478270321241459439899956623408540189275177/12818966182821619734532382093543907143647820508227904000 0 0 0 0)
                               (-102116003386322998978127600084904875522141269364791505043913504184525097818434721165778087547359160299919872547571820573487921693/84016717385376362440519288454722754561118206109968455863629915569413007015484884082989277327146750610032897620427741658059440288 0 0 338590872606752219742507143357021902717271169524361004010718467428498066558752974165816979255870352236800/20308212073515087965058545521329962060416948491603802421256875704911573108931922671691153944392874968051 68189290605616416787948548385820859588684790288743680764422049661712817412461535969920258787664375619072/74463444269555322538548000244876527554862144469213942211275210918009101399417049796200897796107208216187 -1734282043732424474072631498514610096486835338935534079058145376760622893524503274680375038942168945756187943481380463560951840/286345537377499805912462279621622489249909215975695809863482134802066603511244489020404711919081949534640172065152437496912477 -3399549280223124443696423490103003766707892326374755946138975000967466690241111348721006509128775254952212682658842765965521154240000000/212424385105117691648087703103838079790425456287912424851546922389328485500145214289225448961304538830766072442444722564103495915888123 14452808190943733856347403293564049428070036006455540637351575894308889412108389906599600485253194980566957563315340127500000/973298753951638431793701721528200883789914680313298926814615071301495341142665245758696799918623095581715765886887649741383 -847205714160239289113307424793539077951658318917591980262304042838612275700008766016957700930195545053374220841398660187944621107065829310608865394026418258355/63358704383980726998416112830322706485300332630289060627019459285960825979588560697460438306253611095891491565590971432387489415884103732012574255897878321824 115188988949323598098458035263894669359112068207548636038131244599058496172710646622536373145562218909633738697549245770000/22435701423704647109276644681016984863989966659062291511947760084943925084166270812354794844590216383205333034660617626349 0 0 0)
                               (21969012306961489525323859125985377266525845354279828748/84868015648089839210997460517819380601933600521692915045 0 0 -2291872762438069505504/480025046760766258851 -3829018311866050387904/8800459190614048078935 -607977714773374460437401016185253441418120832060126402968/199370728929424959394190105343852509479613745231838418799 5302029233035772894614097632213626682295966947853615180783170000000/950538766256052885387161080614691196420735587733978871061913292363 102968047255116137164987219663037502898143843145000000/16726911019578511096352500731821705820659977305290973 -111383789341965407321602142444917514115800834690201329379027449761759895100011973929185171163615/22003454775272439861723739055800175619777853128055268766511800511549546753240522083740083243539 44737471541467333111555512048686345065750/20391511842264262870398286145868618178341 596546910748352988538198147432444829112451075399436970876618894337461087953328002664759407401623072330633057948252/4431076125983762085449284205348478790535717302043416234911901479328512794465980800998816354448181196721636373483787 0 0)
                               (1066221205855832326088695778460159015192405644968016897066521076847764032613686056268693633/1296431693610525557488309197474904206216262654240544950471874305723890174339356551609704000 0 0 -1335791413506612664643690684478806471077526746614666064/114574907798601779179110271814903983120429559544320175 -1591415543044168099882026495959288688569084060473110176/2100539976307699284950354983273239690541208591645869875 33975758488532631832742416857645572913178866704247539610423012370193845167470455176890924/47586856225469573819304596274208152402640120925455970356063642741972959597009066064956075 12176653428667113090492984656207574633063967759246601254930448409444470870786024235115138527800000/1008353786145118968620988891518234034224047994442049071310258686840184337101721351612973016221399 -339784374935367314296824613776444883113869450234942131172912300100535979345925250000/159698690787587746004588725210359673189662237866695585709500421500486548151424426361 4955095692700499418628052380948016677978733013841365878109775677669056866398110949788869771135857671298802131693154421086808143/2489789885462873158531234022579722982784822257458164105126884288597324542930882581099522281388970940826324647386340365850671680 -563115171027780776675066866318087406247194110301648522108648094708415/2403532595444498372383116767918060257292523183751650851596520916634577 147332487580158450887955957061658718012538967463083369806963200702426559434915876714751833908862217396388157664714990174448521780809/837599084085749358149340415048050308970085851893614803629073546048735327947816070400330404870816820234727495143522673498826476267825 0 0))
                           #(0 1/18 1/12 1/8 5/16 3/8 59/400 93/200 5490023248/9719169821 13/20 30992876149296355/33518267164510641 1 1)
                           #(212810988215683677989664967567559/5097575504458999984164528930580800 0 0 0 0 -570667999368605802515460802224128/10291145812277763122885317774476825 3970894643399159150754126826496000000000000/16592904867230933191457493387696939021741363 177094288219480472437690862000000000000/251729356670100506734814442705774463449 -66822609448295850920212176513645119787713273203022994500406050793972052314809461629969645683/87952305220338336969447643899150816363456821562985998778022435070001091778042097545895594560 314652731163869955629145958568800000/476340207420551356675670184044905167 177014954088789647707522848990757432519504314686067075784476503038212450536095365316360385634933688213244039743969578872631174179769/1119019983628991838522384101261104859676427163726922121733732080377576616485631933067985100908132443862205090961383250990215178108200 -454665916000392064556420344242099/1909482158429176288068071462671400 1/4)
                           #(7136040226482108704342809557217/241464102842794736092004001974880 0 0 0 0 -15349154422148033115423212285265536/18524062462099973621193571994058285 45434521806506196832804182374790400000000/145978635195580057402851847985603569106229 365696286946774693155766999232150000000/148214481030059176862554298041717674741 -836336669851503831866889530158468123932231502753408325817124013619515886965077571/328368994730082689886153304749497093954319862912916225944630536728837081959128864 294694385044387823293019951454286000/204145803180236295718144364590673643 1759482754698187564675489259591170188433054767657805212470918093603353527288272972728828708146708084742711724049636/22155380629918810427246421026742393952678586510217081174559507396642563972329904004994081772240905983608181867418935 2/45 0)
                           7 8
                           y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-euler-1-direct (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Compute YDELTA directly using Euler's method via formulas in the most simplistic way possible.  Used for testing.

$$\Delta\mathbf{\vec{y}}=\Delta{x}\cdot\vec{\mathbf{f}}(x, \vec{\mathbf{y}})$$"
  (declare (ignore y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))
;;  (if (or y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
;;      (error "mjr_ode_erk-step-euler-1-direct: Method doesn't support 'err' or 'y-delta' tolerances."))
  (values (mapcar (lambda (cur-eq cur-y) (mjr_vec_* xdelta (funcall cur-eq x cur-y)))
                  eq
                  y)
          nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-heun-2-direct (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Compute YDELTA directly using Heun's method via formulas in the most simplistic way possible.  Used for testing.

$$\Delta\mathbf{\vec{y}}=
\frac{\Delta{x}}{2}[\vec{\mathbf{f}}(x, \vec{\mathbf{y}}) +
\vec{\mathbf{f}}(x+\Delta{x}, \vec{\mathbf{y}} + 
\Delta{x}\cdot\vec{\mathbf{f}}(x, \vec{\mathbf{y}}))]$$"
  (declare (ignore y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))
;;  (if (or y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
;;      (error "mjr_ode_erk-step-euler-1-direct: Method doesn't support 'err' or 'y-delta' tolerances."))
  (values (mapcar (lambda (cur-eq cur-y)
                    (let* ((f1 (funcall cur-eq x            cur-y))
                           (f2 (funcall cur-eq (+ x xdelta) (mjr_vec_+ cur-y (mjr_vec_* xdelta f1)))))
                      (mjr_vec_* (/ xdelta 2) (mjr_vec_+ f1 f2))))
                  eq
                  y)
          nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_erk-step-runge-kutta-4-direct (eq x y xdelta y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
  "Compute YDELTA directly using RK4 via formulas in the most simplistic way possible.  Used for testing.

$$ \vec{\mathbf{k}}_i = \vec{\mathbf{f}}(x+a_i\Delta{x}, \vec{\mathbf{y}}+\sum_{j=1}^{i-1}b_{i,j}\vec{\mathbf{k}}_j) $$
$$ \Delta\mathbf{\vec{y}}=\sum_{i=1}^{n}c_i^*\vec{\mathbf{k}}_j $$

$$\begin{array}{rcl}
\vec{\mathbf{k}}_1 & = & \vec{\mathbf{f}}(x,                      \vec{\mathbf{y}})                                             \\
\vec{\mathbf{k}}_2 & = & \vec{\mathbf{f}}(x+\frac{1}{2}\Delta{x}, \vec{\mathbf{y}}+\frac{1}{2}\Delta{x}\vec{\mathbf{k}}_1) \\
\vec{\mathbf{k}}_3 & = & \vec{\mathbf{f}}(x+\frac{1}{2}\Delta{x}, \vec{\mathbf{y}}+\frac{1}{2}\Delta{x}\vec{\mathbf{k}}_2) \\
\vec{\mathbf{k}}_4 & = & \vec{\mathbf{f}}(x+\Delta{x},            \vec{\mathbf{y}}+\Delta{x}\vec{\mathbf{k}}_3)            \\
\end{array}$$
$$\Delta\mathbf{\vec{y}}=\frac{\vec{\mathbf{k}}_1+2\vec{\mathbf{k}}_2+2\vec{\mathbf{k}}_3+\vec{\mathbf{k}}_4}{6}$$"
  (declare (ignore y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max))
;;  (if (or y-err-abs-max y-err-rel-max y-delta-abs-max y-delta-rel-max)
;;      (error "mjr_ode_erk-step-euler-1-direct: Method doesn't support 'err' or 'y-delta' tolerances."))
  (values (mapcar (lambda (cur-eq cur-y)
                    (let* ((xdelta2 (/ xdelta 2))
                           (k1 (funcall cur-eq x             cur-y))
                           (k2 (funcall cur-eq (+ x xdelta2) (mjr_vec_+ cur-y (mjr_vec_* xdelta2 k1))))
                           (k3 (funcall cur-eq (+ x xdelta2) (mjr_vec_+ cur-y (mjr_vec_* xdelta2 k2))))
                           (k4 (funcall cur-eq (+ x xdelta)  (mjr_vec_+ cur-y (mjr_vec_* xdelta  k3)))))
                       (mjr_vec_* xdelta (mjr_vec_+ (mjr_vec_/ k1 6) (mjr_vec_/ k2 3) (mjr_vec_/ k3 3) (mjr_vec_/ k4 6)))))
                  eq
                  y)
          nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_slv-ivp-erk-interval (eq ivy x-min x-max &key
                                         max-itr x-delta-min x-delta-max x-delta-init x-delta-fac-max x-delta-fac-min
                                         y-delta-abs-max y-delta-rel-max y-err-abs-max y-err-rel-max x-delta-fac-fuz
                                         suppress-warnings x-delta-min-rej-err show-progress algorithm return-all-steps
                                         out-y-canonical)
  "Solve one or more systems ODEs in EQ at point X-MAX given initial values X-MIN & IVY using an adaptive Runge-Kutta method.

   Arguments:

    Equations, initial values, and solution interval:
       *  eq ................... Lisp functions implementing $f_{i,j}$
                                 Three possible forms:

                                   * List of functions (each representing a system of ODEs) taking a real and a vector and
                                     returning a vector.
                                     IVY must be a list of vectors
                                   * Single function taking a real and a vector and returning a vector
                                     IVY must be a single vector
                                   * Single function taking two reals and returning a real
                                     IVY must be a single number
                                 DEFAULT: NONE!!
       *  ivy .................. Initial value for y (on the first mesh point)
                                 Three possible forms (form for EQ must be compatible with form of IVY):
                                   * List of vectors
                                     (list (vector y_{1,1}(x_0) ... y_{1,n_1}(x_0) )  ;; n_1 initial values for system 1
                                           (vector y_{2,1}(x_0) ... y_{2,n_2}(x_0) )  ;; n_2 initial values for system 2
                                                                ...
                                           (vector y_{m,1}(x_0) ... y_{m,n_m}(x_0) )) ;; n_m initial values for system m
                                   * Single vector
                                     (vector y_{1,1}(x_0) ... y_{1,n_1}(x_0))
                                   * Single number
                                     y_{1,1}(x_0)
                                 DEFAULT: NONE!!
       *  x-min ................ Initial x value (start of solution interval)
                                 DEFAULT: 0
       *  x-max ................ Final x value (end of solution interval)
                                 DEFAULT: 1
    Step size control:
       *  max-itr .............. Maximum number of iterations
                                 DEFAULT: nil
       *  x-delta-min .......... Minimum value x-delta may have
                                 DEFAULT: (max 1d-15 (/ (- x-max x-min) 100))
       *  x-delta-max .......... Maximum value x-delta may have
                                 DEFAULT: (- x-max x-min)
       *  x-delta-init ......... Initial value for x-delta
                                 DEFAULT: (- x-max x-min)
       *  x-delta-fac-max ...... Maximum factor to expand x-delta
                                 DEFAULT: 2
       *  x-delta-fac-min ...... Minimum factor to expand x-delta
                                 DEFAULT: 1/2
       *  y-delta-abs-max ...... Maximum y-delta to accept
                                 May be a list of vectors, a vector, or a number
       *  y-delta-rel-max ...... Maximum relative y-delta to accept
                                 May be a list of vectors, a vector, or a number
       *  y-err-abs-max ........ Maximum y-err to accept
                                 May be a list of vectors, a vector, or a number
       *  y-err-rel-max ........ Maximum relative y-err to accept
                                 May be a list of vectors, a vector, or a number
       *  x-delta-fac-fuz ...... If x-delta-fac is within x-delta-fac-fuz of 1, then (setq x-delta-fac 1)
                                 DEFAULT: 1/10
       *  x-delta-min-rej-err .. If rejecting a step at x-delta-min, then error out
                                 DEFAULT: nil
    Output Control:
       *  return-all-steps ..... Return data for all steps
                                 DEFAULT:  nil
       *  out-y-canonical ...... Returned y is a list of vectors
                                 If nil, then returned y takes the form of IVY
                                 Only used if RETURN-ALL-STEPS is NIL
                                 DEFAULT: nil
    Miscellaneous:
       *  suppress-warnings .... Don't print warnings
                                  * Rejecting a step at x-delta-min
                                 DEFAULT: nil
       *  show-progress ........ Print progress at each step
                                 DEFAULT: nil
       *  algorithm ............ Select the algorithm to use. This is a function that performs a single forward step.
                                 Possible values for adaptive algorithms:
                                     * mjr_ode_erk-step-heun-euler-1-2        --  order 1(2)   EERK
                                     * mjr_ode_erk-step-zonneveld-4-3         --  order 4(3)   EERK
                                     * mjr_ode_erk-step-merson-4-5            --  order 4('5') EERK
                                     * mjr_ode_erk-step-fehlberg-4-5          --  order 4(5)   EERK
                                     * mjr_ode_erk-step-fehlberg-7-8          --  order 7(8)   EERK
                                     * mjr_ode_erk-step-bogackia-shampine-3-2 --  order 3(2)   EERKLE
                                     * mjr_ode_erk-step-cash-karp-5-4         --  order 5(4)   EERKLE
                                     * mjr_ode_erk-step-dormand-prince-5-4    --  order 5(4)   EERKLE
                                     * mjr_ode_erk-step-verner-6-5            --  order 6(5)   EERKLE
                                 When using an embedded algorithm, error is best controlled via the Y-ERR-REL-MAX or Y-ERR-ABS-MAX parameters.  The x and
                                 y-delta parameters can be used to set bounds, and fine tune step sizes, but they are generally not required -- except as a
                                 safety factor for very large steps.

                                 While it is possible to use an RK method without an embedded error estimate, both performance and error control can be quite
                                 poor.  The following should only be used by experts:
                                     * mjr_ode_erk-step-euler-1               --  order 1      ERK
                                     * mjr_ode_erk-step-euler-1-direct        --  order 1      ERK - as above, but a faster direct formula
                                     * mjr_ode_erk-step-heun-2                --  order 2      ERK
                                     * mjr_ode_erk-step-heun-2-direct         --  order 2      ERK - as above, but a faster direct formula
                                     * mjr_ode_erk-step-mid-point-2           --  order 2      ERK
                                     * mjr_ode_erk-step-runge-kutta-4         --  order 4      ERK
                                     * mjr_ode_erk-step-runge-kutta-4-direct  --  order 4      ERK - as above, but a faster direct formula
                                     * mjr_ode_erk-step-kutta-three-eight-4   --  order 4      ERK
                                 When using a non-embedded algorithm, error can't be controlled via the Y-ERR-REL-MAX or Y-ERR-ABS-MAX as the method has no
                                 good error estimation built in.  When using such RK methods, the x and y-delta parameters are the most important -- i.e. you
                                 must take direct control over the points used in the solution.  When using a non-embedded method, one should consider using
                                 mjr_ode_slv-ivp-erk-mesh instead of mjr_ode_slv-ivp-erk-interval as the mesh solver allows more direct control over the
                                 solution points.

                                 DEFAULT: #'mjr_ode_erk-step-cash-karp-5-4"
  (flet ((nil-min   (x y)     (or (and x y (min x y)) x y))
         (nil-max   (x y)     (or (and x y (max x y)) x y))
         (fix-bnd   (m v)     (cond ((null m)    (mapcar (lambda (cur-v) (make-array (length cur-v) :initial-element nil)) v))
                                    ((listp m)   m)
                                    ((vectorp m) (list m))
                                    ((numberp m) (mapcar (lambda (cur-v) (make-array (length cur-v) :initial-element m)) v))
                                    ('t          (error "mjr_ode_slv-ivp-erk-interval: Bad type for y error/delta constraint: ~a" m)))))
    (let* ((y-are-num       (numberp ivy))
           (y-are-vec       (vectorp ivy))
           (y-are-lst       (and ivy (listp ivy)))
           (ivy             (cond (y-are-lst ivy)
                                  (y-are-vec (list ivy))
                                  (y-are-num (list (make-array 1 :initial-element ivy)))
                                  ('t        (error "mjr_ode_slv-ivp-erk-interval: Bad type for ivy (must be number, vector, or list)"))))
           (eq              (cond (y-are-lst eq)
                                  (y-are-vec (list eq))
                                  (y-are-num (list (lambda (x y) (make-array 1 :initial-element (funcall eq x (aref y 0))))))))
           (x-min           (or x-min 0))
           (x-max           (or x-max 1))
           (x-range         (- x-max x-min))
           (x-delta-fac-fuz (or x-delta-fac-fuz 1/10))
           (x-delta-fac-max (or x-delta-fac-max 2))
           (x-delta-fac-min (or x-delta-fac-min 1/2))
           (x-delta-max     (or x-delta-max x-range))
           (x-delta-min     (or x-delta-min (max 1d-15 (min x-delta-max (/ x-range 1000)))))
           (algorithm       (or algorithm #'mjr_ode::mjr_ode_erk-step-cash-karp-5-4))
           (y-err-abs-max   (fix-bnd y-err-abs-max ivy))
           (y-err-rel-max   (fix-bnd y-err-rel-max ivy))
           (y-delta-abs-max (fix-bnd y-delta-abs-max ivy))
           (y-delta-rel-max (fix-bnd y-delta-rel-max ivy))
           (x-delta         (or x-delta-init (nil-max x-delta-min (nil-min x-delta-max x-range))))
           (y               (copy-tree ivy))
           (x               x-min)
           (step-data       (loop for itr from 0
                                  initially (if show-progress (format 't "INFO(mjr_ode_slv-ivp-erk-interval): ~5@a ~20@a ~20@a ~%" "itr" "x" "x-delta"))
                                  when (not (zerop itr))
                                  do (loop for (y-delta x-delta-raw-fac) = (multiple-value-list (funcall algorithm eq x y x-delta
                                                                                                         y-err-abs-max y-err-rel-max
                                                                                                         y-delta-abs-max y-delta-rel-max))
                                           for x-delta-fac = (min x-delta-fac-max
                                                                  (max x-delta-fac-min
                                                                       (if (and x-delta-raw-fac (> (abs (- 1 x-delta-raw-fac)) x-delta-fac-fuz))
                                                                           x-delta-raw-fac
                                                                           1)))
                                           for y-bad? = (< x-delta-fac 1)
                                           for x-at-min = (and x-delta-min (<= x-delta x-delta-min))
                                           when (or x-at-min (not y-bad?))
                                           return (progn (if (and y-bad? x-at-min (not suppress-warnings))
                                                             (warn "mjr_ode_slv-ivp-erk-interval: Violated Y-ERR-ABS-MAX or Y-DELTA-ABS-MAX constraint!"))
                                                         (if (and y-bad? x-at-min x-delta-min-rej-err)
                                                             (error "mjr_ode_slv-ivp-erk-interval: Violated Y-ERR-ABS-MAX or Y-DELTA-ABS-MAX constraint!"))
                                                         (setf y      (mapcar (lambda (cur-y cur-y-delta) (mjr_vec_+ cur-y cur-y-delta)) y y-delta)
                                                               x      (+ x x-delta)
                                                               x-delta (min (- x-max x) (nil-min x-delta-max (nil-max x-delta-min (* x-delta x-delta-fac))))))
                                           when show-progress
                                           do (format 't "INFO(mjr_ode_slv-ivp-erk-interval): ~5d ~20f ~20f ~%" itr x x-delta)
                                           do (setf x-delta (nil-min x-delta-max (nil-max x-delta-min (* x-delta x-delta-fac))))
                                           when (and max-itr (> itr max-itr))
                                           return (error "mjr_ode_slv-ivp-erk-interval: Violated MAX-ITR constraint!"))
                                  when return-all-steps
                                  collect (apply #'concatenate 'list (list x) y)
                                  when show-progress
                                  do (format 't "INFO(mjr_ode_slv-ivp-erk-interval): ~5d ~20f ~20f ***~%" itr x x-delta)
                                  until (> x-delta-min (- x-max x)))))
      (if return-all-steps
          (make-array (list (length step-data) (length (first step-data))) :initial-contents step-data)
          (if out-y-canonical
              y
              (cond (y-are-lst y)
                    (y-are-vec (first y))
                    (y-are-num (aref (first y) 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ode_slv-ivp-erk-mesh (eq ivy mesh &rest rest &key show-progress &allow-other-keys)
  "Approximate solutions to one or more systems of ODEs initial value problems across a mesh of points.

The return value will be a 2D array with a row for each mesh point, and a column for x and all the requested variables.

   Arguments:
    Specifying the mesh (See: MJR_VVEC_KW-NORMALIZE):
       *  POINTS, START, END, STEP, LEN 
    Equations and initial values (See: MJR_ODE_SLV-IVP-ERK-INTERVAL):
       *  EQ & IVY
    Miscellaneous:
       *  SHOW-PROGRESS .. Print progress at each step
                           DEFAULT: nil
    Arguments for MJR_ODE_SLV-IVP-ERK-INTERVAL
       The rest of the keyword arguments are for MJR_ODE_SLV-IVP-ERK-INTERVAL, but some will be suppressed:
         *  :RETURN-ALL-STEPS
         *  :OUT-Y-CANONICAL"
  (let* ((points (mjr_vvec_gen-0sim 'vector mesh)) ;; TODO: Instead of materializing the vector, we could get a forward iterator...
         (len    (length points))
         (kwa    (mjr_util_strip-kwarg rest :strip-list (list :return-all-steps :out-y-canonical)))
         (sol    (loop for i from 0 to (1- len)
                       for xi   = nil then xi+1
                       for xi+1 = (aref points i)
                       for y    = ivy then (apply #'mjr_ode_slv-ivp-erk-interval eq y xi xi+1 :out-y-canonical nil :return-all-steps nil kwa)
                       collect (cond ((numberp y) (list xi+1 y))
                                     ((vectorp y) (concatenate 'list (list xi+1) y))
                                     ('t          (apply #'concatenate 'list (list xi+1) (mapcar (lambda (cy) (if (numberp cy) (list cy) cy)) y))))
                       when show-progress
                       do (format 't "INFO(mjr_ode_slv-ivp-erk-mesh):     ~5d ~20f~%" i xi+1))))
    (make-array (list (length sol) (length (first sol))) :initial-contents sol)))
