;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ratTrig.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     @EOL
;; @std       Common Lisp
;; @see       use-rtrig.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @filedetails
;;
;;  Sometime ago I found myself in need of a rational approximation to the sin function that faithfully reproduced the qualitative aspects of the function,
;;  but didn't necessarily need to be terribly accurate with regard to absolute error.  See the MJR_RTRIG_HELP for the details!
;;  
;;  The first step is to select a rational value for $p$ that is near $\pi$ -- $\frac{355}{113}$ or $\frac{22}{7}$.  For one approximation, I also need a
;;  rational value for $s$ that is near $\frac{1}{\sqrt{2}}$, for which I use $\frac{70}{99}$.  The next step is to fit a polynomial to the trig function on
;;  $\left[0, \frac{p}{2}\right]$ or $[0, 2p]$ with a rational polynomial -- computed by fixing the values of the polynomial and its derivatives
;;  appropriately.  Care must be taken to pick points such that the polynomial will have the appropriate summitry when shifted half a period!
;;  
;;     ---------------------------------------------------------------------------------------------------------------------------------------------------------
;;      Deg  | range   |    012345678 |    max_err | p       | digits | Implimented     | 
;;     ---------------------------------------------------------------------------------------------------------------------------------------------------------
;;       4   | Quarter | P  ***...... | 0.00069828 | 22/7    | 3      | mjr_rirrf_sin-q | #(40474/1449459 -26117/131769 133/11979 1 0)
;;           | [0,p/2] | P' *.*...... |            |         | 3      |                 | 
;;     ---------------------------------------------------------------------------------------------------------------------------------------------------------
;;       9   | Full    | P  *.*.*.*.* | 0.00006467 | 355/113 | 4      | mjr_rirrf_sin-f | #(-212675535432514568/96711681202978424765625
;;           | [0,2p]  | P' *.*.*.*.* |            |         |        |                 |   1882084384358536/30269696777145046875
;;           |         |              |            |         |        |                 |   -3755840961706636/6395006361368671875
;;           |         |              |            |         |        |                 |   8769991453468/6004700808796875 7011036523/1429407056250
;;           |         |              |            |         |        |                 |   18757661/3811752150
;;           |         |              |            |         |        |                 |   -412170551/2415899250 339/252050 1 0)
;;     ---------------------------------------------------------------------------------------------------------------------------------------------------------
;;  
;;  Over time I have repeadly found use for sin/cos approximations where the qualitative aspects are more important than apsolute accuracey, and so I
;;  implimented a few of these in the package :MJR_RTRIG.
;;  
;;  The method used in the lisp code below to compute the polynomials is to construct & solve systems of simultaneous linear equations representing the given
;;  constraints.  An alternate, perhaps more intuitive, approach is to directly formulate the constriction equations and have a CAS solve for the
;;  coefficients.
;;  
;;   * maxima code to reproduce the 4th degree polynomial on the quarter unit circle:
;;      > n : 4;
;;      > polyValConstraints     : [ [0, 0], [p/4, s], [p/2, 1]];
;;      > polyDiffValConstraints : [ [0, 1], [p/2, 0]];
;;      > polyEq : sum(a[i]*x^i, i, 0, 4) = y;
;;      > polyValConstraintsEq : map(lambda([c], subst([x=c[1], y=c[2]], polyEq)), polyValConstraints);
;;      > polyDiffEq : diff(sum(a[i]*x^i, i, 0, 4), x) = y;
;;      > polyDiffValConstraintsEq : map(lambda([c], subst([x=c[1], y=c[2]], polyDiffEq)), polyDiffValConstraints);
;;      > R : solve(append(polyValConstraintsEq, polyDiffValConstraintsEq), makelist(a[i], i, 0, n));
;;      > SP : substitute(R, lhs(polyEq));
;;      > CP : substitute([s=70/99, p=22/7], SP);
;;      > plot2d(CP, [x, 0, %pi/2]);
;;      > plot2d(sin(x)-CP, [x, 0, %pi/2]);
;;      > lmax(makelist(at(abs(sin(x)-CP), x=x0), x0, 0, %pi/2, .001));
;;  
;;   * Maple code to reproduce the 4th degree polynomial on the quarter unit circle:
;;      > restart;
;;      > n:=4:
;;      > PolyValConstraints:=[ [0, 0], [p/2, 1], [p/4, s] ]:
;;      > PolyDiffValConstraints:=[ [0, 1], [p/2, 0] ]:
;;      > PolyEq:=y=sum(a[i]*x^i, i=0..n):
;;      > PolyValConstraintsEPolyEq := seq(subs(x=PolyValConstraints[i][1], y=PolyValConstraints[i][2], PolyEq), i=1..numelems(PolyValConstraints)):
;;      > PolyDiffEq:=y=diff(sum(a[i]*x^i, i=0..n),x):
;;      > PolyDiffValConstraintsEPolyEq := seq(subs(x=PolyDiffValConstraints[i][1], y=PolyDiffValConstraints[i][2], PolyDiffEq), i=1..numelems(PolyDiffValConstraints)):
;;      > R:=solve({PolyValConstraintsEPolyEq, PolyDiffValConstraintsEPolyEq}, {seq(a[i], i=0..n)}); 
;;      > SP:=rhs(subs(op(R), PolyEq));
;;      > CP:=subs(s=70/99,p=22/7,SP);
;;      > plot({sin(x), CP}, x=(0)..(Pi/2));
;;      > plot(sin(x)-CP, x=(0)..(Pi/2));
;;      > max(seq(abs(sin(x)-CP), x=0..evalf(Pi/2), .0001));
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun makePoly (d xpts fpts dpts)
  "Create polynomials that are equal to fpts on xpts and have derivative equal to dpts on xpts.  nil values in fpts and dpts are ignored."
  (let* ((neq  (+ (count-if-not #'null fpts) (count-if-not #'null dpts)))
         (A    (make-array (list neq (1+ d))
                           :initial-contents (concatenate 'list  
                                                          (loop for x in xpts
                                                                for f in fpts
                                                                when f
                                                                collect (loop for i from 0 upto d
                                                                              collect (expt x i)))
                                                          (loop for x in xpts
                                                                for dv in dpts
                                                                when dv
                                                                collect (loop for i from 0 upto d
                                                                              collect (if (zerop i) 0 (* i (expt x (- i 1)))))))))
         (B    (make-array neq :initial-contents (delete-if #'null (concatenate 'list fpts dpts)))))
    (format 't "Degree: ~d~%" d)
    (let ((sol (mjr_mat_solve-sys-sge A B :show-progress nil)))
      (if sol
          (let ((poly (reverse sol)))
            (format 't "Poly: ~a~%" poly)
            poly)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pltErrPoly (func range poly)
  "Plot the diffrence between func and a polynomial over 0..2*PI*RANGE, and return the maximum absolute diffrence."
  (let ((dq (mjr_fsamp_dq-func-r123-r123 (lambda (x) (- (funcall func x) (mjr_poly_eval poly x)))  :xdat (list :start 0.d0 :end (* range 2 pi) :len 10000))))
    (mjr_gnupl_dquad dq)
    (format 't "Range: ~a~%" range)
    (format 't "Max Err: ~f~%" (abs (reduce #'mjr_numu_abs-max (mjr_dquad_get-data-array dq "f"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "================================================================================================================================================~%")
(pltErrPoly #'sin 1/4 (let* ((p 22/7)
                             (s 70/99))
                        (makePoly 4
                                  (list 0   (/ p 4)   (/ p 2))
                                  (list 0   s         1)
                                  (list 1   nil       0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "================================================================================================================================================~%")
(pltErrPoly #'cos 1/4 (let* ((p 22/7)
                             (s 70/99))
                        (makePoly 5
                                  (list 0   (/ p 4)   (/ p 2))
                                  (list 1   (- s)     0)
                                  (list 0   nil       -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "================================================================================================================================================~%")
(pltErrPoly #'sin 1 (let* ((p 355/113))
                      (makePoly 9
                                (list 0   (/ p 2)   p    (* 3 (/ p 2))  (* 2 p))
                                (list 0   1         0    -1             0)
                                (list 1   0         -1   0              1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "================================================================================================================================================~%")
(pltErrPoly #'cos 1 (let* ((p 355/113))
                      (makePoly 8
                                (list 0   (/ p 2)   p    (* 3 (/ p 2))  (* 2 p))
                                (list 1   0        -1    0              1)
                                (list 0   -1        0    1              0))))
