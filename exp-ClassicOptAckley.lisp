;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ClassicOptAckley.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Analysis of the classic Rosenbrock banana optimization test function.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defun ackley (x &key (a 20.0d0) (b 0.2d0) (c (* 2.0d0 pi)))
  "Ackley's Function.  Returns values for function.  Input is a VECTOR!

$$f(\\overline{x})=a+\\exp(1)-a\\exp\\left(-b\\sqrt{\\frac{1}{d}\\sum_{i=1}^dx_i^2}\\right)-\\exp\\left(\\frac{1}{d}\\sum_{i=1}^d\\cos(cx_i)\\right)$$

Recommended parameter values are: $a=20$, $b= \\frac{2}{10}$, and $c=2\\pi$.  Note that $d$ is the length of $\\overline{x}$.

For the case $d=2$, we hvae:

$$f_2(x,y)=a+\\exp(1)-a\\exp\\left(-b\\sqrt{\\frac{x^2+y^2}{d}}\\right)-\\exp\\left(\\frac{\\cos(cx)+\\cos(cy)}{d}\\right)$$

Typical domain: $\\left[-\\frac{4096}{125}, \\frac{4096}{125}\\right]^d$

Global Minimum: $f(\\overline{0})=0$"
  (let ((d (length x)))
    (- (+ a (exp 1))
       (* a (exp (* (- b) (sqrt (/ (reduce #'+ (map 'list (lambda (xi) (* xi xi)) x)) d)))))
       (exp (/ (reduce #'+ (map 'list (lambda (xi) (cos (* c xi))) x)) d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (mjr_vtk_from-dquad "exp-ClassicOptAckley-OUT-2D.vtk" (mjr_fsamp_dq-func-r123-r123 #'ackley
                                                                                         :xdat '(:start -4.0d0 :end 4 :len 300)
                                                                                         :ydat '(:start -4.0d0 :end 4 :len 300)
                                                                                         :arg-mode :arg-vector)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (mjr_vtk_from-dquad "exp-ClassicOptAckley-OUT-3D.vtk" (mjr_fsamp_dq-func-r123-r123 #'ackley
                                                                                         :xdat '(:start -4.0d0 :end 4 :len 50)
                                                                                         :ydat '(:start -4.0d0 :end 4 :len 50)
                                                                                         :zdat '(:start -4.0d0 :end 4 :len 50)
                                                                                         :arg-mode :arg-vector)))
