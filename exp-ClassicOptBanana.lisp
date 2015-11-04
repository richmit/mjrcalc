;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ClassicOptBanana.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Analysis of the classic Rosenbrock banana optimization test function.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012, 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defun rosenbrock-banana (X Y)
  "Rosenbrock's Banana Function.  Returns values for function, gradient, and hessian.

   $$\\begin{array}{rcl}
   f(x,y)                                        &=& 100*(y - x^2)^2 + (1 - x)^2                                                            \\\\
   \\frac{\\partial f}{\\partial x}              &=& -400x(y-x^2)-2(1-x)                                                                    \\\\
   \\frac{\\partial f}{\\partial y}              &=& 200(y-x^2)                                                                             \\\\
   \\frac{\\partial^2 f}{\\partial x^2}          &=& -400(200y^3-1800x^2y^2-y^2+3000x^4y+12x^2y-12xy+2y-1400x^6-15x^4+20x^3)+2412x^2-24x+12 \\\\
   \\frac{\\partial^2 f}{\\partial x\\partial y} &=& -800(300xy^2-600x^3y-xy+y+300x^5+2x^3-3x^2-x)                                          \\\\
   \\frac{\\partial^2 f}{\\partial y\\partial x} &=& -800(300xy^2-600x^3y-xy+y+300x^5+2x^3-3x^2-x)                                          \\\\
   \\frac{\\partial^2 f}{\\partial y^2}          &=& 400(300y^2-600x^2y+300x^4+x^2-2x+1)                                                    \\\\
   \\end{array}$$

Global Minimum: $f(1,1)=0$"
  (values (+ (* 100 (EXPT (- Y (EXPT X 2)) 2)) (EXPT (- 1 X) 2))
          (make-array 2 :initial-contents (list 
                                           (+ (- (* 400 X (- Y (EXPT X 2)))) (- (* 2 (- 1 X))))
                                           (* 200 (- Y (EXPT X 2)))))
          (make-array '(2 2) :initial-contents (list (list (+ (- (* 80000 (EXPT Y 3))) (* 720000 (EXPT X 2) (EXPT Y 2)) (* 400 (EXPT Y 2))
                                                              (- (* 1200000 (EXPT X 4) Y)) (- (* 4800 (EXPT X 2) Y)) (* 4800 X Y)
                                                              (- (* 800 Y)) (* 560000 (EXPT X 6)) (* 6000 (EXPT X 4))
                                                              (- (* 8000 (EXPT X 3))) (* 2412 (EXPT X 2)) (- (* 24 X)) 12)
                                                           (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X))))
                                                     (list (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X)))
                                                           (+ (- (* 120000 (EXPT Y 2)) (* 240000 (EXPT X 2) Y)) (* 120000 (EXPT X 4))
                                                              (* 400 (EXPT X 2)) (- (* 800 X)) 400))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((banana-data (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" '(:start -1.5 :end 2 :len 500)
                                                                          "y" '(:start -0.75 :end 3 :len 500))
                                                #'ROSENBROCK-BANANA
                                                :axes 't
                                                :ano-nam "Rosenbrock-Banana"
                                                :ano-typ :ano-typ-real)))
  (format 't "Nice pic of the global minimum~%")
  (mjr_gnupl_dquad banana-data :xlim '(-1.5 2) :ylim '(-0.75 3) :type :f)
  (format 't "Dump VTK~%")
  (mjr_vtk_from-dquad "exp-ClassicOptBanana-OUT.vtk" banana-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "DONE~%")
