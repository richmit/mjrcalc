;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ClassicOptFreudensteinRoth.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Analysis of the classic Freudenstein-Roth optimization test function.@EOL
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
(defun freudenstein-roth (X Y)
  "Freudenstein and Roth Function.  Returns values for function, gradient, and hessian.

   $$\\begin{array}{rcl}
   f(x,y)                                        &=&  (-13 + x + ((5 - y)y - 2)y)^2 + (-29 + x + ((y + 1)y - 14) y)^2 \\\\
                                                 &=&  2y^6-8y^5+2y^4-80y^3+12xy^2+12y^2-32xy+864y+2x^2-84x+1010       \\\\
   \\frac{\\partial f}{\\partial x}              &=& 12y^2-32y+4x-84                                                  \\\\
   \\frac{\\partial f}{\\partial y}              &=& 12y^5-40y^4+8y^3-240y^2+24xy+24y-32x+864                         \\\\
   \\frac{\\partial^2 f}{\\partial x^2}          &=& 4                                                                \\\\
   \\frac{\\partial^2 f}{\\partial x\\partial y} &=& 24y-32                                                           \\\\
   \\frac{\\partial^2 f}{\\partial y\\partial x} &=& 24y-32                                                           \\\\
   \\frac{\\partial^2 f}{\\partial y^2}          &=& 60y^4-160y^3+24y^2-480y+24x+24                                   \\\\
   \\end{array}$$

Global Minimum: $f(5,4)=0$

Local Minimum:  $f(11.41, -0.8986)=48.9842$"
  (values (+ (EXPT (+ (- 13) X (* (- (* (- 5 Y) Y) 2) Y)) 2)
             (EXPT (+ (- 29) X (* (- (* (+ Y 1) Y) 14) Y)) 2))
          (make-array 2 :initial-contents (list 
                                           (+ (- (* 12 (EXPT Y 2)) (* 32 Y)) (* 4 X) (- 84))
                                           (+ (- (* 12 (EXPT Y 5)) (* 40 (EXPT Y 4)))
                                              (* 8 (EXPT Y 3)) (- (* 240 (EXPT Y 2)))
                                              (* 24 X Y) (* 24 Y) (- (* 32 X)) 864)))
          (make-array '(2 2) :initial-contents (list (list 4
                                                           (- (* 24 y) 32))
                                                     (list (- (* 24 y) 32)
                                                           (+ (- (* 60 (EXPT Y 4)) (* 160 (EXPT Y 3)))
                                                              (* 24 (EXPT Y 2)) (- (* 480 Y)) (* 24 X) 24))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((fr-data (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" '(:start -4 :end 23 :len 500)
                                                                      "y" '(:start -2 :end 5  :len 500))
                                            #'freudenstein-roth
                                            :axes 't
                                            :ano-nam "Freudenstein-Roth"
                                            :ano-typ :ano-typ-real)))
  (format 't "Nice pic of the global minimum~%")
  (mjr_gnupl_dquad fr-data :zlim '(-0 200) :type :f :xlim '(-5 15) :ylim '(3.65 4.27))
  (format 't "Press [ENTER] to continue.~%")
  (read-char)
  (format 't "Nice pic showing both extrema~%")
  (mjr_gnupl_dquad fr-data :zlim '(-0 200) :type :f :xlim '(-4 23) :ylim '(-2 5))
  (format 't "Press [ENTER] to continue.~%")
  (read-char)
  (format 't "Dump VTK~%")
  (mjr_vtk_from-dquad "exp-ClassicOptFreudensteinRoth-OUT.vtk" fr-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format 't "DONE~%")
