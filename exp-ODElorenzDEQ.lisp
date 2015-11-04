;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODElorenzDEQ.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Compute (with :MJR_ODE) and draw the Lorenz strange attracter.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2010,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      Add code to generate povray data.@EOL@EOL
;; @filedetails
;;
;;  Lorenz
;;     $$\begin{array}{l}
;;     \frac{\mathrm{d}x}{\mathrm{d}t} = a(y-x)   \\
;;     \frac{\mathrm{d}y}{\mathrm{d}t} = bx-y-xz  \\
;;     \frac{\mathrm{d}z}{\mathrm{d}t} = xy-cz    \\
;;     \end{array}$$
;;     $$a=10, b=28, c=\frac{8}{3}$$
;;     $$x(0)=\frac{1}{10}, y(0)=0, z(0)=0$$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let* ((sol (mjr_ode_slv-ivp-erk-mesh (list (lambda (tim p)
                                                    (declare (ignore tim))
                                                    (let ((a  10)
                                                          (b  28)
                                                          (c  8/3)
                                                          (x (aref p 0))
                                                          (y (aref p 1))
                                                          (z (aref p 2)))
                                                      (vector (* a (- y x))              ;; dx/dt
                                                              (- (* b x) y (* x z))      ;; dy/dt
                                                              (- (* x y) (* c z))))))    ;; dz/dt
                                            (list #(1 1 1))
                                            '(:start 0 :step 0.009 :len 10000)
                                            :algorithm #'mjr_ode_erk-step-euler-1)))
        ;; Add a single data element with the path points.
        (mjr_dquad_add-data-from-map sol #'vector :data '("y_0" "y_1" "y_2") :ano-nam "path" :ano-typ :ano-typ-rvec)
        (mjr_vtk_from-dsimp "exp-ODElorenzDEQ-OUT.vtk"
                            (mjr_dsimp_make-from-dquad sol 0 "path" :domain-data-names "time")
                            :simplices 1)))
