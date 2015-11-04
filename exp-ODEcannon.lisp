;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODEcannon.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Cannon shot simulation.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      @EOL@EOL
;; @warning   @EOL@EOL
;; @bug       @EOL@EOL
;; @filedetails
;;
;;  Cannon shot from $(0,100 \mathrm{m})$ at angle of $45$ degrees from the horizontal with initial velocity $(100 \mathrm{m/s},100 \mathrm{m/s})$
;;
;;    $$\frac{\mathrm{d}^2\overline{\mathbf{p}}}{\mathrm{d}t^2}=
;;    \left[\begin{array}{c}
;;      0 \\ -g \\
;;    \end{array}\right]
;;    \Rightarrow
;;    \begin{array}{l}
;;    \frac{\mathrm{d}^2p_x}{\mathrm{d}t^2}=0 \\ \\ \\
;;    \frac{\mathrm{d}^2p_y}{\mathrm{d}t^2}=-g \\
;;    \end{array}
;;    \Rightarrow
;;     \begin{array}{l}
;;    \frac{\mathrm{d}p_x}{\mathrm{d}t}=v_x \\
;;    \frac{\mathrm{d}v_x}{\mathrm{d}t}=0   \\ \\ 
;;    \frac{\mathrm{d}p_y}{\mathrm{d}t}=v_y \\ 
;;    \frac{\mathrm{d}v_y}{\mathrm{d}t}=-g  \\ 
;;    \end{array}$$
;;    $$\overline{\mathbf{v}}=
;;    \left[\begin{array}{r}
;;      100\mathrm{\frac{m}{s}}  \\
;;      100\mathrm{\frac{m}{s}} \\
;;    \end{array}\right],
;;    \overline{\mathbf{p}}=
;;    \left[\begin{array}{r}
;;        0\mathrm{m}   \\
;;      100\mathrm{m} \\
;;    \end{array}\right]$$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let ((sol (mjr_ode:mjr_ode_slv-ivp-erk-mesh (list (lambda (tim y)
                                                          (declare (ignore tim))
                                                          (let ((g 9.80665))
                                                            (vector (aref y 2) ;; px
                                                                    (aref y 3) ;; py
                                                                    0          ;; vx
                                                                    (- g)))))  ;; vy
                                                  (list #(0 100 100 100))  
                                                  '(:start 0.0 :end 23 :len 25)
                                                  :algorithm #'mjr_ode_erk-step-dormand-prince-5-4
                                                  :y-err-abs-max 1d-3
                                                  :suppress-warnings 't
                                                  :show-progress nil)))
        ;; Add a vector component for the position of the cannon ball
        (mjr_dquad_add-data-from-map sol #'vector :data '("y_0" "y_1") :ano-nam "path"     :ano-typ :ano-typ-rvec)
        ;; Add a vector component for the velocity of the cannon ball
        (mjr_dquad_add-data-from-map sol #'vector :data '("y_2" "y_3") :ano-nam "velocity" :ano-typ :ano-typ-rvec)
        ;; Draw the cannon ball's path with GNUplot
        (mjr_gnupl_dquad sol :data "path")
        ;; Dump out a VTK file with gridded data -- not very usefull in some tools.
        (mjr_vtk_from-dquad "exp-ODEcannon-OUT-grd.vtk" sol)
        ;; Dump out a VTK file with geometry for the curve, and associated data values (speed, velocity, etc...)
        (mjr_vtk_from-dsimp  "exp-ODEcannon-OUT-crv.vtk"
                             (mjr_dsimp_make-from-dquad sol 0 "path" :domain-data-names "time" :data "velocity")
                             :simplices 1)))
