;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODErossler.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Compute the Rossler strange attracter.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;  The equations:
;;
;;    $$\begin{array}{l}
;;    \frac{\mathrm{d}x}{\mathrm{d}t} = -y-z    \\
;;    \frac{\mathrm{d}y}{\mathrm{d}t} = x+ay    \\
;;    \frac{\mathrm{d}z}{\mathrm{d}t} = b+z(x-c)\\
;;    \end{array}$$
;;    $$a=1/5, b=1/5, c=57/10$$
;;    $$x(0)=1/10, y(0)=0, y(0)=0$$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let* ((ar (mjr_ode_slv-ivp-erk-mesh (list (lambda (tim p)
                                                   (declare (ignore tim))
                                                   (let ((a   0.2d0)
                                                         (b   0.2d0)
                                                         (c   5.7d0)
                                                         (x (aref p 0))
                                                         (y (aref p 1))
                                                         (z (aref p 2)))
                                                     (vector (- (+ y z))             ;; x
                                                             (+ x (* a y))           ;; y
                                                             (+ b (* z (- x c))))))) ;; z
                                           (list #(1/10 0 0))
                                           '(:start 0 :end 250 :len 10000)
                                           :algorithm #'mjr_ode_erk-step-heun-euler-1-2
                                           :y-delta-abs-max 1)))
        (mjr_vtk_from-dsimp  "exp-ODErossler-OUT.vtk"
                             (mjr_dsimp_make-from-points ar :connect-points 't :point-columns '(1 2 3) :data-columns 0 :data-column-names "time")
                             :simplices 1)))
