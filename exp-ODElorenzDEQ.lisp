;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODElorenzDEQ.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute (with :MJR_ODE) and draw the Lorenz strange attracter.@EOL
;; @Std       Common Lisp
;;
;;            Lorenz
;;               $$\begin{array}{l}
;;               \frac{\mathrm{d}x}{\mathrm{d}t} = a(y-x)   \\
;;               \frac{\mathrm{d}y}{\mathrm{d}t} = bx-y-xz  \\
;;               \frac{\mathrm{d}z}{\mathrm{d}t} = xy-cz    \\
;;               \end{array}$$
;;               $$a=10, b=28, c=8/3$$
;;               $$x(0)=1/10, y(0)=0, z(0)=0$$
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(time (let* ((ar (mjr_ode_slv-ivp-erk-mesh (list (lambda (tim p)
                                                   (declare (ignore tim))
                                                   (let ((a  10)
                                                         (b  28)
                                                         (c  8/3)
                                                         (x (aref p 0))
                                                         (y (aref p 1))
                                                         (z (aref p 2)))
                                                     (vector (* a (- y x));; dx/dt
                                                             (- (* b x) y (* x z));; dy/dt
                                                             (- (* x y) (* c z))))));; dz/dt
                                           (list #(1 1 1))
                                           '(:start 0 :step 0.009 :len 10000)
                                           :algorithm #'mjr_ode_erk-step-euler-1
                                           )))
        (mjr_vtk_from-dsimp "exp-ODElorenzDEQ-OUT.vtk"
                            (mjr_dsimp_make-from-points ar :connect-points 't
                                                        :point-columns '(1 2 3) :data-columns 0 :data-column-names "time")
                            :simplices 1)))


