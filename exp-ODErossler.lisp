;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODErossler.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2012 by Mitch Richling.  All rights reserved.
;; @brief     Compute the Rossler strange attracter.@EOL
;; @Std       Common Lisp
;;
;;            The equations:
;;
;;              $$\begin{array}{l}
;;              \frac{\mathrm{d}x}{\mathrm{d}t} = -y-z    \\
;;              \frac{\mathrm{d}y}{\mathrm{d}t} = x+ay    \\
;;              \frac{\mathrm{d}z}{\mathrm{d}t} = b+z(x-c)\\
;;              \end{array}$$
;;              $$a=1/5, b=1/5, c=57/10$$
;;              $$x(0)=1/10, y(0)=0, y(0)=0$$
;;            

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
