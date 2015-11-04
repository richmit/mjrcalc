;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODEcannon.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Cannon shot simulation.@EOL
;; @keywords  cannon shot simulation gravity
;; @Std       Common Lisp, Emacs Lisp
;;
;;            Cannon shot from $(0,100 \mathrm{m})$ at angle of $45$ degrees from the horizontal with initial velocity $(100
;;            \mathrm{m/s},100 \mathrm{m/s})$
;;
;;              $$\frac{\mathrm{d}^2\overline{\mathbf{p}}}{\mathrm{d}t^2}=
;;              \left[\begin{array}{c}
;;                0 \\ -g \\
;;              \end{array}\right]
;;              \Rightarrow
;;              \begin{array}{l}
;;              \frac{\mathrm{d}^2p_x}{\mathrm{d}t^2}=0 \\ \\ \\
;;              \frac{\mathrm{d}^2p_y}{\mathrm{d}t^2}=-g \\
;;              \end{array}
;;              \Rightarrow
;;               \begin{array}{l}
;;              \frac{\mathrm{d}p_x}{\mathrm{d}t}=v_x \\
;;              \frac{\mathrm{d}v_x}{\mathrm{d}t}=0   \\ \\ 
;;              \frac{\mathrm{d}p_y}{\mathrm{d}t}=v_y \\ 
;;              \frac{\mathrm{d}v_y}{\mathrm{d}t}=-g  \\ 
;;              \end{array}$$
;;              $$\overline{\mathbf{v}}=
;;              \left[\begin{array}{r}
;;                100\mathrm{\frac{m}{s}}  \\
;;                100\mathrm{\frac{m}{s}} \\
;;              \end{array}\right],
;;              \overline{\mathbf{p}}=
;;              \left[\begin{array}{r}
;;                  0\mathrm{m}   \\
;;                100\mathrm{m} \\
;;              \end{array}\right]$$
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(time (let ((ar (mjr_ode:mjr_ode_slv-ivp-erk-mesh (list (lambda (tim y)
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
        (mjr_plot_data :dat ar :datcols (list 1 2) :type :b)
        (mjr_vtk_from-dsimp  "exp-ODEcannon-OUT.vtk"
                             (mjr_dsimp_make-from-points ar :connect-points 't :point-columns '(1 2) :data-columns 0 :data-column-names "time")
                             :simplices 1)))
