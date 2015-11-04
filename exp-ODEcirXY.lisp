;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODEcirXY.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1999,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     Example of an ODE plot.@EOL
;; @Keywords  example of an ode plot field direction 
;; @Std       Common Lisp
;;
;;            See: exp-ODEcirYX.lisp
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(time (mjr_vtk_grid-from-func "exp-ODEcirXY.vtk"
                              :s-func (lambda (x y) (vector (* (+ (* x x) (* y y) (- 4)) (* x y))
                                                            (* (+ (* x x) (* y y) (- 4)) (- (* x x) (* y y)))))
                              :xdat '(:start -5 :end 5 :len 501)
                              :ydat '(:start -5 :end 5 :len 501)))


