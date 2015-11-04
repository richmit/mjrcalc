;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODEpopulationCompete.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Plot the direction field for an ODE of two competing populations.@EOL
;; @Keywords  plot direction field ode two competing populations
;; @Std       Common Lisp
;;
;;            Stable point at (.5,.5)
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_vtk_grid-from-func "exp-ODEpopulationCompete.vtk"
                        :s-func (lambda (x y) (vector (* y (- 3 (* 5 x) y))
                                                      (* x (- 1 x y))))
                        :xdat '(:start 0 :end 1 :len 60)
                        :ydat '(:start 0 :end 1 :len 60))



