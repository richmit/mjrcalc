;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODEpopulationCompete.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Plot the direction field for an ODE of two competing populations.@EOL
;; @Std       Common Lisp
;;
;;            Stable point at (.5,.5)
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_vtk_from-dquad "exp-ODEpopulationCompete-OUT.vtk"
                    (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" '(:start 0 :end 1 :len 60)
                                                                           "y" '(:start 0 :end 1 :len 60))
                                                 (lambda (x y) (vector (* y (- 3 (* 5 x) y))
                                                                       (* x (- 1 x y))))
                                                 :axes 't
                                                 :ano-nam "Population"
                                                 :ano-typ :ano-typ-rvec))
