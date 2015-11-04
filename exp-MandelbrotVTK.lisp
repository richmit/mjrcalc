;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotVTK.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012,2015 by Mitch Richling.  All rights reserved.
;; @brief     Draw the Mandelbrot set, and save it as a VTK file.@EOL
;; @Std       Common Lisp
;;
;;            
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(time
 (mjr_vtk_from-dquad "exp-MandelbrotVTK-OUT.vtk"
                     (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" '(:start -2.0 :end 1.0 :len 100)
                                                                            "y" '(:start -1.5 :end 1.5 :len 100))
                                                  (lambda (x y)
                                                    (loop for z = #C(0.0 0.0) then (+ (* z z) (complex x y))
                                                          for cnt from 0
                                                          do (if (or (> (abs z) 2) (> cnt 255)) (return cnt))))
                                                  :axes 't
                                                  :ano-nam "EscapeCount"
                                                  :ano-typ :ano-typ-real)))
