;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-VTKcolorSpace.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw the RGB Cube and HSL sphere.@EOL
;; @Keywords  rgb cube hsl sphere
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
;; RGB Cube
(mjr_vtk_from-dquad "exp-VTKcolorSpace-OUT-RGBcube2.vtk" 
                    (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "r" '(:start 0 :end 1 :len 2)
                                                                           "g" '(:start 0 :end 1 :len 2)
                                                                           "b" '(:start 0 :end 1 :len 2))  
                                                 #'mjr_colorizer_i3-rgb-cube
                                                 :axes 't
                                                 :gd-nam "color" :gd-typ :gd-typ-color :gd-colorspace :cs-rgb))

;;----------------------------------------------------------------------------------------------------------------------------------
;; HSL sphere
(let ((d 0.01))
  (mjr_vtk_polydata-from-func-r12-r123 "exp-VTKcolorSpace-OUT-HSLsphere.vtk"
                             (lambda (u v) (vector (* (sin v) (cos u))
                                                   (* (sin v) (sin u))
                                                   (cos v)))
                             :c-func (mjr_util_func-domain-rect-to-unit #'mjr_colorizer_i2-hsl-cone (vector 0 d) (vector (* pi 2) (- pi d)) :arg-number)
                             :udat (list :start 0 :end  (* pi 2) :len 50)
                             :vdat (list :start d :end (- pi d) :len 50)))
