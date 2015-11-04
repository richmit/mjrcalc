;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-VTKcolorSpace.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw the RGB Cube and HSL sphere.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RGB Cube: Using basic functions
(mjr_vtk_from-dquad "exp-VTKcolorSpace-OUT-RGBcube1.vtk" 
                    (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "r" '(:start 0 :end 1 :len 2)
                                                                           "g" '(:start 0 :end 1 :len 2)
                                                                           "b" '(:start 0 :end 1 :len 2))  
                                                 #'mjr_colorizer_i3-rgb-cube
                                                 :axes 't
                                                 :ano-nam "color" :ano-typ :ano-typ-color :ano-colorspace :cs-rgb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RGB Cube: Using the colorize function 
(mjr_vtk_from-dquad "exp-VTKcolorSpace-OUT-RGBcube2.vtk" 
                    (mjr_dquad_colorize (mjr_dquad_make-from-axis "r" '(:start 0.0 :end 1 :len 2)
                                                                  "g" '(:start 0.0 :end 1 :len 2)
                                                                  "b" '(:start 0.0 :end 1 :len 2))
                                        :axes 't
                                        :color-method #'mjr_colorizer_i3-rgb-cube
                                        :ano-nam "color" :ano-colorspace :cs-rgb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HSL sphere
(let ((d 0.01))
  (mjr_vtk_from-dsimp "exp-VTKcolorSpace-OUT-HSLsphere.vtk"
                      (mjr_fsamp_ds-func-r123-r123 (lambda (u v) (vector (* (sin v) (cos u))
                                                                               (* (sin v) (sin u))
                                                                               (cos v)))
                                                         :xdat (list :start 0 :end (* pi 2) :len 50)
                                                         :ydat (list :start d :end (- pi d) :len 50)
                                                         :func-lab "HSLsphere"
                                                         :xlab "h" :ylab "s"
                                                         :ax-color-meth  #'mjr_colorizer_i2-hsl-cone
                                                         :ax-color-lab  "c")
                      :simplices 2))
  
