;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-VTKcolorSpace.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Draw the RGB Cube and HSL sphere.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2010,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  
