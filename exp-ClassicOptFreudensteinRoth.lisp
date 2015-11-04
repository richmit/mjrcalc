;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ClassicOptFreudensteinRoth.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Analysis of the classic Freudenstein-Roth optimization test function.@EOL
;; @Keywords  optimization root finding test freudenstein roth
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defun freudenstein-roth (X Y)
  "Freudenstein and Roth Function.  Returns values for function, gradient, and hessian.

   $$\\begin{array}{rcl}
   f(x,y)                                        &=&  (-13 + x + ((5 - y)y - 2)y)^2 + (-29 + x + ((y + 1)y - 14) y)^2 \\\\
                                                 &=&  2y^6-8y^5+2y^4-80y^3+12xy^2+12y^2-32xy+864y+2x^2-84x+1010       \\\\
   \\frac{\\partial f}{\\partial x}              &=& 12y^2-32y+4x-84                                                  \\\\
   \\frac{\\partial f}{\\partial y}              &=& 12y^5-40y^4+8y^3-240y^2+24xy+24y-32x+864                         \\\\
   \\frac{\\partial^2 f}{\\partial x^2}          &=& 4                                                                \\\\
   \\frac{\\partial^2 f}{\\partial x\\partial y} &=& 24y-32                                                           \\\\
   \\frac{\\partial^2 f}{\\partial y\\partial x} &=& 24y-32                                                           \\\\
   \\frac{\\partial^2 f}{\\partial y^2}          &=& 60y^4-160y^3+24y^2-480y+24x+24                                   \\\\
   \\end{array}$$

Global Minimum: $f(5,4)=0$

Local Minimum:  $f(11.41, -0.8986)=48.9842$"
  (values (+ (EXPT (+ (- 13) X (* (- (* (- 5 Y) Y) 2) Y)) 2)
             (EXPT (+ (- 29) X (* (- (* (+ Y 1) Y) 14) Y)) 2))
          (make-array 2 :initial-contents (list 
                                           (+ (- (* 12 (EXPT Y 2)) (* 32 Y)) (* 4 X) (- 84))
                                           (+ (- (* 12 (EXPT Y 5)) (* 40 (EXPT Y 4)))
                                              (* 8 (EXPT Y 3)) (- (* 240 (EXPT Y 2)))
                                              (* 24 X Y) (* 24 Y) (- (* 32 X)) 864)))
          (make-array '(2 2) :initial-contents (list (list 4
                                                           (- (* 24 y) 32))
                                                     (list (- (* 24 y) 32)
                                                           (+ (- (* 60 (EXPT Y 4)) (* 160 (EXPT Y 3)))
                                                              (* 24 (EXPT Y 2)) (- (* 480 Y)) (* 24 X) 24))))))




;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Nice pic of the global minimum~%")
(mjr_plot_func-r2-r1 (lambda (x y) (min 100 (freudenstein-roth x y))) :zlim '(-0 200) :type :f :xdat '(:start -5 :end 15 :len 75) :ydat '(:start 3.65 :end 4.27 :len 75))
(format 't "Press [ENTER] to continue.~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Nice pic showing both extrema~%")
(mjr_plot_func-r2-r1 (lambda (x y) (min 100 (freudenstein-roth x y))) :zlim '(-0 200) :type :f :xdat '(:start -4 :end 23 :len 75) :ydat '(:start -2 :end 5 :len 75))
(format 't "Press [ENTER] to continue.~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Dump VTK~%")
(mjr_vtk_from-dquad "exp-ClassicOptFreudensteinRoth-OUT.vtk"
                    (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" '(:start -4 :end 23 :len 500)
                                                                           "y" '(:start -2 :end 5  :len 500))
                                                 #'freudenstein-roth
                                                 :axes 't
                                                 :ano-nam "Freudenstein-Roth"
                                                 :ano-typ :ano-typ-real))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "DONE~%")
