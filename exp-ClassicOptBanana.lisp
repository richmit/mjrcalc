;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ClassicOptBanana.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Analysis of the classic Rosenbrock banana optimization test function.@EOL
;; @Keywords  mjrcalc example optimization root finding test rosenbrock banana
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defun rosenbrock-banana (X Y)
  "Rosenbrock's Banana Function.  Returns values for function, gradient, and hessian.

   $$\\begin{array}{rcl}
   f(x,y)                                        &=& 100*(y - x^2)^2 + (1 - x)^2                                                            \\\\
   \\frac{\\partial f}{\\partial x}              &=& -400x(y-x^2)-2(1-x)                                                                    \\\\
   \\frac{\\partial f}{\\partial y}              &=& 200(y-x^2)                                                                             \\\\
   \\frac{\\partial^2 f}{\\partial x^2}          &=& -400(200y^3-1800x^2y^2-y^2+3000x^4y+12x^2y-12xy+2y-1400x^6-15x^4+20x^3)+2412x^2-24x+12 \\\\
   \\frac{\\partial^2 f}{\\partial x\\partial y} &=& -800(300xy^2-600x^3y-xy+y+300x^5+2x^3-3x^2-x)                                          \\\\
   \\frac{\\partial^2 f}{\\partial y\\partial x} &=& -800(300xy^2-600x^3y-xy+y+300x^5+2x^3-3x^2-x)                                          \\\\
   \\frac{\\partial^2 f}{\\partial y^2}          &=& 400(300y^2-600x^2y+300x^4+x^2-2x+1)                                                    \\\\
   \\end{array}$$

Global Minimum: $f(1,1)=0$"
  (values (+ (* 100 (EXPT (- Y (EXPT X 2)) 2)) (EXPT (- 1 X) 2))
          (make-array 2 :initial-contents (list 
                                           (+ (- (* 400 X (- Y (EXPT X 2)))) (- (* 2 (- 1 X))))
                                           (* 200 (- Y (EXPT X 2)))))
          (make-array '(2 2) :initial-contents (list (list (+ (- (* 80000 (EXPT Y 3))) (* 720000 (EXPT X 2) (EXPT Y 2)) (* 400 (EXPT Y 2))
                                                              (- (* 1200000 (EXPT X 4) Y)) (- (* 4800 (EXPT X 2) Y)) (* 4800 X Y)
                                                              (- (* 800 Y)) (* 560000 (EXPT X 6)) (* 6000 (EXPT X 4))
                                                              (- (* 8000 (EXPT X 3))) (* 2412 (EXPT X 2)) (- (* 24 X)) 12)
                                                           (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X))))
                                                     (list (+ (- (* 240000 X (EXPT Y 2))) (* 480000 (EXPT X 3) Y) (* 800 X Y)
                                                              (- (* 800 Y)) (- (* 240000 (EXPT X 5))) (- (* 1600 (EXPT X 3)))
                                                              (* 2400 (EXPT X 2)) (- (* 800 X)))
                                                           (+ (- (* 120000 (EXPT Y 2)) (* 240000 (EXPT X 2) Y)) (* 120000 (EXPT X 4))
                                                              (* 400 (EXPT X 2)) (- (* 800 X)) 400))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Nice pic of the global minimum~%")
(mjr_plot_func-r2-r1 #'ROSENBROCK-BANANA :xlim '(-1.5 2) :ylim '(-0.75 3) :type :f :zlim '(-1 1000))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Dump VTK~%")
(mjr_vtk_grid-from-func "exp-ClassicOptBanana-OUT.vtk" :s-func #'ROSENBROCK-BANANA :xdat '(:start -1.5 :end 2 :len 500) :ydat '(:start -0.75 :end 3 :len 500))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "DONE~%")
