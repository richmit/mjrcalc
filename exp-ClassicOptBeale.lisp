;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ClassicOptBeale.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Analysis of the classic Beale optimization test function.@EOL
;; @Keywords  optimization root finding test beale
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defun beale (X Y)
  "Beale's Function.  Returns values for function, gradient, and hessian.

   $$\\begin{array}{rcl}
   f(x,y)                                        &=& (\\frac{3}{5} - x(1-y))^2 + (\\frac{9}{4} - x(1-y^2))^2 + (\\frac{21}{8} - x(1-y^3))^2        \\\\
   \\frac{\\partial f}{\\partial x}              &=& 2xy^6+2xy^4-4xy^3+\\frac{21}{4}y^3-2xy^2+\\frac{9}{2}y^2-4xy+\\frac{6}{5}y+6x-\\frac{219}{20} \\\\
   \\frac{\\partial f}{\\partial y}              &=& 6x^2y^5+4x^2y^3-6x^2y^2+\\frac{63}{4}xy^2-2x^2y+9xy-2x^2+\\frac{6}{5}x                        \\\\
   \\frac{\\partial^2 f}{\\partial x^2}          &=& 2y^6+2y^4-4y^3-2y^2-4y+6                                                                      \\\\
   \\frac{\\partial^2 f}{\\partial x\\partial y} &=& 12xy^5+8xy^3-12xy^2+\\frac{63}{4}y^2-4xy+9y-4x+\\frac{6}{5}                                   \\\\
   \\frac{\\partial^2 f}{\\partial y\\partial x} &=& 12xy^5+8xy^3-12xy^2+\\frac{63}{4}y^2-4xy+9y-4x+\\frac{6}{5}                                   \\\\
   \\frac{\\partial^2 f}{\\partial y^2}          &=& 30x^2y^4+12x^2y^2-12x^2y+\\frac{63}{2}xy-2x^2+9x                                              \\\\
   \\end{array}$$

Starting Point: $(1, 1)$

Global Minimum: $f\\left(3,\\frac{1}{2}\\right)=0$"
  (values (+ (EXPT (- 3/2  (* X (- 1 Y)))          2)
             (EXPT (- 9/4  (* X (- 1 (EXPT Y 2)))) 2)
             (EXPT (- 21/8 (* X (- 1 (EXPT Y 3)))) 2))
          (make-array 2 :initial-contents (list 
                                           (+ (* 2 X (EXPT Y 6)) (* 2 X (EXPT Y 4)) (- (* 4 X (EXPT Y 3)))
                                              (/ (* 21 (EXPT Y 3)) 4) (- (* 2 X (EXPT Y 2))) (/ (* 9 (EXPT Y 2)) 2)
                                              (- (* 4 X Y)) (/ (* 6 Y) 5) (* 6 X) (- (/ 219 20)))
                                           (+ (* 6 (EXPT X 2) (EXPT Y 5)) (* 4 (EXPT X 2) (EXPT Y 3))
                                              (- (* 6 (EXPT X 2) (EXPT Y 2))) (/ (* 63 X (EXPT Y 2)) 4)
                                              (- (* 2 (EXPT X 2) Y)) (* 9 X Y) (- (* 2 (EXPT X 2))) (/ (* 6 X) 5))))
          (make-array '(2 2) :initial-contents (list (list 
                                                      (+ (* 2 (EXPT Y 6)) (* 2 (EXPT Y 4)) (- (* 4 (EXPT Y 3))) (- (* 2 (EXPT Y 2)))
                                                         (- (* 4 Y)) 6)
                                                      (+ (* 12 X (EXPT Y 5)) (* 8 X (EXPT Y 3)) (- (* 12 X (EXPT Y 2)))
                                                         (/ (* 63 (EXPT Y 2)) 4) (- (* 4 X Y)) (* 9 Y) (- (* 4 X)) (/ 6 5)))
                                                     (list 
                                                      (+ (* 12 X (EXPT Y 5)) (* 8 X (EXPT Y 3)) (- (* 12 X (EXPT Y 2)))
                                                         (/ (* 63 (EXPT Y 2)) 4) (- (* 4 X Y)) (* 9 Y) (- (* 4 X)) (/ 6 5))
                                                      (+ (* 30 (EXPT X 2) (EXPT Y 4)) (* 12 (EXPT X 2) (EXPT Y 2))
                                                         (- (* 12 (EXPT X 2) Y)) (/ (* 63 X Y) 2) (- (* 2 (EXPT X 2))) (* 9 X)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Nice overview plot~%")
(mjr_plot_func-r2-r1 #'beale :xlim '(0.5 3.5) :ylim '(0.5 2) :type :f :zlim '(-1 100))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Dump VTK~%")
(mjr_vtk_grid-from-func "exp-ClassicOptBeale-OUT.vtk" :s-func #'beale :xdat '(:start 0.5 :end 3.5 :len 500) :ydat '(:start 0.5 :end 2 :len 500))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "DONE~%")
