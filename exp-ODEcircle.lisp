;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODEcircle.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1999,2008,2012,2015 by Mitch Richling.  All rights reserved.
;; @brief     Example of an ODE plot.@EOL
;; @Std       Common Lisp
;;
;; Two slope plots with the x and y reversed.  The results are frequently surprising for newcomers to DEQ.
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time 
 (flet ((xy2x (x y)  (* (+ (* x x) (* y y) (- 4)) (* x y)))
        (xy2y (x y)  (* (+ (* x x) (* y y) (- 4)) (- (* x x) (* y y))))
        (v2slope (v) (if (zerop (aref v 0)) 0 (/ (aref v 1) (aref v 0)))))

   (print "Create dquad object")
   (let ((daData (mjr_dquad_make-from-axis  "x" '(:start -5 :end 5 :len 501)
                                            "y" '(:start -5 :end 5 :len 501))))
     (print "Compute cirXYv")
     (mjr_dquad_add-data-from-map daData
                                  (lambda (x y) (vector (xy2x x y) (xy2y x y)))
                                  :axes 't
                                  :ano-nam "cirXYv"
                                  :ano-typ :ano-typ-rvec)

     (print "Compute cirYXv")
     (mjr_dquad_add-data-from-map daData
                                  (lambda (x y) (vector (xy2y x y) (xy2x x y)))
                                  :axes 't
                                  :ano-nam "cirYXv"
                                  :ano-typ :ano-typ-rvec)

     (print "Compute cirXYs")
     (mjr_dquad_add-data-from-map daData
                                  #'v2slope
                                  :data "cirXYv"
                                  :ano-nam "cirXYs")

     (print "Compute cirYXs")
     (mjr_dquad_add-data-from-map daData
                                  #'v2slope
                                  :data "cirYXv"
                                  :ano-nam "cirYXs")

     (print "Save VTK")
     (mjr_vtk_from-dquad "exp-ODEcircle-OUT.vtk" daData))))

