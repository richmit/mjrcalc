;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-SwirlyGraph.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw a nice and swirly image.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((swirl-data (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis  "x" '(:start -16 :end 16 :len 500)
                                                                          "y" '(:start -16 :end 16 :len 500))
                                               (lambda (x y)
                                                 (let* ((n 8)
                                                        (r  (sqrt (+ (* x x) (* y y))))
                                                        (th (atan y x)))
                                                   (sin (+ (* n (cos r)) (* 5 th)))))
                                               :axes 't
                                               :ano-nam "swirlyField"
                                               :ano-typ :ano-typ-real)))

  (mjr_gnupl_dquad swirl-data :type :i)
  (time (mjr_vtk_from-dquad "exp-SwirlyGraph-OUT.vtk" swirl-data)))

