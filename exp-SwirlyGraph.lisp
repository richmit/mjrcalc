;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-SwirlyGraph.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw a nice and swirly image.@EOL
;; @Keywords  swirly image
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(if nil
    (time
     (mjr_plot_func-r2-r1
      (lambda (x y)
        (let* ((n 8)
               (r  (sqrt (+ (* x x) (* y y))))
               (th (atan y x)))
          (sin (+ (* n (cos r)) (* 5 th)))))
      :type :i
      :xdat '(:start -16.0 :end 16.0 :len 500)
      :ydat '(:start -16   :end 16   :len 500))))

;;----------------------------------------------------------------------------------------------------------------------------------
(if 't
    (time
     (mjr_vtk_grid-from-func "exp-SwirlyGraph-OUT.vtk"
                             :s-func (lambda (x y)
                                       (let* ((n 8)
                                              (r  (sqrt (+ (* x x) (* y y))))
                                              (th (atan y x)))
                                         (sin (+ (* n (cos r)) (* 5 th)))))
                             :xdat '(:start -16 :end 16 :len 500)
                             :ydat '(:start -16 :end 16 :len 500))))
