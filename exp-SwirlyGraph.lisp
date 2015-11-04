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
    (time (mjr_vtk_from-dquad "exp-SwirlyGraph-OUT.vtk"
                              (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis  "x" '(:start -16 :end 16 :len 500)
                                                                                      "y" '(:start -16 :end 16 :len 500))
                                                           (lambda (x y)
                                                             (let* ((n 8)
                                                                    (r  (sqrt (+ (* x x) (* y y))))
                                                                    (th (atan y x)))
                                                               (sin (+ (* n (cos r)) (* 5 th)))))
                                                           :axes 't
                                                           :ano-nam "cirXY"
                                                           :ano-typ :ano-typ-real))))

; MJR SCM NOTE <2015-02-20 23:16:13 CST> exp-SwirlyGraph.lisp: Now uses mjr_vtk_from-dquad and various mjr_dquad functions.

