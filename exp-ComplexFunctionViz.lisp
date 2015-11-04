;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      /home/richmit/world/my_prog/lispStuff/lispy/exp-ComplexFunctionViz.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2010,2011 by Mitch Richling.  All rights reserved.
;; @brief     Complex function visualization via VTK files.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_make-from-func-r2-r1 (f &key arg-mode xdat ydat auto-scale cm-vars color-method color-space max-color show-progress)

(defun mjr_img_make-from-func-c1-c1 (f &key  rdat idat c-func)


(defun mjr_vtk_grid-from-func-c1-c1 (out-file f &key rdat idat c-func)


(mjr_vtk_grid-from-func-c1-c1 "exp-ComplexFunctionViz-OUT.vtk"
                               ;;(lambda (z) (* (+ z 2) (+ z 2) (- z #C(1 2)) (+ z #C(0 1))))
                               ;;(lambda (z) (+ z (/ z)))
                               ;;(lambda (z) (/ (* (- z 2) (- z 2) (+ z #C(1 -2)) (+ z #C(2 2))) (expt z 3)))
                               (lambda (z) (- (expt z 3) 1))
                               ;;(lambda (z) (expt (- (expt z 3) 1) 2))
                               ;;(lambda (z) (+ (/ (+ z 1)) (/ (- z 1))))
                               ;;(lambda (z) (- (/ (+ z 1)) (/ (- z 1))))
                               ;;(lambda (z) (/ (expt z 2)))
                               ;;(lambda (z) (mjr_poly_eval #(1/10 0 -1 1) z))
                               ;;(lambda (z) (expt z z))
                               ;;(lambda (z) (exp (- (z z))))
                               ;;(lambda (z) (+ z (/ (* z z) (sin (- (* z z z z) 1)))))
                               ;;(lambda (z) (* #C(1 1) (log (/ (sin (- (* z z z) 1)) z))))
                               ;;(lambda (z) (log (sin z)))
                               ;;(lambda (z) z)
                               :c-func
                               ;;nil
                               ;;#'mjr_colorizer_r2-1rgb1
                               ;;#'mjr_colorizer_r2-1r.gb.yc.m0
                               ;;#'mjr_colorizer_r2-1yr0
                               ;;#'mjr_colorizer_r2-gr
                               ;;#'mjr_colorizer_r2-hsl-thaller
                               ;;#'mjr_colorizer_r2-hsv-thaller
                               ;;#'mjr_colorizer_r2-hsv-full-v
                               #'mjr_colorizer_r2-hsl-richardson
                               ;;#'mjr_colorizer_r2-checker
                               :rdat (list :start -1.250 :end 1.250 :len 200)
                               :idat (list :start -1.250 :end 1.250 :len 200))
