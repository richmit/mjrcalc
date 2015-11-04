;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-dquad.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Example for :MJR_DQUAD.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            Experimental!!!
;;            




(defvar f-q-r1-r1    nil)
(defvar f-q-r1-r2    nil)
(defvar f-q-r1-r3    nil)
(defvar f-q-r2-r1    nil)
(defvar f-q-r2-r2    nil)
(defvar f-q-r2-r3    nil)
(defvar f-q-r3-r1    nil)
(defvar f-q-r3-r2    nil)
(defvar f-q-r3-r3    nil)
(defvar f-s-r1-r1    nil)
(defvar f-s-r1-r2-o  nil)
(defvar f-s-r1-r2-c  nil)
(defvar f-s-r1-r3-o  nil)
(defvar f-s-r1-r3-c  nil)
(defvar f-s-r2-r1    nil)
(defvar f-s-r2-r3    nil)
(defvar f-s-r2-r3-oo-000-tt nil)
(defvar f-s-r2-r3-oo-00d-tt nil)
(defvar f-s-r2-r3-oo-0y0-tt nil)
(defvar f-s-r2-r3-oo-x00-tt nil)
(defvar f-s-r2-r3-oo-xyd-tt nil)
(defvar f-s-r2-r3-oc-000-tt nil)
(defvar f-s-r2-r3-oc-00d-tt nil)
(defvar f-s-r2-r3-oc-0y0-tt nil)
(defvar f-s-r2-r3-oc-x00-tt nil)
(defvar f-s-r2-r3-oc-xyd-tt nil)
(defvar f-s-r2-r3-co-000-tt nil)
(defvar f-s-r2-r3-co-00d-tt nil)
(defvar f-s-r2-r3-co-0y0-tt nil)
(defvar f-s-r2-r3-co-x00-tt nil)
(defvar f-s-r2-r3-co-xyd-tt nil)
(defvar f-s-r2-r3-cc-000-tt nil)
(defvar f-s-r2-r3-cc-00d-tt nil)
(defvar f-s-r2-r3-cc-0y0-tt nil)
(defvar f-s-r2-r3-cc-x00-tt nil)
(defvar f-s-r2-r3-cc-xyd-tt nil)
(defvar f-s-r2-r3-cc-xyd-t0 nil)
(defvar f-s-r2-r3-co-xyd-t0 nil)
(defvar f-s-r2-r3-oc-xyd-t0 nil)
(defvar f-s-r2-r3-oo-xyd-t0 nil)
(defvar f-s-r2-r3-cc-xyd-0t nil)
(defvar f-s-r2-r3-cc-xyd-00 nil)

(defvar len-vvr1     nil)
(defvar len-vvr2     nil)
(defvar len-vvr3     nil)

;;----------------------------------------------------------------------------------------------------------------------------------
(if nil
    (progn (setq len-vvr1 17)
           (setq len-vvr2 19)
           (setq len-vvr3 23))
    (progn (setq len-vvr1 5)
           (setq len-vvr2 7)
           (setq len-vvr3 11)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 3D
(format 't "f-q-r3-r1~%")
(setq f-q-r3-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr1) 
                                                                       "y" (list :start -2 :end 2 :len len-vvr2)
                                                                       "z" (list :start -2 :end 2 :len len-vvr3))
                                             (lambda (x y z) (* x y z))
                                             :axes 't 
                                             :ano-nam "z"
                                             :ano-typ :ano-typ-real))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r1.vtk" f-q-r3-r1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 2D
;; Rectilinear Surface in 3D -- i.e. the points [x, y, f(x,y)]
(format 't "f-q-r2-r1~%")
(setq f-q-r2-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr1) 
                                                                       "y" (list :start -1 :end 1 :len len-vvr2))
                                             (lambda (x y) (* x y))
                                             :axes 't 
                                             :ano-nam "z"
                                             :ano-typ :ano-typ-real))
(mjr_dquad_colorize f-q-r2-r1 :data 0 :color-method "RGB" :ano-nam "z_color")

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r1.vtk" f-q-r2-r1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 1D
;; Rectilinear Curve in 2D -- i.e. the points [x, f(x)]
(format 't "f-q-r1-r1~%")
(setq f-q-r1-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr1))
                                             (lambda (x) (* x x))
                                             :axes 't
                                             :ano-nam "y"
                                             :ano-typ :ano-typ-real))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r1.vtk" f-q-r1-r1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 3D Vector Field 3D
(format 't "f-q-r3-r3~%")
(setq f-q-r3-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr1) 
                                                                       "y" (list :start -2 :end 2 :len len-vvr2)
                                                                       "z" (list :start -2 :end 2 :len len-vvr3))
                                             (lambda (x y z) (vector (* x x) (* y y) (* z z)))
                                             :axes 't 
                                             :ano-nam "v"
                                             :ano-typ :ano-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r3.vtk" f-q-r3-r3)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 2D
;; Parametric Surface in 3D -- i.e. the points [f_x(u, v), f_y(u, v), f_z(u, v)].  See: mjr_dsimp for options.
(format 't "f-q-r2-r3~%")
(setq f-q-r2-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 6.0 :len len-vvr1)
                                                                       "v" (list :start 0 :end 6.0 :len len-vvr2))
                                             (lambda (u v)
                                               (let ((a 1)
                                                     (c 3))
                                                 (vector (* (cos u) (+ c (* a (cos v))))
                                                         (* (sin u) (+ c (* a (cos v))))
                                                         (* a (sin v)))))
                                             :axes 't
                                             :ano-typ :ano-typ-rvec
                                             :ano-nam "torus"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r3.vtk" f-q-r2-r3)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 3D Vector Field 1D -- i.e. vectors with base points on the x-axis
;; Parametric Curve in 3D -- i.e. the points [f_x(u), f_y(u), f_z(u)].  See: mjr_dsimp for options.
(format 't "f-q-r1-r3~%")
(setq f-q-r1-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr1))
                                             (lambda (u) (vector (sin u) (cos u) (/ u 7.0)))
                                             :axes 't
                                             :ano-nam "xy"
                                             :ano-typ :ano-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r3.vtk" f-q-r1-r3)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 3D -- i.e. 2D vectors with base points in the 3-space.
;; This is an unusual data set.
(format 't "f-q-r3-r2~%")
(setq f-q-r3-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr1)
                                                                       "v" (list :start 0 :end 7.0 :len len-vvr2)
                                                                       "w" (list :start 0 :end 7.0 :len len-vvr3))
                                             (lambda (u v w) (vector (* u w) (* v w)))
                                             :axes 't
                                             :ano-nam "xy"
                                             :ano-typ :ano-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r2.vtk" f-q-r3-r2)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 2D -- i.e. vectors with base points in the x-y plane
;; Frequently used to represent a slope filed for a DEQ
(format 't "f-q-r2-r2~%")
(setq f-q-r2-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr1)
                                                                       "v" (list :start 0 :end 7.0 :len len-vvr2))
                                             (lambda (u v) (vector (sin u) (cos u) (* u v)))
                                             :axes 't
                                             :ano-nam "xy"
                                             :ano-typ :ano-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r2.vtk" f-q-r2-r2)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 1D -- i.e. vectors with base points on the x-axis
;; Parametric Curve in 2D -- i.e. the points [f_x(u), f_y(u)].  See: mjr_dsimp for options.
(format 't "f-q-r1-r2~%")
(setq f-q-r1-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 5.0 :len len-vvr1))
                                             (lambda (u) (vector (sin u) (cos u)))
                                             :axes 't
                                             :ano-nam "xy"
                                             :ano-typ :ano-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r2.vtk" f-q-r1-r2)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Curve in 2D -- i.e. the points [x, f(x)]
(format 't "f-s-r1-r1~%")
(setq f-s-r1-r1 (mjr_dsimp_make-from-dquad f-q-r1-r1 0 0))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r1-r1.vtk" f-s-r1-r1 :simplices 1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Parametric Curve in 2D -- i.e. the points [f_x(u), f_y(u)].  See: mjr_dsimp for options.
(format 't "f-s-r1-r2~%")
(setq f-s-r1-r2-o (mjr_dsimp_make-from-dquad f-q-r1-r2 0 0))
(setq f-s-r1-r2-c (mjr_dsimp_make-from-dquad f-q-r1-r2 0 0 :u-close 't))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r1-r2-o.vtk" f-s-r1-r2-o :simplices 1)
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r1-r2-c.vtk" f-s-r1-r2-c :simplices 1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Parametric Curve in 3D -- i.e. the points [f_x(u), f_y(u), f_z(u)].  See: mjr_dsimp for options.
(format 't "f-s-r1-r3~%")
(setq f-s-r1-r3-o (mjr_dsimp_make-from-dquad f-q-r1-r3 0 0))
(setq f-s-r1-r3-c (mjr_dsimp_make-from-dquad f-q-r1-r3 0 0 :u-close 't))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r1-r3-o.vtk" f-s-r1-r3-o :simplices 1)
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r1-r3-c.vtk" f-s-r1-r3-c :simplices 1)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Surface in 3D -- i.e. the points [x, y, f(x,y)]
(format 't "f-s-r2-r1~%")
(setq f-s-r2-r1 (mjr_dsimp_make-from-dquad f-q-r2-r1 '(0 1) 0))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r1.vtk" f-s-r2-r1 :simplices 2)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Parametric Surface in 3D -- i.e. the points [f_x(u, v), f_y(u, v), f_z(u, v)].  See: mjr_dsimp for options.
(format 't "f-s-r2-r3~%")

(setq f-s-r2-r3-oo-000-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(        ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oo-00d-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(      :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oo-0y0-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(   :y   ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oo-x00-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(:x      ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oo-xyd-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))

(setq f-s-r2-r3-oc-000-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(        ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oc-00d-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(      :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oc-0y0-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(   :y   ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oc-x00-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(:x      ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-oc-xyd-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))

(setq f-s-r2-r3-co-000-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(        ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-co-00d-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(      :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-co-0y0-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(   :y   ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-co-x00-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(:x      ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-co-xyd-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))

(setq f-s-r2-r3-cc-000-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(        ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-cc-00d-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(      :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-cc-0y0-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(   :y   ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-cc-x00-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(:x      ) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))
(setq f-s-r2-r3-cc-xyd-tt (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR 't))

(setq f-s-r2-r3-cc-xyd-t0 (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR nil))
(setq f-s-r2-r3-co-xyd-t0 (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR nil))
(setq f-s-r2-r3-oc-xyd-t0 (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR nil))
(setq f-s-r2-r3-oo-xyd-t0 (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil :surface-grid '(:x :y :d) :SURFACE-ORIENT 't  :SURFACE-TLBR nil))

(setq f-s-r2-r3-cc-xyd-0t (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT nil :SURFACE-TLBR 't ))
(setq f-s-r2-r3-cc-xyd-00 (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't  :surface-grid '(:x :y :d) :SURFACE-ORIENT nil :SURFACE-TLBR nil))


(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-000-tt-000.vtk" f-s-r2-r3-oo-000-tt :simplices '(     ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-000-tt-00t.vtk" f-s-r2-r3-oo-000-tt :simplices '(    2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-00d-tt-0lt.vtk" f-s-r2-r3-oo-00d-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-0y0-tt-0lt.vtk" f-s-r2-r3-oo-0y0-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-x00-tt-0lt.vtk" f-s-r2-r3-oo-x00-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-xyd-tt-0l0.vtk" f-s-r2-r3-oo-xyd-tt :simplices '(  1  ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-xyd-tt-0lt.vtk" f-s-r2-r3-oo-xyd-tt :simplices '(  1 2))
                                                                                                   
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-000-tt-000.vtk" f-s-r2-r3-oc-000-tt :simplices '(     ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-000-tt-00t.vtk" f-s-r2-r3-oc-000-tt :simplices '(    2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-00d-tt-0lt.vtk" f-s-r2-r3-oc-00d-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-0y0-tt-0lt.vtk" f-s-r2-r3-oc-0y0-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-x00-tt-0lt.vtk" f-s-r2-r3-oc-x00-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-xyd-tt-0l0.vtk" f-s-r2-r3-oc-xyd-tt :simplices '(  1  ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-xyd-tt-0lt.vtk" f-s-r2-r3-oc-xyd-tt :simplices '(  1 2))
                                                                                                   
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-000-tt-000.vtk" f-s-r2-r3-co-000-tt :simplices '(     ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-000-tt-00t.vtk" f-s-r2-r3-co-000-tt :simplices '(    2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-00d-tt-0lt.vtk" f-s-r2-r3-co-00d-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-0y0-tt-0lt.vtk" f-s-r2-r3-co-0y0-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-x00-tt-0lt.vtk" f-s-r2-r3-co-x00-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-xyd-tt-0l0.vtk" f-s-r2-r3-co-xyd-tt :simplices '(  1  ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-xyd-tt-0lt.vtk" f-s-r2-r3-co-xyd-tt :simplices '(  1 2))
                                                                                                   
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-000-tt-000.vtk" f-s-r2-r3-cc-000-tt :simplices '(     ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-000-tt-00t.vtk" f-s-r2-r3-cc-000-tt :simplices '(    2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-00d-tt-0lt.vtk" f-s-r2-r3-cc-00d-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-0y0-tt-0lt.vtk" f-s-r2-r3-cc-0y0-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-x00-tt-0lt.vtk" f-s-r2-r3-cc-x00-tt :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-0l0.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(  1  ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-0lt.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(  1 2))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-t0-0lt.vtk" f-s-r2-r3-cc-xyd-t0 :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co-xyd-t0-0lt.vtk" f-s-r2-r3-co-xyd-t0 :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc-xyd-t0-0lt.vtk" f-s-r2-r3-oc-xyd-t0 :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo-xyd-t0-0lt.vtk" f-s-r2-r3-oo-xyd-t0 :simplices '(  1 2))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-0t-0lt.vtk" f-s-r2-r3-cc-xyd-0t :simplices '(  1 2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-00-0lt.vtk" f-s-r2-r3-cc-xyd-00 :simplices '(  1 2))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-p00.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(0    ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-p0t.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(0   2))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-pl0.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(0 1  ))
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc-xyd-tt-plt.vtk" f-s-r2-r3-cc-xyd-tt :simplices '(0 1 2))






;; (format 't "half sphere r2-r3 parametric surface~%")
;; (mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd23.vtk"
;;                                      (lambda (u v) (vector (* 4 (sin u) (cos v)) (* 4 (sin u) (sin v)) (* 4 (cos u))))
;;                                      :udat (list :start  0 :end  pi :len 50)
;;                                      :vdat (list :start .1 :end 3   :len 10)
;;                                      :arg-mode :arg-number)

;; (format 't "egg carton r2-r1 graph~%")
;; (mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd21.vtk"
;;                                      (lambda (u v) (+ (sin u) (cos v)))
;;                                      :udat (list :start -6 :end 6 :len 50)
;;                                      :vdat (list :start -6 :end 6 :len 50)
;;                                      :arg-mode :arg-number)

;; (format 't "spiral r1-r3 parametric curve~%")
;; (mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd13.vtk"
;;                                      (lambda (u) (vector u (* 10 (sin u)) (* 10 (cos u))))
;;                                      :udat (list :start 0 :end  (* 3 pi) :len 50)
;;                                      :arg-mode :arg-number)

;; (format 't "circle r1-r2 parametric curve~%")
;; (mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd12.vtk"
;;                                      (lambda (u) (vector (* 10 (sin u)) (* 10 (cos u))))
;;                                      :udat (list :start 0 :end  (* 2 pi) :len 50)
;;                                      :arg-mode :arg-number)

;; (format 't "sine wave r1-r1 graph~%")
;; (mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd11.vtk"
;;                                      (lambda (u) (sin u))
;;                                      :udat (list :start 0 :end  (* 3 pi) :len 50)
;;                                      :arg-mode :arg-number)

;; (format 't "sine wave r1-r1 graph~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd11.vtk"
;;                         :s-func (lambda (u) (sin u))
;;                         :xdat (list :start 0 :end  (* 3 pi) :len 50)
;;                         :arg-mode :arg-number)

;; (format 't "2d vec on 1d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd12.vtk"
;;                         :v-func (lambda (x) (vector (sin x) (cos x)))
;;                         :xdat '(:start -10 :end 10 :len 10)
;;                         :arg-mode :arg-number)

;; (format 't "3d vec on 1d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd13.vtk"
;;                         :v-func (lambda (x) (vector x (sin x) (cos x)))
;;                         :xdat '(:start -10 :end 10 :len 10)
;;                         :arg-mode :arg-number)

;; (format 't "egg cart r2-r1 graph~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd21.vtk"
;;                         :s-func (lambda (x y) (+ (sin x) (cos y)))
;;                         :xdat (list :start -6 :end 6 :len 50)
;;                         :ydat (list :start -6 :end 6 :len 50)
;;                         :arg-mode :arg-number)

;; (format 't "2d vec on 2d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd22.vtk"
;;                         :v-func (lambda (x y) (vector (sin x) (cos y)))
;;                         :xdat (list :start -6 :end 6 :len 10)
;;                         :ydat (list :start -6 :end 6 :len 10)
;;                         :arg-mode :arg-number)

;; (format 't "3d vec on 2d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd23.vtk"
;;                         :v-func (lambda (x y) (vector x (sin x) (cos y)))
;;                         :xdat (list :start -6 :end 6 :len 10)
;;                         :ydat (list :start -6 :end 6 :len 10)
;;                         :arg-mode :arg-number)

;; (format 't "scalar field in 3d:  r3-r1 graph~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd31.vtk"
;;                         :s-func (lambda (x y z) (+ (sin x) (sin y) (cos z)))
;;                         :xdat (list :start -6 :end 6 :len 50)
;;                         :ydat (list :start -6 :end 6 :len 50)
;;                         :zdat (list :start -6 :end 6 :len 50)
;;                         :arg-mode :arg-number)

;; (format 't "2d vec on 3d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd32.vtk"
;;                         :v-func (lambda (x y z) (vector (sin (+ x z)) (cos (+ y z))))
;;                         :xdat (list :start -6 :end 6 :len 10)
;;                         :ydat (list :start -6 :end 6 :len 10)
;;                         :zdat (list :start -6 :end 6 :len 10)
;;                         :arg-mode :arg-number)

;; (format 't "3d vec on 3d space~%")
;; (mjr_vtk_grid-from-func "utst-vtk-OUT-rd33.vtk"
;;                         :v-func (lambda (x y z) (vector (sin x) (cos y) (sin z)))
;;                         :xdat (list :start -6 :end 6 :len 10)
;;                         :ydat (list :start -6 :end 6 :len 10)
;;                         :zdat (list :start -6 :end 6 :len 10)
;;                         :arg-mode :arg-number)


