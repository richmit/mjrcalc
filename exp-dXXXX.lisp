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
(defvar f-s-r2-r3-cc nil)
(defvar f-s-r2-r3-co nil)
(defvar f-s-r2-r3-oc nil)
(defvar f-s-r2-r3-oo nil)
(defvar len-vvr      nil)

;;----------------------------------------------------------------------------------------------------------------------------------
(setq len-vvr 10)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 3D
(format 't "f-q-r3-r1~%")
(setq f-q-r3-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr) 
                                                                       "y" (list :start -2 :end 2 :len len-vvr)
                                                                       "z" (list :start -2 :end 2 :len len-vvr))
                                             (lambda (x y z) (* x y z))
                                             :axes 't 
                                             :gd-nam "z"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r1.vtk" f-q-r3-r1 '(0 1 2))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 2D
;; Rectilinear Surface in 3D -- i.e. the points [x, y, f(x,y)]
(format 't "f-q-r2-r1~%")
(setq f-q-r2-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr) 
                                                                       "y" (list :start -2 :end 2 :len len-vvr))
                                             (lambda (x y) (* x y))
                                             :axes 't 
                                             :gd-nam "z"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r1.vtk" f-q-r2-r1 '(0 1))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear Scalar Field in 1D
;; Rectilinear Curve in 2D -- i.e. the points [x, f(x)]
(format 't "f-q-r1-r1~%")
(setq f-q-r1-r1 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr))
                                             (lambda (x) (* x x))
                                             :axes 't
                                             :gd-nam "y"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r1.vtk" f-q-r1-r1 0)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 3D Vector Field 3D
(format 't "f-q-r3-r3~%")
(setq f-q-r3-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "x" (list :start -2 :end 2 :len len-vvr) 
                                                                       "y" (list :start -2 :end 2 :len len-vvr)
                                                                       "z" (list :start -2 :end 2 :len len-vvr))
                                             (lambda (x y z) (vector (* x x) (* y y) (* z z)))
                                             :axes 't 
                                             :gd-nam "v"
                                             :gd-typ :gd-typ-rvec))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r3.vtk" f-q-r3-r3 '(0 1 2))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 2D
;; Parametric Surface in 3D -- i.e. the points [f_x(u, v), f_y(u, v), f_z(u, v)].  See: mjr_dsimp for options.
(format 't "f-q-r2-r3~%")
(setq f-q-r2-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 6.0 :len len-vvr)
                                                                       "v" (list :start 0 :end 6.0 :len len-vvr))
                                             (lambda (u v)
                                               (let ((a 1)
                                                     (c 3))
                                                 (vector (* (cos u) (+ c (* a (cos v))))
                                                         (* (sin u) (+ c (* a (cos v))))
                                                         (* a (sin v)))))
                                             :axes 't
                                             :gd-typ :gd-typ-rvec
                                             :gd-nam "torus"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r3.vtk" f-q-r2-r3 '(0 1))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 3D Vector Field 1D -- i.e. vectors with base points on the x-axis
;; Parametric Curve in 3D -- i.e. the points [f_x(u), f_y(u), f_z(u)].  See: mjr_dsimp for options.
(format 't "f-q-r1-r3~%")
(setq f-q-r1-r3 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr))
                                             (lambda (u) (vector (sin u) (cos u) (/ u 7.0)))
                                             :axes 't
                                             :gd-nam "xy"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r3.vtk" f-q-r1-r3 0)

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 3D -- i.e. 2D vectors with base points in the 3-space.
;; This is an unusual data set.
(format 't "f-q-r3-r2~%")
(setq f-q-r3-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr)
                                                                       "v" (list :start 0 :end 7.0 :len len-vvr)
                                                                       "w" (list :start 0 :end 7.0 :len len-vvr))
                                             (lambda (u v w) (vector (* u w) (* v w)))
                                             :axes 't
                                             :gd-nam "xy"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r3-r2.vtk" f-q-r3-r2 '(0 1 2))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 2D -- i.e. vectors with base points in the x-y plane
;; Frequently used to represent a slope filed for a DEQ
(format 't "f-q-r2-r2~%")
(setq f-q-r2-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 7.0 :len len-vvr)
                                                                       "v" (list :start 0 :end 7.0 :len len-vvr))
                                             (lambda (u v) (vector (sin u) (cos u) (* u v)))
                                             :axes 't
                                             :gd-nam "xy"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r2-r2.vtk" f-q-r2-r2 '(0 1))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Rectilinear 2D Vector Field 1D -- i.e. vectors with base points on the x-axis
;; Parametric Curve in 2D -- i.e. the points [f_x(u), f_y(u)].  See: mjr_dsimp for options.
(format 't "f-q-r1-r2~%")
(setq f-q-r1-r2 (mjr_dquad_add-data-from-map (mjr_dquad_make-from-axis "u" (list :start 0 :end 5.0 :len len-vvr))
                                             (lambda (u) (vector (sin u) (cos u)))
                                             :axes 't
                                             :gd-nam "xy"))

(mjr_vtk_from-dquad "exp-dXXXX-OUT-f-q-r1-r2.vtk" f-q-r1-r2 0)

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
(setq f-s-r2-r3-oo (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close nil))
(setq f-s-r2-r3-oc (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close nil :v-close 't))
(setq f-s-r2-r3-co (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close nil))
(setq f-s-r2-r3-cc (mjr_dsimp_make-from-dquad f-q-r2-r3 '(0 1) 0 :u-close 't  :v-close 't))

(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oo.vtk" f-s-r2-r3-oo :simplices 2)
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-oc.vtk" f-s-r2-r3-oc :simplices 2)
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-co.vtk" f-s-r2-r3-co :simplices 2)
(mjr_vtk_from-dsimp "exp-dXXXX-OUT-f-s-r2-r3-cc.vtk" f-s-r2-r3-cc :simplices 2)


