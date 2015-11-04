;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      utst-use-vtk.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
;; @brief     Interactive tests for use-vtk.lisp.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(format 't "half sphere r2-r3 parametric surface~%")
(mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd23.vtk"
                                     (lambda (u v) (vector (* 4 (sin u) (cos v)) (* 4 (sin u) (sin v)) (* 4 (cos u))))
                                     :udat (list :start  0 :end  pi :len 50)
                                     :vdat (list :start .1 :end 3   :len 10)
                                     :arg-mode :arg-number)

(format 't "egg carton r2-r1 graph~%")
(mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd21.vtk"
                                     (lambda (u v) (+ (sin u) (cos v)))
                                     :udat (list :start -6 :end 6 :len 50)
                                     :vdat (list :start -6 :end 6 :len 50)
                                     :arg-mode :arg-number)

(format 't "spiral r1-r3 parametric curve~%")
(mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd13.vtk"
                                     (lambda (u) (vector u (* 10 (sin u)) (* 10 (cos u))))
                                     :udat (list :start 0 :end  (* 3 pi) :len 50)
                                     :arg-mode :arg-number)

(format 't "circle r1-r2 parametric curve~%")
(mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd12.vtk"
                                     (lambda (u) (vector (* 10 (sin u)) (* 10 (cos u))))
                                     :udat (list :start 0 :end  (* 2 pi) :len 50)
                                     :arg-mode :arg-number)

(format 't "sine wave r1-r1 graph~%")
(mjr_vtk_polydata-from-func-r12-r123 "utst-vtk-OUT-pd11.vtk"
                                     (lambda (u) (sin u))
                                     :udat (list :start 0 :end  (* 3 pi) :len 50)
                                     :arg-mode :arg-number)

(format 't "sine wave r1-r1 graph~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd11.vtk"
                        :s-func (lambda (u) (sin u))
                        :xdat (list :start 0 :end  (* 3 pi) :len 50)
                        :arg-mode :arg-number)

(format 't "2d vec on 1d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd12.vtk"
                        :v-func (lambda (x) (vector (sin x) (cos x)))
                        :xdat '(:start -10 :end 10 :len 10)
                        :arg-mode :arg-number)

(format 't "3d vec on 1d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd13.vtk"
                        :v-func (lambda (x) (vector x (sin x) (cos x)))
                        :xdat '(:start -10 :end 10 :len 10)
                        :arg-mode :arg-number)

(format 't "egg cart r2-r1 graph~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd21.vtk"
                        :s-func (lambda (x y) (+ (sin x) (cos y)))
                        :xdat (list :start -6 :end 6 :len 50)
                        :ydat (list :start -6 :end 6 :len 50)
                        :arg-mode :arg-number)

(format 't "2d vec on 2d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd22.vtk"
                        :v-func (lambda (x y) (vector (sin x) (cos y)))
                        :xdat (list :start -6 :end 6 :len 10)
                        :ydat (list :start -6 :end 6 :len 10)
                        :arg-mode :arg-number)

(format 't "3d vec on 2d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd23.vtk"
                        :v-func (lambda (x y) (vector x (sin x) (cos y)))
                        :xdat (list :start -6 :end 6 :len 10)
                        :ydat (list :start -6 :end 6 :len 10)
                        :arg-mode :arg-number)

(format 't "scalar field in 3d:  r3-r1 graph~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd31.vtk"
                        :s-func (lambda (x y z) (+ (sin x) (sin y) (cos z)))
                        :xdat (list :start -6 :end 6 :len 50)
                        :ydat (list :start -6 :end 6 :len 50)
                        :zdat (list :start -6 :end 6 :len 50)
                        :arg-mode :arg-number)

(format 't "2d vec on 3d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd32.vtk"
                        :v-func (lambda (x y z) (vector (sin (+ x z)) (cos (+ y z))))
                        :xdat (list :start -6 :end 6 :len 10)
                        :ydat (list :start -6 :end 6 :len 10)
                        :zdat (list :start -6 :end 6 :len 10)
                        :arg-mode :arg-number)

(format 't "3d vec on 3d space~%")
(mjr_vtk_grid-from-func "utst-vtk-OUT-rd33.vtk"
                        :v-func (lambda (x y z) (vector (sin x) (cos y) (sin z)))
                        :xdat (list :start -6 :end 6 :len 10)
                        :ydat (list :start -6 :end 6 :len 10)
                        :zdat (list :start -6 :end 6 :len 10)
                        :arg-mode :arg-number)
