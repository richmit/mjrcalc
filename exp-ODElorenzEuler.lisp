;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODElorenzEuler.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
;; @brief     Compute without any ODE code and draw the Lorenz strange attracter.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((max-balls 30000)
       (x-dat     (make-array max-balls))
       (y-dat     (make-array max-balls))
       (z-dat     (make-array max-balls))
       (t-dat     (make-array max-balls)))
  (loop with delta = 0.003
        with x0    = 0.1
        with y0    = 0.0
        with z0    = 0.0
        with a     = 10.0
        with b     = 28.0
        with c     = (/ 8 3.0)
        for cur-bal from 0 upto (1- max-balls)
        for x = x0 then (+ X (* A (- Y X) DELTA))
        for y = y0 then (+ Y (* (- (* X (- B Z)) Y) DELTA))
        for z = z0 then (+ Z (* (- (* X Y) (* C Z)) DELTA))
        do (setf (aref x-dat cur-bal) x
                 (aref y-dat cur-bal) y
                 (aref z-dat cur-bal) z
                 (aref t-dat cur-bal) (* cur-bal delta)))
  (mjr_vtk_from-dsimp  "exp-ODElorenzEuler-OUT2.vtk" (mjr_dsimp_make-from-xyz :xdat x-dat :ydat y-dat :zdat z-dat
                                                                              :connect-points 't
                                                                              :data-vectors t-dat :data-vector-names "time")
                       :simplices 1))
