;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PovGraphs.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004 by Mitch Richling.  All rights reserved.
;; @Tested    2014-12-11
;; @brief     Some graphs with Povray -- using general p.pov-like code.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            To render, in a bash shell, with Povray:
;;
;;               for AFILE in exp-PovGraphs-AUX-*.pov; do 
;;                 OFILE=`echo $AFILE | sed 's/-AUX-/-ART-/' | sed 's/\.pov/.png/'`;
;;                 IFILE=`echo $AFILE | sed 's/-AUX-/-OUT-/' | sed 's/[0-9]\.pov/.pov/' | sed 's/[0-9]\.pov/.pov/'`;
;;                 povray -W3840 -H2160 -Q11 +A +AM2 -K0.08 +R4 +J3 -P -D -O$OFILE -I$IFILE  -HI$AFILE;
;;                 #povray -W800 -H450 -Q11 +A +AM2 -K0.08 +R4 +J3 -P -D -O$OFILE -I$IFILE  -HI$AFILE;
;;                 #pqiv -f $OFILE &
;;               done
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(print "Trefoil knot curve")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-knot.pov"
                      (lambda (tim) (vector (* 2 (+ (* 3 (cos (* 2 tim))) (* (cos (* 2 tim)) (cos (/ (* 3 (* 2 tim)) 2)))))
                                            (* 2 (+ (* 3 (sin (* 2 tim))) (* (sin (* 2 tim)) (cos (/ (* 3 (* 2 tim)) 2)))))
                                            (* 2 (sin (/ (* 3 (* 2 tim)) 2)))))
                      :udat (list :start 0 :end (* 2 pi) :len 200))

;;----------------------------------------------------------------------------------------------------------------------------------
(print "simple parametric curve (spiral)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-spiral.pov"
                      (lambda (x) (vector (* 7 (cos x)) (* 7 (sin x)) x))
                      :udat (list :start -7 :end 7 :len 20))

;;----------------------------------------------------------------------------------------------------------------------------------
(print "complicated parametric curve")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-crv.pov"
                      (lambda (tim) (vector (* 7 (cos (* 1/2 tim)))
                                            (* 7 (cos (* 1/5 tim)) (sin tim))
                                            (* 2 (sin (* 1/5 tim)) (cos (* 1/3 tim)))))
                      :udat (list :start (* -5 pi) :end (* 5 pi) :len 250))

;;----------------------------------------------------------------------------------------------------------------------------------
(print "Low density surface mesh (egg carton) with cubic curves")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswfc.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces nil
                      :curve-spline "cubic")
                      
;;----------------------------------------------------------------------------------------------------------------------------------
(print "Low density surface mesh (egg carton) with linear curves")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswfl.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces nil
                      :curve-spline "linear")
                      
;;----------------------------------------------------------------------------------------------------------------------------------
(print "Low density surface mesh (egg carton) with cubic curves and faces")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswff.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces 't
                      :curve-spline "linear")
                      
;;----------------------------------------------------------------------------------------------------------------------------------
(print "A high density surface mesh (drop wave)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-hds.pov"
                      (lambda (i j) (let ((d (sqrt (+ (* i i) (* j j)))))
                                      (* (/ 15 (+ (* d d) 8)) (cos (* 1/5 d d)))))
                      :udat (list :start -7 :end 7 :len 200)
                      :vdat (list :start -7 :end 7 :len 200)
                      :draw-points nil
                      :draw-surface-grid nil
                      :surface-smooth 't)

;;----------------------------------------------------------------------------------------------------------------------------------
(print "simple parametric surface (Sphere)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-sphere.pov"
                      (lambda (u v) (vector (* 3 (sin u) (cos v))
                                            (* 3 (sin u) (sin v))
                                            (* 3 (cos u))))
                      :udat (list :start .1 :end (- pi .1) :len 20)
                      :vdat (list :start 0  :end (* 2 pi)  :len 20)
                      :surface-smooth 't
                      )                      

;;----------------------------------------------------------------------------------------------------------------------------------
(print "Complicated parametric surface")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-wacky.pov"
                      (lambda (u v)
                        (let ((r 5))
                          (vector (/ (* r (sin (* 3 u))) (+ 2 (cos v)))
                                  (/ (* r (+ (sin u) (* 2 (sin (* 2 u))))) (+ 2 (cos (+ v (/ (* pi 2) 3)))))
                                  (/ (* (/ r 2) (- (cos u) (* 2 (cos (* 2 u)))) (+ 2 (cos v)) (+ 2 (cos (+ v (/ (* pi 2) 3))))) 4))))
                      :udat (list :start (- pi) :end (* 2 pi) :len 50)
                      :vdat (list :start (- pi) :end (* 2 pi) :len 55)
                      :draw-points nil
                      :draw-surface-grid nil
                      :surface-smooth 't)

;;----------------------------------------------------------------------------------------------------------------------------------
(print "DONE")
