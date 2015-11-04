;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PovMixRes.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2012 by Mitch Richling.  All rights reserved.
;; @brief     How to draw a surface with a grid at one scale, and lines at another.@EOL
;; @Keywords  povray
;; @Std       Common Lisp
;;
;;            After running, we must stick together the povray files and render like so:
;;
;;              povray -W3840 -H2160 -Q11 -K0.08 +A  +AM5  +R4 +J4 -P -D -Oexp-PovMixRes-ART.png -Iexp-PovMixRes-AUX.pov
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(flet ((spf (u v) (vector (* 4 (sin u) (cos v))
                          (* 4 (sin u) (sin v))
                          (* 4 (cos u)))))
  ;; Draw smooth X grid lines
  (mjr_pov_make-from-func-r12-r13 "exp-PovMixRes-OUT-x.pov"
                     #'spf
                     :udat (list :start 0 :end  pi :len 50)
                     :vdat (list :start .1 :end 3 :len 10)
                     :draw-points nil
                     :surface-smooth nil
                     :draw-surface-grid (list :y)
                     :draw-surfaces nil)
  ;; Draw smooth Y grid lines
  (mjr_pov_make-from-func-r12-r13 "exp-PovMixRes-OUT-y.pov"
                     #'spf
                     :udat (list :start 0  :end  pi :len 10)
                     :vdat (list :start .1 :end 3 :len 50)
                     :draw-points nil
                     :surface-smooth nil
                     :draw-surface-grid (list :x)
                     :draw-surfaces nil)
  ;; Draw smooth surface
  (mjr_pov_make-from-func-r12-r13 "exp-PovMixRes-OUT-s.pov"
                     #'spf
                     :vdat (list :start .1 :end 3 :len 20)
                     :udat (list :start 0 :end  pi :len 20)
                     :draw-points nil
                     :surface-smooth 't
                     :draw-surface-grid nil
                     :draw-surfaces 't))
