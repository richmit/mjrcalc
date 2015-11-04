;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PovWaveGraph.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2010,2011 by Mitch Richling.  All rights reserved.
;; @brief     Generate a height field TGA and image map TGA file for a PovRay render.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            Render with the following
;;              povray -W3840 -H2160 -Q11 +K0.1  +A +AM2 +R10 +J4 -P -D -Oexp-PovWaveGraph-ART.png exp-PovWaveGraph-AUX.pov
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (mjr_pov_tga-write-from-func-r2-r1 "exp-PovWaveGraph-OUT-h.tga"
                                                      "exp-PovWaveGraph-OUT-i.tga"
                                                      (lambda (x y) (+ (sin x) (cos y)))
                                                      :xdat '(:start -7.0 :end 7.0 :len 500)
                                                      :ydat '(:start -7.0 :end 7.0 :len 500)
                                                      :auto-scale 't
                                                      :z-color-method "BCGYR"
                                                      :show-progress 't))
