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

(let ((a-dquad nil))
  (time (progn (format 't "Compute..~%")
               (setq a-dquad (mjr_dquad_make-from-func-r123-r123 (lambda (x y) (+ (sin x) (cos y)))
                                                                 :xdat '(:start -7.0 :end 7.0 :len 1024)
                                                                 :ydat '(:start -7.0 :end 7.0 :len 1024)))
               (mjr_dquad_colorize a-dquad :data 0 :color-method "BCGYR"                                                          :ano-nam "i")
               (mjr_dquad_colorize a-dquad :data 0 :color-method #'mjr_colorized_povray :ano-colorspace :cs-tru :max-color #xFFFF :ano-nam "h")))
  (time (progn (format 't "Write Color Height Field~%")
               (mjr_img_tga-write "exp-PovWaveGraph-OUT-h.tga" (mjr_dquad_get-data-array a-dquad "h") :color-space :cs-tru :color-unpacker #'identity)
               (format 't "Write Color Scheme~%")
               (mjr_img_tga-write "exp-PovWaveGraph-OUT-i.tga" (mjr_dquad_get-data-array a-dquad "i") :color-space :cs-rgb :color-unpacker #'identity))))

; MJR SCM NOTE <2015-03-28 21:55:58 CDT> exp-PovWaveGraph.lisp: Removed pov code.  Added status printing and timers.


;; MJR TODO NOTE <2015-03-28 15:18:59 CDT> a-dquad: Need an easy way to pull out an image from a dquad, and dump it into a file or manipulate it.
;; MJR TODO NOTE <2015-03-28 15:21:48 CDT> exp-PovWaveGraph.lisp: Perhaps 'mjr_img' should be subordinate to mjr_dquad -- i.e. only work with images in a dquad?


