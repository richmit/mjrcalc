;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-MandelbrotPot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw the potential of the mandelbrot set.@EOL
;; @Keywords  potential mandelbrot set
;; @Std       Common Lisp
;;
;;            Render with the following:
;;              povray -W1440 -H1080 -Q11 +A +R10 +J4 +P +D -OexMandelbrotPot-ART-povray.png exMandelbrotPot-AUX.pov
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (flet ((pot-fun (x y)
               (multiple-value-bind
                     (the-z the-count) (loop with ci of-type (complex single-float) = (complex x y)
                                             for  z of-type (complex single-float) = #C(0.0 0.0) then (+ (* z z) ci)
                                             for cnt fixnum from 0
                                             finally (return (values z cnt))
                                             until (or (> (abs z) 100000) (> cnt 768)))
                 (- (max 0.0 (/ (log (abs the-z)) (expt 2.0D0 the-count)))))))
        (let ((daData (mjr_dquad_make-from-axis "x" '(:start -2.0 :end 1.0 :len 200)
                                                "y" '(:start -1.5 :end 1.5 :len 200))))
        (print "Compute dquad...")
        (mjr_dquad_add-data-from-map daData
                                     #'pot-fun
                                     :axes 't
                                     :ano-nam "Mandelbrot-Pot")
        (print "VTK...")
        (mjr_vtk_from-dquad "exp-MandelbrotPot-OUT.vtk" daData)
        (print "gnuplot")
        (mjr_plot_func-r2-r1 #'pot-fun :pal "rainbow" :type :f
                             :xdat '(:start -2.0 :end 1.0 :len 100)
                             :ydat '(:start -1.5 :end 1.5 :len 100)
                             :zlim '(-0.4 0.01))
        (print "povray TGA")
        (mjr_img_tga-write "exp-MandelbrotPot-OUT.tga"
                           (mjr_img_make-from-func-r2-r1 #'pot-fun   
                                              :xdat '(:start -2.0 :end 1.0 :len 1536)
                                              :ydat '(:start -1.5 :end 1.5 :len 1536)
                                              :auto-scale 't
                                              :max-color #xFFFF
                                              :color-method #'mjr_colorized_povray))
        (print "povray geom")
        (mjr_pov_make-from-func-r12-r13 "exp-MandelbrotPot-OUT.pov" 
                           #'pot-fun
                           :udat '(:start -2.0 :end 1.0 :len 100)
                           :vdat '(:start -1.5 :end 1.5 :len 100)
                           :draw-points nil
                           :surface-smooth 't
                           :draw-surface-grid nil
                           :draw-surfaces 't)
        )))
