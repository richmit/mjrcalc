;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-MandelbrotPot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Draw the potential of the mandelbrot set.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2010,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;; @filedetails
;;
;;  Render height field version with the following:
;;    povray -W1440 -H1080 -Q11 +A +R10 +J4 +P +D -OexMandelbrotPot-ART-povray.png exMandelbrotPot-AUX.pov
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let ((numx 200)          ;; Set to 200 for triangular geometry (GNUplot, VTK, Povray), and 1536 for image based geometry
            (numy 200))
        (flet ((pot-fun (x y)
               (multiple-value-bind
                     (the-z the-count) (loop with ci of-type (complex single-float) = (complex x y)
                                             for  z of-type (complex single-float) = #C(0.0 0.0) then (+ (* z z) ci)
                                             for cnt fixnum from 0
                                             finally (return (values z cnt))
                                             until (or (> (abs z) 100000) (> cnt 768)))
                 (- (max 0.0 (/ (log (abs the-z)) (expt 2.0D0 the-count)))))))
        (let ((daData (mjr_dquad_make-from-axis "x" (list :start -2.0 :end 1.0 :len numx)
                                                "y" (list :start -1.5 :end 1.5 :len numy))))
        (print "Compute dquad...")
        (mjr_dquad_add-data-from-map daData #'pot-fun :axes 't :ano-nam "Mandelbrot-Pot" :ano-typ :ano-typ-real)
        (print "Add image to dquad...")
        (mjr_dquad_colorize daData :data 0 :color-method #'mjr_colorized_povray :max-color #xFFFF :auto-scale 't :ano-nam "Mandelbrot-Pot-Img" :ano-colorspace :cs-tru)

        (if (> 500 (max numx numy))
            (progn (print "VTK...")
                   (mjr_vtk_from-dquad "exp-MandelbrotPot-OUT.vtk" daData)
                   (print "gnuplot surface")
                   (mjr_gnupl_dquad daData :pal "rainbow" :type :f :zlim '(-0.4 0.01))
                   (format 't "~%Press [ENTER] to continue.~%")
                   (read-char)
                   (print "gnuplot image")
                   (mjr_gnupl_dquad daData :data-arrays 1 :type :rgb)
                   (print "povray geom")
                   (mjr_pov_make-from-dsimp "exp-MandelbrotPot-OUT.pov"
                                            (mjr_dsimp_make-from-dquad daData '(0 1) 0 :surface-grid nil :surface-normal-name "normals")
                                            :simplices 2 :draw-2-simplex-vertexes nil :draw-2-simplex-edges nil :draw-2-simplex-triangles 't))
            (progn (print "povray TGA")
                   (mjr_img_tga-write "exp-MandelbrotPot-OUT.tga" (mjr_dquad_get-data-array daData 1) :color-unpacker #'identity)))))))
