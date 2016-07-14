;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODEcircle.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Example of an ODE plot.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1999,2008,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      @EOL@EOL
;; @warning   @EOL@EOL
;; @bug       @EOL@EOL
;; @filedetails
;;
;;  Two slope plots with the x and y reversed.  The results are frequently surprising for newcomers to DEQ.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time 
 (flet ((xy2x (x y)  (* (+ (* x x) (* y y) (- 4)) (* x y)))
        (xy2y (x y)  (* (+ (* x x) (* y y) (- 4)) (- (* x x) (* y y))))
        (v2slope (v) (if (zerop (aref v 0)) 0 (/ (aref v 1) (aref v 0)))))

   (print "Create dquad object")
   (let ((daData (mjr_dquad_make-from-axis  "x" '(:start -5 :end 5 :len 501)
                                            "y" '(:start -5 :end 5 :len 501))))
     (print "Compute cirXYv")
     (mjr_dquad_add-data-from-map daData
                                  (lambda (x y) (vector (xy2x x y) (xy2y x y)))
                                  :axes 't
                                  :ano-nam "cirXYv"
                                  :ano-typ :ano-typ-rvec)

     (print "Compute cirYXv")
     (mjr_dquad_add-data-from-map daData
                                  (lambda (x y) (vector (xy2y x y) (xy2x x y)))
                                  :axes 't
                                  :ano-nam "cirYXv"
                                  :ano-typ :ano-typ-rvec)

     (print "Compute cirXYs")
     (mjr_dquad_add-data-from-map daData
                                  #'v2slope
                                  :data "cirXYv"
                                  :ano-nam "cirXYs")

     (print "Compute cirYXs")
     (mjr_dquad_add-data-from-map daData
                                  #'v2slope
                                  :data "cirYXv"
                                  :ano-nam "cirYXs")

     (print "Save VTK")
     (mjr_vtk_from-dquad "exp-ODEcircle-OUT.vtk" daData))))

