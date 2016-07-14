;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PovGraphs.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Some graphs with Povray -- using general p.pov-like code.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  To render, in a bash shell, with Povray:
;;
;;     for AFILE in exp-PovGraphs-AUX-*.pov; do 
;;       OFILE=`echo $AFILE | sed 's/-AUX-/-ART-/' | sed 's/\.pov/.png/'`;
;;       IFILE=`echo $AFILE | sed 's/-AUX-/-OUT-/' | sed 's/[0-9]\.pov/.pov/' | sed 's/[0-9]\.pov/.pov/'`;
;;       povray -W3840 -H2160 -Q11 +A +AM2 -K0.08 +R4 +J3 -P -D -O$OFILE -I$IFILE  -HI$AFILE;
;;       #povray -W800 -H450 -Q11 +A +AM2 -K0.08 +R4 +J3 -P -D -O$OFILE -I$IFILE  -HI$AFILE;
;;       #pqiv -f $OFILE &
;;     done
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "Trefoil knot curve")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-knot.pov"
                      (lambda (tim) (vector (* 2 (+ (* 3 (cos (* 2 tim))) (* (cos (* 2 tim)) (cos (/ (* 3 (* 2 tim)) 2)))))
                                            (* 2 (+ (* 3 (sin (* 2 tim))) (* (sin (* 2 tim)) (cos (/ (* 3 (* 2 tim)) 2)))))
                                            (* 2 (sin (/ (* 3 (* 2 tim)) 2)))))
                      :udat (list :start 0 :end (* 2 pi) :len 200))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "simple parametric curve (spiral)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-spiral.pov"
                      (lambda (x) (vector (* 7 (cos x)) (* 7 (sin x)) x))
                      :udat (list :start -7 :end 7 :len 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "complicated parametric curve")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-crv.pov"
                      (lambda (tim) (vector (* 7 (cos (* 1/2 tim)))
                                            (* 7 (cos (* 1/5 tim)) (sin tim))
                                            (* 2 (sin (* 1/5 tim)) (cos (* 1/3 tim)))))
                      :udat (list :start (* -5 pi) :end (* 5 pi) :len 250))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "Low density surface mesh (egg carton) with cubic curves")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswfc.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces nil
                      :curve-spline "cubic")
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "Low density surface mesh (egg carton) with linear curves")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswfl.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces nil
                      :curve-spline "linear")
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "Low density surface mesh (egg carton) with cubic curves and faces")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-ldswff.pov"
                      (lambda (i j) (+ (sin i) (cos j)))
                      :udat (list :start -7 :end 7 :len 20)
                      :vdat (list :start -7 :end 7 :len 20)
                      :surface-smooth 't
                      :draw-surfaces 't
                      :curve-spline "linear")
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "A high density surface mesh (drop wave)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-hds.pov"
                      (lambda (i j) (let ((d (sqrt (+ (* i i) (* j j)))))
                                      (* (/ 15 (+ (* d d) 8)) (cos (* 1/5 d d)))))
                      :udat (list :start -7 :end 7 :len 200)
                      :vdat (list :start -7 :end 7 :len 200)
                      :draw-points nil
                      :draw-surface-grid nil
                      :surface-smooth 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "simple parametric surface (Sphere)")
(mjr_pov_make-from-func-r12-r13 "exp-PovGraphs-OUT-sphere.pov"
                      (lambda (u v) (vector (* 3 (sin u) (cos v))
                                            (* 3 (sin u) (sin v))
                                            (* 3 (cos u))))
                      :udat (list :start .1 :end (- pi .1) :len 20)
                      :vdat (list :start 0  :end (* 2 pi)  :len 20)
                      :surface-smooth 't
                      )                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "DONE")
