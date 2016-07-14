;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PovMixRes.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     How to draw a surface with a grid at one scale, and lines at another.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  After running, we must stick together the povray files and render like so:
;;    povray -W3840 -H2160 -Q11 -K0.08 +A  +AM5  +R4 +J4 -P -D -Oexp-PovMixRes-ART.png -Iexp-PovMixRes-AUX.pov
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
