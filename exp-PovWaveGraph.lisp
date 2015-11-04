;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PovWaveGraph.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Generate a height field TGA and image map TGA file for a PovRay render.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2010,2011,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      a-dquad: Need an easy way to pull out an image from a dquad, and dump it into a file or manipulate it.@EOL@EOL
;; @todo      exp-PovWaveGraph.lisp: Perhaps 'mjr_img' should be subordinate to mjr_dquad -- i.e. only work with images in a dquad?@EOL@EOL
;; @filedetails
;;
;;  Render with the following
;;    povray -W3840 -H2160 -Q11 +K0.1  +A +AM2 +R10 +J4 -P -D -Oexp-PovWaveGraph-ART.png exp-PovWaveGraph-AUX.pov
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a-dquad nil))
  (time (progn (format 't "Compute..~%")
               (setq a-dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x y) (+ (sin x) (cos y)))
                                                                 :xdat '(:start -7.0 :end 7.0 :len 1024)
                                                                 :ydat '(:start -7.0 :end 7.0 :len 1024)))
               (mjr_dquad_colorize a-dquad :data 0 :color-method "BCGYR"                                                          :ano-nam "i")
               (mjr_dquad_colorize a-dquad :data 0 :color-method #'mjr_colorized_povray :ano-colorspace :cs-tru :max-color #xFFFF :ano-nam "h")))
  (time (progn (format 't "Write Color Height Field~%")
               (mjr_img_tga-write "exp-PovWaveGraph-OUT-h.tga" (mjr_dquad_get-data-array a-dquad "h") :color-space :cs-tru :color-unpacker #'identity)
               (format 't "Write Color Scheme~%")
               (mjr_img_tga-write "exp-PovWaveGraph-OUT-i.tga" (mjr_dquad_get-data-array a-dquad "i") :color-space :cs-rgb :color-unpacker #'identity))))

