;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-IntrpMitch.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Error Interpolating Runge's Function with Mitch Nodes of the First Kind vs Chebyshev Nodes.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;  How to get a nice PNG of the output:
;;   1) In the REPL:
;;       (mjr_gnupl_send-command "set term pdfcairo")
;;       (mjr_gnupl_send-command "set output \"foo.pdf\"")
;;       (load "exp-IntrpMitch.lisp")
;;       (mjr_gnupl_send-command "set output")
;;   2) In shell:
;;       convert -density 300 -resize 1024x768 foo.pdf exIntrpMitch-ART.png
;;       rm foo.pdf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flet ((ifn (x) (/ (1+ (* x x)))))
  (let* ((n   7)
         (xm  (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-mitch1 :start -5 :end 5 :len n)))
         (xc  (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-cheb   :start -5 :end 5 :len n)))
         (xu  (mjr_vvec_gen-0sim 'vector (list                        :start -5 :end 5 :len n)))
         (pm  (mjr_intrp::mjr_intrp_poly-newton  xm (map 'vector #'ifn xm)))
         (pu  (mjr_intrp::mjr_intrp_poly-newton  xu (map 'vector #'ifn xu)))
         (pc  (mjr_intrp::mjr_intrp_poly-newton  xc (map 'vector #'ifn xc)))
         (dat (mjr_dquad_make-from-axis "x"  '(:start -5.0 :end 5.0 :len 1000))))    
    (mjr_dquad_add-data-from-map dat #'ifn                             :axes 't :ano-nam "Function"        :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map dat (lambda (x) (mjr_poly_eval pm x)) :axes 't :ano-nam "Mitch Nodes"     :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map dat (lambda (x) (mjr_poly_eval pu x)) :axes 't :ano-nam "Uniform Nodes"   :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map dat (lambda (x) (mjr_poly_eval pc x)) :axes 't :ano-nam "Chebyshev Nodes" :ano-typ :ano-typ-real)
    (mjr_gnupl_dquad dat :main "Error Interpolating Runge's Function (Uniform, Mitch vs Chebyshev)")))
