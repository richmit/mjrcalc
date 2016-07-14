;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-IntrpRunge.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Demonstrate how adding more interpolation poitns may lead to worse fit.@EOL
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
;; @filedetails
;;
;;  Here we illustrate that adding points to an interpolation may not decrease the maximal deviation between the function being interpolated and the
;;  interpolating polynomial.
;;
;;  Reference:
;;      Runge, Carl (1901); Uber empirische Funktionen und die Interpolation zwischen aquidistanten Ordinaten; Zeitschrift fur Mathematik und Physik; Vol 46; pp 224-243
;;  
;;  How to get a nice PNG of the output:
;;
;;   1) In the REPL:
;;       (mjr_gnupl_send-command "set term pdfcairo")
;;       (mjr_gnupl_send-command "set output \"foo.pdf\"")
;;       (load "exp-IntrpRunge.lisp")
;;       (mjr_gnupl_send-command "set output")
;;   2) In shell:
;;       convert -density 300 -resize 1024x768 foo.pdf exIntrpRunge-ART.png
;;       rm foo.pdf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flet ((ifn (x) (/ (1+ (* x x)))))
  (let* ((x11 (mjr_vec_make-seq :start -5 :end 5 :len 11))
         (x15 (mjr_vec_make-seq :start -5 :end 5 :len 15))
         (p11     (mjr_intrp::mjr_intrp_poly-newton  x11 (map 'vector #'ifn x11)))
         (p15     (mjr_intrp::mjr_intrp_poly-newton  x15 (map 'vector #'ifn x15)))
         (data    (mjr_fsamp_dq-func-r123-r123 (list #'ifn
                                                     (lambda (x) (mjr_poly_eval p15 x))
                                                     (lambda (x) (mjr_poly_eval p11 x)))
                                               :xdat '(:start -5.1 :end 5.1 :len 1000)
                                               :func-lab '("1/(1+x^2)" "15 Uniformly Spaced Nodes" "11 Uniformly Spaced Nodes"))))
    (mjr_gnupl_dquad data :ylim '(-1 7.5) :main "Higher Interpolating Polynomial Oscillation With Increased Node Count")))
