;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-OrthoPolys.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Draw several sets of orthogonal polynomials.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  Create the png files with Bash like so:
;;    for f in exp-OrthoPolys*.pdf; do
;;      convert -density 600 -resize 1024x768 -background white -flatten  $f `echo $f | sed 's/-OUT-/-ART-/' | sed 's/.pdf$/.png/'`
;;    done
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_gnupl_send-command "set term pdfcairo")
(mjr_gnupl_send-command "set output \"exp-OrthoPolys-OUT-chebyshev.pdf\"")
(mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 (loop for i from 1 upto 7
                                             collect (mjr_poly_make-chebyshev i))
                                       :xdat '(:start -1 :end 1 :len 250))
                 :main "Chebyshev Polynomials (degree 1 upto 7)"
                 :title '(nil)
                 :xlab ""
                 :ylab "")
(mjr_gnupl_send-command "set output")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_gnupl_send-command "set term pdfcairo")
(mjr_gnupl_send-command "set output \"exp-OrthoPolys-OUT-legendre.pdf\"")
(mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 (loop for i from 1 upto 7
                                             collect (mjr_poly_make-legendre  i))
                                       :xdat '(:start -1 :end 1 :len 250))
                 :main "Legendre Polynomials (degree 1 upto 7)"
                 :title '(nil)
                 :xlab ""
                 :ylab "")
(mjr_gnupl_send-command "set output")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_gnupl_send-command "set term pdfcairo")
(mjr_gnupl_send-command "set output \"exp-OrthoPolys-OUT-lagrange.pdf\"")
(mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 (loop for i from 0 upto 5
                                             collect (mjr_poly_make-lagrange (mjr_vvec_to-vec (list :start -1 :end 1 :len 6)) i))
                                       :xdat '(:start -1 :end 1 :len 250))
                 :main "Lagrange Polynomials (degree 1 upto 6)"
                 :title '(nil)
                 :xlab ""
                 :ylab "")
(mjr_gnupl_send-command "set output")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_gnupl_send-command "set term pdfcairo")
(mjr_gnupl_send-command "set output \"exp-OrthoPolys-OUT-laguerre.pdf\"")
(mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 (loop for i from 1 upto 7
                                             collect (mjr_poly_make-laguerre  i))
                                       :xdat '(:start -4 :end 10 :len 250))
                 :main "Laguerre Polynomials (degree 1 upto 7)"
                 :title '(nil)
                 :ylim '(-10 14)
                 :xlab ""
                 :ylab "")
(mjr_gnupl_send-command "set output")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_gnupl_send-command "set term pdfcairo")
(mjr_gnupl_send-command "set output \"exp-OrthoPolys-OUT-hermite.pdf\"")
(mjr_gnupl_dquad (mjr_fsamp_dq-poly-r1 (loop for i from 1 upto 5
                                             collect (mjr_poly_make-hermite  i))
                                       :xdat '(:start -4 :end 4 :len 250))
                 :main "Hermite Polynomials (degree 1 upto 5)"
                 :title '(nil)
                 :ylim '(-23 23)
                 :xlab ""
                 :ylab "")
(mjr_gnupl_send-command "set output")
