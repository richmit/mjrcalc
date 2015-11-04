;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-IntrpRunge.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2012 by Mitch Richling.  All rights reserved.
;; @brief     Demonstrate how adding more interpolation poitns may lead to worse fit.@EOL
;; @Std       Common Lisp, Emacs Lisp
;;
;;            Here we illustrate that adding points to an interpolation may not decrease the maximal deviation between the function being interpolated and the
;;            interpolating polynomial.
;;
;;            Reference:
;;                Runge, Carl (1901); Uber empirische Funktionen und die Interpolation zwischen aquidistanten Ordinaten; Zeitschrift fur Mathematik und Physik; Vol 46; pp 224-243
;;            
;;            How to get a nice PNG of the output:
;;
;;             1) In the REPL:
;;                 (mjr_gnupl_send-command "set term pdfcairo")
;;                 (mjr_gnupl_send-command "set output \"foo.pdf\"")
;;                 (load "exp-IntrpRunge.lisp")
;;                 (mjr_gnupl_send-command "set output")
;;             2) In shell:
;;                 convert -density 300 -resize 1024x768 foo.pdf exIntrpRunge-ART.png
;;                 rm foo.pdf

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
