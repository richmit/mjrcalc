;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-OrthoPolys.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
;; @brief     Draw several sets of orthogonal polynomials.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            Create the png files like so:
;;              for f in exp-OrthoPolys*.pdf; do convert -density 600 -resize 1024x768 -background white -flatten  $f `echo $f | sed 's/-OUT-/-ART-/' | sed 's/.pdf$/.png/'` ; done
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output \"exp-OrthoPolys-OUT-chebyshev.pdf\"")
(mjr_plot_poly-r1-r1 (loop for i from 1 upto 7
                           collect (mjr_poly_make-chebyshev i))
                     :xdat '(:start -1 :end 1 :len 250)
                     :main "Chebyshev Polynomials (degree 1 upto 7)"
                     :xlab ""
                     :ylab "")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output")

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output \"exp-OrthoPolys-OUT-legendre.pdf\"")
(mjr_plot_poly-r1-r1 (loop for i from 1 upto 7
                           collect (mjr_poly_make-legendre  i))
                     :xdat '(:start -1 :end 1 :len 250)
                     :main "Legendre Polynomials (degree 1 upto 7)"
                     :xlab ""
                     :ylab "")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output")

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output \"exp-OrthoPolys-OUT-lagrange.pdf\"")
(mjr_plot_poly-r1-r1 (loop for i from 0 upto 5
                           collect (mjr_poly_make-lagrange (mjr_vvec_gen-0sim 'vector (list :start -1 :end 1 :len 6)) i))
                     :xdat '(:start -1 :end 1 :len 250)
                     :main "Lagrange Polynomials (degree 1 upto 6)"
                     :xlab ""
                     :ylab "")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output")

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output \"exp-OrthoPolys-OUT-laguerre.pdf\"")
(mjr_plot_poly-r1-r1 (loop for i from 1 upto 7
                           collect (mjr_poly_make-laguerre  i))
                     :xdat '(:start -4 :end 10 :len 250)
                     :main "Laguerre Polynomials (degree 1 upto 7)"
                     :ylim '(-10 14)
                     :xlab ""
                     :ylab "")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output")

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output \"exp-OrthoPolys-OUT-hermite.pdf\"")
(mjr_plot_poly-r1-r1 (loop for i from 1 upto 5
                           collect (mjr_poly_make-hermite  i))
                     :xdat '(:start -4 :end 4 :len 250)
                     :main "Hermite Polynomials (degree 1 upto 5)"
                     :ylim '(-23 23)
                     :xlab ""
                     :ylab "")
(mjr_plot::mjr_plot_drv-gnup-send-command "set output")
