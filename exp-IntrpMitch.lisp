;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-IntrpMitch.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2012 by Mitch Richling.  All rights reserved.
;; @brief     Error Interpolating Runge's Function with Mitch Nodes of the First Kind vs Chebyshev Nodes.@EOL
;; @Keywords  Error Interpolating Runge's Function with Mitch Nodes of the First Kind vs Chebyshev Nodes
;; @Std       Common Lisp, Emacs Lisp
;;
;;            How to get a nice PNG of the output:
;;             1) In the REPL:
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set output \"foo.pdf\"")
;;                 (load "exp-IntrpMitch.lisp")
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set output")
;;             2) In shell:
;;                 convert -density 300 -resize 1024x768 foo.pdf exIntrpMitch-ART.png
;;                 rm foo.pdf
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(flet ((ifn (x) (/ (1+ (* x x)))))
  (let* ((n  7)
         (xm (mjr_vvec_gen-0sim 'vector '(:vvec-type :vvt-mitch1 :start -5 :end 5 :len n)))
         (xc (mjr_vvec_gen-0sim 'vector '(:vvec-type :vvt-cheb   :start -5 :end 5 :len n)))
         (xu (mjr_vvec_gen-0sim 'vector '(                       :start -5 :end 5 :len n)))
         (pm (mjr_intrp::mjr_intrp_poly-newton  xm (map 'vector #'ifn xm)))
         (pu (mjr_intrp::mjr_intrp_poly-newton  xu (map 'vector #'ifn xu)))
         (pc (mjr_intrp::mjr_intrp_poly-newton  xc (map 'vector #'ifn xc))))
    (mjr_plot_func-r1-r1 (list #'ifn
                               (lambda (x) (mjr_poly_eval pm x))
                               (lambda (x) (mjr_poly_eval pu x))
                               (lambda (x) (mjr_poly_eval pc x)))
                              :xdat '(:start -5.0 :end 5.0 :len 1000)
                              :main "Error Interpolating Runge's Function (Uniform, Mitch vs Chebyshev)"
                              :title '("Function" "Mitch Nodes" "Uniform Nodes" "Chebyshev Nodes"))))
