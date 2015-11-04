;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-IntrpMitch.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2012 by Mitch Richling.  All rights reserved.
;; @brief     Error Interpolating Runge's Function with Mitch Nodes of the First Kind vs Chebyshev Nodes.@EOL
;; @Std       Common Lisp, Emacs Lisp
;;
;;            How to get a nice PNG of the output:
;;             1) In the REPL:
;;                 (mjr_gnupl_send-command "set term pdfcairo")
;;                 (mjr_gnupl_send-command "set output \"foo.pdf\"")
;;                 (load "exp-IntrpMitch.lisp")
;;                 (mjr_gnupl_send-command "set output")
;;             2) In shell:
;;                 convert -density 300 -resize 1024x768 foo.pdf exIntrpMitch-ART.png
;;                 rm foo.pdf
;;            

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
