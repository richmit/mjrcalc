;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PolyLegendreRoots.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
;; @brief     Compute the nodes for Gauss-Legendre integration with high accuracy.@EOL
;; @Keywords  clisp nodes gauss legendre integration arbitrary precision software floating point
;; @Std       clisp
;;
;;            This code uses clisp's (http://www.clisp.org/) arbitrary precision, software floating point implementation to compute
;;            the nodes for Gauss-Legendre integration.
;;

;;----------------------------------------------------------------------------------------------------------------------------------
;; We set the precision like this:

(setf (ext:long-float-digits) (* 32 16)) ;; Now (float-digits 1.1L0) will return 512 -- which is 128 or so decimal digits.

;;----------------------------------------------------------------------------------------------------------------------------------
;; We also must tell clisp to convert floating point types the ANSI way like so:

(setf CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* 't)

;;----------------------------------------------------------------------------------------------------------------------------------
;; We are now ready to compute some Gauss-Legendre nodes:

(mapc (lambda (x) (format 't "~100,80f~%" x))
      (sort (mjr_poly_root-solve-search-deflate (mjr_poly_make-legendre 21)
                                                :show-progress nil
                                                :show-warnings 't
                                                :retry-factor  5
                                                :refine-steps  100
                                                :only-refined  't
                                                :float-type    0.0L0
                                                :xepsr         1.0L-90
                                                :yepsr         1.0L-90
                                                :xeps          1.0L-5
                                                :yeps          1.0L-5)
            (lambda (a b) (< (realpart a) (realpart b)))))
