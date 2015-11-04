;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PrimePi.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1994,1997,1998,2004,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     The prime counting function (pi) and a pair of bounding functions.@EOL
;; @Keywords  prime counting function pi bounds
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(mjr_plot_func-r1-r1 (list (lambda (x) (/ x (+ 2 (log x))))
                           (lambda (x) (/ x (- (log x) 4)))
                           #'mjr_prime_pi-func)
                     :xdat (list :start 1000 :end 10000 :len (+ (- 10000 1000) 1))
                     :main "The prime counting function (pi) and a pair of bounding functions")
