;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-FixedPointItr.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2009,2012 by Mitch Richling.  All rights reserved.
;; @brief     Some :MJR_NLEQ examples.@EOL
;; @Keywords  nonlinear equation solution examples
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

;; Let $f(x)$ be defined as
;; 
;; $$f(x) = 1-x \sin(x)$$
;; 
;; We wish to find the roots of this function, and thus solve the following equation
;; 
;; $$0 = 1-x \sin(x)$$
;; 
;; We may rewrite this equation to obtain a fixed point form in a couple ways:
;; 
;;  $$\frac{1}{\sin(x)} = x$$
;;  $$x=1 - x \sin(x) + x$$
;; 
;; The four roots of smallest magnitude are located at approximately $\pm 1.1141$ and $\pm 2.7726$.
;; 
;; We can graph this function and see the four smallest roots:

(mjr_plot_func-r1-r1 (lambda (x) (- 1 (* (sin x) x))) :xlim '(-5 5))

;; The smallest, positive root may be found via fixed point iteration:

(mjr_nleq_fixed-point-itr (lambda (x) (/ (sin x))) 1 :show-progress 't :max-itr 20)

;; We may also use the alternate fixed point form to find this same root:

(mjr_nleq_fixed-point-itr (lambda (x) (+ x (- 1 (* (sin x) x)))) 1 :show-progress 't :max-itr 20)

;; The fixed point methods agree, and may be verified by bisection:

(mjr_nleq_root-bsect (lambda (x) (- 1 (* (sin x) x))) 0.0 2 :show-progress 't)


