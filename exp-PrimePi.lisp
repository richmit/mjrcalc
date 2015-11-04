;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PrimePi.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1994,1997,1998,2004,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     The prime counting function (pi) and a pair of bounding functions.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (list (lambda (x) (/ x (- (log x) 4)))
                                                    #'mjr_prime_pi-func
                                                    (lambda (x) (/ x (+ 2 (log x)))))
                                              :xdat (list :start 1000 :end 10000 :len (+ (- 10000 1000) 1))
                                              :func-lab '("Upper Bound" "pi" "Lower Bound" ))
                 :main "The prime counting function (pi) and a pair of bounding functions")
