;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-IntFactors.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1992,1994,1997,1998,2004,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     For the integers n in [1,72], list the first integer such that it has n unique divisors.@EOL
;; @Keywords  prime factors divisors
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(let ((mx 0))
  (loop for i from 1 to 10080
        do (let ((n (length (mjr_prime_all-factors i))))
             (if (> n mx) (progn (setq mx n) (format 't "~5d ~5d~%" i n))))))
