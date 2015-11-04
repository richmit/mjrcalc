;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PrimePairs.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1992,1994,1997,1998,2004,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     Find composite numbers that are the product of two primes that are a distance of d apart where d in [1, 150].@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mjr_prime_init-small-prime-list)

(loop with maxnum = 150
      for delta from 1 upto maxnum
      for lst = (loop for i from 0 
                      for p = (mjr_prime_nth-small-prime i) 
                      for qn = (+ delta p)
                      until (or (> p maxnum) (> (* p qn) maxnum))
                      when (mjr_prime_primep qn)
                      collect (* p qn))
      when lst
      do (format 't "~2d ~s~%" delta lst))
