;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-PiRandom.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2007,2012 by Mitch Richling.  All rights reserved.
;; @brief     approximate pi with random numbrs.@EOL
;; @Std       Common Lisp
;;
;;            The math:
;;
;;              The probability that two integers picked at random are coprime, or relatively prime, is $\frac{6}{\pi^2}$.  Two integers $a$ and $b$ are
;;              coprime iff $\text{GCD}(a,b)=1$, where $\text{GCD}(a,b)$ is the greatest positive integer that divides both $a$ and $b$.
;;              
;;            The program: 
;;
;;              Compute random numbers, keep track of the relative frequency of coprime pairs, use that relative frequency to approximate the probability, and
;;              use that approximatetion to approximate $\pi$.  This program will print out successive approximations for every 1e6 pairs of integers
;;              checked..
;;              
;;              This code requires use-a.lisp that supports double floating point, big integers, and has a good random number generator.
;;              
;;            Similar C program:
;;              http://www.mitchr.me/SS/exampleCode/GMP/randPi.c.html
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop                                                                             ;; Loop forever
    with cpc = 0                                                                  ;; Number of coprime integers found
    for i from 0                                                                  ;; Number of coprime integers tested
    for cpa = (if (zerop cpc) 0 (sqrt (/ 6d0 (/ cpc i))))                         ;; Current approximation for PI
    when (= 1 (gcd (random 100000000) (random 100000000))) do (incf cpc)          ;; If our two random numbers are coprime, increment CPC
    when (zerop (mod i 1000000)) do (format 't "~30f ~30f~%" cpa (- cpa pi)))     ;; Print out our approximation every 1e6 pairs tested
