;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PiRandom.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Approximate pi with random numbrs.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2007,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;; @filedetails
;;
;;  The math:
;;
;;    The probability that two integers picked at random are coprime, or relatively prime, is $\frac{6}{\pi^2}$.  Two integers $a$ and $b$ are
;;    coprime iff $\text{GCD}(a,b)=1$, where $\text{GCD}(a,b)$ is the greatest positive integer that divides both $a$ and $b$.
;;    
;;  The program: 
;;
;;    Compute random numbers, keep track of the relative frequency of coprime pairs, use that relative frequency to approximate the probability, and
;;    use that approximatetion to approximate $\pi$.  This program will print out successive approximations for every 1e6 pairs of integers
;;    checked..
;;    
;;    This code requires use-a.lisp that supports double floating point, big integers, and has a good random number generator.
;;    
;;  Similar C program:
;;    http://www.mitchr.me/SS/exampleCode/GMP/randPi.c.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop                                                                             ;; Loop forever
    with cpc = 0                                                                  ;; Number of coprime integers found
    for i from 0                                                                  ;; Number of coprime integers tested
    for cpa = (if (zerop cpc) 0 (sqrt (/ 6d0 (/ cpc i))))                         ;; Current approximation for PI
    when (= 1 (gcd (random 100000000) (random 100000000))) do (incf cpc)          ;; If our two random numbers are coprime, increment CPC
    when (zerop (mod i 1000000)) do (format 't "~30f ~30f~%" cpa (- cpa pi)))     ;; Print out our approximation every 1e6 pairs tested
