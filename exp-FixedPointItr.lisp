;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-FixedPointItr.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Some :MJR_NLEQ examples.@EOL
;; @std       Common Lisp
;; @see       
;; @copyright 
;;  @parblock
;;  Copyright (c) 2009,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x) (- 1 (* (sin x) x))) :xdat '(:start -5 :end 5 :len 100)))

;; The smallest, positive root may be found via fixed point iteration:

(mjr_nleq_fixed-point-itr (lambda (x) (/ (sin x))) 1 :show-progress 't :max-itr 20)

;; We may also use the alternate fixed point form to find this same root:

(mjr_nleq_fixed-point-itr (lambda (x) (+ x (- 1 (* (sin x) x)))) 1 :show-progress 't :max-itr 20)

;; The fixed point methods agree, and may be verified by bisection:

(mjr_nleq_root-bsect (lambda (x) (- 1 (* (sin x) x))) 0.0 2 :show-progress 't)


