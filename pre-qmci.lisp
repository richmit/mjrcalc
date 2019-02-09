;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-qmci.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Quasi-Monte Carlo Integration.@EOL
;; @keywords  quasi-monte carlo integration
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015,2016, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Add more sophisticated seqeunces.@EOL@EOL
;; @todo      Add better error and accuracy control.@EOL@EOL
;; @todo      Add adaptive methods.@EOL@EOL
;; @todo      Add multidimensional methods.@EOL@EOL
;; @todo      Add generic methods to preform integration with various sequences smiple.@EOL@EOL
;; @todo      Add unit tests.@EOL@EOL
;; @warning   Very early stage development.  Suitable primarily for education and demonstration.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_QMCI
  (:USE :COMMON-LISP
        :MJR_MXP
        :MJR_CMP
        :MJR_CHK
        :MJR_EPS)
  (:DOCUMENTATION "Brief: Quasi-Monte Carlo Integration;")
  (:EXPORT #:mjr_qmci_help
           #:mjr_qmci_b-adic-radical-inverse-function-rat
           #:mjr_qmci_2-adic-radical-inverse-function-rat
           #:mjr_qmci_b-adic-radical-inverse-function-real
           #:mjr_qmci_2-adic-radical-inverse-function-real
           #:mjr_qmci_simple-quasi-monte-carlo
           ))

(in-package :MJR_QMCI)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_help ()
  "Quasi-Monte Carlo Integration"
  (documentation 'mjr_qmci_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_b-adic-radical-inverse-function-rat (n b)
  "Return the value of the b-adic radical inverse function as a rational number.

Let $b\in\mathbb{N}$, $b\ge 2$, and let $n=\sum_{k=0}^{b-1}n_kb^k$ be the b-adic digit expansion of $n$.  The b-adic radical inverse function is defined as:

$$\phi_b(n)=\sum_{k=0}^{b-1}\frac{n_k}{b^{k+1}}$$

References:
  G Leobacher and F Pillichshammer (2014); Introduction to Quasi-Monte Carlo Integration and Applications; ISBN: 978-3-319-03424-9; page 17"
  (declare (type fixnum n b))
  (labels ((ivog (n b m)
             (declare (type fixnum n b m))
             (if (zerop n)
                 0
                 (multiple-value-bind (n-left n-digit) (truncate n b)
                   (+ (/ n-digit m)
                      (ivog n-left b (* m b)))))))
    (ivog n b b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_2-adic-radical-inverse-function-rat (n)
  "Return the value of the 2-adic radical inverse function as a rational number.  

See: mjr_qmci_b-adic-radical-inverse-function-rat with b=2 for a definition of the 2-adic radical inverse function"
  (declare (type fixnum n))
  (loop for i fixnum below (integer-length n)
        for m fixnum = 2 then (* m 2)
        when (logbitp i n)
        sum (/ 1 m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_b-adic-radical-inverse-function-real (n b)
  "Return the value of the b-adic radical inverse function as a real, floating point number.  

See: mjr_qmci_b-adic-radical-inverse-function-rat for a definition of the b-adic radical inverse function."
  (declare (type fixnum n b))
  (labels ((ivog (n b m)
             (declare (type fixnum n b))
             (declare (type double-float m))
             (if (zerop n)
                 0.0d0
                 (multiple-value-bind (n-left n-digit) (truncate n b)
                   (+ (/ n-digit m) 
                      (ivog n-left b (* m b)))))))
    (ivog n b (float b 1.0d0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_2-adic-radical-inverse-function-real (n)
  "Return the value of the 2-adic radical inverse function as a real, floating point number.  

Note that this function is optimized to use machine integer binary representations to compute the 2-adic form of n.

See: mjr_qmci_b-adic-radical-inverse-function-rat with b=2 for a definition of the 2-adic radical inverse function.
See: mjr_qmci_b-adic-radical-inverse-function-real for the generalized function."
(declare (type fixnum n))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    ;; Iterate over set bits from least to most significant
    (do ((i n (logand i (1- i)))) ;; (a AND -a) - isolates the least significant set bit - p. 12 in HD
        ((zerop i) sum)
        (declare (type fixnum i))
      (incf sum (/ 1 (* 2.0d0 (logand i (- i)))))))) ;; (a AND (a-1)) - zero the least significant set bit - p. 11 in HD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_qmci_simple-quasi-monte-carlo (fun &key start end (order 10000) qsf qsffa show-progress)
  "Compute the definite integral of FUN on the given interval using the Monte Carlo method."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (let ((wid  (- end start))
          (fsum 0)
          (spi  (if (> order 30) (truncate order 30) 1)))
      (loop for ect from 1 upto order
            for k from 0
            for x = (+ (* wid (apply qsf k qsffa)) start)
            finally (return (* wid fsum (/ order)))
            do (incf fsum (funcall fun x))
            do (if (and show-progress
                        (zerop (mod ect spi)))
                   (format 't "~7d :: ~15f ~%" ect (* wid fsum (/ ect))))))))
