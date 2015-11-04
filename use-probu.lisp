;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-probu.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Computations on PDFs (Probability Distribution Functions).@EOL
;; @std       Common Lisp
;; @see       tst-probu.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010,2011,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PROBU
  (:USE :COMMON-LISP
        :MJR_INTG
        :MJR_PRNG
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Computations on PDFs (Probability Distribution Functions);")
  (:EXPORT #:mjr_probu_help
           #:mjr_probu_pdf2prng
           #:mjr_probu_pdf2cdf
           #:mjr_probu_pdf2ccdf
           #:mjr_probu_icdf2prng
           ))

(in-package :MJR_PROBU)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probu_help ()
  "Help for MJR_PROBU (PROBability Utilities):

This package has several utility functions useful for computations on PDFs (Probability Distribution Functions).

  |----------+-------------------------------------------+--------------------------+-------+--------------------------|
  |                                Terms and Meanings with R cross reference                                           |
  |----------+-------------------------------------------+--------------------------+-------+--------------------------|
  | Acronym  | Meaning                                   | R Term                   | Ex: R | Ex: MJR_PROB             |
  |----------+-------------------------------------------+--------------------------+-------+--------------------------|
  | PDF      | Probability Density Function              | density                  | dnorm | mjr_prob_poisson-pdf     |
  | CDF      | Cumulative Density Function               | distribution function    | pnorm | mjr_prob_std-normal-cdf  |
  | CCDF     | Complimentary Cumulative Density Function |                          |       |                          |
  | ICDF     | Inverse Cumulative Density Function       | quantile function        | qnorm | mjr_prob_std-normal-cdf  |
  | PRNG     | Pseudo Random Number Generator            | generate random deviates | rnorm | mjr_prob_std-normal-prng |
  |----------+-------------------------------------------+--------------------------+-------+--------------------------|
"

  (documentation 'mjr_probu_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probu_pdf2cdf (x min-x max-x pdf-func discrete &rest rest)
  (cond ((not (numberp x))                 (error "mjr_probu_pdf2cdf: X must be a number!"))
        ((complexp x)                      (error "mjr_probu_pdf2cdf: X must be a real number!"))
        ((and min-x (not (numberp min-x))) (error "mjr_probu_pdf2cdf: MIN-X must be a number!"))
        ((and min-x (complexp min-x))      (error "mjr_probu_pdf2cdf: MIN-X must be a real number!"))
        ((and max-x (not (numberp max-x))) (error "mjr_probu_pdf2cdf: MAX-X must be a number!"))
        ((and max-x (complexp max-x))      (error "mjr_probu_pdf2cdf: MAX-X must be a real number!"))
        ((and max-x (> min-x max-x))       (error "mjr_probu_pdf2cdf: MIX-X must be less or equal to MAX-X!"))
        ((not (or max-x min-x))            (error "mjr_probu_pdf2cdf: At least one of MIX-X or MAX-X must be provided!")))
  (cond ((and min-x (< x min-x)) 0)
        ((and max-x (> x max-x)) 1)
        ('t                      (flet ((da-func (v) (apply pdf-func v rest)))
                                   (if (or (null min-x) (and max-x min-x (< (- max-x x) (- x min-x))))
                                       (if discrete
                                           (- 1 (mjr_numu_sum :start x :end max-x :seq-fun #'da-func))
                                           (- 1 (mjr_intg_glb-adp-composite-trapezoidal #'da-func :start x :end max-x)))
                                       (if discrete
                                           (mjr_numu_sum :start min-x :end x :seq-fun #'da-func)
                                           (mjr_intg_glb-adp-composite-trapezoidal #'da-func :start min-x :end x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probu_pdf2ccdf (x min-x max-x pdf-func discrete &rest rest)
  (cond ((not (numberp x))                 (error "mjr_probu_pdf2ccdf: X must be a number!"))
        ((complexp x)                      (error "mjr_probu_pdf2ccdf: X must be a real number!"))
        ((and min-x (not (numberp min-x))) (error "mjr_probu_pdf2ccdf: MIN-X must be a number!"))
        ((and min-x (complexp min-x))      (error "mjr_probu_pdf2ccdf: MIN-X must be a real number!"))
        ((and max-x (not (numberp max-x))) (error "mjr_probu_pdf2ccdf: MAX-X must be a number!"))
        ((and max-x (complexp max-x))      (error "mjr_probu_pdf2ccdf: MAX-X must be a real number!"))
        ((and max-x (> min-x max-x))       (error "mjr_probu_pdf2ccdf: MIX-X must be less or equal to MAX-X!"))
        ((not (or max-x min-x))            (error "mjr_probu_pdf2ccdf: At least one of MIX-X or MAX-X must be provided!")))
  (cond ((and min-x (< x min-x)) 1)
        ((and max-x (> x max-x)) 0)
        ('t                      (flet ((da-func (v) (apply pdf-func v rest)))
                                   (if (or (null max-x) (and max-x min-x (< (- x min-x) (- max-x x))))
                                       (if discrete
                                           (- 1 (mjr_numu_sum :start min-x :end x :seq-fun #'da-func))
                                           (- 1 (mjr_intg_glb-adp-composite-trapezoidal #'da-func :start min-x :end x)))
                                       (if discrete
                                           (mjr_numu_sum :start x :end max-x :seq-fun #'da-func)
                                           (mjr_intg_glb-adp-composite-trapezoidal #'da-func :start x :end max-x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probu_pdf2prng (min-x max-x pdf-func discrete &rest rest)
  "Return random number given a PDF and its range.  The algorithm is the the rejection method."
  (cond ((not (numberp min-x)) (error "mjr_probu_pdf2prng: MIN-X must be a number!"))
        ((complexp min-x)      (error "mjr_probu_pdf2prng: MIN-X must be a real number!"))
        ((not (numberp max-x)) (error "mjr_probu_pdf2prng: MAX-X must be a number!"))
        ((complexp max-x)      (error "mjr_probu_pdf2prng: MAX-X must be a real number!"))
        ((> min-x max-x)       (error "mjr_probu_pdf2prng: MIX-X must be less or equal to MAX-X!")))
  (loop for x = (if discrete
                    (mjr_prng_int-cc   min-x max-x)
                    (mjr_prng_float-cc min-x max-x))
        for y = (mjr_prng_float-cc 0 1)
        for p = (apply pdf-func x rest)
        finally (return x)
        while (> y p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probu_icdf2prng (icdf-func &rest rest)
  "Return random number given an inverse CDF.  The algorithm is the the inverse CDF method."
  (apply icdf-func (mjr_prng_float-oo 0 1) rest))
