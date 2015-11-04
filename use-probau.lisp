;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-probau.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Balls And Urns probability distributions.@EOL
;; @std       Common Lisp
;; @see       tst-probau.lisp
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
;; @todo      mjr_probau_multi-hypergeometric-pdf: Optimize!!  Fix edge cases.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PROBAU
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_COMBE
        :MJR_PROBU
        :MJR_PROB)
  (:DOCUMENTATION "Brief: Balls And Urns probability distributions.;")
  (:EXPORT #:mjr_probau_help
           ;; In :#:MJR_PROBAU and :#:MJR_PROB (with alternate parametrization)
           #:mjr_probau_bernoulli-pdf               #:mjr_probau_bernoulli-cdf               #:mjr_probau_bernoulli-ccdf                #:mjr_probau_bernoulli-prng
           #:mjr_probau_binomial-pdf                #:mjr_probau_binomial-cdf                #:mjr_probau_binomial-ccdf                 #:mjr_probau_binomial-prng 
           #:mjr_probau_geometric-pdf               #:mjr_probau_geometric-cdf               #:mjr_probau_geometric-ccdf                #:mjr_probau_geometric-prng
           ;; In :#:MJR_PROBAU only!
           #:mjr_probau_negative-binomial-pdf       #:mjr_probau_negative-binomial-cdf       #:mjr_probau_negative-binomial-ccdf        #:mjr_probau_negative-binomial-prng
           #:mjr_probau_hypergeometric-pdf          #:mjr_probau_hypergeometric-cdf          #:mjr_probau_hypergeometric-ccdf           #:mjr_probau_hypergeometric-prng
           #:mjr_probau_negative-hypergeometric-pdf #:mjr_probau_negative-hypergeometric-cdf #:mjr_probau_negative-hypergeometric-ccdf  #:mjr_probau_negative-hypergeometric-prng 
           #:mjr_probau_multi-hypergeometric-pdf
           #:mjr_probau_multinomial-pdf
           ))

(in-package :MJR_PROBAU)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_help ()
  "Help for MJR_PROBAU (PROBablity Balls And Urns):

This package focuses exclusively on probability distributions with a clear 'Balls And Urns' definition, and implements the distributions directly in 'BAU'
terms.  For example, the binomial is not parametrized in terms of $p$, the probability of success for the underlying Bernoulli trial, but in terms of the
number of red balls, $n$, and blue balls, $m$, in an urn from which we draw with replacement.

See MJR_PROB for traditionally parametrized implementations of some of the PDFs found here as well as a few other useful PDFs.

  |--------------------------------+--------------------------------------+--------------------------------------------|
  |                             Balls And Urns (BAU) Experiments and Probability                                       |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  |=== Binary BAU (N red balls & M blue balls) ========================================================================|
  |--------------------------------+--------------------------------------+--------------------------------------------|
  |------- Experiment: Draw S balls -----------------------------------------------------------------------------------|
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | probability of                 | sample S with replacement            | sample S without replacement               |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | K red balls in sample          | binomial(k,n,m,s)                    | hypergeometric(k,n,m,s)                    |
  | K red balls in sample (S=1)    | binomial(k,n,m,1) ==                 | hypergeometric(k,n,m,1) ==                 |
  |                                |   bernoulli(k,n,m)                   |   bernoulli(k,n,m)                         |
  | Sth draw is red, rest are blue | binomial(0,n,m,s-1) *                | hypergeometric(0,n,m,s-1) *                |
  |                                |   binomial(1,n,m,1)                  |   hypergeometric(1,n,m,1)                  |
  | at least 1 red ball in sample  | 1-binomial(0,n,m,s) ==               | 1-hypergeometric(0,n,m,s) ==               |
  |                                |   binomial-ccdf(1,n,m,s)             |   hypergeometric-ccdf(1,n,m,s)             |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  |------- Experiment: Draw balls until R red balls drawn -------------------------------------------------------------|
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | probability of                 | draw with replacement                | draw without replacement                   |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | K blue balls in sample         | negative-binomial(k,n,m,r)           | negative_hypergeometric(k,n,m,r)           |
  | K blue balls in sample (r=1)   | negative-binomial(k,n,m,1) ==        | negative_hypergeometric(k,n,m,1) ==        |
  |                                |   geometric(k,n,m)                   |   unhyper_binomial(k,n,m)                  |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  |=== Many ball type BAU (N_i balls of type i) sampling (S balls in sample) problems =================================|
  |--------------------------------+--------------------------------------+--------------------------------------------|
  |------- Experiment: Draw S=sum(K_i) balls --------------------------------------------------------------------------|
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | probability of                 | sample S with replacement            | sample S without replacement               |
  |--------------------------------+--------------------------------------+--------------------------------------------|
  | K_i balls of color i in sample | multinomial(K, N)                    |  multi_hypergeometric(K, N)                |
  |--------------------------------+--------------------------------------+--------------------------------------------|"
  (documentation 'mjr_probu_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_bernoulli-pdf (k n m &key (algorithm :direct))
  "'Balls And Urns' version of the bernoulli PDF

Probability of picking K red balls in 1 draw from a population N red balls and M blue balls"
  (cond ((not (integerp m))                (error "mjr_probau_bernoulli-pdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_bernoulli-pdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_bernoulli-pdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_bernoulli-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_bernoulli-pdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_bernoulli-pdf: K must be an integer!")))
  (mjr_prob_bernoulli-pdf k (/ n (+ n m)) :algorithm algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_bernoulli-cdf (k n m &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_bernoulli-cdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_bernoulli-cdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_bernoulli-cdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_bernoulli-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_bernoulli-cdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_bernoulli-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_probau_bernoulli-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 1 #'mjr_probau_bernoulli-pdf 't n m :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_bernoulli-ccdf (k n m &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_bernoulli-ccdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_bernoulli-ccdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_bernoulli-ccdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_bernoulli-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_bernoulli-ccdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_bernoulli-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_probau_bernoulli-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 1 #'mjr_probau_bernoulli-pdf 't n m :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_bernoulli-prng (n m &key (algorithm :accept-reject) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_bernoulli-prng: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_bernoulli-prng: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_bernoulli-prng: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_bernoulli-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_bernoulli-prng: M and/or N must be non-zero!")))
  (case algorithm
    (:accept-reject  (mjr_probu_pdf2prng 0 1 #'mjr_probau_bernoulli-pdf 't n m :algorithm pdf-algorithm))
    (:bau            (if (< (random (+ n m)) n) 1 0))
    (otherwise       (error "mjr_probau_bernoulli-prng: Unknown algorithm"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_geometric-pdf (k n m &key (algorithm :direct))
  "The probability of K blue balls followed by 1 red ball after k+1 draws with replacement

NOTE: I have used a definition of the geometric distribution that is compatible with R in order to facilitate interoperability with R.

Value of :ALGORITHM determines how the computation is performed.
  * :direct  -- use direct computation using definition

Classical formula:
  $$\\left(\\frac{m}{n+m}\\right)^k\\cdot\\left(\\frac{n}{n+m}\\right)$$"
  (cond ((not (integerp m))                (error "mjr_probau_geometric-pdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_geometric-pdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_geometric-pdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_geometric-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_geometric-pdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_geometric-pdf: K must be an integer!"))
        ((< k 0)                           (error "mjr_probau_geometric-pdf: K must be non-negative!")))
  (mjr_prob_geometric-pdf k (/ n (+ n m)) :algorithm algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_geometric-cdf (k n m &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_geometric-cdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_geometric-cdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_geometric-cdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_geometric-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_geometric-cdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_geometric-cdf: K must be an integer!"))   
        ((< k 0)                           (error "mjr_probau_geometric-cdf: K must be non-negative!")) 
        ((not (equal algorithm :pdf2cdf))  (error "mjr_probau_geometric-cdf: Unknown algorithm!")))     
  (mjr_probu_pdf2cdf k 0 nil #'mjr_probau_geometric-pdf 't n m :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_geometric-ccdf (k n m &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_geometric-ccdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_geometric-ccdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_geometric-ccdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_geometric-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_geometric-ccdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_geometric-ccdf: K must be an integer!"))   
        ((< k 0)                           (error "mjr_probau_geometric-ccdf: K must be non-negative!")) 
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_probau_geometric-ccdf: Unknown algorithm!")))     
  (mjr_probu_pdf2ccdf k 0 nil #'mjr_probau_geometric-pdf 't n m :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_geometric-prng (n m &key (algorithm :bau))
  (cond ((not (integerp m))                (error "mjr_probau_geometric-prng: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_geometric-prng: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_geometric-prng: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_geometric-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_geometric-prng: M and/or N must be non-zero!")))
  (case algorithm
    (:bau           (loop for s = (random (+ n m))
                          count (>= s n)
                          until (< s n)))
    (:exponential   (floor (mjr_prob_exponential-prng (/ n (+ n m)))))
    (otherwise      (error "mjr_probau_geometric-pdf: Unknown algorithm"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_binomial-pdf (k n m s &key (algorithm :direct))
  "'Balls And Urns' version of the binomial PDF

Probability of exactly K red balls in S draws with replacement from a population N red balls and M blue balls (p=N/M)

Value of :ALGORITHM is as in mjr_prob_binomial-pdf"
  (cond ((not (integerp m))                (error "mjr_probau_binomial-pdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_binomial-pdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_binomial-pdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_binomial-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_binomial-pdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_binomial-pdf: K must be an integer!")))
  (mjr_prob_binomial-pdf k (/ n (+ n m)) s :algorithm algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_binomial-cdf (k n m s &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_binomial-cdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_binomial-cdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_binomial-cdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_binomial-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_binomial-cdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_binomial-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_probau_binomial-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 s #'mjr_probau_binomial-pdf 't n m s :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_binomial-ccdf (k n m s &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_binomial-ccdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_binomial-ccdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_binomial-ccdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_binomial-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_binomial-ccdf: M and/or N must be non-zero!"))
        ((not (integerp k))                (error "mjr_probau_binomial-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_probau_binomial-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 s #'mjr_probau_binomial-pdf 't n m s :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_binomial-prng (n m s &key (algorithm :accept-reject) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_binomial-prng: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_binomial-prng: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_binomial-prng: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_binomial-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_binomial-prng: M and/or N must be non-zero!")))
  (case algorithm
    (:accept-reject (mjr_probu_pdf2prng 0 s #'mjr_probau_binomial-pdf 't n m s :algorithm pdf-algorithm))
    (:bau           (loop for i from 1 upto s
                          count (< (random (+ n m)) n)))
    (otherwise      (error "mjr_probau_binomial-pdf: Unknown algorithm"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-binomial-pdf (k n m r &key (algorithm :direct))
  "Probability a sequence of draws with replacement containing K blue balls will have R red ones from a population of N red balls and M blue balls

i.e. we draw till we have R red balls, and p(k) is the probability we drew k blue balls in the process.

Value of :ALGORITHM is as in mjr_prob_negative-binomial-pdf"
  (cond ((not (integerp m))                (error "mjr_probau_negative-binomial-pdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_negative-binomial-pdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_negative-binomial-pdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_negative-binomial-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_negative-binomial-pdf: M and/or N must be non-zero!"))
        ((not (integerp r))                (error "mjr_probau_negative-binomial-pdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_probau_negative-binomial-pdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_probau_negative-binomial-pdf: K must be an integer!")))
  (mjr_prob_negative-binomial-pdf k (/ n (+ n m)) r :algorithm algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-binomial-cdf (k n m r &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_negative-binomial-cdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_negative-binomial-cdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_negative-binomial-cdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_negative-binomial-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_negative-binomial-cdf: M and/or N must be non-zero!"))
        ((not (integerp r))                (error "mjr_probau_negative-binomial-cdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_probau_negative-binomial-cdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_probau_negative-binomial-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_probau_negative-binomial-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 nil #'mjr_probau_negative-binomial-pdf 't n m r :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-binomial-ccdf (k n m r &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_negative-binomial-ccdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_negative-binomial-ccdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_negative-binomial-ccdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_negative-binomial-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_negative-binomial-ccdf: M and/or N must be non-zero!"))
        ((not (integerp r))                (error "mjr_probau_negative-binomial-ccdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_probau_negative-binomial-ccdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_probau_negative-binomial-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_probau_negative-binomial-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 nil #'mjr_probau_negative-binomial-pdf 't n m r :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-binomial-prng (n m r &key (algorithm :bau))
  (cond ((not (integerp m))                (error "mjr_probau_negative-binomial-prng: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_negative-binomial-prng: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_negative-binomial-prng: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_negative-binomial-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_negative-binomial-prng: M and/or N must be non-zero!"))
        ((not (integerp r))                (error "mjr_probau_negative-binomial-prng: R must be an integer!"))
        ((< r 0)                           (error "mjr_probau_negative-binomial-prng: R must be non-negative!"))
        ((not (equal algorithm :bau))      (error "mjr_probau_negative-binomial-prng: Unknown algorithm!")))
  (loop with red  = 0
        with blue = 0
        for i from 1
        finally (return blue)                                                  
        do (if (< (random (+ n m)) n)
               (incf red)
               (incf blue))
        until (= red r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_hypergeometric-pdf (k n m s &key (algorithm :direct))
  "Probability of exactly K red balls in S draws without replacement from a population of N red balls and M blue balls

Value of :ALGORITHM determines how the computation is performed.
  * :direct    -- use direct computation using definition
                  Complexity is generally O(min(m,n)), but better in several special cases:
                    * O(1) if any of the following are true: k>n, s-k>m, k>s, k<0, s=0, n=0, s=m+n, s=1, n=1, m=1
                    * O(min(s,n)) if k=0  (note n<m in this case)
                    * O(min(s,m)) if s=k  (note m<n in this case)
  * :bernoulli -- Use a bernoulli
                  Valid:  S = 1
  * :binomial  -- Use a binomial
                  Valid: M+N,N >> S; N/(N+M) is not close to 0 or 1
  * :normal    -- Use normal
                  Valid: S>>0; N+M, N >> S; N/(N+M) is not close to 0 or 1
  * :floating  -- Use floating point approximations in the standard formula

Classical formula:
  $$\\frac{\\binom{n}{k}\\binom{m}{s-k}}{\\binom{m+n}{s}} = \\frac{m!n!s!(m+n-s)!}{k!(n-k)!(m+k-s)!(s-k)!(m+n)!}$$"
  (cond ((not (integerp m))        (error "mjr_probau_hypergeometric-pdf: M must be an integer!"))
        ((< m 0)                   (error "mjr_probau_hypergeometric-pdf: M must be non-negative!"))
        ((not (integerp n))        (error "mjr_probau_hypergeometric-pdf: N must be an integer!"))
        ((< n 0)                   (error "mjr_probau_hypergeometric-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))     (error "mjr_probau_hypergeometric-pdf: M and/or N must be non-zero!"))
        ((not (integerp s))        (error "mjr_probau_hypergeometric-pdf: S must be an integer!"))
        ((< s 0)                   (error "mjr_probau_hypergeometric-pdf: S must be non-negative!"))
        ((not (integerp k))        (error "mjr_probau_hypergeometric-pdf: K must be an integer!")))
  (let* ((m+n    (+ m n))
         (m+n-s  (- m+n s))
         (m+k    (+ m k))
         (m+k-s  (- m+k s))
         (m-s    (- m s))
         (s-k    (- s k))
         (n-k    (- n k))
         (p      (/ n m+n))
         (q      (- 1 p)))
    (case algorithm
      (:direct          (cond ((> k n)   0) ;; more red in sample than in population
                              ((> s-k m) 0) ;; more blue in sample than in population
                              ((> k s)   0) ;; more red in sample than sample size
                              ((< k 0)   0) ;; negative k
                              ((= s 0)   (if (= k 0) 1 0)) ;; Sample is empty
                              ((= n 0)   (if (= k 0) 1 0)) ;; Population is all blue
                              ((= m 0)   (if (= k s) 1 0)) ;; Population is all red
                              ((= s m+n) (if (= k n) 1 0)) ;; sample is entire space
                              ((= s 1)   (if (= k 1) p q)) ;; sample is 1, reduces to Bernoulli
                              ((= n 1)   (if (= k 0)       ;; 1 red ball in population
                                             (/ m+n-s m+n) ;;   = $1-\frac{s}{m+n} = \frac{m+n-s}{m+n}$
                                             (/ s m+n)))
                              ((= m 1)   (mjr_probau_hypergeometric-pdf s-k m n s)) ;; 1 blue ball
                              ((= k 0)   (if (< s n) ;;  entire sample is blue
                                             ;; $\frac{m!(m+n-s)!}{(m-s)!(m+n)!}$ O(s)
                                             (mjr_numu_prod :start m
                                                            :end (1+ m-s)
                                                            :seq-fun (lambda (i) (/ i (+ n i))))
                                             ;; Need not worry about m<n hear as that would mean s-k>m
                                             ;; $$\prod_{j=m+1}^{m+n}\frac{j-s}{j}$$ O(n)
                                             (mjr_numu_prod :start (1+ m)
                                                            :end m+n
                                                            :seq-fun (lambda (i) (/ (- i s) i)))))
                              ((= k s)   (mjr_probau_hypergeometric-pdf 0 m n s)) ;; entire sample is red
                              ((< m n)   (mjr_probau_hypergeometric-pdf s-k m n s)) ;; Best we can do is O(n), so make n small
                              ;; $$\frac{m!s!}{(s-n)!(m+n)!} = \prod_{j=0}^{n-1}\frac{s-j}{m+n-j}$$
                              ((= n k)   (mjr_numu_prod :start 0 ;; draw every red ball!!
                                                        :end (1- n)
                                                        :seq-fun (lambda (i) (/ (- s i) (- m+n i)))))
                              ;; $$\prod_{j=0}^{k-1}\frac{(n-j)(s-j)}{j+1}\cdot\prod_{j=0}^{n-k+1}(m+n-s-j)\cdot\prod_{j=0}^{n-1}\frac{1}{m+n-j}$$  O(n)
                              ('t        (mjr_numu_prod :start 0
                                                        :end (max (- k 1) (- n (1+ k)) (1- n))
                                                        :seq-fun (lambda (i) (/ (* (if (<= i (1- k))       (/ (* (- n i) (- s i)) (+ i 1)) 1)
                                                                                   (if (<= i (- n (1+ k))) (- m+n s i)                     1))
                                                                                (if (<= i (1- n))          (- m+n i)                       1)))))))
      (:binomial  (mjr_probau_binomial-pdf k n m s))
      (:bernoulli (mjr_probau_bernoulli-pdf k n m))
      (:normal    (mjr_prob_normal-pdf k (* s p) (* s p q)))
      (:naive0    (cond ((> k n)   0)
                        ((> s-k m) 0)
                        ((> k s)   0)
                        ((< k 0)   0)
                        ('t        (/ (* (mjr_combe_comb n k) (mjr_combe_comb m s-k)) (mjr_combe_comb m+n s)))))
      (:floating  (cond ((> k n)   0)
                        ((> s-k m) 0)
                        ((> k s)   0)
                        ((< k 0)   0)
                        ('t        (exp (- (+ (mjr_numu_log-binomial n k) (mjr_numu_log-binomial m s-k)) (mjr_numu_log-binomial m+n s))))))
      (:naive1    (cond ((> k n)   0)
                        ((> s-k m) 0)
                        ((> k s)   0)
                        ((< k 0)   0)
                        ('t        (mjr_numu_prod :start 1
                                                  :end m+n
                                                  :seq-fun (lambda (i) (/ (* (if (<= i m)     i 1)
                                                                             (if (<= i n)     i 1)
                                                                             (if (<= i s)     i 1)
                                                                             (if (<= i m+n-s) i 1))
                                                                          (if (<= i k)     i 1)
                                                                          (if (<= i n-k)   i 1)
                                                                          (if (<= i m+k-s) i 1)
                                                                          (if (<= i s-k)   i 1)
                                                                          (if (<= i m+n)   i 1)))))))
      (otherwise  (error "mjr_probau_hypergeometric-pdf: Unknown algorithm")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_hypergeometric-cdf (k n m s &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_hypergeometric-cdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_hypergeometric-cdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_hypergeometric-cdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_hypergeometric-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_hypergeometric-cdf: M and/or N must be non-zero!"))
        ((not (integerp s))                (error "mjr_probau_hypergeometric-cdf: S must be an integer!"))
        ((< s 0)                           (error "mjr_probau_hypergeometric-cdf: S must be non-negative!"))
        ((not (integerp k))                (error "mjr_probau_hypergeometric-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_probau_hypergeometric-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 (min s (+ n m)) #'mjr_probau_hypergeometric-pdf 't n m s :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_hypergeometric-ccdf (k n m s &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                (error "mjr_probau_hypergeometric-ccdf: M must be an integer!"))
        ((< m 0)                           (error "mjr_probau_hypergeometric-ccdf: M must be non-negative!"))
        ((not (integerp n))                (error "mjr_probau_hypergeometric-ccdf: N must be an integer!"))
        ((< n 0)                           (error "mjr_probau_hypergeometric-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))             (error "mjr_probau_hypergeometric-ccdf: M and/or N must be non-zero!"))
        ((not (integerp s))                (error "mjr_probau_hypergeometric-ccdf: S must be an integer!"))
        ((< s 0)                           (error "mjr_probau_hypergeometric-ccdf: S must be non-negative!"))
        ((not (integerp k))                (error "mjr_probau_hypergeometric-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_probau_hypergeometric-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 (min s (+ n m)) #'mjr_probau_hypergeometric-pdf 't n m s :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_hypergeometric-prng (n m s &key (pdf-algorithm :direct) (algorithm :accept-reject))
  (cond ((not (integerp m))                     (error "mjr_probau_hypergeometric-prng: M must be an integer!"))
        ((< m 0)                                (error "mjr_probau_hypergeometric-prng: M must be non-negative!"))
        ((not (integerp n))                     (error "mjr_probau_hypergeometric-prng: N must be an integer!"))
        ((< n 0)                                (error "mjr_probau_hypergeometric-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))                  (error "mjr_probau_hypergeometric-prng: M and/or N must be non-zero!"))
        ((not (integerp s))                     (error "mjr_probau_hypergeometric-prng: S must be an integer!"))
        ((< s 0)                                (error "mjr_probau_hypergeometric-prng: S must be non-negative!")))
  (case algorithm
    (:bau           (loop with red = 0
                          with blue = 0
                          for i from 1 upto s
                          finally (return red)                                                  
                          do (if (< (random (+ (- n red) (- m blue))) (- n red))
                                 (incf red)
                                 (incf blue))))
    (:accept-reject (mjr_probu_pdf2prng 0 (min s n) #'mjr_probau_hypergeometric-pdf 't n m s :algorithm pdf-algorithm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-hypergeometric-pdf (k n m r &key (algorithm :direct))
  "Probability that a sequence of draws without replacement containing R red balls from a population of N red balls and M blue balls will have exactially K blue balls

i.e. we draw without replacement till we have R red balls, and p(k) is the probability we drew k blue balls in the process.

Classical formula:
  $$\\frac{\\binom{k+r-1}{r-1}\\binom{m+n-k-r}{n-r}}{\\binom{m+n}{n}}$$
  $$\\frac{(k+r-1)! (m+n-k-r) m! n!}{(r-1)! (n-r)! (m-k)! (m+n)! k!}$$"
  (cond ((not (integerp m))        (error "mjr_probau_negative-hypergeometric-pdf: M must be an integer!"))
        ((< m 0)                   (error "mjr_probau_negative-hypergeometric-pdf: M must be non-negative!"))
        ((not (integerp n))        (error "mjr_probau_negative-hypergeometric-pdf: N must be an integer!"))
        ((< n 0)                   (error "mjr_probau_negative-hypergeometric-pdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))     (error "mjr_probau_negative-hypergeometric-pdf: M and/or N must be non-zero!"))
        ((not (integerp r))        (error "mjr_probau_negative-hypergeometric-pdf: R must be an integer!"))
        ((< r 0)                   (error "mjr_probau_negative-hypergeometric-pdf: R must be non-negative!"))
        ((not (integerp k))        (error "mjr_probau_negative-hypergeometric-pdf: K must be an integer!")))
  (let* ((m+n   (+ m n))
         (m-k   (- m k))
         (n-r   (- n r))
         (r-1   (- r 1))
         (k+r   (+ k r)))
    (cond ((< k 0)     0)                ;; Can not pick a negative number of blue balls
          ((< m+n k+r) 0)                ;; Can not pick more than we have
          ((< n r)     0)                ;; Can not pick more red than we have
          ((< m k)     0)                ;; Can not pick more blue than we have
          ((= m 0)     (if (= k 0) 1 0)) ;; Population is all red
          ('t              (case algorithm
                             (:naive0   (/ (* (mjr_combe_comb (- k+r 1) r-1)
                                              (mjr_combe_comb (- m+n k+r) n-r))
                                           (mjr_combe_comb m+n n)))
                             (:direct   (cond ((= k 0) (mjr_numu_prod :start 1 ;; Sample contains no blue balls $$\frac{(m+n-r)!n!}{(m+n)!(n-r)!}$$
                                                                      :end r
                                                                      :seq-fun (lambda (i) (/ (+ i n-r) (+ i m n-r)))))
                                              ((= n r) (* (/ n m+n) (mjr_numu_prod :start 1 ;; Every red ball is in the sample $$\frac{m!k!}{(k-n)!(m+n)!}$$
                                                                                   :end (1- n)
                                                                                   :seq-fun (lambda (i) (/ (+ i k) (+ i m))))))
                                              ((= m k) (mjr_numu_prod :start 1 ;; Every blue ball is in the sample $$\frac{(r-1+m)!n!}{(r-1)!(m+n)!}$$
                                                                      :end m
                                                                      :seq-fun (lambda (i) (/ (+ i r-1) (+ i n)))))
                                              ((= r 1) (* (/ n m+n) (mjr_numu_prod :start 1 ;; Only one red ball is in the sample $$\frac{\binom{m+n-k-1}{n-1}}{\binom{m+n}{n}}$$
                                                                                   :end k
                                                                                   :seq-fun (lambda (i) (/ (+ i m-k) (- (+ i m+n) (+ k 1)))))))
                                              ('t      (mjr_numu_prod :start 1 ;; $$\frac{(m+n-k-r)!(k+r-1)!n!m!}{(m+n)!(r-1)!(n-r)!(m-k)!k!}$$
                                                                      :end k+r
                                                                      :seq-fun (lambda (i) (* (if (<= i k+r)  (/ (- (+ i m+n) k+r))       1)
                                                                                              (if (<= i r)    (+ i n-r)                   1)
                                                                                              (if (<= i k)    (/ (* (+ i r-1) (+ i m-k)) i) 1)))))))
                             (otherwise (error "mjr_probau_negative-hypergeometric-pdf: Unknown algorithm")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-hypergeometric-cdf (k n m r &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                     (error "mjr_probau_negative-hypergeometric-cdf: M must be an integer!"))
        ((< m 0)                                (error "mjr_probau_negative-hypergeometric-cdf: M must be non-negative!"))
        ((not (integerp n))                     (error "mjr_probau_negative-hypergeometric-cdf: N must be an integer!"))
        ((< n 0)                                (error "mjr_probau_negative-hypergeometric-cdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))                  (error "mjr_probau_negative-hypergeometric-cdf: M and/or N must be non-zero!"))
        ((not (integerp r))                     (error "mjr_probau_negative-hypergeometric-cdf: R must be an integer!"))
        ((< r 0)                                (error "mjr_probau_negative-hypergeometric-cdf: R must be non-negative!"))
        ((not (integerp k))                     (error "mjr_probau_negative-hypergeometric-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))       (error "mjr_probau_negative-hypergeometric-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 nil #'mjr_probau_negative-hypergeometric-pdf 't n m r :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-hypergeometric-ccdf (k n m r &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (integerp m))                     (error "mjr_probau_negative-hypergeometric-ccdf: M must be an integer!"))
        ((< m 0)                                (error "mjr_probau_negative-hypergeometric-ccdf: M must be non-negative!"))
        ((not (integerp n))                     (error "mjr_probau_negative-hypergeometric-ccdf: N must be an integer!"))
        ((< n 0)                                (error "mjr_probau_negative-hypergeometric-ccdf: N must be non-negative!"))
        ((and (= m 0) (= n 0))                  (error "mjr_probau_negative-hypergeometric-ccdf: M and/or N must be non-zero!"))
        ((not (integerp r))                     (error "mjr_probau_negative-hypergeometric-ccdf: R must be an integer!"))
        ((< r 0)                                (error "mjr_probau_negative-hypergeometric-ccdf: R must be non-negative!"))
        ((not (integerp k))                     (error "mjr_probau_negative-hypergeometric-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf))      (error "mjr_probau_negative-hypergeometric-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 nil #'mjr_probau_negative-hypergeometric-pdf 't n m r :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_negative-hypergeometric-prng (n m r &key (pdf-algorithm :direct) (algorithm :accept-reject))
  (cond ((not (integerp m))                     (error "mjr_probau_negative-hypergeometric-prng: M must be an integer!"))
        ((< m 0)                                (error "mjr_probau_negative-hypergeometric-prng: M must be non-negative!"))
        ((not (integerp n))                     (error "mjr_probau_negative-hypergeometric-prng: N must be an integer!"))
        ((< n 0)                                (error "mjr_probau_negative-hypergeometric-prng: N must be non-negative!"))
        ((and (= m 0) (= n 0))                  (error "mjr_probau_negative-hypergeometric-prng: M and/or N must be non-zero!"))
        ((not (integerp r))                     (error "mjr_probau_negative-hypergeometric-prng: R must be an integer!"))
        ((< r 0)                                (error "mjr_probau_negative-hypergeometric-prng: R must be non-negative!"))
        ((not (equal algorithm :accept-reject)) (error "mjr_probau_negative-hypergeometric-prng: Unknown algorithm!")))
  (mjr_probu_pdf2prng 0 m #'mjr_probau_negative-hypergeometric-pdf 't n m r :algorithm pdf-algorithm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_multi-hypergeometric-pdf (k n &key (algorithm :direct))
  "Probability of exactly $k_i$ balls of color $i$ drawn without replacement from a population with $n_i$ balls of color $i$

Note that $n$ and $k$ are vectors of length $c$.

Classical formula:
  $$\\prod_{i=1}^c\\frac{\\binom{n_i}{k_i}}{\\binom{N}{s}}$$ where $$N=\\sum_{j=1}^c n_i$$"
  (cond ((not (vectorp n))                 (error "mjr_probau_multi-hypergeometric-pdf: N must be a vector!"))
        ((notevery #'integerp n)           (error "mjr_probau_multi-hypergeometric-pdf: Every element of N must be an integer!"))
        ((some (lambda (x) (< x 0)) n)     (error "mjr_probau_multi-hypergeometric-pdf: all elements of N must be non-negative!"))
        ((every #'zerop n)                 (error "mjr_probau_multi-hypergeometric-pdf: N must contain non-zero elements!"))
        ((not (vectorp k))                 (error "mjr_probau_multi-hypergeometric-pdf: K must be a vector!"))
        ((notevery #'integerp k)           (error "mjr_probau_multi-hypergeometric-pdf: Every element of K must be an integer!"))
        ((some (lambda (x) (< x 0)) k)     (error "mjr_probau_multi-hypergeometric-pdf: all elements of K must be non-negative!"))
        ((every #'zerop k)                 (error "mjr_probau_multi-hypergeometric-pdf: K must contain at least one non-zero element!"))
        ((not (= (length k) (length n)))   (error "mjr_probau_multi-hypergeometric-pdf: K and N must be of the same length!"))
        ((not (equal algorithm :direct))   (error "mjr_probau_multi-hypergeometric-pdf: Unknown algorithm!")))
  (if (some #'> k n)
      0 ;; We tried to get more balls of some color than exist in the population
      (let* ((n-sum  (reduce #'+ n))
             (k-sum  (reduce #'+ k)))
        (/ (mjr_numu_prod :start 0 :end (1- (length k)) :seq-fun (lambda (i) (mjr_combe_comb (aref n i) (aref k i))))
           (mjr_combe_comb n-sum k-sum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probau_multinomial-pdf (k n &key (algorithm :direct))
  "Probability of exactly $k_i$ balls of color $i$ in $\\sum k_i$ draws WITH replacement from a population with $n_i$ balls of color $i$

Note that $n$ and $k$ are vectors of length $c$.

Classical formula:
  $$K \\prod_{i=1}^c \\frac{n_i^{k_i}}{N^{k_i}k_i!}$$ where $$K=\\sum_{i=1}^c k_i$$ and $$N=\\sum_{i=1}^c n_i$$"
  (cond ((not (vectorp n))               (error "mjr_probau_multinomial-pdf: N must be a vector!"))
        ((notevery #'integerp n)         (error "mjr_probau_multinomial-pdf: Every element of N must be an integer!"))
        ((some (lambda (x) (< x 0)) n)   (error "mjr_probau_multinomial-pdf: all elements of N must be non-negative!"))
        ((every #'zerop n)               (error "mjr_probau_multinomial-pdf: N must contain non-zero elements!"))
        ((not (vectorp k))               (error "mjr_probau_multinomial-pdf: K must be a vector!"))
        ((notevery #'integerp k)         (error "mjr_probau_multinomial-pdf: Every element of K must be an integer!"))
        ((some (lambda (x) (< x 0)) k)   (error "mjr_probau_multinomial-pdf: all elements of K must be non-negative!"))
        ((every #'zerop k)               (error "mjr_probau_multinomial-pdf: K must contain non-zero elements!"))
        ((not (= (length k) (length n))) (error "mjr_probau_multinomial-pdf: K and N must be of the same length!"))
        ((not (equal algorithm :direct)) (error "mjr_probau_multinomial-pdf: Unknown algorithm!")))
  (if (some #'> k n)
      0 ;; We tried to get more balls of some color than exist in the population
      (let* ((n-sum  (reduce #'+ n))
             (k-sum  (reduce #'+ k)))
        (* (mjr_combe_! k-sum)
           (mjr_numu_prod :start 0 :end (1- (length k)) :seq-fun (lambda (i) (/ (expt (/ (aref n i) n-sum) (aref k i))
                                                                                (mjr_combe_! (aref k i)))))))))
