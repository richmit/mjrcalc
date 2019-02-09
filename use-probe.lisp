;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-probe.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Empirical probability distriubtions.@EOL
;; @std       Common Lisp
;; @see       tst-probe.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1995,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_probe_ecwt2icdf: Check implimentation -- the (1+ i) is odd...@EOL@EOL
;; @todo      mjr_probe_ecwt2prng: Switch random to mjr_prng_random!!@EOL@EOL
;; @todo      mjr_probe_ecwt2prng: Add option to select random number function.@EOL@EOL
;; @todo      mjr_probe_ecfreq2prng: Switch random to mjr_prng_random!!@EOL@EOL
;; @todo      mjr_probe_ecfreq2prng: Add option to select random number function.@EOL@EOL
;; @todo      mjr_probe_ecwt2icdf: USE QMESH!!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PROBE
  (:USE :COMMON-LISP
        :MJR_PRNG
        :MJR_PWF)
  (:DOCUMENTATION "Brief: Empirical probability distriubtions.;")
  (:EXPORT #:mjr_probe_help
           #:mjr_probe_ewt2ecwt #:mjr_probe_ecwt2ewt
           #:mjr_probe_ewt2epdf #:mjr_probe_ecwt2ecdf
           #:mjr_probe_ewt2efreq #:mjr_probe_ecwt2ecfreq
           #:mjr_probe_data2efreq #:mjr_probe_ewt2data
           #:mjr_probe_data2prng #:mjr_probe_ewt2prng #:mjr_probe_ecwt2prng #:mjr_probe_efreq2prng #:mjr_probe_ecfreq2prng
           #:mjr_probe_ewt2pdf #:mjr_probe_ecwt2cdf #:mjr_probe_ecwt2icdf #:mjr_probe_pdf2epdf #:mjr_probe_cdf2ecdf
           #:mjr_probe_epdf-+ #:mjr_probe_epdf-e #:mjr_probe_epdf-v #:mjr_probe_epdf-moment #:mjr_probe_epdf-int* #:mjr_probe_epdf-*
           #:mjr_probe_dice2epdf #:mjr_probe_dice2prng
           ))

(in-package :MJR_PROBE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_help ()
  "Help for MJR_PROBE:

Empirical Random Variables

All random variables in this package are on $Z_n$, integers in $[0, n-1], and they are explicitly specified via data (i.e. empirically) in the form of
use-a.lisp vector or list that represents an EPDF, ECDF, EWT, ECWT, EFREQ, ECFREQ, or DATA for the random variable in question.  As an example, the random
variable for a fair, six sided die might be represented as:

                                     |--------+-------------------------------+-----------------|
                                     | form   | value for a fair, 6 sided die | Notes           |
                                     |--------+-------------------------------+-----------------|
                                     | EPDF   | #(0 1/6 1/6 1/6 1/6 1/6 1/6)  |                 |
                                     | ECDF   | #(0 1/6 1/3 1/2 2/3 5/6 1)    |                 |
                                     | EWT    | #(0 1/9 1/9 1/9 1/9 1/9 1/9)  | PDF/3           |
                                     | ECWT   | #(0 1/9 2/9 1/3 4/9 5/9 2/3)  | CPDF/3          |
                                     | EFREQ  | #(0 1   1   1   1   1   1)    | Minimal         |
                                     | EFREQ  | #(0 5   5   5   5   5   5)    | Not minimal     |
                                     | ECFREQ | #(0 1   2   3   4   5   6)    | Minimal         |
                                     | DATA   | #(1 2 3 4 5 6)                |                 |
                                     | DATA   | #(1 2 3 4 5 6 1 2 3 4 5 6)    | Each side twice |
                                     |--------+-------------------------------+-----------------|

Let $\\mathbf{X}$ be a random variable taking values in $\\mathbb{Z}_n$.

The \\emph{Probability Density Function (\\emph{\\texttt{PDF}}) for $\\mathbf{X}$} is defined as $P:\\mathbf{Z}_n\\rightarrow[0,1]\\subset\\mathbf{R}$, where
$P(i)=Prob(\\mathbf{X}=i)$.  The \\emph{Empirical Probability Density Function (\\emph{\\texttt{EPDF}}) for $\\mathbf{X}$} is not a function at all, but a
vector defined by $\\overline{P}=(p_i)_{i\\in\\mathbb{Z}_n}\\in\\mathbb{R}^n$ where $p_i=P(i)$.  Thus $\\overline{P}$ is little more than a table of values
for $P$, and so the word \\emph{empirical} is used to mean \\emph{given by data}.

The \\emph{Cumulative Probability Density Function (\\emph{\\texttt{CDF}}) for $\\mathbf{X}$} , is defined as
$C:[0,n-1]\\subset\\mathbf{R}\\rightarrow[0,1]\\subset\\mathbf{R}$, where $C(x)=Prob(\\mathbf{X}\\le x)$.  The \\emph{Empirical Cumulative Probability
Function for $\\mathbf{X}$} (\\emph{\\texttt{ECDF}}) not directly defined in terms of the CDF, $C$; rather it is defined in terms of the EPDF,
$\\overline{P}$.  The ECDF is a vector defined by $\\overline{C}=(c_i)_{i\\in\\mathbb{Z}_n}\\in\\mathbb{R}^n$ where $c_i=\\sum_{j=0}^i p_j$.  This definition
implies that $c_i=C(i)$ for any $i\\in\\mathbb{Z}_n$ and that $C(x)=c_{\\lfloor x\\rfloor}$ for any $x\\in\\mathbb{R}$

We call any $\\overline{W}_G=(w_i)_{i\\in\\mathbb{Z}_n}\\in\\mathbb{R}^n$ where $w_i=G\\cdot p_i$ for some positive, constant
$G\\in\\mathbb{R}$ an \\emph{Empirical Weight for $\\mathbf{X}$} (\\emph{\\texttt{EWTF}}).  Note that
$\\overline{P}=\\overline{W}_1$.  When $G\\cdot p_i\\in\\mathbb{Z}$ for all $i\\in\\mathbb{Z}_n$, we call $\\overline{W}_G$ an
\\emph{Empirical Frequency Function for $\\mathbf{X}$} (\\emph{\\texttt{EFREQ}}).  We leave off the subscript when it is understood
from the context, and often use $\\overline{F}$ for a frequency instead of $\\overline{W}$.  When $\\mathrm{GCD}(\\overline{F})=1$,
we say that $\\overline{F}$ is \\emph{minimal}. Let $\\overline{S}=(s_i)_{i\\in\\mathbb{Z}_n}\\in\\mathbb{R}^n$ where
$s_i=\\sum_{j=0}^i w_j$.  We call $\\overline{S}$ the \\emph{Empirical Cumulative Weight Function for
$\\overline{W}$} (\\emph{\\texttt{ECWTF}}).

Let $\\overline{F}=\\overline{W}_G$ be an $\\texttt{EFREQ}$ and $\\overline{D}\\in\\mathbb{Z}_n^G$ such that $i\\in\\mathbb{Z}_n$ appears in $\\overline{D}$
precisely $f_i=w_i$ times for all $i\\in\\mathbb{Z}_n$.  We call $\\overline{D}$ a realization of the \\texttt{EFREQ} $\\overline{W}_G$.  If $d_i\\le d_j$ for
all $i,j\\in\\mathbb{Z}_n$ with $i\\le j$, then we say $\\overline{D}$ is \\emph{ordered}.  If $\\overline{F}$ is minimal, then we say $\\overline{D}$ is
\\emph{minimal} too.  The minimal, ordered realization for an $\\texttt{EFREQ}$ is unique.

It is frequently the case that we start with $\\overline{D}$, compute an \\texttt{EFREQ} $\\overline{F}$ from it, and then assume $\\mathbf{X}$ to be DEFINED
by $\\overline{F}$ -- i.e. we use the histogram from real data to approximate the theoretical distribution from which the data were drawn.  We can also go the
other way, i.e. construct $\\overline{D}$ given $\\overline{F}$.  Using the statistical package R, one might use something like:
\\verb+rep(1:length(F)-1,F)+.

Functions provided:

  * Convert between density-type and cumulative-type .. mjr_probe_ewt2ecwt mjr_probe_ecwt2ewt
  * Make density-type sum=1 & cumulative-type max=1 ... mjr_probe_ewt2epdf mjr_probe_ecwt2ecdf
  * Make FREQs from other objects ..................... mjr_probe_ewt2efreq mjr_probe_ecwt2ecfreq mjr_probe_data2efreq
  * Generate data ..................................... mjr_probe_ewt2data
  * PRNGs ............................................. mjr_probe_data2prng mjr_probe_ecwt2prng mjr_probe_ewt2prng
  * Tools for :MJR_PROBU compatibility ................ mjr_probe_ewt2pdf mjr_probe_ecwt2cdf mjr_probe_pdf2epdf mjr_probe_cdf2ecdf
  * Mathematical manipulation of random variables ..... mjr_probe_epdf-+ mjr_probe_epdf-e mjr_probe_epdf-v mjr_probe_epdf-moment
  * Other stuff ....................................... mjr_probe_dice2epdf mjr_probe_dice2prng
"
  (documentation 'mjr_probe_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Empirical object conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2ecwt (ewt)
  "Compute ECWT from EWT.  The type of the result (vector or list) is the same as EWT."
  (typecase ewt
    (list    (loop with sum = 0
                   for v in ewt
                   collect (incf sum v)))
    (vector  (make-array (length ewt) :initial-contents (loop with sum = 0
                                                              for v across ewt
                                                              collect (incf sum v))))
    (t       (error "mjr_probe_ewt2ecwt: EWT must be a list or vector"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2ewt (ecwt)
  "Compute EWT from ECWT.  The type of the result (vector or list) is the same as ECWT."
  (typecase ecwt
    (list    (loop for sum = 0 then v
                   for v in ecwt
                   collect (- v sum)))
    (vector  (make-array (length ecwt) :initial-contents (loop for sum = 0 then v
                                                               for v across ecwt
                                                               collect (- v sum))))
    (t       (error "mjr_probe_ecwt2ewt: ECWT must be a list or vector"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2epdf (ewt)
  "Compute EPDF from EWT.  The type of the result (vector or list) is the same as EWT."
  (let ((sum (reduce #'+ ewt)))
    (map (type-of ewt) (lambda (x) (/ x sum)) ewt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2ecdf (ecwt)
  "Compute EPDF from ECWT.  The type of the result (vector or list) is the same as ECWT."
  (let ((sum (elt ecwt (1- (length ecwt)))))
    (map (type-of ecwt) (lambda (x) (/ x sum)) ecwt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2efreq (ewt)
  "Compute minimal EFREQ from EWT.  The type of the result (vector or list) is the same as EWT."
  (let* ((p1 (map 'list #'rationalize ewt))
         (L  (apply #'lcm (mapcar #'denominator p1)))
         (p2 (mapcar (lambda (x) (* x L)) p1))
         (G  (apply #'gcd p2))
         (p3 (map (type-of ewt) (lambda (x) (/ x G)) p2)))
    p3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2ecfreq (ecwt)
  "Compute minimal EFREQ from ECWT.  The type of the result (vector or list) is the same as ECWT."
  (mjr_probe_ewt2efreq ecwt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_data2efreq (data)
  "Compute minimal EFREQ from DATA.  The type of the result (vector or list) is the same as DATA."
  (let ((freq (make-array (1+ (reduce #'max data)) :initial-element 0)))
    (typecase data
      (list    (loop for v in data
                     do (incf (aref freq v))))
      (vector  (loop for v across data
                     do (incf (aref freq v))))
      (t       (error "mjr_probe_data2efreq: DATA must be a list or vector")))
    (mjr_probe_ewt2efreq freq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2data (ewt)
  "Compute minimal, ordered data set from EWT.  The type of the result (vector or list) is the same as EWT."
  (let ((ewt (mjr_probe_ewt2efreq ewt)))
    (typecase ewt
      (list    (loop for i from 0
                     for v in  ewt
                     append (loop for j from 1 upto v
                                  collect i)))
      (vector  (let* ((sum  (reduce #'+ ewt))
                      (data (make-array sum)))
                 (loop with k = 0
                       for i from 0
                       for v across ewt
                       do (loop for j from 1 upto v
                                do (setf (aref data k) i)
                                do (incf k)))
                 data))
      (t       (error "mjr_probe_ewt2data: EWT be a list or vector")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probu section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2pdf (ewt)
  "Return a PDF function (use-a.lisp function) as used by the :MJR_PROBU package."
  (let* ((epdf (mjr_probe_ewt2epdf ewt))
         (len  (length epdf)))
    (eval `(lambda (x)
             (if (and (>= x 0) (< x ,len))
                 (aref ,epdf x)
                 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2cdf (ecwt)
  "Return a CDF function (use-a.lisp function) as used by the :MJR_PROBU package."
  (let* ((ecdf (mjr_probe_ecwt2ecdf ecwt))
         (len  (length ecdf)))
    (eval `(lambda (x)
             (if (and (>= x 0) (< x ,len))
                 (aref ,ecdf x)
                 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_pdf2epdf (min-x max-x pdf-func &rest rest)
  "Return an EPDF given a PDF (as used by the :MJR_PROBU package) and its range."
  (let ((pdf (make-array (1+ max-x) :element-type 'number :initial-element 0)))
    (loop for x from min-x upto max-x
          do (setf (aref pdf x) (apply pdf-func x rest)))
    pdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_cdf2ecdf (min-x max-x cdf-func &rest rest)
  "Return an ECDF given a CDF (as used by the :MJR_PROBU package) and its range."
  (let ((cdf (make-array (1+ max-x) :element-type 'number :initial-element 0)))
    (loop for x from min-x upto max-x
          do (setf (aref cdf x) (apply cdf-func x rest)))
    cdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2icdf (ecwt)
  "Return a function that computes the ICDF (i.e. icdf(r)=PROB(X<=r)."
  (let ((ecdf (mjr_probe_ecwt2ecdf (map 'vector (lambda (x) (float x 1.0d0)) ecwt))))
    (eval `(lambda (r)
             (let ((i (mjr_pwf_search-interval-mesh :interval-type-left-closed ,ecdf r)))
               (case i
                 (-1        0)
                 (-2        ,(1- (length ecdf)))
                 (otherwise (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prng section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecwt2prng (ecwt)
  "Return a PRNG on $Z_n$ given an ECWT"
  (eval `(lambda () (funcall ,(mjr_probe_ecwt2icdf ecwt) (random 1.0d0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ewt2prng (ewt)
  "Return a PRNG on $Z_n$ given an EWT."
  (mjr_probe_ecwt2prng (mjr_probe_ewt2ecwt ewt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_ecfreq2prng (ecfreq)
  "Return a PRNG on $Z_n$ given an ECFREQ."
  (if (listp ecfreq)
      (mjr_probe_ecfreq2prng (concatenate 'array ecfreq))
      (let* ((ecfreq (mjr_probe_ecwt2ecfreq ecfreq))  ;; Reduce to a minimal FREQ
             (len    (length ecfreq))
             (tot    (aref ecfreq (1- len))))
        (eval `(lambda ()
                 (let ((r (random ,tot)))
                   (loop with lo = 0
                         with up = ,(1- len)
                         for g = (truncate (+ lo up) 2)
                         do (if (= lo up)
                                (return g))
                         do (if (> r (aref ,ecfreq g))
                                (if (= g lo)
                                    (return up)
                                    (setf lo g))
                                (if (<= r (if (zerop g) 0 (aref ,ecfreq (1- g))))
                                    (setf up g)
                                    (return g))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_efreq2prng (efreq)
  "Return a PRNG on $Z_n$ given an EFREQ."
  (mjr_probe_ecfreq2prng (mjr_probe_ewt2ecwt efreq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_data2prng (DATA)
  "Return a PRNG returning values from DATA -- i.e. sampling with replacement.  DATA should be an array for best performance."
  (if (listp data)
      (mjr_probe_data2prng (concatenate 'array data))
      (eval `(lambda ()
               (aref ,data (random ,(length data)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EPDFs as mathematical objects to be manipulated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-+ (&rest rest)
  "Return the sum of the given EPDFs.  Result is always an array.  Use arrays for arguments for best performance."
  (if (every #'arrayp rest)
      (let ((len (length rest)))
        (cond ((= len 1)   (first rest))
              ((<= len 2)  (let* ((pdf1 (first rest))
                                  (pdf2 (second rest))
                                  (len1 (length pdf1))
                                  (len2 (length pdf2))
                                  (pdf3 (make-array (1- (+ len1 len2)) :initial-element 0)))
                             (loop for i1 from 0
                                   for p1 across pdf1
                                   do (loop for i2 from 0
                                            for p2 across pdf2
                                            do (incf (aref pdf3 (+ i1 i2)) (* p1 p2))))
                             pdf3))
              ('t          (reduce #'mjr_probe_epdf-+ rest))))
      (apply #'mjr_probe_epdf-+ (mapcar (lambda (x) (concatenate 'vector x)) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-int* (epdf pos-int)
  "Compute product of the EPDF and POS-INT

Uses the right-to-left binary algorithm similar to the 'exponentiation by squaring' algorithm for exponentiation.

References:
  Bruce Schneier (1996); Applied Cryptography: Protocols, Algorithms, and Source Code in C 2nd; ISBN: 978-0471117094; pp224
  David Bressoud (1989); Factorization and primality testing; ISBN: 0-387-97040-1; pp33-34"
  (let ((result #(1)))
    (loop while (not (zerop pos-int)) ;; == (> pos-int 0)
          do (if (logbitp 0 pos-int)  ;; is least significant bit 1? == (not (zerop (logand pos-int 1))) == (= (logand pos-int 1) 1)
                 (setq result (mjr_probe_epdf-+ result epdf)))
          do (setq pos-int (ash pos-int -1)
                   epdf    (mjr_probe_epdf-+ epdf epdf)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-* (&rest rest)
  "Return the product of the given EPDFs.  Result is always an array.  Use arrays for arguments for best performance."
  (if (every #'arrayp rest)
      (let ((len (length rest)))
        (cond ((= len 1)   (first rest))
              ((<= len 2)  (let* ((pdf1 (first rest))
                                  (pdf2 (second rest))
                                  (len1 (length pdf1))
                                  (len2 (length pdf2))
                                  (pdf3 (make-array (1- (* len1 len2)) :initial-element 0)))
                             (loop for i1 from 0
                                   for p1 across pdf1
                                   do (loop for i2 from 0
                                            for p2 across pdf2
                                            do (incf (aref pdf3 (* i1 i2)) (* p1 p2))))
                             pdf3))
              ('t          (reduce #'mjr_probe_epdf-* rest))))
      (apply #'mjr_probe_epdf-* (mapcar (lambda (x) (concatenate 'vector x)) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-moment (epdf n)
  "N-th moment of of EPDF."
  (typecase epdf
    (list    (loop for i from 0
                   for p in epdf
                   sum (* (expt i n) p)))
    (vector  (loop for i from 0
                   for p across epdf
                   sum (* (expt i n) p)))
    (t       (error "mjr_probe_epdf-e: EPDF be a list or vector"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-e (epdf)
  "Expected value of EPDF -- i.e. the 1st moment of EPDF."
  (mjr_probe_epdf-moment epdf 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_epdf-v (epdf)
  "Variance of EPDF."
  (- (mjr_probe_epdf-moment epdf 2)
     (expt (mjr_probe_epdf-moment epdf 1) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_dice2epdf (&rest rest)
  "Dice"
  (apply #'mjr_probe_epdf-+
         (loop for c in rest
               collect (loop for i from 0 upto c
                             collect (if (zerop i) 0 (/ c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_probe_dice2prng (&rest rest)
  "Dice"
  (mjr_probe_data2prng (mjr_probe_ewt2data (apply #'mjr_probe_dice2epdf rest))))

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_probe_hypergeometric2epdf (n m s)
;;   (mjr_probe_pdf2epdf 0 (min n s) #'mjr_probau_hypergeometric-pdf n m s))

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (defun mjr_probe_negative-hypergeometric2epdf (n m r)
;;   (mjr_probe_pdf2epdf 0 m #'mjr_probau_negative-hypergeometric-pdf n m r))
