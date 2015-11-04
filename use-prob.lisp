;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-prob.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2010,2011,2012 by Mitch Richling.  All rights reserved.
;; @brief     Augments and supports :mjr_probau.@EOL
;; @Keywords  lisp interactive probability distributions math library
;; @Std       Common Lisp
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_PROB
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_COMBE
        :MJR_CMP
        :MJR_CHK
        :MJR_PRNG
        :MJR_PROBU)
  (:DOCUMENTATION "Brief: Augments and supports :MJR_PROBAU.;")
  (:EXPORT #:mjr_prob_help
           #:mjr_prob_exponential-pdf        #:mjr_prob_exponential-cdf        #:mjr_prob_exponential-ccdf       #:mjr_prob_exponential-icdf #:mjr_prob_exponential-prng             
           #:mjr_prob_std-normal-pdf         #:mjr_prob_std-normal-cdf         #:mjr_prob_std-normal-ccdf        #:mjr_prob_std-normal-icdf  #:mjr_prob_std-normal-prng              
           #:mjr_prob_normal-pdf             #:mjr_prob_normal-cdf             #:mjr_prob_normal-ccdf                                        #:mjr_prob_normal-prng
           #:mjr_prob_poisson-pdf            #:mjr_prob_poisson-cdf            #:mjr_prob_poisson-ccdf                                       #:mjr_prob_poisson-prng
           #:mjr_prob_bernoulli-pdf          #:mjr_prob_bernoulli-cdf          #:mjr_prob_bernoulli-ccdf                                     #:mjr_prob_bernoulli-prng
           #:mjr_prob_binomial-pdf           #:mjr_prob_binomial-cdf           #:mjr_prob_binomial-ccdf                                      #:mjr_prob_binomial-prng
           #:mjr_prob_geometric-pdf          #:mjr_prob_geometric-cdf          #:mjr_prob_geometric-ccdf                                     #:mjr_prob_geometric-prng
           #:mjr_prob_negative-binomial-pdf  #:mjr_prob_negative-binomial-cdf  #:mjr_prob_negative-binomial-ccdf                             #:mjr_prob_negative-binomial-prng
           ))

(in-package :MJR_PROB)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_help ()
"Help for MJR_PROB:

See :MJR_PROBU for some vocabulary and notation used in this package.

While this package provides some genuinely useful PDFs; The primary point of this package is to both augment and
support :MJR_PROBAU.

It augments it by providing traditionally parametrized versions of some of the PDFs (bernoulli, binomial, geometric,
negative-binomial) in :MJR_PROBAU, and it supports it by providing PDFs (exponential, std-normal, normal, & poisson) useful for
approximating the PDFs in :MJR_PROBAU."
  (documentation 'mjr_probu_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_exponential-pdf (x mu &key (algorithm :direct))
  "PDF: $\mu e^{-\mu x}$"
  (cond ((not (numberp x))               (error "mjr_prob_exponential-pdf: x must be a number!"))
        ((complexp x)                    (error "mjr_prob_exponential-pdf: x must be real (i.e. not complex)!"))
        ((not (numberp mu))              (error "mjr_prob_exponential-pdf: mu must be a number!"))
        ((complexp mu)                   (error "mjr_prob_exponential-pdf: mu must be real (i.e. not complex)!"))
        ((<= mu 0)                       (error "mjr_prob_exponential-pdf: mu must be greater than 0!"))
        ((not (equal algorithm :direct)) (error "mjr_prob_exponential-pdf: Unknown algorithm")))
  (if (< x 0)
      0
      (* mu (exp (* -1 mu x)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_exponential-cdf (x mu &key (algorithm :direct))
  "CDF: $1-e^{-\mu x}$"
  (cond ((not (numberp x))               (error "mjr_prob_exponential-cdf: x must be a number!"))
        ((complexp x)                    (error "mjr_prob_exponential-cdf: x must be real (i.e. not complex)!"))
        ((not (numberp mu))              (error "mjr_prob_exponential-cdf: mu must be a number!"))
        ((complexp mu)                   (error "mjr_prob_exponential-cdf: mu must be real (i.e. not complex)!"))
        ((<= mu 0)                       (error "mjr_prob_exponential-cdf: mu must be greater than 0!"))
        ((not (equal algorithm :direct)) (error "mjr_prob_exponential-cdf: Unknown algorithm")))
  (- 1 (exp (* -1 mu x))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_exponential-ccdf (x mu &key (algorithm :direct))
  (cond ((not (numberp x))               (error "mjr_prob_exponential-ccdf: x must be a number!"))
        ((complexp x)                    (error "mjr_prob_exponential-ccdf: x must be real (i.e. not complex)!"))
        ((not (numberp mu))              (error "mjr_prob_exponential-ccdf: mu must be a number!"))
        ((complexp mu)                   (error "mjr_prob_exponential-ccdf: mu must be real (i.e. not complex)!"))
        ((<= mu 0)                       (error "mjr_prob_exponential-ccdf: mu must be greater than 0!"))
        ((not (equal algorithm :direct)) (error "mjr_prob_exponential-ccdf: Unknown algorithm")))
  (- 1 (mjr_prob_exponential-cdf x mu)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_exponential-icdf (p mu &key (algorithm :direct))
  "ICDF: $\frac{1}{\mu} \ln\left(\frac{-1}{p-1}\right)$"
  (cond ((not (numberp p))               (error "mjr_prob_exponential-icdf: p must be a number!"))
        ((complexp p)                    (error "mjr_prob_exponential-icdf: p must be real (i.e. not complex)!"))
        ((> p 1)                         (error "mjr_prob_exponential-icdf: p must be less or equal to 1!"))
        ((< p 0)                         (error "mjr_prob_exponential-icdf: p must be greater than or equal to 0!"))
        ((not (numberp mu))              (error "mjr_prob_exponential-icdf: mu must be a number!"))
        ((complexp mu)                   (error "mjr_prob_exponential-icdf: mu must be real (i.e. not complex)!"))
        ((<= mu 0)                       (error "mjr_prob_exponential-icdf: mu must be greater than 0!"))
        ((not (equal algorithm :direct)) (error "mjr_prob_exponential-icdf: Unknown algorithm")))
  (/ (log (/ -1 (- p 1))) mu))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_exponential-prng (mu &key (algorithm :icdf))
  (cond ((not (numberp mu))              (error "mjr_prob_exponential-prng: mu must be a number!"))
        ((complexp mu)                   (error "mjr_prob_exponential-prng: mu must be real (i.e. not complex)!"))
        ((<= mu 0)                       (error "mjr_prob_exponential-prng: mu must be greater than 0!"))
        ((not (equal algorithm :icdf))   (error "mjr_prob_exponential-prng: Unknown algorithm")))
  (mjr_prob_exponential-icdf (mjr_prng_float-oo 0.0 1.0) mu))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_std-normal-pdf (x &key (algorithm :direct))
  "Standard Normal PDF (return is always DOUBLE-FLOAT)

When using IEEE double arithmetic, this function returns zero if x<=-39 or if x>=39.  After x<=-5 or x>=5, the returns
are no longer recognized as non-zero by mjr_chk_!=0 using the default epsilon.

The :ALGORITHM argument must be:
  :DIRECT -- use the classical formula.

Classical formula:
  $$\frac{e^{\left(\frac{x^2}{-2}\right)}}{\sqrt{2\pi}}$$"
  (cond ((not (numberp x))               (error "mjr_prob_std-normal-pdf: X must be a number!"))
        ((complexp x)                    (error "mjr_prob_std-normal-pdf: X must be real (i.e. not complex)!"))
        ((not (equal algorithm :direct)) (error "mjr_prob_std-normal-pdf: Unknown algorithm")))
  (let ((x (float x 1.0d0)))
    (/ (exp (* x x -1/2)) (sqrt (* 2 pi)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_std-normal-cdf (x &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp x))                (error "mjr_prob_std-normal-cdf: X must be a number!"))
        ((complexp x)                     (error "mjr_prob_std-normal-cdf: X must be real (i.e. not complex)!"))
        ((not (equal algorithm :pdf2cdf)) (error "mjr_prob_std-normal-cdf: Unknown algorithm")))
  (mjr_probu_pdf2cdf x -6 6 #'mjr_prob_std-normal-pdf nil :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_std-normal-ccdf (x &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp x))                 (error "mjr_prob_std-normal-ccdf: X must be a number!"))
        ((complexp x)                      (error "mjr_prob_std-normal-ccdf: X must be real (i.e. not complex)!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_std-normal-ccdf: Unknown algorithm")))
  (mjr_probu_pdf2ccdf x -6 6 #'mjr_prob_std-normal-pdf nil :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_std-normal-icdf (p &key (algorithm :wichura))
  "Inverse CDF

:algorithm is one of
  * :wichura -- Accurate 1 part in 10^16

References:
  Michael Wichura (1988); The Percentage Points of the Normal Distribution; Applied Statistics; Vol 37, Num 3, Pages 477-484; Algorithm AS 241"
  (cond ((not (numberp p))                (error "mjr_prob_std-normal-icdf: P must be a number!"))
        ((complexp p)                     (error "mjr_prob_std-normal-icdf: P must be real (i.e. not complex)!"))
        ((> p 1)                          (error "mjr_prob_std-normal-icdf: P must be less or equal to 1!"))
        ((< p 0)                          (error "mjr_prob_std-normal-icdf: P must be greater than or equal to 0!"))
        ((not (equal algorithm :wichura)) (error "mjr_prob_std-normal-icdf: Unknown algorithm")))
  (let* ((const1 0.180625d+0)
         (const2 1.6d+0)
         (split1 0.425d+00)
         (split2 5.0d+0)
         (q      (- p 0.5d+0)))
    (let ((t1 (- const1 (* q q))))
      (if (<= (abs q) split1)
          (let ((r  t1)
                (a0 3.3871328727963666080d+0)
                (a1 1.3314166789178437745d+2)
                (a2 1.9715909503065514427d+3)
                (a3 1.3731693765509461125d+4)
                (a4 4.5921953931549871457d+4)
                (a5 6.7265770927008700853d+4)
                (a6 3.3430575583588128105d+4)
                (a7 2.5090809287301226727d+3)
                (b0 1.0000000000000000000d+0)
                (b1 4.2313330701600911252d+1)
                (b2 6.8718700749205790830d+2)
                (b3 5.3941960214247511077d+3)
                (b4 2.1213794301586595867d+4)
                (b5 3.9307895800092710610d+4)
                (b6 2.8729085735721942674d+4)
                (b7 5.2264952788528545610d+3))            
            (/ (* q (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* a7 r) a6) r) a5) r) a4) r) a3) r) a2) r) a1) r) a0))
               (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* b7 r) b6) r) b5) r) b4) r) b3) r) b2) r) b1) r) b0)))
          (let ((t2 (if (< q 0) p (- 1 p))))
            (if (<= t2 0)
                (error "ERROR: INFINITY"))
            (let ((t3 (sqrt (- (log t2))))
                  (t4 (if (< q 0) -1.0d0 1.0d0)))
              (if (<= t3 split2)
                  (let ((r  (- t3 const2))
                        (c0 1.42343711074968357734d+0)
                        (c1 4.63033784615654529590d+0)
                        (c2 5.76949722146069140550d+0)
                        (c3 3.64784832476320460504d+0)
                        (c4 1.27045825245236838258d+0)
                        (c5 2.41780725177450611770d-1)
                        (c6 2.27238449892691845833d-2)
                        (c7 7.74545014278341407640d-4)
                        (d0 1.00000000000000000000d+0)
                        (d1 2.05319162663775882187d+0)
                        (d2 1.67638483018380384940d+0)
                        (d3 6.89767334985100004550d-1)
                        (d4 1.48103976427480074590d-1)
                        (d5 1.51986665636164571966d-2)
                        (d6 5.47593808499534494600d-4)
                        (d7 1.05075007164441684324d-9))
                    (* t4 (/ (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* c7 r) c6) r) c5) r) c4) r) c3) r) c2) r) c1) r) c0)
                             (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* d7 r) d6) r) d5) r) d4) r) d3) r) d2) r) d1) r) d0))))
                  (let ((r   (- t3 split2))
                        (e0  6.65790464350110377720d+0)
                        (e1  5.46378491116411436990d+0)
                        (e2  1.78482653991729133580d+0)
                        (e3  2.96560571828504891230d-1)
                        (e4  2.65321895265761230930d-2)
                        (e5  1.24266094738807843860d-3)
                        (e6  2.71155556874348757815d-5)
                        (e7  2.01033439929228813265d-7)
                        (f0  1.00000000000000000000d+0)
                        (f1  5.99832206555887937690d-1)
                        (f2  1.36929880922735805310d-1)
                        (f3  1.48753612908506148525d-2)
                        (f4  7.86869131145613259100d-4)
                        (f5  1.84631831751005468180d-5)
                        (f6  1.42151175831644588870d-7)
                        (f7  2.04426310338993978564d-15))
                    (* t4 (/ (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* e7 r) e6) r) e5) r) e4) r) e3) r) e2) r) e1) r) e0)
                             (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* f7 r) f6) r) f5) r) f4) r) f3) r) f2) r) f1) r) f0)))))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_std-normal-prng (&key (pdf-algorithm :direct) (algorithm :box-muller))
  "Return a pseudo-random number from the standard normal distribution (mean=0 & variance=1)

The :ALGORITHM argument must be:
 * :box-muller    -- 1.0x -- Box-Muller method with cosine. [Box & Muller (1958)] (DEFAULT)
 * :box-mullers   -- 1.0x -- Box-Muller method with sine. [Box & Muller (1958)]
 * :box-mullercs  -- 2.0x -- Box-Muller method with average of cosine & sine [Box & Muller (1958)]
 * :box-mullerp   -- 3.1x -- Box-Muller polar method with averaging.  More stable. [Bell (1968) & Knop (1969)]
 * :box-mullere   -- 2.7x -- Box-Muller exponential method
 * :toms-712      -- 2.6x -- TOMS 712.  Better accuracy, but slower
 * :icdf          -- 4.7x -- Inverse CDF algorithm
 * :accept-reject -- N/A  -- Only returns values in [-6, 6], and is a bit slow
References:
  Box & Muller (1958); A note on the generation of random normal deviates; Annals Math. Stat; Vol 29; pp. 610-611
  Bell (1968); Normal random deviates; Comm. ACM; Algorithm 334
  Knop (1969); Remark on Algorithm 334: normal random deviates; Comm. ACM
  Kinderman & Monahan (1977); ACM Transactions on Mathematical Software; Vol 3 No. 3
  Joseph L. Leva (1992); ACM Transactions on Mathematical Software; Vol 18 No. 4; pp 434-435; TOMS-712"
  (cond ((not (symbolp algorithm)) (error "mjr_prob_std-normal-prng: :ALGORITHM must be a symbol!")))
  (case algorithm
    (:box-muller     (* (sqrt (* -2 (log (mjr_prng_float-co 0.0d0 1.0d0)))) (cos (* 2 pi (random 1.0)))))
    (:icdf           (mjr_prob_std-normal-icdf (mjr_prng_float-oo 0 1)))
    (:toms-712       (loop for urand1 = (mjr_prng_float-co 0.0d0 1.0d0)
                           for urand2 = (* 1.7156 (- (mjr_prng_float-co 0.0d0 1.0d0) 0.5))
                           for x1     = (- urand1 0.449871)
                           for x2     = (- (abs urand2) -0.386595)
                           for qcrv   = (+ (* x1 x1) (* x2 (- (* 0.196 x2) (* 0.25472 x1))))
                           finally (return (/ urand2 urand1))
                           until (< qcrv 0.27597)
                           while (or (> qcrv 0.27846)
                                     (> (* urand2 urand2) (* -4.0 (exp urand1) urand1 urand1)))))
    (:box-mullers    (* (sqrt (* -2 (log (mjr_prng_float-co 0.0d0 1.0d0)))) (sin (* 2 pi (random 1.0)))))
    (:box-mullercs   (let* ((u1 (mjr_prng_float-co 0.0d0 1.0d0))
                            (u2 (mjr_prng_float-co 0.0d0 1.0d0))
                            (t1 (sqrt (* -2 (log u1))))
                            (t2 (* 2 pi u2)))
                       (/ (+ (* t1 (cos t2)) (* t1 (sin t2))) (sqrt 2))))
    (:box-mullerp    (loop for u1   = (mjr_prng_float-cc -1 1)
                           for u2   = (mjr_prng_float-cc -1 1)
                           for u1u2 = (+ (* u1 u1) (* u2 u2))
                           do (if (and (< u1u2 1) (> u1u2 0))
                                  (return (let* ((t1 (sqrt (* -2 (log u1u2) (/ u1u2))))
                                                 (x1 (* t1 u1))
                                                 (x2 (* t1 u2)))
                                            (/ (+ x1 x2) (sqrt 2)))))))
    (:box-mullere    (loop for y1 = (mjr_prob_exponential-prng 1)
                           for y2 = (mjr_prob_exponential-prng 1)
                           do (if (> y2 (/ (expt (- 1 y1) 2) 2))
                                  (return (if (mjr_prng_boolean)
                                              y1
                                              (- y1))))))
    (:accept-reject  (mjr_probu_pdf2prng -6 6 #'mjr_prob_std-normal-pdf nil :algorithm pdf-algorithm))
    (otherwise       (error "mjr_prob_std-normal-prng: Unknown algorithm"))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_normal-pdf (x mean variance &key (algorithm :direct))
  "Normal PDF (return is always DOUBLE-FLOAT)

NOTE: variance = standard deviation squared

Classical formula:
  $$\frac{e^{\left(\frac{(x-\mu)^2}{-2\sigma^2}\right)}}{\sqrt{2\pi\sigma^2}}$$ where $\mathrm{variance}=\sigma^2$ and $\mathrm{mean}=\mu$"
  (cond ((not (numberp mean))              (error "mjr_prob_normal-pdf: MEAN must be a number!"))
        ((complexp mean)                   (error "mjr_prob_normal-pdf: MEAN must be real (i.e. not complex)!"))
        ((not (numberp variance))          (error "mjr_prob_normal-pdf: VARIANCE must be a number!"))
        ((complexp variance)               (error "mjr_prob_normal-pdf: VARIANCE must be real (i.e. not complex)!"))
        ((not (numberp x))                 (error "mjr_prob_normal-pdf: X must be a number!"))
        ((complexp x)                      (error "mjr_prob_normal-pdf: X must be real (i.e. not complex)!"))
        ((< variance 0)                    (error "mjr_prob_normal-pdf: VARIANCE must be non-negative!"))
        ((not (equal algorithm :direct))   (error "mjr_prob_normal-pdf: Unknown algorithm"))
        ((mjr_cmp_=0 variance 0)           (warn  "mjr_prob_normal-pdf: VARIANCE of zero is silly!")))
  (let ((x         (float x        1.0d0))
        (mean      (float mean     1.0d0))
        (variance  (float variance 1.0d0)))
    (/ (exp (/ (expt (- x mean) 2) (* -2 variance))) (sqrt (* 2 pi variance)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_normal-cdf (x mean variance &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp mean))              (error "mjr_prob_normal-cdf: MEAN must be a number!"))
        ((complexp mean)                   (error "mjr_prob_normal-cdf: MEAN must be real (i.e. not complex)!"))
        ((not (numberp variance))          (error "mjr_prob_normal-cdf: VARIANCE must be a number!"))
        ((complexp variance)               (error "mjr_prob_normal-cdf: VARIANCE must be real (i.e. not complex)!"))
        ((not (numberp x))                 (error "mjr_prob_normal-cdf: X must be a number!"))
        ((complexp x)                      (error "mjr_prob_normal-cdf: X must be real (i.e. not complex)!"))
        ((< variance 0)                    (error "mjr_prob_normal-cdf: VARIANCE must be non-negative!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_normal-cdf: Unknown algorithm"))
        ((mjr_cmp_=0 variance 0)           (warn  "mjr_prob_normal-cdf: VARIANCE of zero is silly!")))
  (mjr_probu_pdf2cdf x (- mean (* 6 variance)) (+ mean (* 6 variance)) #'mjr_prob_normal-pdf nil mean variance :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_normal-ccdf (x mean variance &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp mean))              (error "mjr_prob_normal-ccdf: MEAN must be a number!"))
        ((complexp mean)                   (error "mjr_prob_normal-ccdf: MEAN must be real (i.e. not complex)!"))
        ((not (numberp variance))          (error "mjr_prob_normal-ccdf: VARIANCE must be a number!"))
        ((complexp variance)               (error "mjr_prob_normal-ccdf: VARIANCE must be real (i.e. not complex)!"))
        ((not (numberp x))                 (error "mjr_prob_normal-ccdf: X must be a number!"))
        ((complexp x)                      (error "mjr_prob_normal-ccdf: X must be real (i.e. not complex)!"))
        ((< variance 0)                    (error "mjr_prob_normal-ccdf: VARIANCE must be non-negative!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_normal-ccdf: Unknown algorithm"))
        ((mjr_cmp_=0 variance 0)           (warn  "mjr_prob_normal-ccdf: VARIANCE of zero is silly!")))
  (mjr_probu_pdf2ccdf x (- mean (* 6 variance)) (+ mean (* 6 variance)) #'mjr_prob_normal-pdf nil mean variance :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_normal-prng (mean variance &key (pdf-algorithm :direct) (algorithm :accept-reject) (std-normal-algorithm :box-muller))
  (cond ((not (numberp mean))              (error "mjr_prob_normal-prng: MEAN must be a number!"))
        ((complexp mean)                   (error "mjr_prob_normal-prng: MEAN must be real (i.e. not complex)!"))
        ((not (numberp variance))          (error "mjr_prob_normal-prng: VARIANCE must be a number!"))
        ((complexp variance)               (error "mjr_prob_normal-prng: VARIANCE must be real (i.e. not complex)!"))
        ((< variance 0)                    (error "mjr_prob_normal-prng: VARIANCE must be non-negative!"))
        ((mjr_cmp_=0 variance 0)           (warn  "mjr_prob_normal-prng: VARIANCE of zero is silly!")))
  (case algorithm
    (:accept-reject (mjr_probu_pdf2prng (- mean (* 6 variance)) (+ mean (* 6 variance)) #'mjr_prob_normal-pdf nil mean variance :algorithm pdf-algorithm))
    (:normal        (+ mean (* variance (mjr_prob_std-normal-prng :algorithm std-normal-algorithm))))
    (otherwise      (error "mjr_prob_normal-prng: Unknown algorithm"))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_poisson-pdf (k mu &key (algorithm :direct))
  "Probability (as a DOUBLE-FLOAT) of seeing k events over a time period when the expected number of events over that time is mu.

:algorithm is one of:
   * :direct - Formula may be less stable numerically, but it avoids overflow. approximates the logarithm of the factorial
   * :naive0 - Stable but overflow prone. approximates factorial
   * :naive1 - Stable but overflow prone. exact factorial
   * :normal - Normal approximation good for mu>

Classical formula:
  $$\frac{\lambda^k}{k!}\cdot e^{-\lambda}$$"
  (cond ((not (numberp mu))        (error "mjr_prob_poisson-pdf: MU must be a number!"))
        ((complexp mu)             (error "mjr_prob_poisson-pdf: MU must be real (i.e. not complex)!"))
        ((< mu 0)                  (error "mjr_prob_poisson-pdf: MU must be non-negative!"))
        ((not (integerp k))        (error "mjr_prob_poisson-pdf: K must be an integer!")))
  (cond ((< k 0)          0)
        ((mjr_chk_!=0 mu) (let ((mu (float mu 1.0d0)))
                            (case algorithm
                              (:direct   (exp (- (* k (log mu)) mu (mjr_numu_log! k))))
                              (:naive0   (/ (* (exp (- mu)) (expt mu k)) (mjr_numu_gamma (1+ k))))
                              (:naive1   (/ (* (exp (- mu)) (expt mu k)) (mjr_combe_! k)))
                              (:normal   (mjr_prob_normal-pdf (+ k 0.5) mu mu))
                              (otherwise (error "mjr_prob_poisson-pdf: Unknown algorithm")))))
        ('t               0))) ;; mu=0 case

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_poisson-cdf (k mu &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp mu))                (error "mjr_prob_poisson-cdf: MU must be a number!"))
        ((complexp mu)                     (error "mjr_prob_poisson-cdf: MU must be real (i.e. not complex)!"))
        ((< mu 0)                          (error "mjr_prob_poisson-cdf: MU must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_poisson-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_poisson-cdf: Unknown algorithm")))
  (let ((xmax (ceiling (max 300 (cond ((>= mu 100) (+ mu (* 6   mu)))
                                      ((>= mu 50)  (+ mu (* 10  mu)))
                                      ((>= mu 1)   (+ mu (* 200 mu)))
                                      ('t          1))))))
    (mjr_probu_pdf2cdf k 0 xmax #'mjr_prob_poisson-pdf 't mu :algorithm pdf-algorithm)))
;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_poisson-ccdf (k mu &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp mu))                (error "mjr_prob_poisson-ccdf: MU must be a number!"))
        ((complexp mu)                     (error "mjr_prob_poisson-ccdf: MU must be real (i.e. not complex)!"))
        ((< mu 0)                          (error "mjr_prob_poisson-ccdf: MU must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_poisson-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_poisson-ccdf: Unknown algorithm")))
  (let ((xmax (ceiling (max 300 (cond ((>= mu 100) (+ mu (* 6   mu)))
                                      ((>= mu 50)  (+ mu (* 10  mu)))
                                      ((>= mu 1)   (+ mu (* 200 mu)))
                                      ('t          1))))))
    (mjr_probu_pdf2ccdf k 0 xmax #'mjr_prob_poisson-pdf 't mu :algorithm pdf-algorithm)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_poisson-prng (mu &key (algorithm :knuth))
  (cond ((not (numberp mu))             (error "mjr_prob_poisson-prng: MU must be a number!"))
        ((complexp mu)                  (error "mjr_prob_poisson-prng: MU must be real (i.e. not complex)!"))
        ((< mu 0)                       (error "mjr_prob_poisson-prng: MU must be non-negative!"))
        ((not (equal algorithm :knuth)) (error "mjr_prob_poisson-prng: Unknown algorithm")))
  (loop with l = (exp (- mu))
        for k from 1 
        for u = (random 1.0)
        for p = 1 then (* p u)
        finally (return (- k 1))
        while (> p l)))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_bernoulli-pdf (k p &key (algorithm :direct))
;; MJR TODO NOTE <2012-09-20 23:35:17 CDT> mjr_prob_bernoulli-pdf: Reconsider "always double-float" rule.  Document why we do it.
  "Probability of having k successes when trying an experiment 1 time when the probability of success is P

NOTE: If p is a float (single or double), then the result is a DOUBLE-FLOAT

Classical formula:
  $$\begin{cases}
      1-p & \text{iff $k=0$} \\
      p   & \text{iff $k=1$} \\
      0   & \text{otherwise} \\
    \end{cases}$$"
  (cond ((not (numberp p))                 (error "mjr_prob_bernoulli-pdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_bernoulli-pdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_bernoulli-pdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_bernoulli-pdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_bernoulli-pdf: K must be an integer!"))
        ((not (equal algorithm :direct))   (error "mjr_prob_bernoulli-pdf: Unknown algorithm!")))
  (let ((p (mjr_numu_max-accuracy p)))
    (case k
      (0         (- 1 p))
      (1         p)
      (otherwise 0))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_bernoulli-cdf (k p &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_bernoulli-cdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_bernoulli-cdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_bernoulli-cdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_bernoulli-cdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_bernoulli-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_bernoulli-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 1 #'mjr_prob_bernoulli-pdf 't p  :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_bernoulli-ccdf (k p &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_bernoulli-ccdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_bernoulli-ccdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_bernoulli-ccdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_bernoulli-ccdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_bernoulli-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_bernoulli-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 1 #'mjr_prob_bernoulli-pdf 't p  :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_bernoulli-prng (p &key (algorithm :accept-reject) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_bernoulli-prng: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_bernoulli-prng: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_bernoulli-prng: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_bernoulli-prng: P must be greater than or equal to 0!")))
  (case algorithm
    (:accept-reject (mjr_probu_pdf2prng 0 1 #'mjr_prob_bernoulli-pdf 't p  :algorithm pdf-algorithm))
    (:bau           (if (< (random 1.0) p) 1 0))
    (otherwise      (error "mjr_prob_bernoulli-prng: Unknown algorithm"))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_geometric-pdf (k p &key (algorithm :direct))
  "The probability of K failures followed by 1 success for K+1 Bernoulli trials (each with success probability of P)

Alternate statement: Number of Bernoulli trial failures required until a success occurs

NOTE: This definition of the geometric distribution is compatible with R in order to facilitate interoperability.

Value of :ALGORITHM determines how the computation is performed.
  * :direct  -- use direct computation using definition

Classical formula:
  $$(1-p)^k\cdot p$$"
  (cond ((not (numberp p))                 (error "mjr_prob_geometric-pdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_geometric-pdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_geometric-pdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_geometric-pdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_geometric-pdf: K must be an integer!"))
        ((< k 0)                           (error "mjr_prob_geometric-pdf: K must be non-negative!"))
        ((not (equal algorithm :direct))   (error "mjr_prob_geometric-pdf: Unknown algorithm!")))
  (* p (expt (- 1 p) k)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_geometric-cdf (k p &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_geometric-cdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_geometric-cdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_geometric-cdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_geometric-cdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_geometric-cdf: K must be an integer!"))
        ((< k 0)                           (error "mjr_prob_geometric-cdf: K must be non-negative!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_geometric-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 nil #'mjr_prob_geometric-pdf 't p :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_geometric-ccdf (k p &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_geometric-ccdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_geometric-ccdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_geometric-ccdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_geometric-ccdf: P must be greater than or equal to 0!"))
        ((not (integerp k))                (error "mjr_prob_geometric-ccdf: K must be an integer!"))
        ((< k 0)                           (error "mjr_prob_geometric-ccdf: K must be non-negative!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_geometric-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 nil #'mjr_prob_geometric-pdf 't p :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_geometric-prng (p &key (algorithm :bau))
  (cond ((not (numberp p))                 (error "mjr_prob_geometric-prng: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_geometric-prng: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_geometric-prng: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_geometric-prng: P must be greater than or equal to 0!")))
  (case algorithm
    (:bau           (loop for s = (random 1.0)
                          count (>= s p)
                          until (< s p)))
    (:exponential   (floor (mjr_prob_exponential-prng p)))
    (otherwise      (error "mjr_prob_geometric-pdf: Unknown algorithm"))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_binomial-pdf (k p s &key (algorithm :direct))
  "Probability of exactly K successes in S trials when the probably of success for each trial is P.

Value of :ALGORITHM determines how the computation is performed.
  * :direct  -- use direct computation using definition
  * :poisson -- Use the Poisson approximation
  * :normal  -- Use the Normal approximation (normally good enough
  * :auto    -- Use :poisson when s>100 and p<0.005
                Use :normal when s>1000, p>.2, and p<.8
                Use :direct otherwise

Classical formula:
  $$\binom{s}{k} p^k (1-p)^{s-k}$$"
  (cond ((not (numberp p))                 (error "mjr_prob_binomial-pdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_binomial-pdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_binomial-pdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_binomial-pdf: P must be greater than or equal to 0!"))
        ((not (integerp s))                (error "mjr_prob_binomial-pdf: S must be an integer!"))
        ((< s 0)                           (error "mjr_prob_binomial-pdf: S must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_binomial-pdf: K must be an integer!")))
  (let ((algorithm (if (equalp algorithm :auto)
                       (cond ((and (> s 100)  (< p 0.01))        :poisson)
                             ((and (> s 1000) (> p .2) (< p .8)) :normal)
                             ('t                                 :direct))
                       algorithm)))
    (case algorithm
      (:direct   (* (mjr_combe_comb s k) (expt p k) (expt (- 1 p) (- s k))))
      (:poisson  (mjr_prob_poisson-pdf k (* s p)))
      (:normal   (mjr_prob_normal-pdf  k (* s p) (* s p (- 1 p))))
      (otherwise (error "mjr_prob_binomial-pdf: Unknown algorithm")))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_binomial-cdf (k p s &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_binomial-cdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_binomial-cdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_binomial-cdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_binomial-cdf: P must be greater than or equal to 0!"))
        ((not (integerp s))                (error "mjr_prob_binomial-cdf: S must be an integer!"))
        ((< s 0)                           (error "mjr_prob_binomial-cdf: S must be non-negative!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_binomial-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 s #'mjr_prob_binomial-pdf 't p s :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_binomial-ccdf (k p s &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_binomial-ccdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_binomial-ccdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_binomial-ccdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_binomial-ccdf: P must be greater than or equal to 0!"))
        ((not (integerp s))                (error "mjr_prob_binomial-ccdf: S must be an integer!"))
        ((< s 0)                           (error "mjr_prob_binomial-ccdf: S must be non-negative!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_binomial-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 s #'mjr_prob_binomial-pdf 't p s :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_binomial-prng (p s &key (algorithm :accept-reject) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_binomial-prng: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_binomial-prng: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_binomial-prng: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_binomial-prng: P must be greater than or equal to 0!"))
        ((not (integerp s))                (error "mjr_prob_binomial-prng: S must be an integer!"))
        ((< s 0)                           (error "mjr_prob_binomial-prng: S must be non-negative!")))
  (case algorithm
    (:accept-reject (mjr_probu_pdf2prng 0 s #'mjr_prob_binomial-pdf 't p s :algorithm pdf-algorithm))
    (:bau           (loop for i from 1 upto s
                          count (<= (random 1.0) p)))
    (otherwise      (error "mjr_prob_binomial-pdf: Unknown algorithm"))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_negative-binomial-pdf (k p r &key (algorithm :direct))
  "Probability of K failures in a sequence of bernoulli trials with R successes (bernoulli probability is P)

i.e. we do trials till we have R successes, and p(k) is the probability we had k failures in the process.

NOTE: I have used a definition of the negative binomial that is compatible with R in order to facilitate interoperability with
      R.  It is important to note that many sources have K being the successes and R being the failures...

Classical formula:
  $$\binom{k+r-1}{k}\cdot (1-p)^k\cdot p^r$$
Note that:
  $$(-1)^k \frac{(-r)(-r-1)(-r-2)\cdots(-r-k+1)}{k!} = (-1)^k\binom{-r}{k}$$"
  ;; MJR TODO NOTE mjr_prob_negative-binomial-pdf: Optimize!
  ;; MJR TODO NOTE mjr_prob_negative-binomial-pdf: Add a :poisson option for :algorithm
  (cond ((not (numberp p))                 (error "mjr_prob_negative-binomial-pdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_negative-binomial-pdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_negative-binomial-pdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_negative-binomial-pdf: P must be greater than or equal to 0!"))
        ((not (integerp r))                (error "mjr_prob_negative-binomial-pdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_prob_negative-binomial-pdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_negative-binomial-pdf: K must be an integer!"))
        ((not (equal algorithm :direct))   (error "mjr_prob_negative-binomial-pdf: Unknown algorithm!")))
  (cond ((< k 0) 0)
        ('t      (* (mjr_combe_comb (1- (+ k r)) k)(expt p r) (expt (- 1 p) k)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_negative-binomial-cdf (k p r &key (algorithm :pdf2cdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_negative-binomial-cdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_negative-binomial-cdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_negative-binomial-cdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_negative-binomial-cdf: P must be greater than or equal to 0!"))
        ((not (integerp r))                (error "mjr_prob_negative-binomial-cdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_prob_negative-binomial-cdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_negative-binomial-cdf: K must be an integer!"))
        ((not (equal algorithm :pdf2cdf))  (error "mjr_prob_negative-binomial-cdf: Unknown algorithm!")))
  (mjr_probu_pdf2cdf k 0 nil #'mjr_prob_negative-binomial-pdf 't p r :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_negative-binomial-ccdf (k p r &key (algorithm :pdf2ccdf) (pdf-algorithm :direct))
  (cond ((not (numberp p))                 (error "mjr_prob_negative-binomial-ccdf: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_negative-binomial-ccdf: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_negative-binomial-ccdf: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_negative-binomial-ccdf: P must be greater than or equal to 0!"))
        ((not (integerp r))                (error "mjr_prob_negative-binomial-ccdf: R must be an integer!"))
        ((< r 0)                           (error "mjr_prob_negative-binomial-ccdf: R must be non-negative!"))
        ((not (integerp k))                (error "mjr_prob_negative-binomial-ccdf: K must be an integer!"))
        ((not (equal algorithm :pdf2ccdf)) (error "mjr_prob_negative-binomial-ccdf: Unknown algorithm!")))
  (mjr_probu_pdf2ccdf k 0 nil #'mjr_prob_negative-binomial-pdf 't p r :algorithm pdf-algorithm))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prob_negative-binomial-prng (p r &key (algorithm :bau))
  (cond ((not (numberp p))                 (error "mjr_prob_negative-binomial-prng: P must be a number!"))
        ((complexp p)                      (error "mjr_prob_negative-binomial-prng: P must be real (i.e. not complex)!"))
        ((> p 1)                           (error "mjr_prob_negative-binomial-prng: P must be less than or equal to 1!"))
        ((< p 0)                           (error "mjr_prob_negative-binomial-prng: P must be greater than or equal to 0!"))
        ((not (integerp r))                (error "mjr_prob_negative-binomial-prng: R must be an integer!"))
        ((< r 0)                           (error "mjr_prob_negative-binomial-prng: R must be non-negative!"))
        ((not (equal algorithm :bau))      (error "mjr_prob_negative-binomial-prng: Unknown algorithm!")))
  (loop with red  = 0
        with blue = 0
        for i from 1
        finally (return blue)                                                  
        do (if (<= (random 1.0) p)
               (incf red)
               (incf blue))
        until (= red r)))
