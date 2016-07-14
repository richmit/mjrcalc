;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-prob.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-prob.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010,2011,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  Status:
;;
;;    Well tested:
;;
;;     mjr_prob_help mjr_prob_exponential-pdf mjr_prob_exponential-icdf mjr_prob_std-normal-pdf mjr_prob_std-normal-icdf
;;     mjr_prob_normal-pdf mjr_prob_poisson-pdf mjr_prob_binomial-pdf
;;
;;    Suspect:
;;
;;     mjr_prob_bernoulli-pdf mjr_prob_geometric-pdf mjr_prob_negative-binomial-pdf
;;
;;    Dep on PDF and pdf2ccdf:
;;
;;     mjr_prob_exponential-ccdf mjr_prob_std-normal-ccdf mjr_prob_normal-ccdf mjr_prob_poisson-ccdf
;;     mjr_prob_bernoulli-ccdf mjr_prob_binomial-ccdf mjr_prob_geometric-ccdf mjr_prob_negative-binomial-ccdf
;;
;;    Dep on PDF and pdf2cdf:
;;
;;     mjr_prob_exponential-cdf mjr_prob_std-normal-cdf mjr_prob_normal-cdf mjr_prob_poisson-cdf mjr_prob_bernoulli-cdf
;;     mjr_prob_binomial-cdf mjr_prob_geometric-cdf mjr_prob_negative-binomial-cdf
;;
;;    Seem to work, but testing random number generators is hard. :)
;;
;;     mjr_prob_exponential-prng mjr_prob_std-normal-prng mjr_prob_normal-prng mjr_prob_poisson-prng
;;     mjr_prob_bernoulli-prng mjr_prob_binomial-prng mjr_prob_geometric-prng mjr_prob_negative-binomial-prng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PROB-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PROB :MJR_PRNG :MJR_EPS))

(in-package :MJR_PROB-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_exponential-pdf
  ;; check on a grid (Data from R)
  (loop for x in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5
                   3.6 3.7 3.8 3.9 4.0 4.1 4.2 4.3 4.4 4.5 4.6 4.7 4.8 4.9 5.0)
        for f in '(1.000000000 0.904837418 0.818730753 0.740818221 0.670320046 0.606530660 0.548811636 0.496585304 0.449328964 0.406569660 0.367879441 0.332871084
                   0.301194212 0.272531793 0.246596964 0.223130160 0.201896518 0.182683524 0.165298888 0.149568619 0.135335283 0.122456428 0.110803158 0.100258844
                   0.090717953 0.082084999 0.074273578 0.067205513 0.060810063 0.055023220 0.049787068 0.045049202 0.040762204 0.036883167 0.033373270 0.030197383
                   0.027323722 0.024723526 0.022370772 0.020241911 0.018315639 0.016572675 0.014995577 0.013568559 0.012277340 0.011108997 0.010051836 0.009095277
                   0.008229747 0.007446583 0.006737947)
        do (assert-equality #'mjr_eps_= f (mjr_prob_exponential-pdf x 1) (list x f)))
  ;; check on a grid (Data from R)
  (loop for x in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5
                   3.6 3.7 3.8 3.9 4.0 4.1 4.2 4.3 4.4 4.5 4.6 4.7 4.8 4.9 5.0)
        for f in '(0.50000000 0.47561471 0.45241871 0.43035399 0.40936538 0.38940039 0.37040911 0.35234404 0.33516002 0.31881408 0.30326533 0.28847491 0.27440582
                   0.26102289 0.24829265 0.23618328 0.22466448 0.21370747 0.20328483 0.19337051 0.18393972 0.17496887 0.16643554 0.15831838 0.15059711 0.14325240
                   0.13626590 0.12962013 0.12329848 0.11728514 0.11156508 0.10612399 0.10094826 0.09602495 0.09134176 0.08688697 0.08264944 0.07861858 0.07478431
                   0.07113704 0.06766764 0.06436745 0.06122821 0.05824208 0.05540158 0.05269961 0.05012942 0.04768458 0.04535898 0.04314679 0.04104250)
        do (assert-equality #'mjr_eps_= f (mjr_prob_exponential-pdf x .5) (list x f)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_std-normal-pdf
  (dotimes (i 1000)
    (let* ((x        (mjr_prng_float-cc -500 500))
           (p-direct (mjr_prob_std-normal-pdf x :algorithm :direct)))
      (assert-true  (numberp p-direct)  (list x p-direct))
      (assert-false (complexp p-direct) (list x p-direct))
      (assert-true  (<= 0 p-direct)     (list x p-direct))
      (assert-true  (>= 1 p-direct)     (list x p-direct))))
  ;; check on a grid (Data from R)
  (loop for x in '(-5.0 -4.9 -4.8 -4.7 -4.6 -4.5 -4.4 -4.3 -4.2 -4.1 -4.0 -3.9 -3.8 -3.7 -3.6 -3.5 -3.4 -3.3 -3.2 -3.1 -3.0 -2.9 -2.8 -2.7 -2.6 -2.5 -2.4 -2.3 -2.2
                   -2.1 -2.0 -1.9 -1.8 -1.7 -1.6 -1.5 -1.4 -1.3 -1.2 -1.1 -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7
                    0.8  0.9  1.0  1.1  1.2  1.3  1.4  1.5  1.6  1.7  1.8  1.9  2.0  2.1  2.2  2.3  2.4  2.5  2.6  2.7  2.8  2.9  3.0  3.1  3.2  3.3  3.4  3.5  3.6
                    3.7  3.8  3.9  4.0  4.1  4.2  4.3  4.4  4.5  4.6  4.7  4.8  4.9  5.0)
        for f in '(1.486720e-06 2.438961e-06 3.961299e-06 6.369825e-06 1.014085e-05 1.598374e-05 2.494247e-05 3.853520e-05 5.894307e-05 8.926166e-05 1.338302e-04
                   1.986555e-04 2.919469e-04 4.247803e-04 6.119019e-04 8.726827e-04 1.232219e-03 1.722569e-03 2.384088e-03 3.266819e-03 4.431848e-03 5.952532e-03
                   7.915452e-03 1.042093e-02 1.358297e-02 1.752830e-02 2.239453e-02 2.832704e-02 3.547459e-02 4.398360e-02 5.399097e-02 6.561581e-02 7.895016e-02
                   9.404908e-02 1.109208e-01 1.295176e-01 1.497275e-01 1.713686e-01 1.941861e-01 2.178522e-01 2.419707e-01 2.660852e-01 2.896916e-01 3.122539e-01
                   3.332246e-01 3.520653e-01 3.682701e-01 3.813878e-01 3.910427e-01 3.969525e-01 3.989423e-01 3.969525e-01 3.910427e-01 3.813878e-01 3.682701e-01
                   3.520653e-01 3.332246e-01 3.122539e-01 2.896916e-01 2.660852e-01 2.419707e-01 2.178522e-01 1.941861e-01 1.713686e-01 1.497275e-01 1.295176e-01
                   1.109208e-01 9.404908e-02 7.895016e-02 6.561581e-02 5.399097e-02 4.398360e-02 3.547459e-02 2.832704e-02 2.239453e-02 1.752830e-02 1.358297e-02
                   1.042093e-02 7.915452e-03 5.952532e-03 4.431848e-03 3.266819e-03 2.384088e-03 1.722569e-03 1.232219e-03 8.726827e-04 6.119019e-04 4.247803e-04
                   2.919469e-04 1.986555e-04 1.338302e-04 8.926166e-05 5.894307e-05 3.853520e-05 2.494247e-05 1.598374e-05 1.014085e-05 6.369825e-06 3.961299e-06
                   2.438961e-06 1.486720e-06)
        do (assert-equality #'mjr_eps_= f (mjr_prob_std-normal-pdf x) (list x f)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_normal-pdf
  ;; Make sure values are real and in [0,1]
  (dotimes (i 1000)
    (let* ((m       (mjr_prng_float-cc -100 100))
           (v       (mjr_prng_float-cc 0 100))
           (x       (mjr_prng_float-cc -500 500))
           (p-direct (mjr_prob_normal-pdf x m v :algorithm :direct)))
      (assert-true  (numberp p-direct)  (list x m v p-direct))
      (assert-false (complexp p-direct) (list x m v p-direct))
      (assert-true  (<= 0 p-direct)     (list x m v p-direct))
      (assert-true  (>= 1 p-direct)     (list x m v p-direct))))
  ;; check on a grid (Data from R)
  (loop for x in '( -5.0 -4.9 -4.8 -4.7 -4.6 -4.5 -4.4 -4.3 -4.2 -4.1 -4.0 -3.9 -3.8 -3.7 -3.6 -3.5 -3.4 -3.3 -3.2 -3.1 -3.0 -2.9 -2.8 -2.7 -2.6 -2.5 -2.4 -2.3 -2.2
                   -2.1 -2.0 -1.9 -1.8 -1.7 -1.6 -1.5 -1.4 -1.3 -1.2 -1.1 -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7
                    0.8  0.9  1.0  1.1  1.2  1.3  1.4  1.5  1.6  1.7  1.8  1.9  2.0  2.1  2.2  2.3  2.4  2.5  2.6  2.7  2.8  2.9  3.0  3.1  3.2  3.3  3.4  3.5  3.6
                    3.7  3.8  3.9  4.0  4.1  4.2  4.3  4.4  4.5  4.6  4.7  4.8  4.9  5.0)
        for f in '(0.0004363413 0.0005191406 0.0006161096 0.0007293654 0.0008612845 0.0010145240 0.0011920441 0.0013971292 0.0016334095 0.0019048810 0.0022159242
                   0.0025713205 0.0029762662 0.0034363833 0.0039577258 0.0045467813 0.0052104674 0.0059561218 0.0067914846 0.0077246736 0.0087641502 0.0099186772
                   0.0111972651 0.0126091100 0.0141635189 0.0158698259 0.0177372964 0.0197750208 0.0219917980 0.0243960093 0.0269954833 0.0297973530 0.0328079074
                   0.0360324372 0.0394750792 0.0431386594 0.0470245387 0.0511324623 0.0554604173 0.0600045003 0.0647587978 0.0697152832 0.0748637328 0.0801916637
                   0.0856842960 0.0913245427 0.0970930275 0.1029681344 0.1089260885 0.1149410703 0.1209853623 0.1270295282 0.1330426249 0.1389924431 0.1448457764
                   0.1505687161 0.1561269667 0.1614861798 0.1666123014 0.1714719275 0.1760326634 0.1802634812 0.1841350702 0.1876201735 0.1906939077 0.1933340584
                   0.1955213470 0.1972396655 0.1984762737 0.1992219570 0.1994711402 0.1992219570 0.1984762737 0.1972396655 0.1955213470 0.1933340584 0.1906939077
                   0.1876201735 0.1841350702 0.1802634812 0.1760326634 0.1714719275 0.1666123014 0.1614861798 0.1561269667 0.1505687161 0.1448457764 0.1389924431
                   0.1330426249 0.1270295282 0.1209853623 0.1149410703 0.1089260885 0.1029681344 0.0970930275 0.0913245427 0.0856842960 0.0801916637 0.0748637328
                   0.0697152832 0.0647587978)
        do (assert-equality #'mjr_eps_= f (mjr_prob_normal-pdf x 2 4) (list x f)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_poisson-pdf
  ;; Some special test cases
  (assert-equal 0 (mjr_prob_poisson-pdf -5 10))
  (assert-equal 0 (mjr_prob_poisson-pdf  0  0))
  (assert-equal 0 (mjr_prob_poisson-pdf 10  0))
  ;; check on a grid (Data from R)
  (loop with f = (make-array '(9 6) :initial-contents '((9.048374e-01 9.048374e-02 4.524187e-03 1.508062e-04 3.770156e-06 7.540312e-08)
                                                        (8.187308e-01 1.637462e-01 1.637462e-02 1.091641e-03 5.458205e-05 2.183282e-06)
                                                        (7.408182e-01 2.222455e-01 3.333682e-02 3.333682e-03 2.500261e-04 1.500157e-05)
                                                        (6.703200e-01 2.681280e-01 5.362560e-02 7.150080e-03 7.150080e-04 5.720064e-05)
                                                        (0.6065306597 0.3032653299 0.0758163325 0.0126360554 0.0015795069 0.0001579507)
                                                        (0.5488116361 0.3292869817 0.0987860945 0.0197572189 0.0029635828 0.0003556299)
                                                        (0.4965853038 0.3476097127 0.1216633994 0.0283881265 0.0049679221 0.0006955091)
                                                        (0.4493289640 0.3594631710 0.1437852690 0.0383427380 0.0076685480 0.0012269680)
                                                        (0.4065696600 0.3659126940 0.1646607120 0.0493982140 0.0111145980 0.0020006280)))
        for x in '(0 1 2 3 4 5)
        for j from 0
        do (loop for i from 0 upto 8
                 for mu = (float (/ (1+ i) 10))
                 do (assert-equality #'mjr_eps_= (aref f i j) (mjr_prob_poisson-pdf x mu) (list x mu))))
  ;; Make sure values are real and in [0,1]
  (dotimes (i 1000)
    (let* ((k       (mjr_prng_int-cc 0 100))
          (mu       (mjr_prng_int-cc 0 100))
          (p-direct (mjr_prob_poisson-pdf k mu :algorithm :direct)))
      (assert-true  (numberp p-direct)  (list k mu p-direct))
      (assert-false (complexp p-direct) (list k mu p-direct))
      (assert-true  (<= 0 p-direct)     (list k mu p-direct))
      (assert-true  (>= 1 p-direct)     (list k mu p-direct))))
  ;; Make sure diffrent algroithms agree
  (dotimes (i 400)
    (let* ((k       (mjr_prng_int-cc 0 10))
          (mu       (mjr_prng_int-cc 0 10))
          (p-direct (mjr_prob_poisson-pdf k mu :algorithm :direct))
          (p-naive0 (mjr_prob_poisson-pdf k mu :algorithm :naive0))
          (p-naive1 (mjr_prob_poisson-pdf k mu :algorithm :naive1)))
      (assert-true  (numberp p-direct)  (list k mu p-direct))
      (assert-false (complexp p-direct) (list k mu p-direct))
      (assert-true  (<= 0 p-direct)     (list k mu p-direct))
      (assert-true  (>= 1 p-direct)     (list k mu p-direct))
      (assert-equality #'mjr_eps_= p-direct p-naive0)
      (assert-equality #'mjr_eps_= p-direct p-naive1)))
  ;; Make sure normal approximation is good
  (dotimes (i 1000)
    (let* ((k        (mjr_prng_int-cc 0 200))
           (mu       (mjr_prng_int-cc 100 5000))
           (p-direct (mjr_prob_poisson-pdf k mu :algorithm :direct))
           (p-normal (mjr_prob_poisson-pdf k mu :algorithm :normal)))
      (assert-equality (mjr_eps_make-fixed= .001) p-direct p-normal (list k mu))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_binomial-pdf
  ;; Make sure values are real and in [0,1]
  (dotimes (i 1000)
    (let* ((k       (mjr_prng_int-cc 0 100))
           (s       (mjr_prng_int-cc 0 100))
           (p       (mjr_prng_float-cc 0 1))
           (p-direct (mjr_prob_binomial-pdf k p s :algorithm :direct)))
      (assert-true  (numberp p-direct)  (list k p s p-direct))
      (assert-false (complexp p-direct) (list k p s p-direct))
      (assert-true  (<= 0 p-direct)     (list k p s p-direct))
      (assert-true  (>= 1 p-direct)     (list k p s p-direct))))

 ;; Make sure normal approximation is good
 (dotimes (i 100)
   (let* ((k       (mjr_prng_int-cc 0 30))
          (s       (mjr_prng_int-cc 1001 2000))
          (p       (rationalize (mjr_prng_float-cc .2 .8)))
          (p-direct (mjr_prob_binomial-pdf k p s :algorithm :direct))
          (p-normal (mjr_prob_binomial-pdf k p s :algorithm :normal)))
     (assert-equality (mjr_eps_make-fixed= .001) p-direct p-normal (list k p s))))

 ;; Make sure poisson approximation is good
 (dotimes (i 1000)
   (let* ((k         (mjr_prng_int-cc 0 30))
          (s         (mjr_prng_int-cc 101 200))
          (p         (rationalize (mjr_prng_float-cc 0.0 0.002)))
          (p-direct  (mjr_prob_binomial-pdf k p s :algorithm :direct))
          (p-poisson (mjr_prob_binomial-pdf k p s :algorithm :poisson)))
     (assert-equality (mjr_eps_make-fixed= .001) p-direct p-poisson (list k p s))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_std-normal-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_std-normal-prng))))
  (dotimes (i 100)
    (dolist (alg '(:box-muller :toms-712 :box-mullers :box-mullercs :box-mullerp :icdf :box-mullere))
      (let ((x (mjr_prob_std-normal-prng :algorithm alg)))
        (assert-true (numberp x)  x))))
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_std-normal-prng :algorithm :accept-reject)))
    (assert-true (numberp (mjr_prob_std-normal-prng :algorithm :accept-reject :pdf-algorithm :direct))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_normal-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_normal-prng 0 1)))
    (assert-true (numberp (mjr_prob_normal-prng 0 1 :algorithm :normal)))
    (assert-true (numberp (mjr_prob_normal-prng 0 1 :algorithm :normal :std-normal-algorithm :box-muller)))
    (assert-true (numberp (mjr_prob_normal-prng 0 1 :algorithm :normal :std-normal-algorithm :toms-712))))
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_normal-prng 0 1 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_prob_normal-prng 0 1 :algorithm :accept-reject :pdf-algorithm :direct))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_poisson-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_poisson-prng 10)))
    (assert-true (numberp (mjr_prob_poisson-prng 10 :algorithm :knuth))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_bernoulli-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_bernoulli-prng .5)))
    (assert-true (numberp (mjr_prob_bernoulli-prng .5 :algorithm :bau)))
    (assert-true (numberp (mjr_prob_bernoulli-prng .5 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_prob_bernoulli-prng .5 :algorithm :accept-reject :pdf-algorithm :direct))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_binomial-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_binomial-prng .5 10)))
    (assert-true (numberp (mjr_prob_binomial-prng .5 10 :algorithm :bau)))
    (assert-true (numberp (mjr_prob_binomial-prng .5 10 :algorithm :accept-reject :pdf-algorithm :poisson)))
    (assert-true (numberp (mjr_prob_binomial-prng .5 10 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_prob_binomial-prng .5 10 :algorithm :accept-reject :pdf-algorithm :direct)))
    (assert-true (numberp (mjr_prob_binomial-prng .5 10 :algorithm :accept-reject :pdf-algorithm :normal))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_exponential-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_exponential-prng .5)))
    (assert-true (numberp (mjr_prob_exponential-prng .5 :algorithm :icdf))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_geometric-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_geometric-prng .5)))
    (assert-true (numberp (mjr_prob_geometric-prng .5 :algorithm :bau)))
    (assert-true (numberp (mjr_prob_geometric-prng .5 :algorithm :exponential))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_negative-binomial-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_prob_negative-binomial-prng .5 10)))
    (assert-true (numberp (mjr_prob_negative-binomial-prng .5 10 :algorithm :bau))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_std-normal-icdf
  (loop for p in '(0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28
                   0.29 0.30 0.31 0.32 0.33 0.34 0.35 0.36 0.37 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.45 0.46 0.47 0.48 0.49 0.50 0.51 0.52 0.53 0.54 0.55 0.56 0.57
                   0.58 0.59 0.60 0.61 0.62 0.63 0.64 0.65 0.66 0.67 0.68 0.69 0.70 0.71 0.72 0.73 0.74 0.75 0.76 0.77 0.78 0.79 0.80 0.81 0.82 0.83 0.84 0.85 0.86
                   0.87 0.88 0.89 0.90 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99)
        for x in '(-2.32634787 -2.05374891 -1.88079361 -1.75068607 -1.64485363 -1.55477359 -1.47579103 -1.40507156 -1.34075503 -1.28155157 -1.22652812
                   -1.17498679 -1.12639113 -1.08031934 -1.03643339 -0.99445788 -0.95416525 -0.91536509 -0.87789630 -0.84162123 -0.80642125 -0.77219321 -0.73884685
                   -0.70630256 -0.67448975 -0.64334541 -0.61281299 -0.58284151 -0.55338472 -0.52440051 -0.49585035 -0.46769880 -0.43991317 -0.41246313 -0.38532047
                   -0.35845879 -0.33185335 -0.30548079 -0.27931903 -0.25334710 -0.22754498 -0.20189348 -0.17637416 -0.15096922 -0.12566135 -0.10043372 -0.07526986
                   -0.05015358 -0.02506891  0.00000000  0.02506891  0.05015358  0.07526986  0.10043372  0.12566135  0.15096922  0.17637416  0.20189348  0.22754498
                    0.25334710  0.27931903  0.30548079  0.33185335  0.35845879  0.38532047  0.41246313  0.43991317  0.46769880  0.49585035  0.52440051  0.55338472
                    0.58284151  0.61281299  0.64334541  0.67448975  0.70630256  0.73884685  0.77219321  0.80642125  0.84162123  0.87789630  0.91536509  0.95416525
                    0.99445788  1.03643339  1.08031934  1.12639113  1.17498679  1.22652812  1.28155157  1.34075503  1.40507156  1.47579103  1.55477359  1.64485363
                    1.75068607  1.88079361  2.05374891  2.32634787)
        do (assert-equality #'mjr_eps_= x (mjr_prob_std-normal-icdf p) (list p x)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prob_exponential-icdf
  (loop for p in '(0.00 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15 0.16 0.17 0.18 0.19 0.20 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28
                   0.29 0.30 0.31 0.32 0.33 0.34 0.35 0.36 0.37 0.38 0.39 0.40 0.41 0.42 0.43 0.44 0.45 0.46 0.47 0.48 0.49 0.50 0.51 0.52 0.53 0.54 0.55 0.56 0.57
                   0.58 0.59 0.60 0.61 0.62 0.63 0.64 0.65 0.66 0.67 0.68 0.69 0.70 0.71 0.72 0.73 0.74 0.75 0.76 0.77 0.78 0.79 0.80 0.81 0.82 0.83 0.84 0.85 0.86
                   0.87 0.88 0.89 0.90 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99)
        for x in '(0.00000000 0.01005034 0.02020271 0.03045921 0.04082199 0.05129329 0.06187540 0.07257069 0.08338161 0.09431068 0.10536052 0.11653382 0.12783337
                   0.13926207 0.15082289 0.16251893 0.17435339 0.18632958 0.19845094 0.21072103 0.22314355 0.23572233 0.24846136 0.26136476 0.27443685 0.28768207
                   0.30110509 0.31471074 0.32850407 0.34249031 0.35667494 0.37106368 0.38566248 0.40047757 0.41551544 0.43078292 0.44628710 0.46203546 0.47803580
                   0.49429632 0.51082562 0.52763274 0.54472718 0.56211892 0.57981850 0.59783700 0.61618614 0.63487827 0.65392647 0.67334455 0.69314718 0.71334989
                   0.73396918 0.75502258 0.77652879 0.79850770 0.82098055 0.84397007 0.86750057 0.89159812 0.91629073 0.94160854 0.96758403 0.99425227 1.02165125
                   1.04982212 1.07880966 1.10866262 1.13943428 1.17118298 1.20397280 1.23787436 1.27296568 1.30933332 1.34707365 1.38629436 1.42711636 1.46967597
                   1.51412773 1.56064775 1.60943791 1.66073121 1.71479843 1.77195684 1.83258146 1.89711998 1.96611286 2.04022083 2.12026354 2.20727491 2.30258509
                   2.40794561 2.52572864 2.65926004 2.81341072 2.99573227 3.21887582 3.50655790 3.91202301 4.60517019)
        do (assert-equality #'mjr_eps_= x (mjr_prob_exponential-icdf p 1) (list p x)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
)
