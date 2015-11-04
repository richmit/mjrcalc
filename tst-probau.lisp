;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-probau.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Test package for use-probau.lisp.@EOL
;; @Keywords
;; @Std       Common Lisp
;;
;;            Status:
;;
;;              Well tested:
;;
;;                mjr_probau_help mjr_probau_hypergeometric-pdf
;;
;;              Suspect (Basic computation tested in :mjr_prob-tests, just need sanity checks here)
;;
;;                mjr_probau_bernoulli-pdf mjr_probau_binomial-pdf mjr_probau_geometric-pdf 
;;
;;              Suspect:
;;
;;                mjr_probau_negative-binomial-pdf mjr_probau_hypergeometric-pdf mjr_probau_negative-hypergeometric-pdf
;;                mjr_probau_multi-hypergeometric-pdf mjr_probau_multinomial-pdf
;;
;;              Dep on PDF and pdf2ccdf:
;;
;;                 mjr_probau_bernoulli-cdf mjr_probau_binomial-cdf mjr_probau_geometric-cdf mjr_probau_negative-binomial-cdf
;;                 mjr_probau_hypergeometric-cdf mjr_probau_negative-hypergeometric-cdf
;;
;;              Dep on PDF and pdf2cdf:
;;
;;                 mjr_probau_bernoulli-ccdf mjr_probau_binomial-ccdf mjr_probau_geometric-ccdf mjr_probau_negative-binomial-ccdf
;;                 mjr_probau_hypergeometric-ccdf mjr_probau_negative-hypergeometric-ccdf
;;
;;              Seem to work, but testing random number generators is hard. :)
;;
;;                 mjr_probau_bernoulli-prng mjr_probau_binomial-prng mjr_probau_geometric-prng mjr_probau_negative-binomial-prng
;;                 mjr_probau_hypergeometric-prng mjr_probau_negative-hypergeometric-prng

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PROBAU-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PROBAU :MJR_PRNG :MJR_EPS))

(in-package :MJR_PROBAU-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_hypergeometric-pdf
  ;; Random tests to make sure naive algorithms match standard one
  (dotimes (i 100)
    (let* ((m (mjr_prng_int-cc 0 1000))
           (n (mjr_prng_int-cc 0 1000))
           (s (mjr_prng_int-cc 0 1000))
           (k (mjr_prng_int-cc 0 1000))
           (h (mjr_probau_hypergeometric-pdf k n m s)))
      (assert-equal (mjr_probau_hypergeometric-pdf k n m s :algorithm :naive1) h (list k n m s))
      (assert-equal (mjr_probau_hypergeometric-pdf k n m s :algorithm :naive0) h (list k n m s))))
  ;; R> n<-5; m<-7; sapply(0:(n+m), function (s) dhyper(0:(n+m),n,m,s))
  (loop with n = 5
        with m = 7
        with d = #2a((1 0.5833333 0.3181818 0.15909091 0.07070707 0.026515152 0.007575758 0.001262626 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.4166667 0.5303030 0.47727273 0.35353535 0.220959596 0.113636364 0.044191919 0.01010101 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.1515152 0.31818182 0.42424242 0.441919192 0.378787879 0.265151515 0.14141414 0.04545455 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.04545455 0.14141414 0.265151515 0.378787879 0.441919192 0.42424242 0.31818182 0.1515152 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.01010101 0.044191919 0.113636364 0.220959596 0.35353535 0.47727273 0.5303030 0.4166667 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.001262626 0.007575758 0.026515152 0.07070707 0.15909091 0.3181818 0.5833333 1)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0)
                     (0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0))
        for k from 0 upto (+ n m)
        do (loop for s from 0 upto (+ n m)
                 do (assert-equality #'mjr_eps_= (aref d k s) (float (mjr_probau_hypergeometric-pdf k n m s)) (list k n m s))))
  ;; R> n<-7; m<-5; sapply(0:(n+m), function (s) dhyper(0:n,n,m,s))
  (loop with n = 7
        with m = 5
        with d = #2a((1.0 0.4166667 0.1515152 0.04545455 0.01010101 0.001262626 0.000000000 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0.0)
                     (0.0 0.5833333 0.5303030 0.31818182 0.14141414 0.044191919 0.007575758 0.000000000 0.00000000 0.00000000 0.0000000 0.0000000 0.0)
                     (0.0 0.0000000 0.3181818 0.47727273 0.42424242 0.265151515 0.113636364 0.026515152 0.00000000 0.00000000 0.0000000 0.0000000 0.0)
                     (0.0 0.0000000 0.0000000 0.15909091 0.35353535 0.441919192 0.378787879 0.220959596 0.07070707 0.00000000 0.0000000 0.0000000 0.0)
                     (0.0 0.0000000 0.0000000 0.00000000 0.07070707 0.220959596 0.378787879 0.441919192 0.35353535 0.15909091 0.0000000 0.0000000 0.0)
                     (0.0 0.0000000 0.0000000 0.00000000 0.00000000 0.026515152 0.113636364 0.265151515 0.42424242 0.47727273 0.3181818 0.0000000 0.0)
                     (0.0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.007575758 0.044191919 0.14141414 0.31818182 0.5303030 0.5833333 0.0)
                     (0.0 0.0000000 0.0000000 0.00000000 0.00000000 0.000000000 0.000000000 0.001262626 0.01010101 0.04545455 0.1515152 0.4166667 1.0))
        for k from 0 upto n
        do (loop for s from 0 upto (+ n m)
                 do (assert-equality #'mjr_eps_= (aref d k s) (float (mjr_probau_hypergeometric-pdf k n m s)) (list k n m s))))
  ;; Speical case: all red balls
  ;; R> n<-5; m<-0; sapply(0:(n+m), function (s) dhyper(0:(n+m),n,m,s))
  (loop with n = 5
        with m = 0
        with d = #2a((1    0    0    0    0    0)
                     (0    1    0    0    0    0)
                     (0    0    1    0    0    0)
                     (0    0    0    1    0    0)
                     (0    0    0    0    1    0)
                     (0    0    0    0    0    1))
        for k from 0 upto (+ n m)
        do (loop for s from 0 upto (+ n m)
                 do (assert-equal (aref d k s) (mjr_probau_hypergeometric-pdf k n m s) (list k n m s))))
  ;; Speical case: 0 red balls
  ;; R> n<-0; m<-5; sapply(0:(n+m), function (s) dhyper(0:(n+m),n,m,s))
  (loop with n = 0
        with m = 5
        with d = #2a((1    1    1    1    1    1)
                     (0    0    0    0    0    0)
                     (0    0    0    0    0    0)
                     (0    0    0    0    0    0)
                     (0    0    0    0    0    0)
                     (0    0    0    0    0    0))
        for k from 0 upto (+ n m)
        do (loop for s from 0 upto (+ n m)
                 do (assert-equal (aref d k s) (mjr_probau_hypergeometric-pdf k n m s) (list k n m s))))
  ;; R> n<-1; m<-5; sapply(0:(n+m), function (s) dhyper(0:(n+m),n,m,s))
  ;; Speical case: 1 red ball
  (loop with n = 1
        with m = 5
        with d = #2a((1.0 0.8333333 0.6666667  0.5 0.3333333 0.1666667    0.0)
                     (0.0 0.1666667 0.3333333  0.5 0.6666667 0.8333333    1.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0))
        for k from 0 upto (+ n m)
        do (loop for s from 0 upto (+ n m)
                 do (assert-equality #'mjr_eps_= (aref d k s) (float (mjr_probau_hypergeometric-pdf k n m s)) (list k n m s))))
  ;; R> n<-5; m<-1; sapply(0:(n+m), function (s) dhyper(0:(n+m),n,m,s))
  ;; Speical case: 1 blue ball
  (loop with n = 5
        with m = 1
        with d = #2a((1.0 0.1666667 0.0000000  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.8333333 0.3333333  0.0 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.6666667  0.5 0.0000000 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.5 0.6666667 0.0000000    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.3333333 0.8333333    0.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.1666667    1.0)
                     (0.0 0.0000000 0.0000000  0.0 0.0000000 0.0000000    0.0))
        for k from 0 upto (+ n m)
        do (loop for s from 0 upto (+ n m)
                 do (assert-equality #'mjr_eps_= (aref d k s) (float (mjr_probau_hypergeometric-pdf k n m s)) (list k n m s))))

  ;; Special case: sample size is 1 (Bernoulli)
  (assert-equal 0    (mjr_probau_hypergeometric-pdf -1 5 7 1))
  (assert-equal 7/12 (mjr_probau_hypergeometric-pdf  0 5 7 1))
  (assert-equal 5/12 (mjr_probau_hypergeometric-pdf  1 5 7 1))
  (assert-equal 0    (mjr_probau_hypergeometric-pdf  2 5 7 1))
  ;; Special Case: draw every red ball!!
  ;;R> n_max<-7; m<-4; sapply(0:(n_max+m), function (s) dhyper(0:(n_max+m),0:(n_max+m),m,s))
  (loop with n-max = 7
        with m = 4
        with d = #2a((1.0  1.0 1.00000000 1.00000000 1.0000000  0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000  0.000000000)
                     (0.0  0.2 0.40000000 0.60000000 0.80000000 1.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000  0.000000000)
                     (0.0  0.0 0.06666667 0.20000000 0.40000000 0.666666667 1.000000000 0.000000000 0.000000000 0.000000000 0.000000000  0.000000000)
                     (0.0  0.0 0.00000000 0.02857143 0.11428571 0.285714286 0.571428571 1.000000000 0.000000000 0.000000000 0.000000000  0.000000000)
                     (0.0  0.0 0.00000000 0.00000000 0.01428571 0.071428571 0.214285714 0.500000000 1.000000000 0.000000000 0.000000000  0.000000000)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.007936508 0.047619048 0.166666667 0.444444444 1.000000000 0.000000000  0.000000000)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.004761905 0.033333333 0.133333333 0.400000000 1.000000000  0.000000000)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.000000000 0.003030303 0.024242424 0.109090909 0.363636364 1.0000000000)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.002020202 0.018181818 0.090909091 0.3333333333)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.000000000 0.001398601 0.013986014 0.0769230769)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000999001 0.0109890110)
                     (0.0  0.0 0.00000000 0.00000000 0.00000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.0007326007))
        for k from 0 upto (+ n-max m)
        do (loop for s from 0 upto (+ n-max m)
                 do (assert-equality #'mjr_eps_= (aref d k s) (float (mjr_probau_hypergeometric-pdf k k m s)) (list k k m s))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_multi-hypergeometric-pdf
  (assert-equal 30/377 (mjr_probau_multi-hypergeometric-pdf #(2 2 2)  #(5 10 15)))
  (assert-equal 0      (mjr_probau_multi-hypergeometric-pdf #(2 12 2) #(5 10 15)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_multinomial-pdf
  (assert-equal 27/200 (mjr_probau_multinomial-pdf #(1 2 3)  #(20 30 50)))
  (assert-equal 0      (mjr_probau_multinomial-pdf #(21 2 3) #(20 30 50)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_negative-binomial-pdf
  ;; MJR TODO NOTE <2013-03-15 16:42:43 CDT> mjr_probau_negative-binomial-pdf: BROKEN!!
  ;;(assert-equal 27/200 (mjr_probau_negative-binomial-pdf #(1 2 3) #(20 30 50)))
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_negative-hypergeometric-pdf
  ;; A couple hand picked test cases
  (assert-equal 8/143              (mjr_probau_negative-hypergeometric-pdf 5  4 10 1))
  (assert-equal 20/253             (mjr_probau_negative-hypergeometric-pdf 6  4 20 2))
  (assert-equal 14766367/589559541 (mjr_probau_negative-hypergeometric-pdf 5 12 74 3))
  ;; MJR SCM NOTE mjr_probau_negative-binomial-pdf: MORE!!
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_bernoulli-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_probau_bernoulli-prng 10 20)))
    (assert-true (numberp (mjr_probau_bernoulli-prng 10 20 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_probau_bernoulli-prng 10 20 :algorithm :accept-reject :pdf-algorithm :direct)))
    (assert-true (numberp (mjr_probau_bernoulli-prng 10 20 :algorithm :bau))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_binomial-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_probau_binomial-prng 10 20 10)))
    (assert-true (numberp (mjr_probau_binomial-prng 10 20 10 :algorithm :bau)))
    (assert-true (numberp (mjr_probau_binomial-prng 10 20 10 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_probau_binomial-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :direct))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_geometric-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_probau_geometric-prng 10 20)))
    (assert-true (numberp (mjr_probau_geometric-prng 10 20 :algorithm :exponential)))
    (assert-true (numberp (mjr_probau_geometric-prng 10 20 :algorithm :bau))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_negative-binomial-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_probau_negative-binomial-prng 10 20 10)))
    (assert-true (numberp (mjr_probau_negative-binomial-prng 10 20 10 :algorithm :bau))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_hypergeometric-prng
  (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10)))
  (dotimes (i 100)
    ;;(assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :bernoulli))) ;; This :pdf-algorithm is not supported yet
    ;;(assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :floating)))  ;; This :pdf-algorithm is not supported yet
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :naive0)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :naive1)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :direct)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :bernoulli)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :floating)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :binomial)))
    (assert-true (numberp (mjr_probau_hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :normal))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_probau_negative-hypergeometric-prng
  (dotimes (i 100)
    (assert-true (numberp (mjr_probau_negative-hypergeometric-prng 10 20 10)))
    (assert-true (numberp (mjr_probau_negative-hypergeometric-prng 10 20 10 :algorithm :accept-reject)))
    (assert-true (numberp (mjr_probau_negative-hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :naive0)))
    (assert-true (numberp (mjr_probau_negative-hypergeometric-prng 10 20 10 :algorithm :accept-reject :pdf-algorithm :direct))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
)
