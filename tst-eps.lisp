;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-eps.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for use-eps.lisp@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_EPS-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_EPS :MJR_PRNG))

(in-package :MJR_EPS-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_BASIC

  (let* ((new-eps   (float 1/1000))
         (new-eps/2 (/ new-eps 2))
         (new-eps*2 (* new-eps 2))
         (old-eps   *mjr_eps_eps*))
    (setq *mjr_eps_eps* new-eps)

    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1))
    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1.0))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1.0))

    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1))
    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1.0))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1.0))

    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1    new-eps))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1    new-eps))
    (assert-true   (mjr_eps_= (+ 1 new-eps/2) 1.0  new-eps))
    (assert-true   (mjr_eps_= (- 1 new-eps/2) 1.0  new-eps))

    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1    new-eps))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1    new-eps))
    (assert-false  (mjr_eps_= (+ 1 new-eps*2) 1.0  new-eps))
    (assert-false  (mjr_eps_= (- 1 new-eps*2) 1.0  new-eps))

    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))
    (assert-true   (mjr_eps_=0 new-eps/2))

    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))
    (assert-false  (mjr_eps_=0 new-eps*2))

    (setq *mjr_eps_eps* old-eps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_integerp

(assert-true  (mjr_eps_integerp  0))
(assert-true  (mjr_eps_integerp  1))
(assert-true  (mjr_eps_integerp -1))

(assert-true  (mjr_eps_integerp  0.0))
(assert-true  (mjr_eps_integerp  1.0))
(assert-true  (mjr_eps_integerp -1.0))

(assert-false (mjr_eps_integerp  1/2))
(assert-false (mjr_eps_integerp -1/2))
(assert-false (mjr_eps_integerp  3/2))
(assert-false (mjr_eps_integerp -3/2))

(assert-false (mjr_eps_integerp  0.5))
(assert-false (mjr_eps_integerp -0.5))
(assert-false (mjr_eps_integerp  1.5))
(assert-false (mjr_eps_integerp -1.5))

  (loop for i from -500 upto 500
        do (assert-true  (mjr_eps_integerp i))
        do (assert-true  (mjr_eps_integerp (float i)))
        do (assert-false (mjr_eps_integerp (+ 1/2 i)))
        do (assert-false (mjr_eps_integerp (+ 0.5 i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_integerp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_integerp (- i (/ *mjr_eps_eps* j))))
                 do (assert-false (mjr_eps_integerp (+ i (* *mjr_eps_eps* j))))
                 do (assert-false (mjr_eps_integerp (- i (* *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_evenp
  (assert-false (mjr_eps_evenp  1))
  (assert-false (mjr_eps_evenp -1))
  (assert-true  (mjr_eps_evenp  2))
  (assert-true  (mjr_eps_evenp -2))

  (assert-false (mjr_eps_evenp  1.0))
  (assert-false (mjr_eps_evenp -1.0))
  (assert-true  (mjr_eps_evenp  2.0))
  (assert-true  (mjr_eps_evenp -2.0))

  (loop for i from -500 upto 500 by 2
        do (assert-true  (mjr_eps_evenp i))
        do (assert-true  (mjr_eps_evenp (float i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_evenp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_evenp (- i (/ *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_oddp
  (assert-true  (mjr_eps_oddp  1))
  (assert-true  (mjr_eps_oddp -1))
  (assert-false (mjr_eps_oddp  2))
  (assert-false (mjr_eps_oddp -2))

  (assert-true  (mjr_eps_oddp  1.0))
  (assert-true  (mjr_eps_oddp -1.0))
  (assert-false (mjr_eps_oddp  2.0))
  (assert-false (mjr_eps_oddp -2.0))

  (loop for i from -501 upto 500 by 2
        do (assert-true  (mjr_eps_oddp i))
        do (assert-true  (mjr_eps_oddp (float i)))
        do (loop for j from 3 upto 3
                 do (assert-true  (mjr_eps_oddp (+ i (/ *mjr_eps_eps* j))))
                 do (assert-true  (mjr_eps_oddp (- i (/ *mjr_eps_eps* j))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_eps_zap
  (assert-equalp #(1 2 3 4)                                   (mjr_eps_zap #(1 2 3 4)))
  (assert-equalp #(1 2 3 0)                                   (mjr_eps_zap #(1 2 3 1d-15)))
  ;; Specify eps
  (assert-equalp #(0 2 3 4)                                   (mjr_eps_zap #(1 2 3 4) 2))
  (assert-equalp #(1 2 3 0)                                   (mjr_eps_zap #(1 2 3 1d-15) .1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
