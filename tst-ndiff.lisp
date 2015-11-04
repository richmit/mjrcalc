;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-ndiff.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Unit tests for :mjr_ndiff.@EOL
;; @Keywords  unit tests lisp interactive numerical differentiation
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_NDIFF-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_EPS :MJR_NDIFF))

(in-package :MJR_NDIFF-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_ndiff_central
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 3 upto 15 by 2
                       do (assert-equality (mjr_eps_make-fixed= 0.01)
                                           (funcall dfunc x)
                                           (mjr_ndiff_central func x :order 1 :points p)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_ndiff_backward
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 2 upto 8 by 2
                          do (assert-equality (mjr_eps_make-fixed= .01)
                                           (funcall dfunc x)
                                           (mjr_ndiff_backward func x :order 1 :points p)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_ndiff_forward
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 2 upto 8 by 2
                          do (assert-equality (mjr_eps_make-fixed= .01)
                                              (funcall dfunc x)
                                              (mjr_ndiff_forward func x :order 1 :points p)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_ndiff_complex
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (assert-equality (mjr_eps_make-fixed= .01)
                                     (funcall dfunc x)
                                     (mjr_ndiff_complex func x))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_ndiff_lagrange
  (loop for x in '(-0.5 0.0 0.5)
        do (loop for func in (list #'cos #'sin #'tan #'exp)
                 do (loop with h = (* 300 (sqrt single-float-epsilon))  ;; We use a big h because of errors in interpolation
                          for i from 1 upto 7
                          for p = (1+ (* 2 i))
                          do (assert-equality (mjr_eps_make-fixed= 0.001)
                                              (mjr_ndiff_central  func x :order 1 :points p :h h)
                                              (mjr_ndiff_lagrange func x (loop for j from (- i) upto i collect (* j h)))
                                              (list func x p)))))

  (loop for x in '(-0.5 0.0 0.5)
        do (loop for func in (list #'cos #'sin #'tan #'exp)
                 do (loop with h = (* 300 (sqrt single-float-epsilon))  ;; We use a big h because of errors in interpolation
                          for p from 2 upto 10 by 2
                          do (assert-equality (mjr_eps_make-fixed= 0.001)
                                              (mjr_ndiff_forward  func x :order 1 :points p :h h)
                                              (mjr_ndiff_lagrange func x (loop for j from 1 upto p collect (* (1- j) h)))
                                              (list func x p))
                          do (assert-equality (mjr_eps_make-fixed= 0.001)
                                              (mjr_ndiff_backward func x :order 1 :points p :h h)
                                              (mjr_ndiff_lagrange func x (loop for j from 1 upto p collect (- (* (1- j) h))))
                                              (list func x p)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
