;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-ndiff.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Numerical differentiation.@EOL
;; @std       Common Lisp
;; @see       use-ndiff.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2010,2012,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_NDIFF-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_EPS :MJR_NDIFF))

(in-package :MJR_NDIFF-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ndiff_central
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 3 upto 15 by 2
                       do (assert-equality (mjr_eps_make-fixed= 0.01)
                                           (funcall dfunc x)
                                           (mjr_ndiff_central func x :order 1 :points p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ndiff_backward
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 2 upto 8 by 2
                          do (assert-equality (mjr_eps_make-fixed= .01)
                                           (funcall dfunc x)
                                           (mjr_ndiff_backward func x :order 1 :points p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ndiff_forward
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (loop for p from 2 upto 8 by 2
                          do (assert-equality (mjr_eps_make-fixed= .01)
                                              (funcall dfunc x)
                                              (mjr_ndiff_forward func x :order 1 :points p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_ndiff_complex
  (loop for x in '(-1.0 -0.5 0.0 0.5 1.0)
        do (loop for func  in (list #'cos                    #'sin #'tan                             #'exp)
                 for dfunc in (list (lambda (x) (- (sin x))) #'cos (lambda (x) (expt (/ (cos x)) 2)) #'exp)
                 do (assert-equality (mjr_eps_make-fixed= .01)
                                     (funcall dfunc x)
                                     (mjr_ndiff_complex func x))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
