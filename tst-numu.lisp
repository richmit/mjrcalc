;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-numu.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-numu.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_NUMU-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_NUMU :MJR_PRNG :MJR_EPS))

(in-package :MJR_NUMU-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_hypot
  (assert-equal 5   (mjr_numu_hypot 3        4))
  (assert-equal 5   (mjr_numu_hypot 3        4))
  (assert-equal 5   (mjr_numu_hypot #C(3 0)  4))
  (assert-equal 5   (mjr_numu_hypot #C(3 0)  #C(4 0)))
  (assert-equal 5   (mjr_numu_hypot #C(3 0)  #C(0 4)))
  (assert-equal 5   (mjr_numu_hypot #C(0 3)  #C(0 4)))
  (assert-equal 5   (mjr_numu_hypot 0        #C(3 4)))
  (assert-equal 5   (mjr_numu_hypot #C(3 4)  0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_argument
  (assert-equality (mjr_eps_make-fixed= 0.001) 0                 (mjr_numu_argument #C(1 0)))
  (assert-equality (mjr_eps_make-fixed= 0.001) (/ pi 2)          (mjr_numu_argument #C(0 1)))
  (assert-equality (mjr_eps_make-fixed= 0.001) pi                (mjr_numu_argument #C(-1 0)))
  (assert-equality (mjr_eps_make-fixed= 0.001) (/ (* 3 pi) 2)    (mjr_numu_argument #C(0 -1)))
  (dotimes (i 100)
    (let ((x (mjr_prng_float-co -20 20))
          (y (mjr_prng_float-co 0 20)))
      (assert-equal (phase (complex x y)) (mjr_numu_argument (complex x y)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_signum-pos
  (assert-equal  1 (mjr_numu_signum-pos 3))
  (assert-equal -1 (mjr_numu_signum-pos -3))
  (assert-equal  1 (mjr_numu_signum-pos #C(3 0)))
  (assert-equal -1 (mjr_numu_signum-pos #C(-3 0)))
  (assert-equal  1 (mjr_numu_signum-pos 0))
  (assert-equal  1 (mjr_numu_signum-pos #C(0 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_csign
  (assert-equal  1 (mjr_numu_csign 3))
  (assert-equal -1 (mjr_numu_csign -3))
  (assert-equal  1 (mjr_numu_csign #C(3 0)))
  (assert-equal -1 (mjr_numu_csign #C(-3 0)))
  (assert-equal  0 (mjr_numu_csign 0))
  (assert-equal  0 (mjr_numu_csign #C(0 0)))
  (assert-equal  1 (mjr_numu_csign #C(0 3)))  ; real parts zero, so we check complex parts
  (assert-equal -1 (mjr_numu_csign #C(0 -3))) ; real parts zero, so we check complex parts
  ; Should match signum for real arguments
  (dotimes (i 100)
    (let ((j (mjr_prng_int-cc -20 20))
          (r (mjr_prng_float-co -20.0 20.0)))
      (assert-equal (mjr_numu_csign j) (signum j))
      (assert-equal (mjr_numu_csign r) (signum r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_absdif
  ;; Integers
  (assert-equal 1 (mjr_numu_absdif  0  1))
  (assert-equal 1 (mjr_numu_absdif  1  0))
  (assert-equal 1 (mjr_numu_absdif  0 -1))
  (assert-equal 1 (mjr_numu_absdif -1  0))
  (assert-equal 0 (mjr_numu_absdif  1  1))
  (assert-equal 0 (mjr_numu_absdif -1 -1))
  (assert-equal 2 (mjr_numu_absdif  1 -1))
  (assert-equal 2 (mjr_numu_absdif -1  1))
  ;; Rationals
  (assert-equal 0 (mjr_numu_absdif  1/2  1/2))
  (assert-equal 0 (mjr_numu_absdif -1/2 -1/2))
  (assert-equal 1 (mjr_numu_absdif  1/2 -1/2))
  (assert-equal 1 (mjr_numu_absdif -1/2  1/2))
  ;; Floating point
  (assert-equal 1.0 (mjr_numu_absdif  0  1.0))
  (assert-equal 1.0 (mjr_numu_absdif  1  0.0))
  (assert-equal 1.0 (mjr_numu_absdif  0 -1.0))
  (assert-equal 1.0 (mjr_numu_absdif -1  0.0))
  (assert-equal 0.0 (mjr_numu_absdif  1  1.0))
  (assert-equal 0.0 (mjr_numu_absdif -1 -1.0))
  (assert-equal 2.0 (mjr_numu_absdif  1 -1.0))
  (assert-equal 2.0 (mjr_numu_absdif -1  1.0))
  ;; These always return a floating point number
  (assert-equal  0   (mjr_numu_absdif #C(1 1) #C(1 1))) ;; Most LISPs return 0, but a few do 0.0
  (assert-equal  5.0 (mjr_numu_absdif #C(0 0) #C(3 4))) ;; Most LISPs return 5.0, but a few do 5
  (assert-equal  5.0 (mjr_numu_absdif #C(1 1) #C(4 5))) ;; Most LISPs return 5.0, but a few do 5
  ;; Errors
  (assert-error 'error (mjr_numu_absdif 't 1))    ;; non-number
  (assert-error 'error (mjr_numu_absdif 1  't))   ;; non-number
  (assert-error 'error (mjr_numu_absdif 't 't))   ;; non-number
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_fnd-max-periodic-point
  ;; Center
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point 0 1 1/2 3/2))
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point 0 1 0.5 1.5))
  (assert-equal 11 (mjr_numu_fnd-max-periodic-point 0 1 10.5 11.5))
  ;; Make sure it will go beyond first match
  (assert-equal  20 (mjr_numu_fnd-max-periodic-point 0 1 1 20.5))
  ;; Left
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point 0 1 1 1.5))
  ;; Right
  (assert-equal  2 (mjr_numu_fnd-max-periodic-point  0 1 1 2))
  (assert-equal  20 (mjr_numu_fnd-max-periodic-point 0 1 1 20))
  ;; non-zero start
  ;; Center
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point -10 1 1/2 3/2))
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point -10 1 0.5 1.5))
  (assert-equal 11 (mjr_numu_fnd-max-periodic-point -10 1 10.5 11.5))
  ;; Make sure it will go beyond first match
  (assert-equal  20 (mjr_numu_fnd-max-periodic-point -10 1 1 20.5))
  ;; Left
  (assert-equal  1 (mjr_numu_fnd-max-periodic-point -10 1 1 1.5))
  ;; Right
  (assert-equal  2 (mjr_numu_fnd-max-periodic-point  -10 1 1 2))
  (assert-equal  20 (mjr_numu_fnd-max-periodic-point -10 1 1 20))
  ;; Errors
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 't 1  2  3))  ;; non-number
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0  't 2  3))  ;; non-number
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0  1  't 3))  ;; non-number
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0  1  2  't)) ;; non-number
  (assert-error 'error (mjr_numu_fnd-max-periodic-point #C(1 1) 1       2       3))       ;; complex
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0       #C(1 1) 2       3))       ;; complex
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0       1       #C(1 1) 3))       ;; complex
  (assert-error 'error (mjr_numu_fnd-max-periodic-point 0       1       2       #C(1 1))) ;; complex
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_near-periodic-point?
  (assert-true (mjr_numu_near-periodic-point?  0 1 1))
  (assert-true (mjr_numu_near-periodic-point?  0 1 2))
  (assert-true (mjr_numu_near-periodic-point?  0 1 5))
  (assert-true (mjr_numu_near-periodic-point?  1 1 1))
  (assert-true (mjr_numu_near-periodic-point?  1 1 2))
  (assert-true (mjr_numu_near-periodic-point?  1 1 5))
  (assert-true (mjr_numu_near-periodic-point? -1 1 1))
  (assert-true (mjr_numu_near-periodic-point? -1 1 2))
  (assert-true (mjr_numu_near-periodic-point? -1 1 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_code
  ;; Make sure default is the same as :matlab
  (dotimes (i 20)
    (let ((ri (mjr_prng_int-co -1000 1000))
          (rr (/ (mjr_prng_int-co -1000 1000)
                 (mjr_prng_int-co -1000 1000)))
          (rf (mjr_prng_float-co -1000.0 1000.0))
          (rc (complex (mjr_prng_float-co -1000 1000)
                       (mjr_prng_float-co -1000 1000))))
      (assert-equal (mjr_numu_code rr) (mjr_numu_code rr :lang :lang-matlab))
      (assert-equal (mjr_numu_code ri) (mjr_numu_code ri :lang :lang-matlab))
      (assert-equal (mjr_numu_code rf) (mjr_numu_code rf :lang :lang-matlab))
      (assert-equal (mjr_numu_code rc) (mjr_numu_code rc :lang :lang-matlab))))
  ;; integer
  (assert-equal "0"   (mjr_numu_code 0))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-matlab))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-octave))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-mathematica))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-idl))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-hp48))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-maple))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-maxima))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-r))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-csv))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-csvl))
  (assert-equal "0"   (mjr_numu_code 0 :lang :lang-lisp))
  ;; Real number
  (assert-equal "0.0" (mjr_numu_code 0.0))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-matlab))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-octave))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-mathematica))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-idl))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-hp48))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-maple))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-maxima))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-r))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-csv))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-csvl))
  (assert-equal "0.0" (mjr_numu_code 0.0 :lang :lang-lisp))
  ;; Rational number
  (assert-equal "0.5" (mjr_numu_code 1/2))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-matlab))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-octave))
  (assert-equal "1/2" (mjr_numu_code 1/2 :lang :lang-mathematica))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-idl))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-hp48))
  (assert-equal "1/2" (mjr_numu_code 1/2 :lang :lang-maple))
  (assert-equal "1/2" (mjr_numu_code 1/2 :lang :lang-maxima))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-r))
  (assert-equal "0.5" (mjr_numu_code 1/2 :lang :lang-csv))
  (assert-equal "1/2" (mjr_numu_code 1/2 :lang :lang-csvl))
  (assert-equal "1/2" (mjr_numu_code 1/2 :lang :lang-lisp))
  ;; Complex number
  (assert-equal "(0.5+0.5i)"        (mjr_numu_code #C(1/2 1/2)))
  (assert-equal "(0.5+0.5i)"        (mjr_numu_code #C(1/2 1/2) :lang :lang-matlab))
  (assert-equal "(0.5+0.5i)"        (mjr_numu_code #C(1/2 1/2) :lang :lang-octave))
  (assert-equal "(1/2+I1/2)"        (mjr_numu_code #C(1/2 1/2) :lang :lang-mathematica))
  (assert-equal "complex(0.5,0.5)"  (mjr_numu_code #C(1/2 1/2) :lang :lang-idl))
  (assert-equal "(0.5,0.5)"         (mjr_numu_code #C(1/2 1/2) :lang :lang-hp48))
  (assert-equal "(1/2+i*1/2)"       (mjr_numu_code #C(1/2 1/2) :lang :lang-maple))
  (assert-equal "(1/2+1/2*%i)"      (mjr_numu_code #C(1/2 1/2) :lang :lang-maxima))
  (assert-equal "(0.5+0.5i)"        (mjr_numu_code #C(1/2 1/2) :lang :lang-r))
  (assert-equal "(0.5+i0.5)"        (mjr_numu_code #C(1/2 1/2) :lang :lang-csv))
  (assert-equal "#C(1/2 1/2)"       (mjr_numu_code #C(1/2 1/2) :lang :lang-csvl))
  (assert-equal "#C(1/2 1/2)"       (mjr_numu_code #C(1/2 1/2) :lang :lang-lisp))
  ;; Errors
  (assert-error 'error (mjr_numu_code 0 :lang :foo))  ;; Bad language
  (assert-error 'error (mjr_numu_code 't))            ;; non-number
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_sqrt
    (assert-equal                 5          (mjr_numu_sqrt 25))
    (assert-equality  #'mjr_eps_= 5.0        (mjr_numu_sqrt 25.0))
    (assert-equality  #'mjr_eps_= 5.0        (mjr_numu_sqrt 25.000001))
    (assert-equality  #'mjr_eps_= 5.0        (mjr_numu_sqrt 24.999998))
    (assert-equal                 1/5        (mjr_numu_sqrt 1/25))
    (assert-equality  #'mjr_eps_= 0.2        (mjr_numu_sqrt 0.04))
    (assert-equal                 6/5        (mjr_numu_sqrt 36/25))
    (assert-equality  #'mjr_eps_= 1.2        (mjr_numu_sqrt 1.44))
    (assert-equality  #'mjr_eps_= 5.0990195  (mjr_numu_sqrt 26))
    (assert-equal                 5          (mjr_numu_sqrt 25          :aggressive-conversion :near-int-convert))
    (assert-equal                 5          (mjr_numu_sqrt 25.0        :aggressive-conversion :near-int-convert))
    (assert-equal                 5          (mjr_numu_sqrt 25.000001   :aggressive-conversion :near-int-convert))
    (assert-equal                 5          (mjr_numu_sqrt 24.999998   :aggressive-conversion :near-int-convert))
    (assert-equal                 1/5        (mjr_numu_sqrt 1/25        :aggressive-conversion :near-int-convert))
    (assert-equality  #'mjr_eps_= 0.2        (mjr_numu_sqrt 0.04        :aggressive-conversion :near-int-convert))
    (assert-equal                 6/5        (mjr_numu_sqrt 36/25       :aggressive-conversion :near-int-convert))
    (assert-equality  #'mjr_eps_= 1.2        (mjr_numu_sqrt 1.44        :aggressive-conversion :near-int-convert))
    (assert-equality  #'mjr_eps_= 5.0990195  (mjr_numu_sqrt 26          :aggressive-conversion :near-int-convert))
    (assert-equal                 5          (mjr_numu_sqrt 25          :aggressive-conversion :near-rat-convert))
    (assert-equal                 5          (mjr_numu_sqrt 25.0        :aggressive-conversion :near-rat-convert))
    (assert-equal                 5          (mjr_numu_sqrt 25.000001   :aggressive-conversion :near-rat-convert))
    (assert-equal                 5          (mjr_numu_sqrt 24.999998   :aggressive-conversion :near-rat-convert))
    (assert-equal                 1/5        (mjr_numu_sqrt 1/25        :aggressive-conversion :near-rat-convert))
    (assert-equal                 1/5        (mjr_numu_sqrt 0.04        :aggressive-conversion :near-rat-convert))
    (assert-equal                 6/5        (mjr_numu_sqrt 36/25       :aggressive-conversion :near-rat-convert))
    (assert-equal                 6/5        (mjr_numu_sqrt 1.44        :aggressive-conversion :near-rat-convert))
    (assert-equality  #'mjr_eps_= 5.0990195  (mjr_numu_sqrt 26          :aggressive-conversion :near-rat-convert))
    ;; Do some random cases
    (dotimes (i 200)
      (let ((z (complex (mjr_prng_random 100) (mjr_prng_int-co 1 100)))
            (r (mjr_prng_random 100)))
        (assert-equal z  (mjr_numu_sqrt (* z z)))
        (assert-equal r  (mjr_numu_sqrt (* r r)))))
    ;; Errors
    (assert-error 'error     (mjr_numu_sqrt 't))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_gamma-lanczos
  ;; Maxima
  ;;   gamma(5.3);       => 38.07797644995235
  ;;   gamma(0.3);       => 2.99156898768759
  ;;   gamma(-0.3);      => -4.326851108825193
  ;;   gamma(-5.3);      => 0.0192416582798931
  ;;   gamma(5);         => 24
  ;;   gamma(-5.2+2*%i); => (-9.465845276366529e-5 %i) -2.387829915766869e-5
  ;;   gamma(5.2+2*%i);  => (-0.3030012629029413 %i) -21.57090971944615
  ;;   gamma(5.2-2*%i);  => 0.3030012629029413 %i -21.57090971944615
  ;;   gamma(-5.2-2*%i)  => 9.465845276366529e-5 %i -2.387829915766869e-5

  (assert-equality (mjr_eps_make-fixed= 0.0001) 38.07797644995235                               (mjr_numu_gamma-lanczos  5.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 2.99156898768759                                (mjr_numu_gamma-lanczos  0.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) -4.326851108825193                              (mjr_numu_gamma-lanczos -0.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 0.0192416582798931                              (mjr_numu_gamma-lanczos -5.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 24                                              (mjr_numu_gamma-lanczos  5))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-2.387829915766869e-5 -9.465845276366529e-5) (mjr_numu_gamma-lanczos #C(-5.2 2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-21.57090971944615 -0.3030012629029413)      (mjr_numu_gamma-lanczos #C(5.2 2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-21.57090971944615 0.3030012629029413)       (mjr_numu_gamma-lanczos #C(5.2 -2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-2.387829915766869e-5 9.465845276366529e-5)  (mjr_numu_gamma-lanczos #C(-5.2 -2)))
  (assert-error 'error                                                                          (mjr_numu_gamma-lanczos 0))
  (assert-error 'error                                                                          (mjr_numu_gamma-lanczos -1))
  ;; Check mirror sym (should be identical)
  (loop for x from -20.5 upto 20.5 by 1.0
        do (loop for y from -20.5 upto 20.5 by 1.0
                 for z = (complex x y)
                 do (assert-equality (mjr_eps_make-fixed= 0.000001) (conjugate (mjr_numu_gamma-lanczos z)) (mjr_numu_gamma-lanczos (conjugate z)) z)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_gamma-spouge
  (assert-equality (mjr_eps_make-fixed= 0.0001) 38.07797644995235                               (mjr_numu_gamma-spouge  5.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 2.99156898768759                                (mjr_numu_gamma-spouge  0.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) -4.326851108825193                              (mjr_numu_gamma-spouge -0.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 0.0192416582798931                              (mjr_numu_gamma-spouge -5.3))
  (assert-equality (mjr_eps_make-fixed= 0.0001) 24                                              (mjr_numu_gamma-spouge  5))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-2.387829915766869e-5 -9.465845276366529e-5) (mjr_numu_gamma-spouge #C(-5.2 2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-21.57090971944615 -0.3030012629029413)      (mjr_numu_gamma-spouge #C(5.2 2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-21.57090971944615 0.3030012629029413)       (mjr_numu_gamma-spouge #C(5.2 -2)))
  (assert-equality (mjr_eps_make-fixed= 0.0001) #C(-2.387829915766869e-5 9.465845276366529e-5)  (mjr_numu_gamma-spouge #C(-5.2 -2)))
  (assert-error 'error                                                                          (mjr_numu_gamma-spouge 0))
  (assert-error 'error                                                                          (mjr_numu_gamma-spouge -1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_gamma
  ;; This function is tested by mjr_numu_gamma-lanczos

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_gamma-spouge-lanczos
  ;; Positive, real
  (loop for x from 0.0001d0 upto 120.0d0 by 0.002d0
        do (assert-equality (mjr_eps_make-fixed= -0.10)  (mjr_numu_gamma-spouge x) (mjr_numu_gamma-lanczos x) x)) ;; Good to .1%
  ;; Complex plane
  (loop for x from -20.11 upto 20.110 by 0.2d0
        do (loop for y from -20.11d0 upto 20.11d0 by 0.2d0
                 for z = (complex x y)
                 do (assert-equality (mjr_eps_make-fixed= -0.010)  (mjr_numu_gamma-spouge z) (mjr_numu_gamma-lanczos z) z))) ;; Good to .1%
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_sum
  (assert-equal 385  (mjr_numu_sum :start 1 :end 10 :step 1 :seq-fun (lambda (x) (* x x))))
  (assert-equal 385  (mjr_numu_sum :start 1 :end 10 :len 10 :seq-fun (lambda (x) (* x x))))

  (assert-equal 165  (mjr_numu_sum :start 1 :end 10 :step 1 :seq-fun (lambda (x) (if (oddp x) (* x x) 0))))
  (assert-equal 165  (mjr_numu_sum :start 1 :end 10 :step 1 :seq-fun (lambda (x) (and (oddp x) (* x x)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_prod
  (assert-equal 13168189440000  (mjr_numu_prod :start 1 :end 10 :step 1 :seq-fun (lambda (x) (* x x))))
  (assert-equal 13168189440000  (mjr_numu_prod :start 1 :end 10 :len 10 :seq-fun (lambda (x) (* x x))))

  (assert-equal 893025          (mjr_numu_prod :start 1 :end 10 :step 1 :seq-fun (lambda (x) (if (oddp x) (* x x) 1))))
  (assert-equal 893025          (mjr_numu_prod :start 1 :end 10 :step 1 :seq-fun (lambda (x) (and (oddp x) (* x x)))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_abs-max
  (assert-equal  2 (mjr_numu_abs-max  2))
  (assert-equal -2 (mjr_numu_abs-max -2))
  (assert-equal  0 (mjr_numu_abs-max  0))

  (assert-equal -2 (mjr_numu_abs-max -2  2)) ;; Stable -- find FIRST biggest one
  (assert-equal  2 (mjr_numu_abs-max  2 -2))

  (assert-equal -3 (mjr_numu_abs-max -3   2))
  (assert-equal  3 (mjr_numu_abs-max  3   2))
  (assert-equal -3 (mjr_numu_abs-max -3  -2))
  (assert-equal  3 (mjr_numu_abs-max  3  -2))

  (assert-equal  5 (mjr_numu_abs-max  1 2 3 4 5))
  (assert-equal  5 (mjr_numu_abs-max  5 4 3 2 1))
  (assert-equal  5 (mjr_numu_abs-max  1 2 3 4 5 -5))
  (assert-equal  5 (mjr_numu_abs-max  5 4 3 2 1 -5))
  (assert-equal -6 (mjr_numu_abs-max  1 2 3 4 5 -6))
  (assert-equal -6 (mjr_numu_abs-max  5 4 3 2 1 -6))
  (assert-equal -6 (mjr_numu_abs-max  1 2 3 -6 4 5))
  (assert-equal -6 (mjr_numu_abs-max  5 4 3 -6 2 1))
  (assert-equal -6 (mjr_numu_abs-max  -6 1 2 3 4 5))
  (assert-equal -6 (mjr_numu_abs-max  -6 5 4 3 2 1))
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_numu_abs-min
  (assert-equal  2 (mjr_numu_abs-min  2))
  (assert-equal -2 (mjr_numu_abs-min -2))
  (assert-equal  0 (mjr_numu_abs-min  0))

  (assert-equal -2 (mjr_numu_abs-min -2  2)) ;; Stable -- find FIRST biggest one
  (assert-equal  2 (mjr_numu_abs-min  2 -2))

  (assert-equal -2 (mjr_numu_abs-min -2   3))
  (assert-equal  2 (mjr_numu_abs-min  2   3))
  (assert-equal -2 (mjr_numu_abs-min -2  -3))
  (assert-equal  2 (mjr_numu_abs-min  2  -3))

  (assert-equal  1 (mjr_numu_abs-min  1 2 3 4 5))
  (assert-equal  1 (mjr_numu_abs-min  5 4 3 2 1))
  (assert-equal  1 (mjr_numu_abs-min  1 2 3 4 5 -1))  ;; stable
  (assert-equal  1 (mjr_numu_abs-min  5 4 3 2 1 -1))  ;; stable
  (assert-equal -1 (mjr_numu_abs-min  2 3 4 5 6 -1))
  (assert-equal -1 (mjr_numu_abs-min  6 5 4 3 2 -1))
  (assert-equal -1 (mjr_numu_abs-min  2 3 4 -1 5 6))
  (assert-equal -1 (mjr_numu_abs-min  6 5 4 -1 3 2))
  (assert-equal -1 (mjr_numu_abs-min  -1 2 3 4 5 6))
  (assert-equal -1 (mjr_numu_abs-min  -1 6 5 4 3 2))

 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
