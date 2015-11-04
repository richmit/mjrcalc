;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-gfp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       lib-gfp.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_GFP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_GFP :MJR_PRIME :MJR_INTU :MJR_PRNG))

(in-package :MJR_GFP-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_iexpt
  ;; Make sure everything seems OK based on intu integer expt.
  (loop for i from 1 upto 10000
        for p = (mjr_prime_random-small-prime)
        for x = (random 100000)
        for n = (random 100000)
        for p1 = (mjr_gfp_iexpt p x n)
        for p2 = (mjr_intu_mod-expt x n p)
        do (assert-equal p2   p1 (list p x n)))
  ;; Errors
  (assert-error 'error (mjr_gfp_iexpt 't      3       10))
  (assert-error 'error (mjr_gfp_iexpt 2       't      10))
  (assert-error 'error (mjr_gfp_iexpt 2       3       't))

  (assert-error 'error (mjr_gfp_iexpt #C(1 1) 3       10))
  (assert-error 'error (mjr_gfp_iexpt 2       #C(1 1) 10))
  (assert-error 'error (mjr_gfp_iexpt 2       3       #C(1 1)))
  (assert-error 'error (mjr_gfp_iexpt 2       3       1.1))       ;; n must be an integer.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_gfpp
  ;; Test cases
  (assert-false  (mjr_gfp_gfpp 2       't))      ;; symbol
  (assert-true   (mjr_gfp_gfpp 2       1.2))     ;; float (they get truncated)
  (assert-false  (mjr_gfp_gfpp 2       #C(1 1))) ;; complex
  (assert-true   (mjr_gfp_gfpp 2        2))
  (assert-true   (mjr_gfp_gfpp 2       -2))
  (assert-true   (mjr_gfp_gfpp 2        3))
  (assert-true   (mjr_gfp_gfpp 2       -3))

  ;; Test the special kinds of non-nil
  (assert-equalp nil (mjr_gfp_gfpp 2       't))      ;; symbol
  (assert-equalp 3   (mjr_gfp_gfpp 2       1.2))     ;; float (they get truncated)
  (assert-equalp nil (mjr_gfp_gfpp 2       #C(1 1))) ;; complex
  (assert-equalp 2   (mjr_gfp_gfpp 2        2))
  (assert-equalp 2   (mjr_gfp_gfpp 2       -1))
  (assert-equalp 2   (mjr_gfp_gfpp 2       -2))
  (assert-equalp 2   (mjr_gfp_gfpp 2        2))
  (assert-equalp 2   (mjr_gfp_gfpp 2        3))
  (assert-equalp 2   (mjr_gfp_gfpp 2       -3))
  (assert-equalp 1   (mjr_gfp_gfpp 2        0))
  (assert-equalp 1   (mjr_gfp_gfpp 2        1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_simplify
  (dotimes (i 10000)
    (let* ((r  (mjr_prng_int-cc -1000 1000))
           (p  (mjr_prime_random-small-prime))
           (rp (mjr_gfp_simplify p r)))
      (assert-true (integerp rp))
      (assert-true (<  rp p))
      (assert-true (>= rp 0))))
  ;; p=2
  (assert-equalp  0 (mjr_gfp_simplify 2      -2))
  (assert-equalp  1 (mjr_gfp_simplify 2      -1))
  (assert-equalp  0 (mjr_gfp_simplify 2       0))
  (assert-equalp  1 (mjr_gfp_simplify 2       1))
  (assert-equalp  0 (mjr_gfp_simplify 2       2))
  (assert-equalp  0 (mjr_gfp_simplify 2      -2.1))
  (assert-equalp  1 (mjr_gfp_simplify 2      -1.1))
  (assert-equalp  0 (mjr_gfp_simplify 2       0.1))
  (assert-equalp  1 (mjr_gfp_simplify 2       1.1))
  (assert-equalp  0 (mjr_gfp_simplify 2       2.1))
  ;; p=3
  (assert-equalp  0 (mjr_gfp_simplify 3      -3))
  (assert-equalp  1 (mjr_gfp_simplify 3      -2))
  (assert-equalp  2 (mjr_gfp_simplify 3      -1))
  (assert-equalp  0 (mjr_gfp_simplify 3       0))
  (assert-equalp  1 (mjr_gfp_simplify 3       1))
  (assert-equalp  2 (mjr_gfp_simplify 3       2))
  (assert-equalp  0 (mjr_gfp_simplify 3       3))
  (assert-equalp  0 (mjr_gfp_simplify 3      -3.1))
  (assert-equalp  1 (mjr_gfp_simplify 3      -2.1))
  (assert-equalp  2 (mjr_gfp_simplify 3      -1.1))
  (assert-equalp  0 (mjr_gfp_simplify 3       0.1))
  (assert-equalp  1 (mjr_gfp_simplify 3       1.1))
  (assert-equalp  2 (mjr_gfp_simplify 3       2.1))
  (assert-equalp  0 (mjr_gfp_simplify 3       3.1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_*
  (assert-equalp 5  (mjr_gfp_* 19 4 3 2 1))
  (assert-equalp 5  (mjr_gfp_* 19 4 3 2  ))
  (assert-equalp 12 (mjr_gfp_* 19 4 3    ))
  (assert-equalp 4  (mjr_gfp_* 19 4      ))
  (assert-equalp 1  (mjr_gfp_* 19        ))

  (assert-equalp 5  (mjr_gfp_* 19 4.1 3 2 1))
  (assert-equalp 5  (mjr_gfp_* 19 4.1 3 2  ))
  (assert-equalp 12 (mjr_gfp_* 19 4.1 3    ))
  (assert-equalp 4  (mjr_gfp_* 19 4.1      ))
  (assert-equalp 1  (mjr_gfp_* 19        ))

  (assert-equalp 5  (mjr_gfp_* 19 4.1 3.1 2 1))
  (assert-equalp 5  (mjr_gfp_* 19 4.1 3.1 2  ))
  (assert-equalp 12 (mjr_gfp_* 19 4.1 3.1    ))


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_+
  (assert-equalp 10 (mjr_gfp_+ 19 4 3 2 1))
  (assert-equalp 9  (mjr_gfp_+ 19 4 3 2  ))
  (assert-equalp 7  (mjr_gfp_+ 19 4 3    ))
  (assert-equalp 4  (mjr_gfp_+ 19 4      ))
  (assert-equalp 0  (mjr_gfp_+ 19        ))

  (assert-equalp 10 (mjr_gfp_+ 19 4.1 3 2 1))
  (assert-equalp 9  (mjr_gfp_+ 19 4.1 3 2  ))
  (assert-equalp 7  (mjr_gfp_+ 19 4.1 3    ))
  (assert-equalp 4  (mjr_gfp_+ 19 4.1      ))
  (assert-equalp 0  (mjr_gfp_+ 19        ))

  (assert-equalp 10 (mjr_gfp_+ 19 4.1 3.1 2 1))
  (assert-equalp 9  (mjr_gfp_+ 19 4.1 3.1 2  ))
  (assert-equalp 7  (mjr_gfp_+ 19 4.1 3.1    ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_-
  (assert-equalp 9     (mjr_gfp_- 19 4 3 2 1))
  (assert-equalp 10    (mjr_gfp_- 19 4 3 2  ))
  (assert-equalp 12    (mjr_gfp_- 19 4 3    ))
  (assert-equalp 15    (mjr_gfp_- 19 4      ))

  (assert-equalp 9     (mjr_gfp_- 19 4.1 3 2 1))
  (assert-equalp 10    (mjr_gfp_- 19 4.1 3 2  ))
  (assert-equalp 12    (mjr_gfp_- 19 4.1 3    ))
  (assert-equalp 15    (mjr_gfp_- 19 4.1      ))

  (assert-equalp 9     (mjr_gfp_- 19 4.1 3.1 2 1))
  (assert-equalp 10    (mjr_gfp_- 19 4.1 3.1 2  ))
  (assert-equalp 12    (mjr_gfp_- 19 4.1 3.1    ))
  (assert-equalp 15    (mjr_gfp_- 19 4.1      ))

  (assert-error 'error (mjr_gfp_- 19        ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_/
  (assert-equalp 7     (mjr_gfp_/ 19 4 3 2 1))
  (assert-equalp 7     (mjr_gfp_/ 19 4 3 2  ))
  (assert-equalp 14    (mjr_gfp_/ 19 4 3    ))
  (assert-equalp 5     (mjr_gfp_/ 19 4      ))
  (assert-error 'error (mjr_gfp_/ 19        ))
  (assert-error 'error (mjr_gfp_/ 19 0      ))
  (assert-error 'error (mjr_gfp_/ 19 19     ))
  (assert-error 'error (mjr_gfp_/ 19 -19    ))

  (assert-equalp 7     (mjr_gfp_/ 19 4.1 3 2 1))
  (assert-equalp 7     (mjr_gfp_/ 19 4.1 3 2  ))
  (assert-equalp 14    (mjr_gfp_/ 19 4.1 3    ))
  (assert-equalp 5     (mjr_gfp_/ 19 4.1      ))
  (assert-error 'error (mjr_gfp_/ 19          ))
  (assert-error 'error (mjr_gfp_/ 19 0.1      ))
  (assert-error 'error (mjr_gfp_/ 19 19.1     ))
  (assert-error 'error (mjr_gfp_/ 19 -19.1    ))

  ;; Make sure mjr_gfp_/ finds multiplicative inverses correctly.
  (dotimes (i 5000)
    (let* ((a  (mjr_prng_int-cc -1000 1000))
           (p  (mjr_prime_random-small-prime))
           (ar (mjr_gfp_simplify p a)))
      (if (zerop ar)
          (assert-error 'error (mjr_gfp_/ p a)  p a)
          (let ((ai (mjr_gfp_/ p a)))
            (assert-true (integerp  ai)         p a)
            (assert-true (<  ai p)              p a)
            (assert-true (>= ai 0)              p a)
            (assert-equalp 1 (mjr_gfp_* p a ai) p a)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_divides?
  (assert-true         (mjr_gfp_divides? 19 1 2))
  (assert-true         (mjr_gfp_divides? 19 2 3))
  (assert-true         (mjr_gfp_divides? 19 0 0))
  (assert-true         (mjr_gfp_divides? 19 2 0))
  (assert-true         (mjr_gfp_divides? 19 3 0))
  (assert-true         (mjr_gfp_divides? 19 4 0))
  (assert-false        (mjr_gfp_divides? 19 0 1))

  (assert-true         (mjr_gfp_divides? 19 1 2.1))
  (assert-true         (mjr_gfp_divides? 19 2 3.1))
  (assert-true         (mjr_gfp_divides? 19 0 0.1))
  (assert-true         (mjr_gfp_divides? 19 2 0.1))
  (assert-true         (mjr_gfp_divides? 19 3 0.1))
  (assert-true         (mjr_gfp_divides? 19 4 0.1))
  (assert-false        (mjr_gfp_divides? 19 0 1.1))

  (assert-true         (mjr_gfp_divides? 19 1.1 2))
  (assert-true         (mjr_gfp_divides? 19 2.1 3))
  (assert-true         (mjr_gfp_divides? 19 0.1 0))
  (assert-true         (mjr_gfp_divides? 19 2.1 0))
  (assert-true         (mjr_gfp_divides? 19 3.1 0))
  (assert-true         (mjr_gfp_divides? 19 4.1 0))
  (assert-false        (mjr_gfp_divides? 19 0.1 1))

  (assert-true         (mjr_gfp_divides? 19 1.1 2.1))
  (assert-true         (mjr_gfp_divides? 19 2.1 3.1))
  (assert-true         (mjr_gfp_divides? 19 0.1 0.1))
  (assert-true         (mjr_gfp_divides? 19 2.1 0.1))
  (assert-true         (mjr_gfp_divides? 19 3.1 0.1))
  (assert-true         (mjr_gfp_divides? 19 4.1 0.1))
  (assert-false        (mjr_gfp_divides? 19 0.1 1.1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_onep
  (assert-false        (mjr_gfp_onep 19 0))
  (assert-true         (mjr_gfp_onep 19 1))
  (assert-false        (mjr_gfp_onep 19 2))
  (assert-false        (mjr_gfp_onep 19 3))
  (assert-false        (mjr_gfp_onep 19 4))
  (assert-false        (mjr_gfp_onep 19 5))
  (assert-false        (mjr_gfp_onep 19 6))

  (assert-false        (mjr_gfp_onep 19 0.1))
  (assert-true         (mjr_gfp_onep 19 1.1))
  (assert-false        (mjr_gfp_onep 19 2.1))
  (assert-false        (mjr_gfp_onep 19 3.1))
  (assert-false        (mjr_gfp_onep 19 4.1))
  (assert-false        (mjr_gfp_onep 19 5.1))
  (assert-false        (mjr_gfp_onep 19 6.1))

  (dotimes (i 500)
    (let* ((p  (mjr_prime_random-small-prime)))
      (assert-true         (mjr_gfp_onep p 1.1))
      (assert-true         (mjr_gfp_onep p 1))
      (assert-false        (mjr_gfp_onep p p))
      (assert-true         (mjr_gfp_onep p (1+ p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_zerop
  (assert-true         (mjr_gfp_zerop 19 0))
  (assert-false        (mjr_gfp_zerop 19 1))
  (assert-false        (mjr_gfp_zerop 19 2))
  (assert-false        (mjr_gfp_zerop 19 3))
  (assert-false        (mjr_gfp_zerop 19 4))
  (assert-false        (mjr_gfp_zerop 19 5))
  (assert-false        (mjr_gfp_zerop 19 6))

  (assert-true         (mjr_gfp_zerop 19 0.1))
  (assert-false        (mjr_gfp_zerop 19 1.1))
  (assert-false        (mjr_gfp_zerop 19 2.1))
  (assert-false        (mjr_gfp_zerop 19 3.1))
  (assert-false        (mjr_gfp_zerop 19 4.1))
  (assert-false        (mjr_gfp_zerop 19 5.1))
  (assert-false        (mjr_gfp_zerop 19 6.1))

  (dotimes (i 500)
    (let* ((p  (mjr_prime_random-small-prime)))
      (assert-true         (mjr_gfp_zerop p 0.1))
      (assert-true         (mjr_gfp_zerop p 0))
      (assert-true         (mjr_gfp_zerop p p))
      (assert-false        (mjr_gfp_zerop p (1+ p)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_gfp_imul
  ;; Just make sure that mjr_gfp_* and mjr_gfp_imul are the same -- just in case we introduce a bug in the future.
  (dotimes (i 1000)
    (let* ((a  (mjr_prng_int-cc -1000 1000))
           (b  (mjr_prng_int-cc -1000 1000))
           (p  (mjr_prime_random-small-prime)))
      (assert-equalp (mjr_gfp_* p a b) (mjr_gfp_imul p a b))))
  ;; n must be an integer
  (assert-error 'error (mjr_gfp_imul 3 2 1.1))
  (assert-error 'error (mjr_gfp_imul 3 2 't))
  (assert-error 'error (mjr_gfp_imul 3 2 #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
