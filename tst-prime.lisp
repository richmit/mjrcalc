;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-prime.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-prime.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1992,1994,1997,1998,2004,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_prime_pimep-miller-rabin-deterministic: Correct refs and fix name.@EOL@EOL
;; @todo      mjr_prime_pimep-miller-rabin-deterministic: Correct refs.@EOL@EOL
;; @todo      mjr_prime_probable-primep-lucas-selfridge: Finish documentation string.@EOL@EOL
;; @todo      mjr_prime_probable-primep-lucas-selfridge: Add alternative selection for D.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PRIME-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PRIME :MJR_PRNG :MJR_INTU))

(in-package :MJR_PRIME-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the small prime list..

(if (null mjr_prime::*mjr_prime_small-list*)
    (mjr_prime_init-small-prime-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_all-factors-naive (n)
  "Return a list of all of the factors of the given number.
Prime numbers will have a return list that contains only two elements (the input number and the number 1).
This function is very slow, but provides a regression test for mjr_prime_all-factors -- use that function instead of this one."
  (if (integerp n)
      (let ((small-factors (loop for i from 1 upto (isqrt (abs n)) when (= 0 (mod n i)) collect i)))
        (remove-duplicates (append small-factors (reverse (mapcar (lambda (x) (/ (abs n) x)) small-factors)))))
      (error "mjr_prime_all-factors: input must be an integer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_jacobi-symbol-naive (a n)
  (reduce #'* (mapcar (lambda (x) (expt (mjr_prime_legendre-symbol a (car x)) (cdr x))) (mjr_prime_prime-factorization n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_lucas-sequence-naive (n p q modn retu)
  ""
  (flet ((modit (k) (if modn (mod k modn) k)))
    (let ((i    1)                              ;; Start with i=1
          (ui   (modit 1))                      ;; $U_{1}=1$
          (ui+1 (modit P))                      ;; $U_{2}=P$
          (vi   (modit P))                      ;; $V_{1}=P$
          (vi+1 (modit (- (* P P) (* 2 Q)))))   ;; $V_{2}=P^2-2Q$
      (cond ((= n 0) (if retu 0    2))          ;; $U_{0}=0$, $V_{0}=2$
            ((= n 1) (if retu ui   vi))
            ((= n 2) (if retu ui+1 vi+1))
            ((> n 1) (progn
                       (loop for qti = (if modn (mjr_intu_mod-expt q i modn) (expt q i))
                                  while (< (* i 2) (1+ n))
                                  do (psetq i    (* i 2)
                                            ui   (modit (* ui vi))                   ;; $$U_{2i}   = U_i V_i$$
                                            ui+1 (modit (- (* ui+1 vi) qti))         ;; $$U_{2i+1} = U_{i+1} V_i - Q^i$$
                                            vi   (modit (- (* vi vi) (* 2 qti)))     ;; $$V_{2i}   = V_i^2 - 2Q^i$$
                                            vi+1 (modit (- (* vi+1 vi) (* p qti))))) ;; $$V_{2i+1} = V_{i+1} V_i - PQ^i$$

                            (loop while (< i n)
                                  do (psetq i (+ i 1)
                                            ui   (if retu       ui+1)
                                            ui+1 (if retu       (modit (- (* p ui+1) (* q ui))))   ;; $U_n = P U_{n-1} - Q U_{n-2}$
                                            vi   (if (not retu) vi+1)
                                            vi+1 (if (not retu) (modit (- (* p vi+1) (* q vi)))))) ;; $V_n = P V_{n-1} - Q V_{n-2}$
                            (if retu
                                ui
                                vi)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_prime_small-list-naive* nil
  "An array of the first few prime numbers.")
(defvar *mjr_prime_small-bitmap-naive* nil
  "An bitmap of the first few prime numbers.")
(defvar *mjr_prime_small-count-naive* 0
  "Number of prime numbers in the *mjr_prime_small-list-naive* array.  Normally 6 million, but may be smaller")
(defvar *mjr_prime_small-max-naive* 0
  "Largest prime number in the *mjr_prime_small-list-naive* array")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prime_init-small-prime-list-naive (&optional (max-prime-can 104395301))
  "This function uses a slow and RAM hungry implementation of the sieve of Atkin to do what MJR_PRIME_INIT-SMALL-PRIME-LIST can do.
This function provides a simplistic implementation of a sieve using an entirely different algorithm so that we can use this
function as a part of a regression test to help verify the correctness of MJR_PRIME_INIT-SMALL-PRIME-LIST.

References:
  Atkin & Bernstein (2004); Prime sieves using binary quadratic forms; Mathematics of Computation 73."
  (if (> max-prime-can array-dimension-limit) (error "mjr_prime_init-small-prime-list-naive: The array size limit is too small for such a large sieve!"))
  (let ((abv (make-array (1+ max-prime-can) :element-type 'bit  :initial-element 0)))
    (mapc (lambda (n) (setf (aref abv n) 1)) (list 2 3))
    (loop for x from 1 upto (isqrt max-prime-can) ; Solve the equation
          do (loop for y from 1 upto (isqrt max-prime-can)
                   do (let* ((n (+ (* 4 x x) (* y y))))
                        (if (and (< n max-prime-can) (member (mod n 12) (list 1 5)))
                            (setf (aref abv n) (if (= 1 (aref abv n)) 0 1))))
                   do (let* ((n (+ (* 3 x x) (* y y))))
                        (if (and (< n max-prime-can) (= (mod n 12) 7))
                            (setf (aref abv n) (if (= 1 (aref abv n)) 0 1))))
                   do (let* ((n (- (* 3 x x) (* y y))))
                        (if (and (> x y) (< n max-prime-can) (= (mod n 12) 11))
                            (setf (aref abv n) (if (= 1 (aref abv n)) 0 1))))))
    (loop for i from 5 upto max-prime-can  ; Keep only square-free numbers
          when (= 1 (aref abv i))
          do (loop for j from (* i i) upto max-prime-can by (* i i)
                   do (setf (aref abv j) 0)))
    (setq *mjr_prime_small-list-naive* (make-array (loop for b bit across abv when (= b 1) count 1)
                                             :element-type 'fixnum
                                             :initial-contents (loop for i from 2 upto max-prime-can when (= 1 (aref abv i)) collect i)))
    (setq *mjr_prime_small-count-naive* (length *mjr_prime_small-list-naive*))
    (setq *mjr_prime_small-max-naive* (aref *mjr_prime_small-list-naive* (1- *mjr_prime_small-count-naive*)))
    (setq *mjr_prime_small-bitmap-naive* abv)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_init-small-prime-list

  ; Make sure small prime list gets created.
  (assert-true mjr_prime::*mjr_prime_small-list*)
  (assert-true mjr_prime::*mjr_prime_small-bitmap*)
  (assert-true (< 0 mjr_prime::*mjr_prime_small-count*))
  (assert-true (< 0 mjr_prime::*mjr_prime_small-max*))

  ;; atkin will not get a list of the same length given the same starting point, so we check the values that are computed by both
  (mjr_prime_init-small-prime-list-naive)
  (assert-equalp  (length mjr_prime::*mjr_prime_small-list*) mjr_prime::*mjr_prime_small-count*)
  (assert-equalp  (length *mjr_prime_small-list-naive*)      *mjr_prime_small-count-naive*)
  (assert-equalp  5999999                                    (length *mjr_prime_small-list-naive*))
  (assert-equalp  6000001                                    (length mjr_prime::*mjr_prime_small-list*))
  (assert-equalp  27838752                                   (length mjr_prime::*mjr_prime_small-bitmap*))
  (dotimes (i (min (length mjr_prime::*mjr_prime_small-list*) (length *mjr_prime_small-list-naive*)))
    (assert-equalp (aref *mjr_prime_small-list-naive* i) (aref mjr_prime::*mjr_prime_small-list* i) i))
  (setq *mjr_prime_small-list-naive* nil)
  (setq *mjr_prime_small-bitmap-naive* nil)
  (setq *mjr_prime_small-count-naive* 0)
  (setq *mjr_prime_small-max-naive* 0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_nth-small-prime
  ; Too small
  (assert-equal nil (mjr_prime_nth-small-prime -1))
  (assert-equal 2   (mjr_prime_nth-small-prime  0))
  ; Good indexes
  ;; OEIS A000040
  (loop with lst = '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149
                     151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271)
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_nth-small-prime i) i))
  ; Too big
  (assert-equal nil (mjr_prime_nth-small-prime  mjr_prime::*mjr_prime_small-count*))
  (assert-equal nil (mjr_prime_nth-small-prime  (1+ mjr_prime::*mjr_prime_small-count*)))
  (assert-equal nil (mjr_prime_nth-small-prime  (expt 2 64)))
  ;; Make sure mjr_prime_small-prime-index and mjr_prime_nth-small-prime are consistant
  (dotimes (i 200)
    (let ((r (random mjr_prime::*mjr_prime_small-count*)))
      (assert-equal r (mjr_prime_small-prime-index (mjr_prime_nth-small-prime r)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_small-prime-index
  ;; XREF: Tested in mjr_prime_nth-small-prime
  ;; TODO: Code converage should be increased
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_all-factors
  (dotimes (i 50)
    (let ((j (random 1000000000)))
      (assert-equalp (mjr_prime_all-factors j) (mjr_prime_all-factors-naive j))))
  (assert-equal '(1)                    (mjr_prime_all-factors 1))
  (assert-equal '(1 2)                  (mjr_prime_all-factors 2))
  (assert-equal '(1 3)                  (mjr_prime_all-factors 3))
  (assert-equal '(1 2 4)                (mjr_prime_all-factors 4))
  (assert-equal '(1 5)                  (mjr_prime_all-factors 5))
  (assert-equal '(1 2 3 6)              (mjr_prime_all-factors 6))
  (assert-equal '(1 7)                  (mjr_prime_all-factors 7))
  (assert-equal '(1 2 4 8)              (mjr_prime_all-factors 8))
  (assert-equal '(1 3 5 15)             (mjr_prime_all-factors 15))
  (assert-equal '(1 2 3 5 6 10 15 30)   (mjr_prime_all-factors 30))
  (assert-equal '(1 2 3 4 6 9 12 18 36) (mjr_prime_all-factors 36))
  ;; Errors
  (assert-error 'error (mjr_prime_all-factors 't))
  ;; MJR TODO NOTE mjr_prime_all-factors: Fix next test
  ;(assert-error 'error (mjr_prime_all-factors 10.0))
  (assert-error 'error (mjr_prime_all-factors #C(1 1)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_deficient?
  ;; OEIS A005100
  (loop with lst = '(1 2 3 4 5 7 8 9 10 11 13 14 15 16 17 19 21 22 23 25 26 27 29 31 32 33 34 35 37 38 39 41 43 44 45 46 47 49 50 51
                     52 53 55 57 58 59 61 62 63 64 65 67 68 69 71 73 74 75 76 77 79 81 82 83 85 86)
        with c = (car lst)
        for n from 1
        while c
        do (if (= n c)
               (progn
                 (setq lst (cdr lst)
                       c   (car lst))
                 (assert-true (mjr_prime_deficient? n))
               (assert-true  (mjr_prime_deficient? n)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_abundant?
  ;; OEIS A005101
  (loop with lst = '(12 18 20 24 30 36 40 42 48 54 56 60 66 70 72 78 80 84 88 90 96 100 102 104 108 112 114 120 126 132 138 140 144
                     150 156 160 162 168 174 176 180 186 192 196 198 200 204 208 210 216 220 222 224 228 234 240 246 252 258 260
                     264 270)
        with c = (car lst)
        for n from 1
        while c
        do (if (= n c)
               (progn
                 (setq lst (cdr lst)
                       c   (car lst))
                 (assert-true (mjr_prime_abundant? n))
               (assert-true  (mjr_prime_abundant? n)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_perfect?
  ;; OEIS A000396
  (loop with lst = '(6 28 496 8128 33550336)
        with c = (car lst)
        for n from 1
        while c
        do (if (= n c)
               (progn
                 (setq lst (cdr lst)
                       c   (car lst))
                 (assert-true (mjr_prime_perfect? n))
               (assert-true  (mjr_prime_perfect? n)))))
  ;; OEIS A000396
  (loop for n in '(8589869056 137438691328 2305843008139952128)
        do (assert-true (mjr_prime_perfect? n)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_strong-probable-primep
  (loop for (a . lst) in '((2         . (2047 3277 4033 4681 8321 15841 29341 42799 49141 52633 65281 74665 80581 85489 88357 90751
                                         104653 130561 196093 220729 233017 252601 253241 256999 271951 280601 314821 357761 390937
                                         458989 476971 486737))
                           (3         . (121 703 1891 3281 8401 8911 10585 12403 16531 18721 19345 23521 31621 44287 47197 55969
                                         63139 74593 79003 82513 87913 88573 97567 105163 111361 112141 148417 152551 182527 188191
                                         211411 218791 221761 226801))
                           (5         . (781 1541 5461 5611 7813 13021 14981 15751 24211 25351 29539 38081 40501 44801 53971 79381
                                         100651 102311 104721 112141 121463 133141 141361 146611 195313 211951 216457 222301 251521
                                         289081 290629 298271 315121))
                           (7         . (25 325 703 2101 2353 4525 11041 14089 20197 29857 29891 39331 49241 58825 64681 76627
                                         78937 79381 87673 88399 88831 102943 109061 137257 144901 149171 173951 178709 188191
                                         197633 219781 227767 231793 245281))
                           (11        . (133 793 2047 4577 5041 12403 13333 14521 17711 23377 43213 43739 47611 48283 49601 50737
                                         50997 56057 58969 68137 74089 85879 86347 87913 88831 102173 111055 114211 115231 137149
                                         139231 171601 172369 193249 196555))
                           (13        . (85 1099 5149 7107 8911 9637 13019 14491 17803 19757 20881 22177 23521 26521 35371 44173
                                         45629 54097 56033 57205 75241 83333 85285 86347 102719 110309 153401 184339 191959 222529
                                         242845 253021 253927 269861))
                           (17        . (9 91 145 781 1111 2821 4033 4187 5365 5833 6697 7171 15805 19729 21781 22791 24211 26245
                                         31621 33001 33227 34441 35371 38081 42127 49771 71071 74665 77293 78881 88831 96433 97921
                                         98671 101101 102311 125563 129493))
                           (19        . (9 49 169 343 1849 2353 2701 4033 4681 6541 6697 7957 9997 12403 13213 13747 15251 16531
                                         18769 19729 24761 30589 31621 31861 32477 41003 49771 63139 64681 65161 66421 68257 73555
                                          96049 102831 118957 129961 137311))
                         ;;((2 3 5 7) . (3215031751 118670087467 307768373641 315962312077 354864744877 457453568161
                         ;;              528929554561 546348519181 602248359169))
                         ;;((2 3 5)   . (25326001 161304001 960946321 1157839381 3215031751 3697278427 5764643587 6770862367
                         ;;              14386156093 15579919981 18459366157 19887974881 21276028621))
                         ;;((2 3)     . (1373653 1530787 1987021 2284453 3116107 5173601 6787327 11541307 13694761 15978007
                         ;;              16070429 16879501 25326001 27509653 27664033 28527049 54029741 61832377 66096253))
                           ((2 3)     . (1373653 1530787 1987021))
                           )
        do (loop with mx = (apply #'max lst)
                 with c = (car lst)
                 for n from 1 upto mx
                 for b = (and (not (mjr_prime_primep n)) (mjr_prime_strong-probable-primep n a))
                 do (if (= n c)
                        (progn
                          (setq lst (cdr lst)
                                c   (car lst))
                          (assert-true b))
                        (assert-false b))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_pimep-miller-rabin-deterministic
  (loop for i from 1 upto (min mjr_prime::*mjr_prime_small-max* 500000)
        do (assert-equalp (mjr_prime_primep-small i) (mjr_prime_pimep-miller-rabin-deterministic i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_legendre-symbol
  (assert-equalp 0   (mjr_prime_legendre-symbol 5 5))
  (assert-equalp 0   (mjr_prime_legendre-symbol 0 5))
  (assert-equalp -1  (mjr_prime_legendre-symbol 2 3))
  (assert-equalp -1  (mjr_prime_legendre-symbol 2 5))
  (assert-equalp 0   (mjr_prime_legendre-symbol 10 5))
  (assert-equalp 1   (mjr_prime_legendre-symbol 10 3))
  (assert-equalp -1  (mjr_prime_legendre-symbol -3 5))
  (assert-equalp 1   (mjr_prime_legendre-symbol -3 2))
  (assert-equalp -1  (mjr_prime_legendre-symbol 5 3))
  (assert-equalp -1  (mjr_prime_legendre-symbol 5 7))
  (assert-equalp -1  (mjr_prime_legendre-symbol 2 5))
  (assert-equalp -1  (mjr_prime_legendre-symbol 3 5))
  (assert-equalp 0   (mjr_prime_legendre-symbol 5 5))
  (assert-equalp -1  (mjr_prime_legendre-symbol 7 5))
  (assert-equalp 1   (mjr_prime_legendre-symbol 11 5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_jacobi-symbol
  (assert-equalp 1  (mjr_prime_jacobi-symbol 1236 20003))
  (assert-equalp 1  (mjr_prime_jacobi-symbol 1236 1111))
  (assert-equalp 0  (mjr_prime_jacobi-symbol 1236 21))
  (assert-equalp -1 (mjr_prime_jacobi-symbol 1236 221))
  (assert-equalp -1 (mjr_prime_jacobi-symbol 1001 9907))

  ;; mjr_prime_jacobi-symbol === mjr_prime_legendre-symbol when second argument is an odd prime...
  (loop for i from 1 upto 1000
        for p = (aref mjr_prime::*mjr_prime_small-list* i)  ;; primes from 3 on
        for a = (mjr_prng_int-cc 0 10000)
        do (assert-equalp (mjr_prime_jacobi-symbol a p) (mjr_prime_legendre-symbol a p) a p))
  ;; Naive version should match production version
  (loop for i from 1 upto 1000
        for noe = (mjr_prng_int-cc 0 10000)
        for n = (if (evenp noe) (1+ noe) noe)
        for a = (mjr_prng_int-cc 0 10000)
        do (assert-equalp (mjr_prime_jacobi-symbol-naive a n) (mjr_prime_jacobi-symbol a n) a n))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_probable-primep-lucas-selfridge
  ;; First 10 are from "Perfect, Amicable, and Sociable Numbers: A Computational Approach" By Song Y. Yan
  (loop with lst = '(323 377 1159 1829 3827 5459 5777 9071 9179 10877 11419 11663 13919 14839 16109 16211 18407 18971 19043 22499
                     23407 24569 25199 25877 26069 27323 32759 34943 35207 39059 39203 39689 40309 44099 46979 47879)
        with c = (car lst)
        for n from 1
        for bs = (mjr_prime_primep-small n)
        for bl = (mjr_prime_probable-primep-lucas-selfridge n)
        while c
        do (if (= n c)
               (progn
                 (setq lst (cdr lst)
                       c   (car lst))
                 (assert-false (equalp bs bl) n))
               (assert-true  (equalp bs bl)   n)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_probable-primep-bpsw
  (assert-false (mjr_prime_probable-primep-bpsw 3825123056546413051))

  (loop for i from 1 upto (min mjr_prime::*mjr_prime_small-max* 500000)
        do (assert-equalp (mjr_prime_primep-small i) (mjr_prime_probable-primep-bpsw i) i))

  ;; A000668 Mersenne primes
  (loop for i in '(3 7 31 127 8191 131071 524287 2147483647 2305843009213693951 618970019642690137449562111
                   162259276829213363391578010288127 170141183460469231731687303715884105727)
        do (assert-true (mjr_prime_probable-primep-bpsw i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_lucas-sequence
  ;; Random tests
  (dotimes (i 10000)
    (let ((n (mjr_prng_int-cc 0 10000))
          (p (mjr_prng_int-cc -10 10))
          (q (mjr_prng_int-cc -10 10))
          (m (mjr_prng_int-cc 100 1000)))
      (assert-equalp (mjr_prime_lucas-sequence-naive n p q m 't) (mjr_prime_lucas-sequence n p q m 't) n p q m)))

  ;; A few lucas sequences with closed form solutions for the terms
  (dotimes (k 1000)
    (assert-equalp (mjr_prime_lucas-sequence k 3 2 nil 't)  (1- (expt 2 k))       k)  ;; p=3, q=2, U_k=2^k-1       (OEIS A000225)
    (assert-equalp (mjr_prime_lucas-sequence k 3 2 nil nil) (1+ (expt 2 k))       k)  ;; p=3, q=2, V_k=2^k+1       (OEIS A000051)
    (assert-equalp (mjr_prime_lucas-sequence k 5 4 nil 't)  (/ (1- (expt 4 k)) 3) k)  ;; p=5, q=4, U_k=(4^k - 1)/3 (OEIS A002450)
    (assert-equalp (mjr_prime_lucas-sequence k 5 4 nil nil) (1+ (expt 4 k))       k)  ;; p=5, q=4, V_k=(4^k + 1)   (OEIS A052539)
    (assert-equalp (mjr_prime_lucas-sequence k 4 3 nil 't)  (/ (1- (expt 3 k)) 2) k)  ;; p=4, q=3, U_k=(3^n - 1)/2 (OEIS A003462)
    (assert-equalp (mjr_prime_lucas-sequence k 4 3 nil nil) (1+ (expt 3 k))       k)) ;; p=4, q=3, V_k=(3^k + 1)   (OEIS A034472)

  ;; NOTE: This function is tested heavily by other tests too: mjr_prime_fibonacci-number mjr_prime_lucas-number
  ;; mjr_prime_pell-lucas-number mjr_prime_pell-number mjr_prime_jacobsthal-number mjr_prime_jacobsthal-lucas-number
  ;; mjr_prime_probable-primep-lucas-selfridge mjr_prime_probable-primep-bpsw
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_fibonacci-number
  ;; Sloan A000045
  (loop with lst = '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393
                     196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169)
        for i from 0 upto (1- (length lst))
        for f in lst
        do (assert-equalp f (mjr_prime_fibonacci-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_lucas-number
  ;; Sloan A000204
  (loop with lst = '(1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349 15127 24476 39603 64079 103682 167761
                     271443 439204 710647 1149851 1860498 3010349 4870847 7881196 12752043 20633239 33385282 54018521 87403803
                     141422324)
        for i from 1 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_lucas-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_pell-lucas-number
  ;; Sloan A002203
  (loop with lst = '(2 2 6 14 34 82 198 478 1154 2786 6726 16238 39202 94642 228486 551614 1331714 3215042 7761798 18738638 45239074
                     109216786 263672646 636562078 1536796802 3710155682 8957108166 21624372014 52205852194 126036076402
                     304278004998 )
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_pell-lucas-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_pell-number
  ;; Sloan A000129
  (loop with lst = '(0 1 2 5 12 29 70 169 408 985 2378 5741 13860 33461 80782 195025 470832 1136689 2744210 6625109 15994428
                     38613965 93222358 225058681 543339720 1311738121 3166815962 7645370045 18457556052 44560482149 107578520350
                     259717522849)
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_pell-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_jacobsthal-number
  ;; Sloan A001045
  (loop with lst = '(0 1 1 3 5 11 21 43 85 171 341 683 1365 2731 5461 10923 21845 43691 87381 174763 349525 699051 1398101 2796203
                     5592405 11184811 22369621 44739243 89478485 178956971 357913941 715827883 1431655765 2863311531)
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_jacobsthal-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_jacobsthal-lucas-number
  ;; Sloan A014551
  (loop with lst = '(2 1 5 7 17 31 65 127 257 511 1025 2047 4097 8191 16385 32767 65537 131071 262145 524287 1048577 2097151 4194305
                     8388607 16777217 33554431 67108865 134217727 268435457 536870911 1073741825 2147483647 4294967297 8589934591)
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_jacobsthal-lucas-number i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_phi-func
  ;; OEIS A000010
  (loop with lst = '(1 1 2 2 4 2 6 4 6 4 10 4 12 6 8 8 16 6 18 8 12 10 22 8 20 12 18 12 28 8 30 16 20 16 24 12 36 18 24 16 40 12 42
                     20 24 22 46 16 42 20 32 24 52 18 40 24 36 28 58 16 60 30 36 32 48 20 66 32 44 )
        for i from 1 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_phi-func i) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_pi-func
  ;; OEIS A006880       Number of primes < 10^n.
  (loop with lst = '( 0 4 25 168 1229 9592 78498 664579 5761455 50847534 455052511 4118054813 37607912018 346065536839 3204941750802
                     29844570422669 279238341033925 2623557157654233 24739954287740860 234057667276344607 2220819602560918840)
        for i from 0 upto (length lst)
        for f in lst
        do (assert-equalp f (mjr_prime_pi-func (expt 10 i)) i))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_prime_square-free?
  ;; OEIS A005117
  (loop with lst = '(1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62
                     65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95 97 101 102 103 105 106 107 109 110 111 113)
        with c = (car lst)
        for n from 1
        while c
        do (if (= n c)
               (progn
                 (setq lst (cdr lst)
                       c   (car lst))
                 (assert-true (mjr_prime_square-free? n))
               (assert-true  (mjr_prime_square-free? n)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )

;; lucas primes. (sloan A005479)
;; Note: If Ln is prime, then n=0 || n is prime || n=2^m where m is 1,2,3,4
;;
;; (loop for n from 1
;;       for l = (mjr_prime_lucas-number n)
;;       until (> l mjr_prime::*mjr_prime_small-max*)
;;       when (mjr_prime_primep-small l)
;;       collect l)
;;
;; (3 7 11 29 47 199 521 2207 3571 9349 3010349 54018521)

;; lucas psuoprimes (composit n for which n|[L(n) - 1] (sloan A005845)
;;
;; (loop for n from 1 upto 7000
;;       for ln = (mjr_prime_lucas-number n)
;;       when (and (not (mjr_prime_primep-small n)) (zerop (rem (1- ln) n)))
;;       collect n))
;;
;; (1 705 2465 2737 3745 4181 5777 6721)


;; ;; A000668 Mersenne primes
;; (loop for j from 0 upto 89
;;       for p = (mjr_prime_nth-small-prime j)
;;       for i = (1- (expt 2 p))
;;       when (mjr_prime_probable-primep-bpsw i)
;;       collect i)

;; ;; A000043 Mersenne exponents: primes p such that 2^p - 1 is prime.
;; '(2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 3217 4253 4423 9689 9941 11213 19937 21701 23209 44497 86243 110503
;;   132049 216091 756839 859433 1257787 1398269 2976221 3021377 6972593 13466917 20996011 24036583 25964951)
