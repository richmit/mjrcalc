;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-intu.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Handy integer utilities.@EOL
;; @std       Common Lisp
;; @see       tst-intu.lisp
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
(defpackage :MJR_INTU
  (:USE :COMMON-LISP
        :MJR_CMP)
  (:DOCUMENTATION "Brief: Handy integer utilities.;")
  (:EXPORT #:mjr_intu_help
           #:mjr_intu_pa #:mjr_intu_pbb
           #:mjr_intu_pd #:mjr_intu_px #:mjr_intu_po #:mjr_intu_pb
           #:mjr_intu_divides?
           #:mjr_intu_extended-gcd
           #:mjr_intu_quadratic-residue?
           #:mjr_intu_log-floor #:mjr_intu_log #:mjr_intu_mod-expt
           #:mjr_intu_convert-to-digit-list #:mjr_intu_convert-from-digit-list
           #:mjr_intu_sum-digits #:mjr_intu_count-digits #:mjr_intu_same-digits? #:mjr_intu_palindromic? #:mjr_intu_square?
           ))

(in-package :MJR_INTU)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_help ()
  "Help for MJR_INTU: Handy integer utilities."
  (documentation 'mjr_intu_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_divides? (n1 n2)
  "Return the quotient N2/N1 if N1 divides N2, and NIL otherwise."
  (multiple-value-bind (quo rem) (truncate n2 n1)
    (if (zerop rem)
        quo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_log-floor (n base)
  "Return the largest M and (EXPT BASE M) such that (EXPT BASE M) is less than or equal to N.  Return NIL if no such M exists."
  (cond ((not (integerp base))  (error "mjr_intu_int-log: BASE must be an integer"))
        ((not (integerp n))     (error "mjr_intu_int-log: N must be an integer"))
        ((<= n 0)               (error "mjr_intu_int-log: N must be positive"))
        ((<= base 0)            (error "mjr_intu_int-log: BASE must be positive")))
  (loop for m from 0
        for p = 1 then (* p base)
        when (> p n)
        do (return (values (1- m) (/ p base)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_log (n base)
  "Return M such that (EXPT BASE M) equals N or return NIL if no such M exists."
  (multiple-value-bind (m n-pow) (mjr_intu_log-floor n base)
    (and (= n-pow n) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_pg (x &key (separator-digits 3) (separator-character " ") (base 10))
  "Convert integer to a string in base BASE representation with place separator symbols of SEPARATOR-CHARACTER every SEPARATOR-DIGITS digits.

If SEPARATOR-DIGITS is NIL, then no separations are printed -- SEPARATOR-DIGITS is ignored."
  (cond ((complexp x)      (error "mjr_intu_pg: Argument must not be complex"))
        ((not (numberp x)) (error "mjr_intu_pg: Argument must be a number")))
  (if (mjr_cmp_= (round x) x)
      (if separator-character
          (format nil (format nil "~~~d,,,'~a,~d:r" base separator-character separator-digits) (round x))
          (format nil (format nil "~~~dr" base) (round x)))
      (error "mjr_intu_pg: Argument was not close enough to an integer to print!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_pd (x &key (separator-digits 3) (separator-character ","))
  "Convert integer to a string in decimal representation with commas every three digits."
  (mjr_intu_pg x :separator-digits separator-digits :separator-character separator-character :base 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_px (x &key (separator-digits 4) (separator-character " "))
  "Convert integer to a string in hexadecimal representation with spaces every four digits."
  (mjr_intu_pg x :separator-digits separator-digits :separator-character separator-character :base 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_po (x &key (separator-digits 3) (separator-character " "))
  "Convert integer to a string in octal representation with spaces every three digits."
  (mjr_intu_pg x :separator-digits separator-digits :separator-character separator-character :base 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_pb (x &key (separator-digits 8) (separator-character " "))
  "Convert integer to a string in binary representation with spaces every 8 digits."
  (mjr_intu_pg x :separator-digits separator-digits :separator-character separator-character :base 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_pa (x)
  "Convert an integer into a string containing representations in hexadecimal, decimal, octal, and binary."
  (let ((printwid (+ 4 (length (mjr_intu_pb x)))))
  (format nil (format nil "~%~~~d@a HEX~%~~~d@a DEC~%~~~d@a OCT~%~~~d@a BIN~%" printwid printwid printwid printwid)
          (mjr_intu_px x)
          (mjr_intu_pd x)
          (mjr_intu_po x)
          (mjr_intu_pb x)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_convert-to-digit-list (n &optional in-order)
  "Convert an integer into a list of digits"
  (funcall (if in-order
               #'reverse
               #'identity)
           (loop for (n-left dig) = (multiple-value-list (truncate n 10)) then (multiple-value-list (truncate n-left 10))
              collect dig
              until (zerop n-left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_pbb (x &key (index-base 0))
  "Convert integer to a binary string with bit and byte index positions marked."
  (let* ((str  (format nil "~b" x))
         (len  (1- (length str)))
         (ndg  (length (mjr_intu_convert-to-digit-list len)))
         (idxs (make-array ndg :initial-element ""))
         (bb0   "")
         (bb1   "")
         (b1?   (loop for i from len downto 0
                      for dl = (mjr_intu_convert-to-digit-list (+ i index-base))
                      for bytei = (mjr_intu_convert-to-digit-list (+ (truncate (- len i) 8) index-base))
                      for bytei0 = (first bytei)
                      for bytei1 = (or (second bytei) 0)
                      do (loop for j from 0 upto (1- ndg)
                               for d = (format nil "~d" (or (nth j dl) 0))
                               do (setf (aref idxs j) (concatenate 'string (aref idxs j) d)))
                      do (setf bb0 (concatenate 'string (if (zerop (mod (- len i) 8)) (format nil "~d" bytei0) " ") bb0))
                      do (setf bb1 (concatenate 'string (if (zerop (mod (- len i) 8)) (format nil "~d" bytei1) " ") bb1))
                      maximize bytei1)))
    (apply #'concatenate 'string (append (loop for j from (1- ndg) downto 0
                                               collect (format nil "~%")
                                               collect (aref idxs j))
                                         (list (format nil "~%")
                                               str
                                               (format nil "~%"))
                                         (if (> b1? 0)
                                             (list bb1
                                                   (format nil "~%")))
                                         (list bb0
                                               (format nil "~%"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_extended-gcd (a b)
  "Return a, b, and g such that $a*x+b*y=g=\gcd(a,b)$"
  (if (zerop b)
      (values 1 0 a)
      (multiple-value-bind (quo rem) (truncate a b)
        ;(declare (ignore rem))
        (multiple-value-bind (s1 s2 gcd) (mjr_intu_extended-gcd b rem)
          (values s2 (- s1 (* quo s2)) gcd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_mod-expt (x y n)
  "Compute (X^Y mod n) --- N need not be prime (See: MJR_GFP_IEXPT for a faster version when N is prime)

Uses the right-to-left binary algorithm (also known as 'exponentiation by squaring' and 'binary exponentiation').

References:
  Bruce Schneier (1996); Applied Cryptography: Protocols, Algorithms, and Source Code in C 2nd; ISBN: 978-0471117094; pp224
  David Bressoud (1989); Factorization and primality testing; ISBN: 0-387-97040-1; pp33-34"
  (let ((result 1)
        (x (mod x n)))
    (loop while (not (zerop y)) ;; == (> y 0)
          do (if (logbitp 0 y)  ;; is least significant bit 1? == (not (zerop (logand y 1))) == (= (logand y 1) 1)
                 (setq result (mod (* result x) n)))
          do (setq y (ash y -1)
                   x (mod (* x x) n)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_sum-digits (n)
  "Sum of the decimal digits of N"
  (loop for (n-left dig) = (multiple-value-list (truncate n 10)) then (multiple-value-list (truncate n-left 10))
     sum dig
     until (zerop n-left)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_convert-from-digit-list (dig-list &optional in-order)
  "Convert a list of digits to an integer"
  (loop for dig in (if in-order
                       (reverse dig-list)
                       dig-list)
     for fac = 1 then (* 10 fac)
     sum (* dig fac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_count-digits (n &optional (base 10))
  "Convert a list of digits to an integer"
  (ceiling (log (abs n) base)))
;; (length (format nil "~d" (abs n))) ;; alternate base 10 way

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_same-digits? (m n)
  "Non-NIL if the digits of M and M are the same (perhaps in a different order)"
  (equalp (sort (format nil "~d" n) #'char-greaterp)
          (sort (format nil "~d" m) #'char-greaterp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_palindromic? (n &optional (base 10))
  "Non-NIL if M is palindromic.  BASE must be 2 or 10"
  (let ((ms (format nil (if (= 2 base) "~b" "~d") n)))
    (equalp ms (reverse ms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_square? (n)
  "Return the square root of N if N is a perfect square (and a LISP integer), and NIL otherwise."
  (if (integerp n)
      (let ((r (isqrt n)))
        (if (= n (expt r 2))
            r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intu_quadratic-residue? (a p)
  "Here $a$ is a positive integer, and $p$ an odd prime."
  (or (zerop (mod a p))
      (= 1 (mod (expt a (/ (- p 1) 2)) p))))
