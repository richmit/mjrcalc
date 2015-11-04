;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-intu.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     Handy integer utilities.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_INTU
  (:USE :COMMON-LISP
        :MJR_CMP)
  (:DOCUMENTATION "Brief: Handy integer utilities.;")
  (:EXPORT #:mjr_intu_pc #:mjr_intu_px 
           #:mjr_intu_divides?
           #:mjr_intu_extended-gcd
           #:mjr_intu_quadratic-residue?
           #:mjr_intu_log-floor #:mjr_intu_log #:mjr_intu_mod-expt
           #:mjr_intu_convert-to-digit-list #:mjr_intu_convert-from-digit-list
           #:mjr_intu_sum-digits #:mjr_intu_count-digits #:mjr_intu_same-digits? #:mjr_intu_palindromic? #:mjr_intu_square?
           ))

(in-package :MJR_INTU)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_divides? (n1 n2)
  "Return the quotient N2/N1 if N1 divides N2, and NIL otherwise."
  (multiple-value-bind (quo rem) (truncate n2 n1)
    (if (zerop rem)
        quo)))

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_log (n base)
  "Return M such that (EXPT BASE M) equals N or return NIL if no such M exists."
  (multiple-value-bind (m n-pow) (mjr_intu_log-floor n base)
    (and (= n-pow n) m)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_pc (x) 
  "Convert integer to a string in decimal representation with commas."
  (cond ((complexp x)      (error "mjr_intu_pc: Argument must not be complex"))
        ((not (numberp x)) (error "mjr_intu_pc: Argument must be a number")))
  (if (integerp x)
      (format nil "~:d" x)
      (if (mjr_cmp_= (round x) x)
          (format nil "~:d" (round x))
          (error "mjr_intu_pc: Argument was not close enough to an integer to print!"))))


;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_px (x)
  "Convert integer to a string in hexadecimal representation."
  (cond ((complexp x)      (error "mjr_intu_px: Argument must not be complex"))
        ((not (numberp x)) (error "mjr_intu_px: Argument must be a number")))
  (if (integerp x)
      (format nil "~x" x)
      (if (mjr_cmp_= (round x) x)
          (format nil "~x" (round x))
          (error "mjr_intu_px: Argument was not close enough to an integer to print!"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_extended-gcd (a b)
  "Return a, b, and g such that $a*x+b*y=g=\gcd(a,b)$"
  (if (zerop b)
      (values 1 0 a)
      (multiple-value-bind (quo rem) (truncate a b)
        ;(declare (ignore rem))
        (multiple-value-bind (s1 s2 gcd) (mjr_intu_extended-gcd b rem)
          (values s2 (- s1 (* quo s2)) gcd)))))

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_sum-digits (n)
  "Sum of the decimal digits of N"
  (loop for (n-left dig) = (multiple-value-list (truncate n 10)) then (multiple-value-list (truncate n-left 10))
     sum dig
     until (zerop n-left)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_convert-to-digit-list (n &optional in-order)
  "Convert an integer into a list of digits"
  (funcall (if in-order
               #'reverse
               #'identity)
           (loop for (n-left dig) = (multiple-value-list (truncate n 10)) then (multiple-value-list (truncate n-left 10))
              collect dig
              until (zerop n-left))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_convert-from-digit-list (dig-list &optional in-order)
  "Convert a list of digits to an integer"
  (loop for dig in (if in-order
                       (reverse dig-list)
                       dig-list)
     for fac = 1 then (* 10 fac)
     sum (* dig fac)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_count-digits (n &optional (base 10))
  "Convert a list of digits to an integer"
  (ceiling (log (abs n) base)))
;; (length (format nil "~d" (abs n))) ;; alternate base 10 way

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_same-digits? (m n)
  "Non-NIL if the digits of M and M are the same (perhaps in a different order)"
  (equalp (sort (format nil "~d" n) #'char-greaterp)
          (sort (format nil "~d" m) #'char-greaterp)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_palindromic? (n &optional (base 10))
  "Non-NIL if M is palindromic.  BASE must be 2 or 10"
  (let ((ms (format nil (if (= 2 base) "~b" "~d") n)))
    (equalp ms (reverse ms))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_square? (n)
  "Return the square root of N if N is a perfect square (and a LISP integer), and NIL otherwise."
  (if (integerp n)
      (let ((r (isqrt n)))
        (if (= n (expt r 2))
            r))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_intu_quadratic-residue? (a p)
  "Here $a$ is a positive integer, and $p$ an odd prime."
  (or (zerop (mod a p))
      (= 1 (mod (expt a (/ (- p 1) 2)) p))))
