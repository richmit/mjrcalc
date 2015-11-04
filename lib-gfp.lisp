;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      lib-gfp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Interactive GF(p) library -- modular arithmatic.@EOL
;; @Keywords  lisp interactive gf(p) library modular arithmetic prime finite field
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_GFP
  (:USE :COMMON-LISP
        :MJR_INTU)
  (:DOCUMENTATION "Brief: Interactive GF(p) library -- modular arithmatic.;")
  (:EXPORT #:mjr_gfp_help
           #:mjr_gfp_simplify
           #:mjr_gfp_+ #:mjr_gfp_- #:mjr_gfp_* #:mjr_gfp_/
           #:mjr_gfp_iexpt #:mjr_gfp_imul
           #:mjr_gfp_< #:mjr_gfp_> #:mjr_gfp_=
           #:mjr_gfp_zerop #:mjr_gfp_onep #:mjr_gfp_gfpp
           #:mjr_gfp_divides?
           ))

(in-package :MJR_GFP)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_help ()
  "Basic arithmetic over prime order finite fields (i.e. known as prime order modular arithmetic in computer science circles).

I should have said 'in computer science FIELDS' -- HA.  No.  Really.  That was funny! Well, OK... Fine...

While this library may be used interactively, it proimarly provides a support roll for other libraries like MJR_POLYGFP and MJR_PRIME."
  (documentation 'mjr_gfp_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defconstant +mjr_gfp_zero+ 0
  "Additive unit in the field GF(p)")

;;----------------------------------------------------------------------------------------------------------------------------------
(defconstant +mjr_gfp_one+  1
  "Multiplicative unit in the field GF(p)")

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_gfpp (p a)
  "non-NIL if object is, or can be safely coerced into an element of GF(p).

In particular, the return will be one of the following:

   * 1 ...... An integer with no need of simplification
   * 2 ...... An integer in need of simplification
   * 3 ...... A non-complex number (it will be truncated by mjr_gfp_simplify)
   * nil .... All other cases."
  (if (integerp a)
      (if (and (<= 0 a) (< a p))
          1
          2)
      (if (and (numberp a) (not (complexp a)))
          3)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_simplify (p a)
  "Truncate lisp numbers to integers if required and then simplify the result mod P"
  (if (integerp a)
      (mod a p)
      (mjr_gfp_simplify p (truncate a))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_= (p a b)
  "non-NIL if A=B mod P"
  (= (mjr_gfp_simplify p a) (mjr_gfp_simplify p b)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_onep (p a)
  "non-NIL if A=1 mod P"
  (mjr_gfp_= p a +mjr_gfp_one+))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_zerop (p a)
  "non-NIL if A=0 mod P"
  (mjr_gfp_= p a +mjr_gfp_zero+))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_divides? (p a b)
  "non-NIL if a divides b ---  $a|b$ if $ak=b$ for some integer $k$.  $\\mathrm{GF}(p)$ is a FIELD, so $a|b$ unless $a=0$ and $b\\neq0$."
  (if (mjr_gfp_zerop p a)
      (mjr_gfp_zerop p b)
      't))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_< (p a b)
  "non-NIL if A<B mod P"
  (< (mjr_gfp_simplify p a) (mjr_gfp_simplify p b)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_> (p a b)
  "non-NIL if A>B mod P"
  (> (mjr_gfp_simplify p a) (mjr_gfp_simplify p b)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_+ (p &rest rest)
  "Compute (X+Y mod P). Return additive identity when given no arguments."
  (reduce (lambda (x y) (mjr_gfp_simplify p (+ x (mjr_gfp_simplify p y)))) rest :initial-value +mjr_gfp_zero+))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_- (p &rest rest)
  "Compute (X-Y mod P).  Return additive inverse when given a single argument."
  (if rest
      (reduce (lambda (x y) (mjr_gfp_simplify p (- x (mjr_gfp_simplify p y)))) rest :initial-value +mjr_gfp_zero+)
      (error "mjr_gfp_-: Missing argument")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_* (p &rest rest)
  "Compute (X*Y mod P).  Return multiplicative identity when given no arguments."
  (reduce (lambda (x y) (mjr_gfp_simplify p (* x (mjr_gfp_simplify p y)))) rest :initial-value +mjr_gfp_one+))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_/ (p &rest rest)
  "Compute (X/Y mod P).  Returns multiplicative inverse when given a single argument."
  (if (cdr rest)
      (reduce (lambda (x y) (mjr_gfp_simplify p (* x (mjr_gfp_/ p y)))) (cdr rest) :initial-value (mjr_gfp_simplify p (car rest)))
      (if rest
          (let ((tmp (mjr_gfp_simplify p (car rest))))
            (if (= tmp +mjr_gfp_zero+)
                (error "mjr_gfp_/: DIVISION-BY-ZERO!")
                (multiple-value-bind (s1 s2 gcd) (mjr_intu_extended-gcd tmp p)
                  (declare (ignore s2))
                  (if (= 1 gcd)
                      (mjr_gfp_simplify p s1)))))
          (error "mjr_gfp_/: Missing argument"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_iexpt (p x n)
  "Compute (X^N mod P)

Uses the right-to-left binary algorithm (also known as 'exponentiation by squaring' and 'binary exponentiation').  Also uses a
trick to shrink exponents mod (p-1) -- making it faster exponents much larger than p, but a tiny bit slower for smaller
exponents.

References:
  Bruce Schneier (1996); Applied Cryptography: Protocols, Algorithms, and Source Code in C 2nd; ISBN: 978-0471117094; pp224
  David Bressoud (1989); Factorization and primality testing; ISBN: 0-387-97040-1; pp33-34"
  (cond ((not (integerp n))  (error "mjr_gfp_imul: n must be an integer!")))
  (let ((result +mjr_gfp_one+)
        (x      (mjr_gfp_simplify p x))
        (n      (mjr_gfp_simplify (1- p) n)))  ;; This will always work because p is prime...
    (loop while (not (zerop n))
          do (if (logbitp 0 n)
                 (setq result (mjr_gfp_simplify p (* result x))))
          do (setq n (ash n -1)
                   x (mjr_gfp_simplify p (* x x))))
    result))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_gfp_imul (p x n)
  "Compute (X*N mod P) -- for GF(p) this is easy, for other rings it must be implemented as repeated addition.."
  (cond ((not (integerp n))  (error "mjr_gfp_imul: n must be an integer!")))
  (mjr_gfp_* p x n))

