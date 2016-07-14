;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-chk.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Floating point comparison: disaster prevention.@EOL
;; @std       Common Lisp
;; @see       tst-chk.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2012,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CHK
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Floating point comparison: disaster prevention.;")
  (:EXPORT #:*mjr_chk_eps*
           #:mjr_chk_help
           #:mjr_chk_mk-eps-dtype
           #:mjr_chk_dflt-eps
           #:mjr_chk_< #:mjr_chk_> #:mjr_chk_!= #:mjr_chk_!=0
           #:mjr_chk_minusp #:mjr_chk_plusp
           ))

(in-package :MJR_CHK)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_help ()
  "Help for MJR_CHK:

The functions in this package are designed for use in situations requiring a high degree of confidence in the correctness of non-NIL (true) return values, but
that do not require this same degree of confidence for NIL (false) return values.  The canonical example is using MJR_CHK_!=0 to avoid a (/ 0) condition -- an
incorrect non-NIL return will lead to disaster.

When functions in this package return non-NIL, the result is true with a very high degree of confidence; however, a NIL means the value is less certain:

  * If all non-EPS arguments are rational, then both non-NIL and NIL results are certain.
  * If any of the non-EPS arguments involve floating point numbers:
    * A non-NIL result will be true with an additional EPSilon margin for error.
    * A NIL result may be wrong due to round off error or even an EPSilon that was too large."
  (documentation 'mjr_chk_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_mk-eps-dtype (&rest rest)
  "Guess a safe epsilon with the types of the values of X and Y.
If necessary, the epsilon returned will error on the large side."
  (if (cdr rest)
      (apply #'max (mapcar #'mjr_chk_mk-eps-dtype rest))
      (let ((x (car rest)))
              (typecase x
                (rational      0)
                (single-float  (max (abs single-float-negative-epsilon) (abs single-float-epsilon)))
                (double-float  (max (abs double-float-negative-epsilon) (abs double-float-epsilon)))
                (complex       (max (mjr_chk_mk-eps-dtype (realpart x))
                                    (mjr_chk_mk-eps-dtype (imagpart x))))
                (short-float   (max (abs short-float-negative-epsilon) (abs short-float-epsilon)))
                (long-float    (max (abs long-float-negative-epsilon)  (abs long-float-epsilon)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *mjr_chk_eps* 0.00001
  "The default epsilon used for floating point check tests")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_dflt-eps (eps &rest rest)
  (if (null eps)
      (or *mjr_chk_eps*
          (error "mjr_chk_dflt-eps: *mjr_chk_eps* can not be NIL!!"))
      (if (< eps -0.5)
          (case eps
            (-1 (apply #'mjr_chk_mk-eps-dtype rest)))
          eps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_< (a b &optional eps)
  "Return non-NIL if A is certainly less than B

If the first two arguments are rational, then an exact numerical comparison will be used; otherwise, then non-NIL is returned iff A+EPS<B."
  (cond ((not (numberp a))                  (error "mjr_chk_<: The A argument must be a number"))
        ((not (numberp b))                  (error "mjr_chk_<: The B argument must be a number"))
        ((complexp a)                       (error "mjr_chk_<: The A argument must be real"))
        ((complexp b)                       (error "mjr_chk_<: The B argument must be real"))
        ((and (rationalp a) (rationalp b))  (< a b))
        ('t                                 (< (+ a (mjr_chk_dflt-eps eps a b)) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_> (a b &optional eps)
  "Return non-NIL if A is certainly greater than B

If the first two arguments are rational, then an exact numerical comparison will be used; otherwise, non-NIL is returned iff A>B+EPS."
  (cond ((not (numberp a))                  (error "mjr_chk_>: The A argument must be a number"))
        ((not (numberp b))                  (error "mjr_chk_>: The B argument must be a number"))
        ((complexp a)                       (error "mjr_chk_>: The A argument must be real"))
        ((complexp b)                       (error "mjr_chk_>: The B argument must be real"))
        ((and (rationalp a) (rationalp b))  (> a b))
        ('t                                 (> a (+ b (mjr_chk_dflt-eps eps a b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_!= (a b &optional eps)
  "Return non-NIL if A is certainly not equal to B

If the first two arguments are rational, then an exact numerical comparison will be used; otherwise, non-NIL is returned iff |A-B|>EPS."
  (cond ((not (numberp a))                  (error "mjr_chk_!=: The A argument must be a number"))
        ((not (numberp b))                  (error "mjr_chk_!=: The B argument must be a number"))
        ;;((complexp a)                       (error "mjr_chk_!=: The A argument must be real"))
        ;;((complexp b)                       (error "mjr_chk_!=: The B argument must be real"))
        ((and (rationalp a) (rationalp b))  (not (= a b)))
        ('t                                 (> (abs (- a b)) (mjr_chk_dflt-eps eps a b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_!=0 (a &optional eps)
  "Return non-NIL if A is certainly not zero

If A is rational, then an exact numerical comparison will be used; otherwise, non-NIL is returned iff iff |A|>EPS."
  (cond ((not (numberp a)) (error "mjr_chk_!=0: The A argument must be a number"))
        ;;((complexp a)      (error "mjr_chk_!=0: The A argument must be real"))
        ((rationalp a)     (not (= a 0)))
        ('t                (> (abs a) (mjr_chk_dflt-eps eps a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_minusp (a &optional eps)
  "Return non-NIL if A is certainly negative

If A is rational, then an exact numerical comparison will be used; otherwise, non-NIL is returned iff iff A<-EPS."
  (mjr_chk_< a 0 eps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_chk_plusp (a &optional eps)
  "Return non-NIL if A is certainly positive

If A is rational, then an exact numerical comparison will be used; otherwise, non-NIL is returned iff iff A>EPS."
  (mjr_chk_> a 0 eps))
