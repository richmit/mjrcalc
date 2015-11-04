;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-cmp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     Floating point comparison: best guess.@EOL
;; @Keywords  lisp interactive fuzzy floating point comparison
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CMP
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Floating point comparison: best guess.;")
  (:EXPORT #:mjr_cmp_help #:mjr_cmp_< #:mjr_cmp_> #:mjr_cmp_= #:mjr_cmp_<= #:mjr_cmp_>= #:mjr_cmp_=0 #:mjr_cmp_=1 #:mjr_cmp_=onep
           #:mjr_cmp_!= #:mjr_cmp_zerop #:mjr_cmp_!=0 #:mjr_cmp_not-zerop #:mjr_cmp_abs< #:mjr_cmp_abs> #:mjr_cmp_abs<= 
           #:mjr_cmp_abs>= :mjr_cmp_min #:mjr_cmp_max #:mjr_cmp_abs-min #:mjr_cmp_abs-max #:*mjr_cmp_eps* #:mjr_cmp_eps-for-data
           #:mjr_cmp_signum :mjr_cmp_integerp #:mjr_cmp_evenp #:mjr_cmp_oddp #:mjr_cmp_negativep #:mjr_cmp_positivep
           ))

(in-package :MJR_CMP)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_help ()
"Help for MJR_CMP:

This package implements 'best guess' comparisons -- exact when arguments are rational, fuzzy when they are floating point.
These should NOT be used for assertions that are used to avoid algorithm failure (i.e. checks for zero before division) -- use
the mjr_fchk package for that.

 Expr                           Real  C-abs
 (a<b)  || (a>b)   => a!=b       *      -
 (a<b)  || (a>b)  <=  a!=b       *      -
 !(a>b) && !(a<b)  => a=b        *      -
 !(a>b) && !(a<b) <=  a=b        *      -
 (a!=b)           <=> !(a=b)     *      *
 (a<b)            <=> !(a>b)     *      -
 a<=b              => !(a>b)     *      -
 a>=b              => !(a<b)     *      -
 (a<b)||(a=b)      => a<=b       *      -
 (a>b)||(a=b)      => a>=b       *      -
 (a<=b)&&(a>=b)    => a=b        *      -
 (a<=b)&&(a>=b)    <= a=b        *      -
 a=b && b=c        => a=c        0      0
"
  (documentation 'mjr_cmp_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defparameter *mjr_cmp_eps* 0.00001
  "The default epsilon used for fuzzy comparisons")

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_< (a b &optional eps)
  (if (and (rationalp a) (rationalp b))
      (< a b)
      (< (+ a (or eps *mjr_cmp_eps*)) b)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_> (a b &optional eps)
  (if (and (rationalp a) (rationalp b))
      (> a b)
      (> a (+ b (or eps *mjr_cmp_eps*)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_= (a b &optional eps)
  (cond ((and (rationalp a) (rationalp b))    (= a b))
        ((or (complexp a)   (complexp b))     (< (abs (- a b)) (or eps *mjr_cmp_eps*)))
        ('t                                   (and (not (mjr_cmp_> a b eps)) (not (mjr_cmp_< a b eps))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_!= (a b &optional eps)
  (cond ((and (rationalp a) (rationalp b))    (not (= a b)))
        ((or (complexp a)   (complexp b))     (not (mjr_cmp_= a b eps)))
        ('t                                   (or (mjr_cmp_> a b eps) (mjr_cmp_< a b eps)))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Combination comparison with an inequality and equality.
(defun mjr_cmp_<=        (a b &optional eps) (not (mjr_cmp_> a b eps)))
(defun mjr_cmp_>=        (a b &optional eps) (not (mjr_cmp_< a b eps)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Composite fuzzy comparison functions
(defun mjr_cmp_=0        (a   &optional eps) (mjr_cmp_=   a 0 eps))
(defun mjr_cmp_zerop     (a   &optional eps) (mjr_cmp_=   a 0 eps))
(defun mjr_cmp_=1        (a   &optional eps) (mjr_cmp_=   a 1 eps))
(defun mjr_cmp_onep      (a   &optional eps) (mjr_cmp_=   a 1 eps))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Second order composite fuzzy comparison functions.  The "promise of certainty in 't" drives the use of < and >.
(defun mjr_cmp_!=0       (a   &optional eps) (mjr_cmp_!= a 0 eps))
(defun mjr_cmp_not-zerop (a   &optional eps) (mjr_cmp_!= a 0 eps))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Fuzzy comparison functions on absolute value (magnitude) of arguments.
(defun mjr_cmp_abs<      (a b &optional eps) (mjr_cmp_<  (abs a) (abs b) eps))
(defun mjr_cmp_abs>      (a b &optional eps) (mjr_cmp_>  (abs a) (abs b) eps))
(defun mjr_cmp_abs<=     (a b &optional eps) (mjr_cmp_<= (abs a) (abs b) eps))
(defun mjr_cmp_abs>=     (a b &optional eps) (mjr_cmp_>= (abs a) (abs b) eps))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Test if input is near an integer, near an even integer, or near an odd integer
(defun mjr_cmp_integerp (a &optional eps)  (mjr_cmp_= a (truncate a) eps))
(defun mjr_cmp_evenp    (a &optional eps)  (and (mjr_cmp_integerp a eps) (evenp (truncate a))))
(defun mjr_cmp_oddp     (a &optional eps)  (and (mjr_cmp_integerp a eps) (oddp  (truncate a))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Some useful tests
(defun mjr_cmp_negativep (a &optional eps)  (mjr_cmp_< a 0            eps))
(defun mjr_cmp_positivep (a &optional eps)  (mjr_cmp_> a 0            eps))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Max/Min functions based upon fuzzy comparison.  Preference given to first argument when they are numerically equal!
(defun mjr_cmp_min       (a b &optional eps) (if (mjr_cmp_<=    a b eps) a b))
(defun mjr_cmp_max       (a b &optional eps) (if (mjr_cmp_>=    a b eps) a b))

;;----------------------------------------------------------------------------------------------------------------------------------
;; The comparison is "abs", but the return is one of the inputs a or b.  Preference given to first argument when they are
;; numerically equal in absolute value.
(defun mjr_cmp_abs-min   (a b &optional eps) (if (mjr_cmp_abs<= a b eps) a b))
(defun mjr_cmp_abs-max   (a b &optional eps) (if (mjr_cmp_abs>= a b eps) a b))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_eps-for-data (x &optional y)
  "Guess the epsilon for the given X and Y.  
If necessary, the epsilon returned will error on the large side."
  (if y
      (max (mjr_cmp_eps-for-data x)
           (mjr_cmp_eps-for-data y))
      (cond ((typep x 'rational)     0)
            ((typep x 'single-float) (max (abs single-float-negative-epsilon) (abs single-float-epsilon)))
            ((typep x 'double-float) (max (abs double-float-negative-epsilon) (abs double-float-epsilon)))
            ((typep x 'complex)      (max (mjr_cmp_eps-for-data (realpart x))
                                          (mjr_cmp_eps-for-data (imagpart x))))
            ((typep x 'short-float)  (max (abs short-float-negative-epsilon) (abs short-float-epsilon)))
            ((typep x 'long-float)   (max (abs long-float-negative-epsilon)  (abs long-float-epsilon))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cmp_signum (a &optional eps)
  "Computes the signum along with flag indicating ambiguity with regard to near zero values of A.

Returns +1 (-1) only if it is sure that A is greater (less) than 0.  Returns 0 otherwise.  The second
return is non-nil if 0 is returned, and A is not precisely zero."
  (cond ((mjr_cmp_> a 0 eps)  (values +1 nil))
        ((mjr_cmp_< a 0 eps)  (values -1 nil))
        ((zerop a)            (values 0  nil))
        ('t                   (values 0  't))))
  
