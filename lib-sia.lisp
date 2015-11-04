;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      lib-sia.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995,1997,1998,2004,2015 by Mitch Richling.  All rights reserved.
;; @brief     Simple Interval Arithmetic (with guess).@EOL
;; @Keywords  lisp interactive interval arithmetic
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_SIA
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Simple Interval Arithmetic (with guess);")
  (:EXPORT #:mjr_sia_help
           #:mjr_sia_valid?  #:mjr_sia_valide
           #:mjr_sia_left #:mjr_sia_right #:mjr_sia_ctr #:mjr_sia_width #:mjr_sia_hwide #:mjr_sia_guess
           #:mjr_sia_left= #:mjr_sia_right=  #:mjr_sia_error #:mjr_sia_guess= #:mjr_sia_ctr=
           #:mjr_sia_ctr=guess?  
           #:mjr_sia_zapguess 
           #:mjr_sia_in? #:mjr_sia_intersect? #:mjr_sia_same?
           #:mjr_sia_< #:mjr_sia_> 
           #:mjr_sia_dist
           #:mjr_sia_monfun #:mjr_sia_apply2guess #:mjr_sia_apply2ends
           #:mjr_sia_* #:mjr_sia_/ #:mjr_sia_+ #:mjr_sia_-
           #:mjr_sia_min #:mjr_sia_max
           #:mjr_sia_intersect
           #:mjr_sia_log #:mjr_sia_exp #:mjr_sia_sqrt #:mjr_sia_sq #:mjr_sia_expt #:mjr_sia_inv
           #:mjr_sia_sin #:mjr_sia_cos #:mjr_sia_tan
           ))

(in-package :MJR_SIA)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_sia_help ()
"Help for MJR_SIA:

This library provides support for interval arithmetic functions.  The intent is that this library can form the basis for other
software.  Performance wins over safety. See the ia_* library for an example of a library making use of this package.

In addition to doing traditional interval arithmetic, this library has special support for dealing with intervals used to represent
uncertain values -- i.e. values with measurement uncertainty or other forms of error.  Generally one represents such quantities as a
'best guess' and an 'error bar' -- together they form an interval centered upon the 'best guess' and 2x the 'error bar' wide.  An
example might be a length: 2m +/- 1cm.  During the course of doing arithmetic on intervals, the mid-points of the input intervals
may not be transformed to the mid-point of the resultant interval via the same set of mathematical manipulations. For example:

     [1 3] * [4 6] = [4 18]

In this case, the centers we begin with (2 & 5) are not transformed onto the center of the result (11) via the expected
calculation (2 * 5 = 10). This can be quite frustrating when the intervals represent 'error bars' on a 'best guess' measurement.
For this reason it is nice to keep track of the 'best guess' points -- common practice in error analysis is to do the arithmetic
with the 'best guess' values, and then give a +/- range.  That range is generally derived in one of two ways: 1) the error bar is
selected so that it covers the intervals that would have resulted from standard interval arithmetic or 2) the error bar is selected
based upon a statistical analysis of the expected probability distribution of the errors.  Occasionally the error bar is not
symmetric.  An example: 2m +1cm or -2cm.  This library will keep track of the 'best guess' when it moves from the center of an
interval, and will allow the direct specification of a 'best guess' in intervals.  Several functions are provided to work with 'best
guess' values -- for example, one may recenter an interval onto a new best guess and expand the error bar to cover the interval.

An interval, [a,b], is represented by use-a.lisp list of two numbers like so: '(a b).  If an explicit 'best guess is provided, then
it is the third element of the list: '(a b g)"
  (documentation 'mjr_sia_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Return non-nil (true) if a it is valid, else return nil (false)
(defun mjr_sia_valid? (a)
  (and (listp a)
       (>= (length a) 2) (<= (length a) 3)
       (numberp (first a)) (numberp (second a))
       (not (complexp (first a))) (not (complexp (second a)))
       (<= (first a) (second a))
       (or (= (length a) 2)
           (and (numberp (third a)) (not (complexp (third a)))
                (< (first a) (third a)) (< (third a) (second a))))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Return a if it is valid, else error out.
(defun mjr_sia_valide (a) (if (mjr_sia_valid? a)
                          a
                        (error "mjr_sia_valide: Invalid interval: ~s" a)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Get attributes of an interval
(defun mjr_sia_left  (a) (first a))
(defun mjr_sia_right (a) (second a))
(defun mjr_sia_ctr   (a) (/ (+ (mjr_sia_left a) (mjr_sia_right a)) 2))
(defun mjr_sia_width (a) (- (mjr_sia_right a) (mjr_sia_left a)))
(defun mjr_sia_hwide (a) (/ (mjr_sia_width a) 2))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_sia_left=  (a left)  (append (list left) (cdr a)))
(defun mjr_sia_right= (a right) (append (list (mjr_sia_left a)) (list right) (cddr a)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Get attributes of an interval with respect to the "best guess"
(defun mjr_sia_guess (a) (if (= 3 (length a)) (third a) (mjr_sia_ctr a)))
(defun mjr_sia_error (a) (let ((guess (mjr_sia_guess a))) (max (abs (- guess (mjr_sia_left a))) (abs (- guess (mjr_sia_right a))))))

(defun mjr_sia_ctr=guess? (a &optional eps) (mjr_cmp_= (mjr_sia_ctr a) (mjr_sia_guess a) eps))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Remove explicit best guess if it is near the interval center
(defun mjr_sia_zapguess (a &optional zap-no-matter-what)
  (if (or zap-no-matter-what (mjr_sia_ctr=guess? a))
      (list (mjr_sia_left a) (mjr_sia_right a))
    a))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Set the best guess to the given value, or to the interval center if no value is provided
(defun mjr_sia_guess=   (a &optional g) (mjr_sia_zapguess (if g (list (mjr_sia_left a) (mjr_sia_right a) g) (mjr_sia_guess= a (mjr_sia_ctr a)))))
;; Set the center to the value, and adjust interval so that it contains previous interval.  Given no new center, use guess.
(defun mjr_sia_ctr=     (a &optional c) (mjr_sia_zapguess
                                         (if c (let ((delta (max (abs (- (mjr_sia_left a) c)) (abs (- (mjr_sia_right a) c)))))
                                                 (list (- c delta) (+ c delta) (mjr_sia_guess a)))
                                             (mjr_sia_ctr= a (mjr_sia_guess a)))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Is the number or interval (first arg) contained in the interval (second arg).  The third option specifies the method:
;; 'i=>interval contained in, 'c=>center contained in, 'g=>guess contained int.Default is 'i.  If a is numeric, then they are all
;; equivalent so the third argument is ignored.
(defun mjr_sia_in? (a b &optional method)
  (cond ((numberp a)                      (and (>= a (mjr_sia_left b)) (<= a (mjr_sia_right b))))
        ((or (eq method 'i) (not method)) (and (>= (mjr_sia_left a) (mjr_sia_left b)) (<= (mjr_sia_right a) (mjr_sia_right b))))
        ((eq method 'c)                   (mjr_sia_in? (mjr_sia_ctr a) b))
        ((eq method 'g)                   (mjr_sia_in? (mjr_sia_guess a) b))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Do the two intervals have a non-empty intersection?
(defun mjr_sia_intersect? (a b) (or (mjr_sia_in? (mjr_sia_left  a) b) (mjr_sia_in? (mjr_sia_right a) b)
                                    (mjr_sia_in? (mjr_sia_left  b) a) (mjr_sia_in? (mjr_sia_right b) a)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Are the intervals the same.  method determines how "same" is computed.
;; default is '1. 'i=>intervals, 'c=>mutual center containment, 'g=>mutual guess containment
(defun mjr_sia_same? (a b &optional method) (and (mjr_sia_in? a b method) (mjr_sia_in? b a method)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Order relationships
(defun mjr_sia_<   (a b) (< (mjr_sia_right a) (mjr_sia_left  b)))
(defun mjr_sia_>   (a b) (> (mjr_sia_left  a) (mjr_sia_right b)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Apply a function to the best guess(s) of a or a&b.
(defun mjr_sia_apply2guess (f a &optional b) (if b
                                                 (funcall f (mjr_sia_guess a) (mjr_sia_guess b))
                                                 (funcall f (mjr_sia_guess a))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Apply function to the corners of the cube, square, or line segment defined by the given interval(s).
;; Very useful for monotone function application to interval(s).
(defun mjr_sia_apply2ends (f a &optional b c)
  (if c (list (funcall f (mjr_sia_left  a) (mjr_sia_left  b) (mjr_sia_left  c))
              (funcall f (mjr_sia_left  a) (mjr_sia_left  b) (mjr_sia_right c))
              (funcall f (mjr_sia_left  a) (mjr_sia_right b) (mjr_sia_left  c))
              (funcall f (mjr_sia_left  a) (mjr_sia_right b) (mjr_sia_right c))
              (funcall f (mjr_sia_right a) (mjr_sia_left  b) (mjr_sia_left  c))
              (funcall f (mjr_sia_right a) (mjr_sia_left  b) (mjr_sia_right c))
              (funcall f (mjr_sia_right a) (mjr_sia_right b) (mjr_sia_left  c))
              (funcall f (mjr_sia_right a) (mjr_sia_right b) (mjr_sia_right c)))
    (if b (list (funcall f (mjr_sia_left  a) (mjr_sia_left  b))
                (funcall f (mjr_sia_left  a) (mjr_sia_right b))
                (funcall f (mjr_sia_right a) (mjr_sia_left  b))
                (funcall f (mjr_sia_right a) (mjr_sia_right b)))
      (list (funcall f (mjr_sia_left  a))
            (funcall f (mjr_sia_right b))))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Distance between intervals computed using one of three methods: 'c => between centers, 'g => between guesses, 'i =>
(defun mjr_sia_dist (a b &optional method)
  (cond ((eq method 'c)         (mjr_numu_absdif (mjr_sia_ctr a) (mjr_sia_ctr b)))
        ((eq method 'g)         (mjr_numu_absdif (mjr_sia_guess a) (mjr_sia_guess b)))
        ((or (eq method 'i) 't) (if (mjr_sia_intersect? a b)
                                    0
                                    (apply 'min (mjr_sia_apply2ends 'mjr_numu_absdif a b))))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Apply a monotone function
(defun mjr_sia_monfun (f a b) (mjr_sia_guess=
                               (let ((allval (mjr_sia_apply2ends f a b)))
                                 (list (apply 'min allval) (apply 'max allval)))
                               (mjr_sia_apply2guess f a b)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Arithmetic and various 2-var functions
(defun mjr_sia_*   (a b) (mjr_sia_monfun '* a b))
(defun mjr_sia_/   (a b) (if (mjr_sia_in? 0 b) (error "mjr_sia_/: Division by interval containing zero: ~s~&" b) (mjr_sia_monfun '/ a b)))
(defun mjr_sia_+   (a b) (mjr_sia_guess= (list (+ (mjr_sia_left a) (mjr_sia_left b))   (+ (mjr_sia_right a) (mjr_sia_right b)))   (mjr_sia_apply2guess '+ a b)))
(defun mjr_sia_-   (a b) (mjr_sia_guess= (list (- (mjr_sia_left a) (mjr_sia_right b))  (- (mjr_sia_right a) (mjr_sia_left b)))    (mjr_sia_apply2guess '- a b)))
(defun mjr_sia_min (a b) (mjr_sia_guess= (list (min (mjr_sia_left a) (mjr_sia_left b)) (min (mjr_sia_right a) (mjr_sia_right b))) (mjr_sia_apply2guess 'min a b)))
(defun mjr_sia_max (a b) (mjr_sia_guess= (list (max (mjr_sia_left a) (mjr_sia_left b)) (max (mjr_sia_right a) (mjr_sia_right b))) (mjr_sia_apply2guess 'max a b)))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Set intersection
(defun mjr_sia_intersect (a b) (if (<= (mjr_sia_left a) (mjr_sia_left b))
                                   (if (>= (mjr_sia_right a) (mjr_sia_right b))
                                       b
                                       (if (>= (mjr_sia_right a) (mjr_sia_left b))
                                           (list (mjr_sia_left b) (mjr_sia_right a))))
                                   (if (<= (mjr_sia_right a) (mjr_sia_right b))
                                       a
                                       (if (<= (mjr_sia_left a) (mjr_sia_right b))
                                           (list (mjr_sia_left a) (mjr_sia_right b))))))

;;----------------------------------------------------------------------------------------------------------------------------------
;; Various 1-var functions
(defun mjr_sia_log  (a) (if (<= a 0)
                            (error "mjr_sia_log: Logarithm of interval containing non-positive elements: ~s~&" a)
                            (mjr_sia_guess= (list (log (mjr_sia_left a))  (log (mjr_sia_right a)))  (mjr_sia_apply2guess 'log a))))
(defun mjr_sia_exp  (a) (mjr_sia_guess= (list (exp (mjr_sia_left a))  (exp (mjr_sia_right a)))  (mjr_sia_apply2guess 'exp a)))
(defun mjr_sia_sqrt (a)  (if (< a 0)
                             (error "mjr_sia_sqrt: Square root of interval containing negative elements: ~s~&" a)
                             (mjr_sia_guess= (list (sqrt (mjr_sia_left a)) (sqrt (mjr_sia_right a))) (mjr_sia_apply2guess 'sqrt a))))
(defun mjr_sia_sq   (a) (mjr_sia_guess= (cond ((>= (mjr_sia_left a)  0) (list (expt (mjr_sia_left a) 2)  (expt (mjr_sia_right a) 2)))
                                              ((<= (mjr_sia_right a) 0) (list (expt (mjr_sia_right a) 2) (expt (mjr_sia_left a) 2)))
                                              ('t                   (list 0                      (max (expt (mjr_sia_right a) 2) (expt (mjr_sia_left a) 2)))))
                                        (expt (mjr_sia_guess a) 2)))
(defun mjr_sia_inv  (a) (if (mjr_sia_in? 0 a)
                            (error "mjr_sia_inv: Division by interval containing zero: ~s~&" a)
                            (mjr_sia_guess= (list (/ 1 (mjr_sia_right a)) (/ 1 (mjr_sia_left a))) (/ 1 (mjr_sia_guess a)))))
(defun mjr_sia_expt (a b) (if (not (complexp b))
                              (if (numberp b)
                                  (if (< b 0)
                                      (mjr_sia_inv (mjr_sia_expt a (- b)))
                                      (if (integerp b)
                                          (mjr_sia_guess=
                                           (if (= b 0)
                                               (if (mjr_sia_in? 0 a)
                                                   (if (and (mjr_cmp_= (mjr_sia_left a) 0) (mjr_cmp_= (mjr_sia_right a) 0))
                                                       (list 0 0)
                                                       (list 0 1))
                                                   (list 1 1))
                                               (if (evenp b)
                                                   (cond ((>= (mjr_sia_left a)  0) (list (expt (mjr_sia_left  a) b) (expt (mjr_sia_right a) b)))
                                                         ((>= (mjr_sia_left a)  0) (list (expt (mjr_sia_right a) b) (expt (mjr_sia_left  a) b)))
                                                         ('t                   (list 0                      (max (expt (mjr_sia_left  a) b) (expt (mjr_sia_right a) b)))))
                                                   (list (expt (mjr_sia_left  a) b) (expt (mjr_sia_right a) b))))
                                           (expt (mjr_sia_guess a) b))
                                          (error "mjr_sia_expt: Exponent not integer: ~s~&" b)))
                                  (error "mjr_sia_expt: Exponent not numeric: ~s~&" b))
                              (error "mjr_sia_expt: Exponent is complex: ~s~&" b)))
(defun mjr_sia_sin (a) (mjr_sia_guess= (let ((sinX (sin (mjr_sia_left a)))
                                             (sinY (sin (mjr_sia_right a))))
                                         (list
                                          (if (mjr_numu_fnd-max-periodic-point (/ (* 3 pi) 2) (* 2 pi) (mjr_sia_left a) (mjr_sia_right a))
                                              -1
                                              (min sinX sinY))
                                          (if (mjr_numu_fnd-max-periodic-point (/ pi 2) (* 2 pi) (mjr_sia_left a) (mjr_sia_right a))
                                              1
                                              (max sinX sinY))))
                                       (sin (mjr_sia_guess a))))
(defun mjr_sia_cos (a) (mjr_sia_guess= (let ((cosX (cos (mjr_sia_left a)))
                                             (cosY (cos (mjr_sia_right a))))
                                         (list
                                          (if (mjr_numu_fnd-max-periodic-point pi (* 2 pi) (mjr_sia_left a) (mjr_sia_right a))
                                              -1
                                              (min cosX cosY))
                                          (if (mjr_numu_fnd-max-periodic-point 0 (* 2 pi) (mjr_sia_left a) (mjr_sia_right a))
                                              1
                                              (max cosX cosY))))
                                       (cos (mjr_sia_guess a))))
(defun mjr_sia_tan (a);; tan is monotone up between singularities!
  (if (or (mjr_numu_near-periodic-point?  (/ (* 3 pi) 2) pi (mjr_sia_left a)) (mjr_numu_near-periodic-point?  (/ (* 3 pi) 2) pi (mjr_sia_right a)))
      (error "mjr_sia_tan: Interval end point near singularity for tan: ~s~&" a)
      (mjr_sia_guess= (list (tan (mjr_sia_left a)) (tan (mjr_sia_right a))) (tan (mjr_sia_guess a)))))
