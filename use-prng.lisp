;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-prng.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2010,2011,2013 by Mitch Richling.  All rights reserved.
;; @brief     Uniform random deviate generators.@EOL
;; @Keywords  lisp interactive prng uniform random distributions math library
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_PRNG
  (:USE :COMMON-LISP
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Uniform random deviate generators.;")
  (:EXPORT #:mjr_prng_help
           #:mjr_prng_boolean #:mjr_prng_sign #:mjr_prng_bit             ;; Random Boolean-like stuff
           #:mjr_prng_int-oo   #:mjr_prng_int-cc   #:mjr_prng_int-co     ;; integers (uniform PDF)
           #:mjr_prng_float-oo #:mjr_prng_float-cc #:mjr_prng_float-co   ;; floats (uniform PDF)
           #:mjr_prng_tbd-uniform-co                                     ;; numbers (uniform PDF)
           #:*mjr_prng_rand-func* #:mjr_prng_random                      ;; LISP-like random numbers (uniform PDF)
           #:mjr_prng_vector #:mjr_prng_array #:mjr_prng_list            ;; Generic composite objects (might move these)
           #:mjr_prng_bitvector                                          ;; Specialized composite objects
           ))

(in-package :MJR_PRNG)

;; -----------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_help ()
  "Help for MJR_PRNG:

Several UNIFORM PRNG functions are provided in forms that are a bit more handy to use than the built in RANDOM function. 
In a real way, this package is intended to replace the built in RANDOM function.

For more mathematically oriented random numbers involving other PDFs see the MJR_PROB: package."
  (documentation 'mjr_prng_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_prng_rand-func* nil
  "The standard LISP random-like function to use for uniform random numbers")

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_random (arg)
  (declare (number arg))
  (if *mjr_prng_rand-func*
      (funcall *mjr_prng_rand-func* arg)
      (random arg)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_boolean (&optional p)
  "Return non-nil with probably $p$, and nil with probability $1-p$ ($p=1/2$ if $p$ is missing) or nil."
  (if p
      (cond ((not (numberp p)) (error "mjr_prng_boolean: P must be an integer!"))
            ((complexp p)      (error "mjr_prng_boolean: P must be a non-complex number!"))
            ((< p 0)           (error "mjr_prng_boolean: P must be non-negative!"))
            ((> p 1)           (error "mjr_prng_boolean: P must be less than or equal to 1!"))
            ('t                (< (mjr_prng_random 1.0) p)))
      (= (mjr_prng_random 2) 1)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_sign (&optional p)
  "Return 1 with probably $p$, and nil with probability $1-p$ ($p=1/2$ if $p$ is missing) or -1."
  (if (mjr_prng_boolean p)
      1
      -1))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_bit (&optional p)
  "Return 1 with probably $p$, and nil with probability $1-p$ ($p=1/2$ if $p$ is missing) or 0."
  (if p
      (cond ((not (numberp p)) (error "mjr_prng_bit: P must be an integer!"))
            ((complexp p)      (error "mjr_prng_bit: P must be a non-complex number!"))
            ((< p 0)           (error "mjr_prng_bit: P must be non-negative!"))
            ((> p 1)           (error "mjr_prng_bit: P must be less than or equal to 1!"))))
  (if (mjr_prng_boolean p)
      1
      0))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_tbd-uniform-co (a b)
  "Return a pseudo-random number from the uniform distribution on [a,b) -- closed/open.  Return type determined as in RANDOM."
  (cond ((not (numberp a))     (error "mjr_prng_tbd-uniform-co: A must be a number!"))
        ((complexp a)          (error "mjr_prng_tbd-uniform-co: A must be a non-complex number!"))
        ((not (numberp b))     (error "mjr_prng_tbd-uniform-co: B must be a number!"))
        ((complexp b)          (error "mjr_prng_tbd-uniform-co: B must be a non-complex number!"))
        ((and (or (floatp a)
                  (floatp b))
              (> a b))         (error "mjr_prng_tbd-uniform-co: B must be at least as big as A!"))
        ((and (not (floatp a))
              (not (floatp b))
              (>= a b))        (error "mjr_prng_tbd-uniform-co: B must greater than A!")))
  (+ a (mjr_prng_random (- b a))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_int-oo (a b)
  "Return a pseudo-random number from the discrete uniform distribution on (a,b) -- open/open."
  (cond ((not (integerp a))    (error "mjr_prng_int-oo: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-oo: B must be an integer!"))
        ((>= a (1- b))         (error "mjr_prng_int-oo: B must be at least 2 greater than A!")))
  (+ (1+ a) (mjr_prng_random (- b a 1))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_int-cc (a b)
  "Return a pseudo-random number from the discrete uniform distribution on [a,b] -- closed/closed."
  (cond ((not (integerp a))    (error "mjr_prng_int-cc: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-cc: B must be an integer!"))
        ((> a b)               (error "mjr_prng_int-cc: A must be less than or equal to B!")))
  (+ a (mjr_prng_random (1+ (- b a)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_int-co (a b)
  "Return a pseudo-random number from the discrete uniform distribution on [a,b) -- closed/open."
  (cond ((not (integerp a))    (error "mjr_prng_int-co: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-co: B must be an integer!"))
        ((> a (1- b))          (error "mjr_prng_int-co: B must be at least 1 greater than A!")))
  (+ a (mjr_prng_random (- b a))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_float-oo (a b)
  "Return a pseudo-random number from the continuous uniform distribution on (a,b) -- open/open."
  (cond ((not (numberp a))     (error "mjr_prng_float-oo: A must be a number!"))
        ((complexp a)          (error "mjr_prng_float-oo: A must be a real number!"))
        ((not (numberp b))     (error "mjr_prng_float-oo: B must be a number!"))
        ((complexp b)          (error "mjr_prng_float-oo: B must be a real number!"))
        ((>= a b)              (error "mjr_prng_float-oo: A must be less than B!")))
  (let ((a (if (floatp a) a (float a 1d0)))
        (b (if (floatp b) b (float b 1d0))))
    (if (mjr_prng_boolean)
        (+ (/ (+ a b) 2) (mjr_prng_random (/ (- b a) 2)))
        (- (/ (+ a b) 2) (mjr_prng_random (/ (- b a) 2))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_float-cc (a b)
  "Return a pseudo-random number from the continuous uniform distribution on [a,b] -- closed/closed."
  (cond ((not (numberp a))     (error "mjr_prng_float-cc: A must be a number!"))
        ((complexp a)          (error "mjr_prng_float-cc: A must be a real number!"))
        ((not (numberp b))     (error "mjr_prng_float-cc: B must be a number!"))
        ((complexp b)          (error "mjr_prng_float-cc: B must be a real number!"))
        ((> a b)               (error "mjr_prng_float-cc: A must be less than or equal to B!")))
  (let ((a (if (floatp a) a (float a 1d0)))
        (b (if (floatp b) b (float b 1d0))))
    (if (mjr_prng_boolean)
        (+ a (mjr_prng_random (- b a)))
        (- b (mjr_prng_random (- b a))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_float-co (a b)
  "Return a pseudo-random number from the continuous uniform distribution on [a,b) -- closed/open."
  (cond ((not (numberp a))     (error "mjr_prng_float-co: A must be a number!"))
        ((complexp a)          (error "mjr_prng_float-co: A must be a real number!"))
        ((not (numberp b))     (error "mjr_prng_float-co: B must be a number!"))
        ((complexp b)          (error "mjr_prng_float-co: B must be a real number!"))
        ((> a b)               (error "mjr_prng_float-co: A must be less than or equal to B!")))
  (let ((a (if (not (floatp a)) (float a 1e0) a))
        (b (if (not (floatp b)) (float b 1e0) b)))
    (+ a (mjr_prng_random (- b a)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_vector (n func &rest rest)
  "Return a vector of length n filled with elements from the prng specified by func."
  (cond ((not (integerp n))     (error "mjr_prng_vector: A must be an integer!")))
  (if (vectorp (first rest))
      (apply #'map-into (make-array n) func rest)
      (loop with rv = (make-array n)
            for i from 0 upto (1- n)
            do (setf (aref rv i) (apply func rest))
            finally (return rv))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_bitvector (n &optional p)
  "Return a bitvector of length n filled with elements from the prng specified by func."
  (cond ((not (integerp n))     (error "mjr_prng_bitvector: A must be an integer!")))
  (let ((rv (make-array n :element-type 'bit :initial-element 0)))
  (if *mjr_prng_rand-func*  ;; optimize this because we make BIG bitvectors
      (if p
          (loop for i from 0 upto (1- n)
                when (< (funcall *mjr_prng_rand-func* 1.0) p)
                do (setf (aref rv i) 1))
          (loop for i from 0 upto (1- n)
                when (zerop (funcall *mjr_prng_rand-func* 2))
                do (setf (aref rv i) 1)))
      (if p
          (loop for i from 0 upto (1- n)
                when (< (random 1.0) p)
                do (setf (aref rv i) 1))
          (loop for i from 0 upto (1- n)
                when (zerop (random 2))
                do (setf (aref rv i) 1))))
        rv))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_list (n func &rest rest)
  "Return a list of length n filled with elements from the prng specified by func."
  (cond ((not (integerp n))     (error "mjr_prng_list: A must be an integer!")))
  (if (listp (first rest))
      (apply #'mapcar func rest)
      (loop for i from 0 upto (1- n)
            collect (apply func rest))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_prng_array (dims func &rest rest)
  "Return a vector of length n filled with elements from the prng specified by func."
  (cond ((not (listp dims))              (error "mjr_prng_vector: DIMS must be a list!"))
        ((null (cdr dims))               (error "mjr_prng_vector: DIMS must be a non-empty list!"))
        ((not (every #'integerp dims))   (error "mjr_prng_vector: DIMS must be a non-empty list of integers!")))
  (if (arrayp (first rest))
      (apply #'mjr_arr_map func rest)
      (mjr_arr_nreflow (apply #'mjr_prng_vector (apply #'* dims) func rest) dims)))
