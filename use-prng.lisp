;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-prng.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Uniform random deviate generators.@EOL
;; @std       Common Lisp
;; @see       tst-prng.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2010,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PRNG
  (:USE :COMMON-LISP
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Uniform random deviate generators.;")
  (:EXPORT #:mjr_prng_help
           #:mjr_prng_boolean #:mjr_prng_sign #:mjr_prng_bit                       ;; Random Boolean-like stuff
           #:mjr_prng_int-oo   #:mjr_prng_int-cc   #:mjr_prng_int-co               ;; integers (uniform PDF)
           #:mjr_prng_float-oo #:mjr_prng_float-cc #:mjr_prng_float-co             ;; floats (uniform PDF)
           #:mjr_prng_tbd-uniform-co                                               ;; numbers (uniform PDF)
           #:*mjr_prng_rand-func* #:mjr_prng_random                                ;; LISP-like random numbers (uniform PDF)
           #:mjr_prng_vector #:mjr_prng_array #:mjr_prng_list #:mjr_prng_bitvector ;; Generic composite objects
           ))

(in-package :MJR_PRNG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_help ()
  "Help for MJR_PRNG:

Several UNIFORM PRNG functions are provided in forms that are a bit more handy to use than the built in RANDOM function.  In a real way, this package is
intended to replace the built in RANDOM function.

For more mathematically oriented random numbers involving other PDFs see the MJR_PROB: package."
  (documentation 'mjr_prng_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_prng_rand-func* nil
  "The standard LISP random-like function to use for uniform random numbers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_random (arg)
  (declare (number arg))
  (if *mjr_prng_rand-func*
      (funcall *mjr_prng_rand-func* arg)
      (random arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_boolean (&optional p)
  "Return non-nil with probably $p$, and nil with probability $1-p$ ($p=1/2$ if $p$ is missing)."
  (if p
      (cond ((not (numberp p)) (error "mjr_prng_boolean: P must be an integer!"))
            ((complexp p)      (error "mjr_prng_boolean: P must be a non-complex number!"))
            ((< p 0)           (error "mjr_prng_boolean: P must be non-negative!"))
            ((> p 1)           (error "mjr_prng_boolean: P must be less than or equal to 1!"))
            ('t                (< (mjr_prng_random 1.0) p)))
      (= (mjr_prng_random 2) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_sign (&optional p)
  "Return 1 with probably $p$, and nil with probability $1-p$ ($p=1/2$ if $p$ is missing)."
  (if (mjr_prng_boolean p)
      1
      -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_bit (&optional p)
  "Return 1 with probably $p$, and 0 with probability $1-p$ ($p=1/2$ if $p$ is missing)."
  (if p
      (if (mjr_prng_boolean p)
          1
          0)
      (mjr_prng_random 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_int-oo (a b)
  "Return a pseudo-random number from the discrete uniform distribution on (a,b) -- open/open."
  (cond ((not (integerp a))    (error "mjr_prng_int-oo: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-oo: B must be an integer!"))
        ((>= a (1- b))         (error "mjr_prng_int-oo: B must be at least 2 greater than A!")))
  (+ (1+ a) (mjr_prng_random (- b a 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_int-cc (a b)
  "Return a pseudo-random number from the discrete uniform distribution on [a,b] -- closed/closed."
  (cond ((not (integerp a))    (error "mjr_prng_int-cc: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-cc: B must be an integer!"))
        ((> a b)               (error "mjr_prng_int-cc: A must be less than or equal to B!")))
  (+ a (mjr_prng_random (1+ (- b a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_int-co (a b)
  "Return a pseudo-random number from the discrete uniform distribution on [a,b) -- closed/open."
  (cond ((not (integerp a))    (error "mjr_prng_int-co: A must be an integer!"))
        ((not (integerp b))    (error "mjr_prng_int-co: B must be an integer!"))
        ((> a (1- b))          (error "mjr_prng_int-co: B must be at least 1 greater than A!")))
  (+ a (mjr_prng_random (- b a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_vector (n func &rest rest)
  "Return a vector of length N filled with elements from the prng specified by FUNC."
  (cond ((not (integerp n))     (error "mjr_prng_vector: A must be an integer!")))
  (let ((rv (make-array n)))
    (dotimes (i n rv)
      (setf (aref rv i) (apply func rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_bitvector (n func &rest rest)
  "Return a bitvector of length N filled elements from the prng specified by FUNC.

Performance can be critical for this function (frequently random bit vectors are very long).  Avoid the call overhead of the MJR_PRNG_* functions for best
performance, and directly pass the random number code you wish to use.  The following are some run time measurements using SBCL on my 2015 vantage netbook:

  * (time (null (mjr_prng_bitvector 100000000 (lambda () (if (< (random 1.0) .5) 1 0))))) ==> 1.5 seconds
  * (time (null (mjr_prng_bitvector 100000000 (lambda () (random 2)))))                   ==> 2.0 seconds
  * (time (null (mjr_prng_bitvector 100000000 (lambda () (if (< (random 1.0) .2) 1 0))))) ==> 1.5 seconds
  * (time (null (mjr_prng_bitvector 100000000 #'mjr_prng_bit)))                           ==> 2.0 seconds
  * (time (null (mjr_prng_bitvector 100000000 #'mjr_prng_bit .2)))                        ==> 5.5 seconds"
  (cond ((not (integerp n))     (error "mjr_prng_vector: N must be an integer!")))
  (let ((rv (make-array n :element-type 'bit)))
    (dotimes (i n rv)
      (setf (bit rv i) (apply func rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_list (n func &rest rest)
  "Return a list of length N filled with elements from the prng specified by FUNC."
  (cond ((not (integerp n))     (error "mjr_prng_list: A must be an integer!")))
  (if (listp (first rest))
      (apply #'mapcar func rest)
      (loop for i from 0 upto (1- n)
            collect (apply func rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_prng_array (dims func &rest rest)
  "Return a vector of length N filled with elements from the prng specified by FUNC."
  (cond ((not (listp dims))              (error "mjr_prng_vector: DIMS must be a list!"))
        ((null (cdr dims))               (error "mjr_prng_vector: DIMS must be a non-empty list!"))
        ((not (every #'integerp dims))   (error "mjr_prng_vector: DIMS must be a non-empty list of integers!")))
  (if (arrayp (first rest))
      (apply #'mjr_arr_map func rest)
      (mjr_arr_nreflow (apply #'mjr_prng_vector (apply #'* dims) func rest) dims)))
