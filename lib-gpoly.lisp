;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      lib-gpoly.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Generate Polynomial Code For General Fields.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1994,1997,1998,2004,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_GPOLY
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Generate Polynomial Code For General Fields.;")
  (:EXPORT #:mjr_gpoly_help
           #:mjr_gpoly_make-simplify
           #:mjr_gpoly_make-+
           #:mjr_gpoly_make--
           #:mjr_gpoly_make-*
           #:mjr_gpoly_make-iexpt
           #:mjr_gpoly_make-coeff
           #:mjr_gpoly_make-scale
           #:mjr_gpoly_make-truncate
           #:mjr_gpoly_make-rem
           #:mjr_gpoly_make-mod
           #:mjr_gpoly_make-divides?
           #:mjr_gpoly_make-gcd
           #:mjr_gpoly_make-degree
           #:mjr_gpoly_make-leading-coeff
           #:mjr_gpoly_make-constant-coeff
           #:mjr_gpoly_make-zerop
           #:mjr_gpoly_make-onep
           #:mjr_gpoly_make-constantp
           #:mjr_gpoly_make-eval
           #:mjr_gpoly_make-diff
           #:mjr_gpoly_make-subst
           #:mjr_gpoly_make-density
           #:mjr_gpoly_make-index
           ;; TODO:
           ;;#:mjr_gpoly_make-2square-free
           ))

(in-package :MJR_GPOLY)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gpoly_help ()
  "Back end code to generate code for univariate, dense polynomial libraries.  This is not intended for interactive use.

The idea is that a ring structure is defined and codified in another package, and that package is then exploited to generate code for polynomials over that
base ring.

Naming conventions:
  * A base ring is defined in a package called 'MJR_FOO' ('foo' is the logical name of the ring)
  * The package containing the code for the polynomial ring over 'foo' is called 'MJR_POLYFOO'

MJR_GFP & MJR_POLYGFP are the canonical examples of how to do this."
  (documentation 'mjr_gpoly_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gpoly_to-sym-str (&rest rest)
  "Convert objects to uppercase strings and concatenate them.  If given a single list, apply fucntion to elements."
  (if (and rest (listp (car rest)) (null (cdr rest)))
      (mapcar (lambda (y) (mjr_gpoly_to-sym-str y)) (car rest))
      (format nil "~:@(~{~a~^~}~)" rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gpoly_to-sym (&rest rest)
  ""
  (if (and rest (listp (car rest)) (null (cdr rest)))
      (mapcar (lambda (y) (mjr_gpoly_to-sym y)) (car rest))
      (intern (apply #'mjr_gpoly_to-sym-str rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gpoly_get-op (ring-name ring-op)
  "Return the symbol to use for the operation"
  (let ((std-rng (zerop (length ring-name))))
    (destructuring-bind (op-name pk-name) (if std-rng
                                              (or (cdr (assoc ring-op
                                                              '(("zerop"     . ("MJR_CMP_=0"        "MJR_CMP"))
                                                                ("onep"      . ("MJR_CMP_=1"        "MJR_CMP"))
                                                                ("divides?"  . ("MJR_INTU_DIVIDES?" "MJR_INTU"))
                                                                ("numberp"   . ("NUMBERP"           "COMMON-LISP"))
                                                                ("iexpt"     . ("EXPT"              "COMMON-LISP"))
                                                                ("imul"      . ("*"                 "COMMON-LISP")))
                                                              :test #'equalp))
                                                  (list (mjr_gpoly_to-sym-str ring-op) "COMMON-LISP"))
                                              (list (mjr_gpoly_to-sym-str "mjr_" ring-name "_" (if (equalp ring-op "numberp")
                                                                                                   (format nil "~ap" ring-name)
                                                                                                   ring-op))
                                                    (mjr_gpoly_to-sym-str "mjr_" ring-name)))
      (if (find-symbol op-name pk-name)
          (intern op-name pk-name)
          (if (not (and std-rng (equalp "simplify" op-name)))
              (warn "WARNING: Could not find ring (~a) operation (~a) -- function (~a) in package (~a)!" ring-name ring-op op-name pk-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-scale (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_SCALE."
  (let* ((ring-div-func      (mjr_gpoly_get-op ring-name "/"))
         (ring-mul-func      (mjr_gpoly_get-op ring-name "*"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_scale")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly x &optional invert-x)
       "Multiply, on the left, the coefficients of POLY by X (or 1/X if INVERT-X is non-NIL)."
       (if (not (,ring-numberp-func ,@ring-parameters x))
           (error ,(concatenate 'string new-func-name ": The second argument (X) must be a base ring element)")))
       (let ((fact (if invert-x
                       (,ring-div-func ,@ring-parameters x)
                       x)))
         (,poly-simplify-func ,@ring-parameters (map 'vector (lambda (x) (,ring-mul-func ,@ring-parameters x fact)) poly))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-coeff (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_COEFF."
  (let ((ring-simplify-func (mjr_gpoly_get-op ring-name "simplify"))
        (ring-add-func      (mjr_gpoly_get-op ring-name "+"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_coeff")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly n)
       "Return the coefficient on the term of degree n when n is non-negative, or the -n'th (1 based) coefficient if n is negative.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)"
       (let ((poly (if (vectorp poly) poly (vector poly)))
             (idx  (if (minusp n)
                       (- -1 n)
                       (let ((plen (length poly)))
                         (1- (- plen n))))))
         (if (array-in-bounds-p poly idx)
             ,(if ring-simplify-func
                  `(,ring-simplify-func ,@ring-parameters (aref poly idx))
                  `(aref poly idx))
             ,(apply ring-add-func ring-parameters))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-simplify (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_SIMPLIFY."
  (let ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
        (ring-simplify-func (mjr_gpoly_get-op ring-name "simplify"))
        (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_simplify")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       ,(concatenate 'string "Simplify the polynomial by removing leading zero terms.  If nothing needs to be done, then POLY is returned (not a copy).

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)

This function is almost FREE if the poly is already simplified, so it can be used to verify a polynomial is simplified without introducing too much
overhead.")
     (if (vectorp poly)
         (let* ((lt-idx (if (not (,ring-zerop-func ,@ring-parameters (aref poly 0)))
                            0
                            (or (position-if (lambda (x) (not (,ring-zerop-func ,@ring-parameters x))) poly)
                                (1- (length poly)))))
                (npoly  (if (zerop lt-idx)
                            poly
                            (subseq poly lt-idx))))
           ,(if ring-simplify-func
                `(map 'vector (lambda (x) (,ring-simplify-func ,@ring-parameters x)) npoly)
                `npoly))
         (if (,ring-numberp-func ,@ring-parameters poly)
           ,(if ring-simplify-func
                `(vector (,ring-simplify-func ,@ring-parameters poly))
                `(vector poly))
             (error ,(concatenate 'string new-func-name ": POLY must be a polynomial or base ring element")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-eval (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_EVAL."
  (let ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
        (ring-mul-func      (mjr_gpoly_get-op ring-name "*"))
        (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
        (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_eval")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly x)
       "Evaluate polynomial.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)"
       (if (not (,ring-numberp-func ,@ring-parameters x))
           (error ,(concatenate 'string new-func-name ": Polynomials may only be evaluated on base ring elements (i.e. X must be a base ring element)")))
       (let* ((poly (,poly-simplify-func ,@ring-parameters poly))
              (plen (length poly))
              (pval ,(apply ring-add-func ring-parameters)))
         (loop for i from 0 upto (1- plen)
               do (setq pval (,ring-add-func ,@ring-parameters (,ring-mul-func ,@ring-parameters x pval) (aref poly i)))
               finally (return pval))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-+ (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_+."
  (let ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
        (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
        (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_+")))
    `(defun ,(intern new-func-name) (,@ring-parameters &rest polys)
       ,(concatenate 'string "Add the given polynomials and base ring elements.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)")
       (cond ((null polys)       (vector ,(apply ring-add-func ring-parameters))) ;; (,ring-add-func ,@ring-parameters)))
             ((null (cdr polys)) (if (vectorp (car polys))
                                     (,poly-simplify-func ,@ring-parameters (car polys))
                                     (if (,ring-numberp-func ,@ring-parameters  (car polys))
                                         (vector (car polys))
                                         (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))
             ('t                 (loop for x in polys
                                       for len = (if (vectorp x)
                                                     (length x)
                                                     (if (,ring-numberp-func ,@ring-parameters x)
                                                         1
                                                         (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements"))))
                                       when (zerop len)
                                       do (error ,(concatenate 'string new-func-name ": Polynomials must not be of zero length!"))
                                       count    (vectorp x) into pcnt
                                       collect  len         into plens
                                       maximize len         into nplen
                                       finally (return (if (zerop pcnt)
                                                           (vector (reduce (lambda (x y) (,ring-add-func ,@ring-parameters x y)) polys))
                                                           (loop with newpoly = (make-array nplen :initial-element (,ring-add-func ,@ring-parameters))
                                                                 for poly in polys
                                                                 for plen in plens
                                                                 do (loop for i downfrom (1- nplen) downto 0
                                                                          for j downfrom (1- plen)  downto 0
                                                                          do (setf (aref newpoly i)
                                                                                   (,ring-add-func ,@ring-parameters
                                                                                                   (aref newpoly i)
                                                                                                   (if (vectorp poly) (aref poly j) poly))))
                                                                 finally (return (,poly-simplify-func ,@ring-parameters newpoly)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-- (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_-."
  (let ((ring-sub-func      (mjr_gpoly_get-op ring-name "-"))
        (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
        (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
        (poly-add-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_+"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "MJR_POLY" ring-name "_-")))
    `(defun ,(intern new-func-name) (,@ring-parameters &rest polys)
       ,(concatenate 'string "Subtract the given polynomials.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)")
       (cond ((null polys)       (error ,(concatenate 'string new-func-name ": At least one argument is required!")))
             ((null (cdr polys)) (if (vectorp (car polys))
                                     (,poly-simplify-func ,@ring-parameters (map 'vector (lambda (x) (,ring-sub-func ,@ring-parameters x)) (car polys)))
                                     (if (,ring-numberp-func ,@ring-parameters (car polys))
                                         (vector (,ring-sub-func ,@ring-parameters (car polys)))
                                         (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))
             ('t                 (loop with lhp     = (,poly-simplify-func ,@ring-parameters (car polys))
                                       with lhp-len = (length lhp)
                                       with rhp     = (apply #',poly-add-func ,@ring-parameters (cdr polys))
                                       with rhp-len = (length rhp)
                                       with ans-len = (max lhp-len rhp-len)
                                       with newpoly = (make-array ans-len :initial-element 0)
                                       for lhp-i downfrom (1- lhp-len)
                                       for rhp-i downfrom (1- rhp-len)
                                       for ans-i downfrom (1- ans-len) downto 0
                                       do (setf (aref newpoly ans-i) (- (if (minusp lhp-i) 0 (aref lhp lhp-i))
                                                                        (if (minusp rhp-i) 0 (aref rhp rhp-i))))
                                       finally (return (,poly-simplify-func ,@ring-parameters newpoly))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-* (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_*."
  (let ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
        (ring-mul-func      (mjr_gpoly_get-op ring-name "*"))
        (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
        (poly-mul-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_*"))
        (poly-add-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_+"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_*")))
    `(defun ,(intern new-func-name) (,@ring-parameters &rest polys)
       "Multiply the given polynomials and/or base ring elements.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :).

poly*poly = multiply as polynomials, s*poly & poly*s = multiply s by each element, s*s = multiply base ring elements."
       (let ((num-polys (length polys)))
         (case num-polys
           (0         (vector ,(apply ring-mul-func ring-parameters)))
           (1         (,poly-simplify-func ,@ring-parameters (car polys)))
           (2         (let ((poly1 (first  polys))
                            (poly2 (second polys)))
                        (let* ((poly1 (,poly-simplify-func ,@ring-parameters poly1))
                               (poly2 (,poly-simplify-func ,@ring-parameters poly2))
                               (plen1 (length poly1))
                               (plen2 (length poly2)))
                          (apply #',poly-add-func
                                 ,@ring-parameters
                                 (loop for i from 0 upto (1- plen1)
                                       for pcoef1 across poly1
                                       for npl = (make-array (- (+ plen1 plen2) i 1) :initial-element ,(apply ring-add-func ring-parameters))
                                       do (loop for j from 0
                                                for c across poly2
                                                do (setf (aref npl j) (,ring-mul-func ,@ring-parameters c pcoef1)))
                                       collect npl)))))
           (otherwise (reduce (lambda (x y) (,poly-mul-func ,@ring-parameters x y)) polys)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-iexpt (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_IEXPT."
  (let ((poly-mul-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_*"))
        (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
        (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_iexpt")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly n)
       ,(concatenate 'string "Compute poly^n

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :).

References:
  David Bressoud (1989); Factorization and primality testing; ISBN: 0-387-97040-1; pp33-34")
       (cond ((not (integerp n)) (error ,(concatenate 'string new-func-name ": N must be an integer!")))
             ((minusp n)         (error ,(concatenate 'string new-func-name ": N must be non-negative!"))))
       (if (zerop n)
           (,poly-mul-func ,@ring-parameters)
           (let ((pp (,poly-mul-func ,@ring-parameters)))
             (loop while (not (zerop n))
                   finally (return pp)
                   when (oddp n)
                   do (setf pp (,poly-mul-func ,@ring-parameters pp poly)
                            n  (- n 1))
                   do (setf poly (,poly-mul-func ,@ring-parameters poly poly)
                            n    (truncate n 2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-diff (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_DIFF."
  (let* ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
         (ring-imul-func    (mjr_gpoly_get-op ring-name "imul"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_diff"))
         (new-func-name-sym  (intern new-func-name)))
    `(defun ,new-func-name-sym (,@ring-parameters poly &optional (order 1))
       "Return the ORDER'th derivative of POLY."
       (let ((plen (length poly)))
         (cond ((< order 0)     (error ,(concatenate 'string new-func-name ": ORDER must be non-negative!")))
               ((= order 0)     (copy-seq poly))
               ((<= plen 1)     (make-array 1 :initial-element ,(apply ring-add-func ring-parameters)))
               ((<= plen order) (make-array 1 :initial-element ,(apply ring-add-func ring-parameters)))
               ((> order 1)     (,new-func-name-sym (,new-func-name-sym poly 1) (1- order)))
               ('t              (,poly-simplify-func ,@ring-parameters
                                                     (let ((dpoly (make-array (1- plen) :initial-element ,(apply ring-add-func ring-parameters))))
                                                       (loop for i below (1- plen)
                                                             do (setf (aref dpoly i) (,ring-imul-func ,@ring-parameters (aref poly i) (- plen i 1)))
                                                             finally (return dpoly))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-leading-coeff (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_LEADING-COEFF."
  (let* ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
         (ring-add-func      (mjr_gpoly_get-op ring-name "+"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-simplify-func (mjr_gpoly_get-op ring-name "simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_leading-coeff")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return the leading term (the non-zero coefficient of the term with highest power).

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :).

Returns 0 if all the coefficients were zero -- note that zero is the additive identity in the base ring."
       (let ((lc (if (vectorp poly)
                     (or (find-if (lambda (x) (not (,ring-zerop-func ,@ring-parameters x))) poly)
                         ,(apply ring-add-func ring-parameters))
                     (if (,ring-numberp-func ,@ring-parameters poly)
                         poly
                         (error ,(concatenate 'string new-func-name ": Arguments must be a polynomial or base ring element"))))))
         ,(if ring-simplify-func
              `(,ring-simplify-func ,@ring-parameters lc)
              `lc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-degree (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_DEGREE."
  (let* ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_degree")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return the degree of POLY.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (let ((pos  (position-if (lambda (x) (not (,ring-zerop-func ,@ring-parameters x))) poly))
                 (plen (length poly)))
             (if pos
                 (- plen pos 1)
                 (if (> plen 0)
                     0
                     (error ,(concatenate 'string new-func-name ": Polynomials must be of non-zero length")))))
           (if (,ring-numberp-func ,@ring-parameters poly)
               0
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-density (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_DENSITY."
  (let* ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_density")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return the density of POLY (i.e. The number of non-zero terms)

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (count-if (lambda (x) (not (,ring-zerop-func ,@ring-parameters x))) poly)
           (if (,ring-numberp-func ,@ring-parameters poly)
               (if (,ring-zerop-func ,@ring-parameters poly)
                   1
                   0)
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-index (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_INDEX."
  (let* ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_index")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return the index of POLY (i.e. Sum of the exponents of the non-zero terms)

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (loop for i from (1- (length poly)) downto 0
                 for c across poly
                 when (not (,ring-zerop-func ,@ring-parameters c))
                 sum i)
           (if (,ring-numberp-func ,@ring-parameters poly)
               0
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-constant-coeff (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_CONSTANT-COEFF."
  (let* ((ring-simplify-func (mjr_gpoly_get-op ring-name "simplify"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_constant-coeff")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return the coefficient on the term with no x (i.e. zero power term).

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (let ((ct (if (vectorp poly)
                     (aref poly (1- (length poly)))
                     (if (,ring-numberp-func ,@ring-parameters poly)
                         poly
                         (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements"))))))
         ,(if ring-simplify-func
              `(,ring-simplify-func ,@ring-parameters ct)
              `ct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-onep (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_ONEP."
  (let* ((ring-onep-func    (mjr_gpoly_get-op ring-name "onep"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_onep")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return non-NIL if the polynomial is the unit (i.e. == 1).

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (if (= 1 (length poly))
               (,ring-onep-func ,@ring-parameters (aref poly 0))
               (let ((poly (,poly-simplify-func ,@ring-parameters poly)))
                 (and (= (length poly) 1) (,ring-onep-func ,@ring-parameters (aref poly 0)))))
           (if (,ring-numberp-func ,@ring-parameters poly)
               (,ring-onep-func ,@ring-parameters poly)
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-constantp (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_CONSTANTP."
  (let* ((ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_constantp")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Return non-NIL if the polynomial is a constant polynomial (i.e. degree 1)

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (= 1 (length (,poly-simplify-func ,@ring-parameters poly)))
           (if (,ring-numberp-func ,@ring-parameters poly)
               't
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-zerop (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_ZEROP."
  (let* ((ring-zerop-func    (mjr_gpoly_get-op ring-name "zerop"))
         (ring-numberp-func  (mjr_gpoly_get-op ring-name "numberp"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_zerop")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly)
       "Returns non-NIL if the polynomial is the zero polynomial.

Things that are not vectors are assumed to be ring elements.  This works well when ring elements are not vectors too. :)."
       (if (vectorp poly)
           (every (lambda (x) (,ring-zerop-func ,@ring-parameters x)) poly)
           (if (,ring-numberp-func ,@ring-parameters poly)
               (,ring-zerop-func ,@ring-parameters poly)
               (error ,(concatenate 'string new-func-name ": Arguments must be Polynomials and/or base ring elements")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-truncate (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_TRUNCATE."
  (let* ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
         (ring-sub-func      (mjr_gpoly_get-op ring-name "-"))
         (ring-div-func      (mjr_gpoly_get-op ring-name "/"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_truncate")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly1 poly2)
       "Return the quotient and the remainder from POLY1/POLY2.

Based upon a generalization of synthetic division published in 2003 by Lianghuo FAN.  The algorithm implemented is a minor generalization of FAN's work that
supports non-monic polynomial divisors.  Note that this implementation is not optimized.

Reference: Lianghuo FAN (2003)
           A Generalization of Synthetic Division and A General Theorem of Division of Polynomials; Mathematical Medley, June 2003; pp30-37"
       (let* ((poly1 (,poly-simplify-func ,@ring-parameters poly1))
              (poly2 (,poly-simplify-func ,@ring-parameters poly2))
              (len1  (length poly1))
              (len2  (length poly2)))
         (if (> len2 len1)
             (values (vector ,(apply ring-add-func ring-parameters))
                     poly1)
             (if (= 1 (length poly2))
                 (values (,poly-simplify-func ,@ring-parameters (map 'vector
; MJR TODO NOTE <2011-11-23 13:19:59 CST> mjr_gpoly_make-truncate: Use _scale here
                                                                     (lambda (x) (,ring-div-func ,@ring-parameters x (aref poly2 0)))
                                                                     poly1))
                         (vector ,(apply ring-add-func ring-parameters)))
                 (let ((tmpvec (make-array len1 :initial-element (aref poly1 0))))
                   (loop for i from 1 upto (1- len1)
                         do (setf (aref tmpvec i) (,ring-add-func (aref poly1 i)
                                                                  (,ring-div-func (loop for j from (max 1 (+ (- len2 len1) i)) upto (1- len2)
                                                                                        sum (* (if (<= 0 (- i j))
                                                                                                   (aref tmpvec (- i j))
                                                                                                   ,(apply ring-add-func ring-parameters))
                                                                                               (,ring-sub-func (aref poly2 j))))
                                                                                  (aref poly2 0)))))
                   (values (,poly-simplify-func ,@ring-parameters (map 'vector
; MJR TODO NOTE <2011-11-23 13:19:59 CST> mjr_gpoly_make-truncate: Use _scale here
                                                                       (lambda (x) (,ring-div-func ,@ring-parameters x (aref poly2 0)))
                                                                       (subseq tmpvec 0 (1+ (- len1 len2)))))
                           (,poly-simplify-func ,@ring-parameters (subseq tmpvec (1+ (- len1 len2))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-mod (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_MOD."
  (let* ((poly-truncate-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_truncate"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_mod")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly1 poly2)
       "Returns the quotient of poly1/poly2"
       (multiple-value-bind (quo rem) (,poly-truncate-func ,@ring-parameters poly1 poly2)
         (declare (ignore rem))
         quo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-rem (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_REM."
  (let* ((poly-truncate-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_truncate"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_rem")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly1 poly2)
       "Returns the quotient of poly1/poly2"
       (multiple-value-bind (quo rem) (,poly-truncate-func ,@ring-parameters poly1 poly2)
         (declare (ignore quo))
         rem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-divides? (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_DIVIDES?."
  (let* ((ring-divides?-func (mjr_gpoly_get-op ring-name "divides?"))
         (poly-truncate-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_truncate"))
         (poly-lc-func       (mjr_gpoly_to-sym "mjr_poly" ring-name "_leading-coeff"))
         (poly-cc-func       (mjr_gpoly_to-sym "mjr_poly" ring-name "_constant-coeff"))
         (poly-zerop-func    (mjr_gpoly_to-sym "mjr_poly" ring-name "_zerop"))
         (poly-degree-func   (mjr_gpoly_to-sym "mjr_poly" ring-name "_degree"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_divides?")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly1 poly2 &key require-integer-quotient)
       "Returns the quotient of poly2/poly1 if poly1 divides poly2, and NIL otherwise."
       (and (,ring-divides?-func ,@ring-parameters (,poly-lc-func ,@ring-parameters poly1)  (,poly-lc-func ,@ring-parameters poly2))
            (,ring-divides?-func ,@ring-parameters (,poly-cc-func ,@ring-parameters poly1) (,poly-cc-func ,@ring-parameters poly2))
            (<= (,poly-degree-func ,@ring-parameters poly1) (,poly-degree-func ,@ring-parameters poly2))
            (multiple-value-bind (quo rem) (,poly-truncate-func ,@ring-parameters poly2 poly1)
              (if (,poly-zerop-func ,@ring-parameters rem)
                  (if (or (not require-integer-quotient) (every #'integerp quo))
                      quo)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-gcd (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_GCD."
  (let* ((ring-add-func      (mjr_gpoly_get-op ring-name "+"))
         (ring-div-func      (mjr_gpoly_get-op ring-name "/"))
         (poly-lc-func       (mjr_gpoly_to-sym "mjr_poly" ring-name "_leading-coeff"))
         (poly-rem-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_rem"))
         (poly-zerop-func    (mjr_gpoly_to-sym "mjr_poly" ring-name "_zerop"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_gcd")))
    `(defun ,(intern new-func-name) (,@ring-parameters poly1 poly2)
       "Return monic GCD of poly1 and poly2

References:
  Joel S. Cohen (2003); Computer Algebra and Symbolic Computation: Mathematical Methods; ISBN: 1568811594; pp130"
       (if (and (,poly-zerop-func ,@ring-parameters poly1) (,poly-zerop-func ,@ring-parameters poly2))
           (vector ,(apply ring-add-func ring-parameters))
           (let ((poly1w (copy-seq poly1))
                 (poly2w (copy-seq poly2)))
             (loop while (not (,poly-zerop-func ,@ring-parameters poly2w))
                   do (psetq poly1w poly2w
                             poly2w (,poly-rem-func ,@ring-parameters poly1w poly2w)))
; MJR TODO NOTE <2011-11-23 13:19:59 CST> mjr_gpoly_make-truncate: Use _scale here
             (map 'vector
                  (lambda (x) (,ring-div-func ,@ring-parameters x (,poly-lc-func ,@ring-parameters poly1w)))
                  poly1w))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mjr_gpoly_make-subst (ring-name &rest ring-parameters)
  "Construct and defun for MJR_POLY<RING>_SUBST."
  (let* ((poly-mul-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_*"))
         (poly-add-func      (mjr_gpoly_to-sym "mjr_poly" ring-name "_+"))
         (poly-simplify-func (mjr_gpoly_to-sym "mjr_poly" ring-name "_simplify"))
         (ring-parameters    (mjr_gpoly_to-sym ring-parameters))
         (new-func-name      (mjr_gpoly_to-sym-str "mjr_poly" ring-name "_subst")))
    `(defun ,(intern new-func-name) (,@ring-parameters sub-poly poly)
       "Substitute SUB-POLY into POLY.
Not very fast, but very flexible."
       (let* ((poly     (,poly-simplify-func ,@ring-parameters poly))
              (sub-poly (,poly-simplify-func ,@ring-parameters sub-poly)))
         (apply #',poly-add-func
                ,@ring-parameters
                (loop for pow-val = #(1) then (,poly-mul-func ,@ring-parameters pow-val sub-poly)
                      for i from (1- (length poly)) downto 0
                      when (not (zerop (aref poly i)))
                      collect (,poly-mul-func ,@ring-parameters (aref poly i) pow-val)))))))
