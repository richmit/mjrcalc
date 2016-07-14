;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-vvec.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Virtual vectors.@EOL
;; @std       Common Lisp
;; @see       tst-vvec.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2008,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_VVEC
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Virtual vectors.;")
  (:EXPORT #:mjr_vvec_help

           ;; Not exported (internal use)
           ;; #:mjr_vvec_check-len
           ;; #:mjr_vvec_kw-val-type-check
           ;; #:mjr_vvec_representation

           #:mjr_vvec_normalize-kw-vvt-nil
           #:mjr_vvec_normalize-kw-vvt-aseq
           #:mjr_vvec_normalize-kw-vvt-points
           #:mjr_vvec_normalize-kw-vvt-rep
           #:mjr_vvec_normalize-kw-vvt-mitch1
           #:mjr_vvec_normalize-kw-vvt-cheb
           #:mjr_vvec_normalize-kw-vvt-nfun
           #:mjr_vvec_normalize-kw-vvt-rfun
           #:mjr_vvec_normalize-kw-vvt-any

           #:mjr_vvec_kw-type

           ;; #:mjr_vvec_vvec2rai -- Not implemented.  "Random Access Iterator"
           #:mjr_vvec_vvec2fi

           #:mjr_vvec_map-filter-reduce

           #:mjr_vvec_gen-1sim
           #:mjr_vvec_to-vec
           #:mjr_vvec_to-list

           #:mjr_vvec_map-sum
           #:mjr_vvec_map-prod
           #:mjr_vvec_map-maxi
           #:mjr_vvec_map-mini
           ))

(in-package :MJR_VVEC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_help ()
  "Help for MJR_VVEC: Virtual VECtor

Representation

  A 'Virtual Vector' is a numeric vector that may be specified 'virtually' in various ways:

     |-----------------------+--------------------------------------------------------------------|
     | Vector Representation | Description                                                        |
     |-----------------------+--------------------------------------------------------------------|
     | :VVR-INT              | An integer, $n$, which specifies an $\vert n\vert$ element vector: |
     |                       | * $n=0$ .. The empty vector                                        |
     |                       | * $n>0$ .. $[0,1,...,n-1]$                                         |
     |                       | * $n<0$ .. $[1,2,...,-n]$, or the empty vector                     |
     |-----------------------+--------------------------------------------------------------------|
     | :VVR-VEC              | An vector of numbers explicitly listing the vector contents        |
     |-----------------------+--------------------------------------------------------------------|
     | :VVR-LIST             | An list of numbers explicitly listing the vector contents          |
     |-----------------------+--------------------------------------------------------------------|
     | :VVR-KW               | A list vvec keywords and values. This is the canonical form        |
     |-----------------------+--------------------------------------------------------------------|

Keyword Representation

  Virtual vectors with a keyword representation (:vvr-kw) are lists of keywords and values -- like a &key list.  The keywords are:

      - :VVEC-TYPE .. The 'type' of the :vvt-kw virtual vectors.  Types are specified by one of several symbols:
                      :VVT-NIL, :VVT-ASEQ, :VVT-REP, :VVT-CHEB, :VVT-MITCH1, :VVT-POINTS, :VVT-RFUN, & :VVT-NFUN
      - :POINTS ..... Used for directly specifying virtual vectors by explicitly listing the elements
      - :START ...... Used for :VVT-ASEQ & :VVT-POINTS
      - :END ........ Used for :VVT-ASEQ & :VVT-POINTS
      - :STEP ....... Used for :VVT-ASEQ & :VVT-POINTS
      - :LEN ........ Used for all virtual vectors types
                      If :LEN is a float, then it is rounded to an integer.
                      If :LEN is a vector or list, then the length of the vector or list is used
      - :MAP-FUN .... Map virtual vector values just before they are accessed.  Used for all virtual vectors types.
      - :NFUN ....... Used for virtual vectors defined via 'generator' -- think python
      - :RFUN ....... Used for virtual vectors defined via a recurrence relationship
.
  The meanings of the keyword arguments, and other special cases, are defined in the fallowing table:

     |-------------+-----------+------------------+---------------------------------------------+-----------------------------------------------|
     | Type        | Rep       | Description      | Parameters                                  | Sequence                                      |
     |-------------+-----------+------------------+---------------------------------------------+-----------------------------------------------|
     | :VVT-NIL    | :vvr-kw   | Empty vector     | :LEN 0                                      | nil                                           |
     | :VVT-ASEQ   | :vvr-kw   | Arithmetic Seq   | :START # :END # :LEN # :STEP #              | n_i = START+i*STEP                            |
     | :VVT-ASEQ   | :vvr-int  | Zero based Count | positive integer N (Like :LEN N)            | n_i = i                                       |
     | :VVT-ASEQ   | :vvr-int  | One based Count  | Negative integer N (Like :START 1 :LEN N)   | n_i = i+1                                     |
     | :VVT-REP    | :vvr-kw   | Repeat           | :START # :LEN # :VVEC-TYPE :VVT-REP         | n_i = START                                   |
     | :VVT-CHEB   | :vvr-kw   | Chebyshev        | :START # :END :LEN # :VVEC-TYPE :VVT-CHEB   | Chebyshev interpolation points                |
     | :VVT-MITCH1 | :vvr-kw   | Chebyshev-like   | :START # :END :LEN # :VVEC-TYPE :VVT-MITCH1 | Chebyshev-like interpolation points           |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S                                   | n_i = (elt S i)                               |
     | :VVT-POINTS | :vvr-vec  | Empirical Data   | LISP vector V (Same as :POINTS V)           | n_i = (aref V i)                              |
     | :VVT-POINTS | :vvr-list | Empirical Data   | LISP list L (Same as :POINTS L)             | n_i = (nth L i)                               |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S :LEN #                            | n_i = (elt S i) for i<LEN                     |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S :START #                          | n_i = (elt S (+ START i))                     |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S :END #                            | n_i = (elt S i) for i<=END                    |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S :START # :END #                   | n_i = (elt S (+ START i)) for i<=END-START+1  |
     | :VVT-POINTS | :vvr-kw   | Empirical Data   | :POINTS S :START # :LEN #                   | n_i = (elt S (+ START i)) for i<LEN           |
     | :VVT-RFUN   | :vvr-kw   | Recurrence       | :START # :LEN # :RFUN f                     | n_i = f(n_{i-1}, ...)                         |
     |             |           |                  |                                             | If START is an n element list, f takes n args |
     | :VVT-NFUN   | :vvr-kw   | No-Arg Function  | :LEN # :NFUN f                              | n_i = f()                                     |
     |-------------+-----------+------------------+---------------------------------------------+-----------------------------------------------|

  The final step of the vector generation process is to map the elements via MAP-FUN, which may be provided for any :VVEC-TYPE.  Note that :MAP-FUN is called
  for each value of the sequence, even for :VVT-REP, to account for possible side effects in MAP-FUN."
  (documentation 'mjr_vvec_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_representation (vvec &optional error-if-unknown)
  "If VVEC appears to have a valid VVEC representation, return one of the :VVR-* symbols documented in MJR_VVEC_HELP, and NIL or error otherwise.

   NOTE: This function doesn't check syntax.  For example, if VVEC is a vector, then :vvr-vec is returned -- we don't check to make sure that the vector
   contains only numbers.  As another example, we don't check to make sure keyword arguments are used correctly."
  (or (typecase vvec
        (vector   :vvr-vec)
        (integer  :vvr-int)
        (list     (if (and (> 22 (length vvec))
                           (intersection '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec))
                      :vvr-kw
                      :vvr-list)))
      (if error-if-unknown
          (error "mjr_vvec_representation: Invalid virtual vector representation"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_check-len (len len-supplied-p)
  "Return an integer length based on the :len keyword value.  An error is generated if we can't do the conversion."
  (if len-supplied-p
      (typecase len
        (integer    len)
        (float      (round len))
        (string     (error "mjr_vvec_check-len: :LEN was a string!"))
        (vector     (length len))
        (list       (length len))
        (otherwise  (error "mjr_vvec_check-len: :LEN was an invalid object")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_kw-type (kw-vvec)
  "Return a symbol for the keyword type (one of the :VVT-* symbols documented in the MJR_VVEC_HELP function). Errors are possible.

The sequence type is one of the following:
    * :VVT-ASEQ   -- arithmetic sequence
                       Assumed if :STEP is non-NIL or :VVEC-TYPE is :VVT-ASEQ
                       Assumed if none of the following rules match, and len, start, or end is provided.
                       See: MJR_VVEC_KW-NORMALIZE-ASEQ
    * :VVT-POINTS -- Explicitly enumerated sequence (list or vector)
                       Assumed if :POINTS is provided or :VVEC-TYPE is :VVT-POINTS
                       See: MJR_VVEC_KW-NORMALIZE-POINTS
    * :VVT-REP    -- Repeat a value
                       :VVEC-TYPE must be explicitly set to :VVT-REP
                       Fully specified by: :START & :LEN
                       See: MJR_VVEC_KW-NORMALIZE-REP
    * :VVT-CHEB   -- Chebyshev nodes
                       :VVEC-TYPE must be explicitly set to :VVT-CHEB
                       Fully specified by: :START, :END, :LEN
                       See: MJR_VVEC_KW-NORMALIZE-CHEB
    * :VVT-MITCH1 -- Chebyshev'esq nodes
                       :VVEC-TYPE must be explicitly set to :VVT-MITCH1
                       Fully specified by: :START, :END, :LEN
                       See: MJR_VVEC_KW-NORMALIZE-MITCH1
    * :VVT-RFUN   -- Recurrence. First $k$ elements in :START & :RFUN returns $x_n$ with $n>k$ given the previous $k$ sequence elements.
                       Assumed if :RFUN is provided or :VVEC-TYPE is :VVT-RFUN
                       :START & :LEN is required.
                       See: MJR_VVEC_KW-NORMALIZE-RFUN
    * :VVT-NFUN   -- No-arg Function
                       Assumed if :NFUN is provided or if :VVEC-TYPE is :VVT-NFUN
                       :LEN is required
                       See: MJR_VVEC_KW-NORMALIZE-NFUN"
  (if (equalp :vvr-kw (mjr_vvec_representation kw-vvec 't))
      (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
          (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
        (multiple-value-bind (vvec-type len)
            (mjr_util_get-kwarg-vals '(:vvec-type :len) kw-vvec)
          (let ((len (mjr_vvec_check-len len len-supplied-p)))
            (cond (vvec-type-supplied-p  (if (member vvec-type '(:VVT-ASEQ :VVT-CHEB :VVT-MITCH1 :VVT-NFUN :VVT-NIL :VVT-POINTS :VVT-REP :VVT-RFUN))
                                             vvec-type
                                             (error "mjr_vvec_kw-type: :VVEC-TYPE was provided, but was an invalid value")))
                  ((and len-supplied-p
                        (zerop len))     :vvt-nil)
                  (points-supplied-p     :vvt-points)
                  (nfun-supplied-p       :vvt-nfun)
                  (rfun-supplied-p       :vvt-rfun)
                  (step-supplied-p       :vvt-aseq)
                  ((or end-supplied-p
                       start-supplied-p
                       len-supplied-p)   :vvt-aseq)
                  ('t                    (error "mjr_vvec_kw-type: Unable to determine virtual vector type!"))))))
      (error "mjr_vvec_kw-type: VVEC must be a keyword virtual vector!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_kw-val-type-check (kw-vvec)
  "Check if kw-vvec is a keyword vector and that the types of the values are correct -- errors if a problem is found."
  (if (equalp :vvr-kw (mjr_vvec_representation kw-vvec 't))
      (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p map-fun-supplied-p nfun-supplied-p rfun-supplied-p)
          (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) kw-vvec)
        (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
            (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) kw-vvec 't)
          (cond ((and vvec-type-supplied-p (not (member vvec-type
                                                        '(:VVT-ASEQ :VVT-CHEB
                                                          :VVT-MITCH1 :VVT-NFUN
                                                          :VVT-NIL :VVT-POINTS
                                                          :VVT-REP :VVT-RFUN))))     (error "mjr_vvec_kw-val-type-check: Invalid :vvec-type value"))
                ((and points-supplied-p    (not (or (vectorp points)
                                                    (listp points))))                (error "mjr_vvec_kw-val-type-check: Invalid :points value"))
                ((and start-supplied-p     (not (or (numberp start)
                                                    (and start
                                                         (listp start)
                                                         (every #'numberp start))))) (error "mjr_vvec_kw-val-type-check: Invalid :start value: ~a" start))
                ((and end-supplied-p       (not (numberp end)))                      (error "mjr_vvec_kw-val-type-check: Invalid :end value"))
                ((and step-supplied-p      (not (numberp step)))                     (error "mjr_vvec_kw-val-type-check: Invalid :step value"))
                ((and len-supplied-p       (or (stringp len)
                                               (not (or (integerp len)
                                                        (floatp   len)
                                                        (vectorp  len)
                                                        (listp    len)))))           (error "mjr_vvec_kw-val-type-check: Invalid :len value"))
                ((and map-fun-supplied-p   (null map-fun))                           (error "mjr_vvec_kw-val-type-check: Invalid :map-fun value"))
                ((and nfun-supplied-p      (null nfun))                              (error "mjr_vvec_kw-val-type-check: Invalid :nfun value"))
                ((and rfun-supplied-p      (null rfun))                              (error "mjr_vvec_kw-val-type-check: Invalid :rfun value")))))
      (error "mjr_vvec_kw-type: VVEC must be a keyword virtual vector!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-aseq (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-ASEQ -- i.e. a finite arithmetic sequence virtual vector.

On success, a :vvr-kw virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-ASEQ with the following keywords defined: :VVEC-TYPE, :START, :END, :STEP, :LEN, & :MAP-FUN.

    Note: :END may not be the value that was given -- it may be corrected so that it is consistent with the other values.
    Note: NEVER use the value returned for :end!

An arithmetic sequence may be fully specified by any tree of the following: first element (START), last element (END), the number of terms (LEN), and the
delta between each pair of terms (STEP).  This function is designed to normalize such parameters describing an arithmetic sequence. Intelligent defaults are
provided for cases when fewer than three values are provided.

How the arguments are processed until ERROR, EMPTY_VECTOR, or CHECK results by repeatedly applying the following, 'switch' statement:

  |------+----------+------+-------+-------+-------+-------+--------------------+----------------------------------+----------------------------------------------|
  |      | case     |      | START | END   | STEP  | LEN   | Condition          | Action.                          | Action Sequence                              |
  |------+----------+------+-------+-------+-------+-------+--------------------+----------------------------------+----------------------------------------------|
  |    1 | case 0.0 | xxx1 | N/A   | N/A   | N/A   | 0     |                    | START, END, STEP <- NIL          | EMPTY_VECTOR                                 |
  |    2 | case 0.1 | 111x | KNOWN | KNOWN | KNOWN | N/A   | START>END & STEP>0 | LEN   <- NIL                     | case 0.0 => EMPTY VECTOR                     |
  |    3 | case 0.2 | 111x | KNOWN | KNOWN | KNOWN | N/A   | START<END & STEP<0 | LEN   <- NIL                     | case 0.0 => EMPTY VECTOR                     |
  |    4 | case 1.0 | 1111 | KNOWN | KNOWN | KNOWN | KNOWN |                    | Checked for consistency.         | CHECK                                        |
  |    5 | case 2.1 | 0111 |       | KNOWN | KNOWN | KNOWN |                    | START <- END-(LEN-1)*STEP        | case 1.0  => CHECK                           |
  |    6 | case 2.2 | 1011 | KNOWN |       | KNOWN | KNOWN |                    | END   <- START+(LEN-1)*STEP      | case 1.0  => CHECK                           |
  |  7.1 | case 2.3 | 1101 | KNOWN | KNOWN |       | KNOWN | LEN=1              | STEP  <- 0                       | case 1.0  => CHECK                           |
  |  7.2 | case 2.3 | 1101 | KNOWN | KNOWN |       | KNOWN |                    | STEP  <- (END-START)/(LEN-1)     | case 1.0  => CHECK                           |
  |    8 | case 2.4 | 1110 | KNOWN | KNOWN | KNOWN |       |                    | LEN   <- (END-START)/STEP+1      | case 1.0  => CHECK                           |
  |    9 | case 3.1 | 0011 |       |       | KNOWN | KNOWN |                    | START <- 0                       | case 2.2  => case 1.0  => CHECK              |
  |   10 | case 3.2 | 0101 |       | KNOWN |       | KNOWN |                    | START <- 0                       | case 2.3  => case 1.0  => CHECK              |
  |   11 | case 3.3 | 0110 |       | KNOWN | KNOWN |       |                    | START <- 0                       | case 2.4  => case 1.0  => CHECK              |
  | 12.1 | case 3.4 | 1001 | KNOWN |       |       | KNOWN | LEN=1              | STEP  <- 1                       | case 2.2  => case 1.0  => CHECK              |
  | 12.2 | case 3.4 | 1001 | KNOWN |       |       | KNOWN |                    | STEP  <- 1                       | case 2.2  => case 1.0  => CHECK              |
  |   13 | case 3.5 | 1010 | KNOWN |       | KNOWN |       |                    | ERROR                            | ERROR                                        |
  |   14 | case 3.6 | 1100 | KNOWN | KNOWN |       |       |                    | LEN   <- 1+abs(round(end-start)) | case 2.3  => case 1.0  => CHECK              |
  |   15 | case 4.1 | 0001 |       |       |       | KNOWN |                    | START <- 0                       | case 3.4  => case 2.2  => case 1.0  => CHECK |
  |   16 | case 4.2 | 0010 |       |       | KNOWN |       |                    | ERROR                            | ERROR                                        |
  |   17 | case 4.3 | 0100 |       | KNOWN |       |       |                    | START <- 0                       | case 3.6  => case 2.4  => case 1.0  => CHECK |
  |   18 | case 4.4 | 1000 | KNOWN |       |       |       |                    | ERROR                            | ERROR                                        |
  |   19 | case 4.5 | 0000 |       |       |       |       |                    | ERROR                            | ERROR                                        |
  |------+----------+------+-------+-------+-------+-------+--------------------+----------------------------------+----------------------------------------------|

Once the arguments are normalized, an arithmetic sequence can be generated.  We ALWAYS generate the Ith (zero indexed) element with $START+i*STEP$ -- i.e. the
first element is $START+0*STEP=START$.  For only sequential access we could successively add STEP to an initial value; however, we wish to use the same
formula for random and sequential access in order to insure the same results even in the face of round-off error.  We never use END to generate or check for
termination because we wish to avoid length 'twitter' due to round-off error.  Note that one will normally use the MJR_VVEC_VVEC2FI function to iterate over
a :VVT-ASEQ virtual vector; however, this is how one might 'do it by hand':

  (multiple-value-bind (start step len)
      (mjr_util_get-kwarg-vals '(:start :step :len) (mjr_vvec_normalize-kw-vvt-any vvec))
    (if (not (zerop len))
        (loop for i from 0 to (1- len)
              for x = (+ start (* i step))
              collect x)))
This function uses MJR_CMqP_!=0 against :START, :END, :STEP, and/or :LEN."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p map-fun-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type start end step len map-fun)
        (mjr_util_get-kwarg-vals '(:vvec-type :start :end :step :len :map-fun) kw-vvec 't)
      ;;(print kw-vvec)
      (let* ((len       (mjr_vvec_check-len len len-supplied-p))
             (ap        (+ (if start-supplied-p 8 0) (if end-supplied-p 4 0) (if step-supplied-p 2 0) (if len-supplied-p 1 0)))
             (rngR      (and start-supplied-p end-supplied-p (- end start)))
             (rngL      (and step-supplied-p len-supplied-p (* step (1- len))))
             (step=0    (and step-supplied-p (mjr_cmp_=0 step 1e-6))) ;; Set this small so we can use tiny steps
             (havSESnL  (and start-supplied-p end step (null len)))
             (len=1     (and len (mjr_cmp_= 1 len)))
             (rngR=0    (and rngR (mjr_cmp_=0 rngR)))
             (maparg    (if map-fun-supplied-p (list :map-fun map-fun)))
             (vvec-type (if vvec-type-supplied-p vvec-type :vvt-aseq)))
        (cond ((not (equalp vvec-type :vvt-aseq))         (error "mjr_vvec_normalize-kw-vvt-aseq: :VVEC-TYPE must be :VVT-ASEQ!"))
              (points-supplied-p                          (error "mjr_vvec_normalize-kw-vvt-aseq: Incompatible with :POINTS"))
              (rfun-supplied-p                            (error "mjr_vvec_normalize-kw-vvt-aseq: Incompatible with :RFUN"))
              (nfun-supplied-p                            (error "mjr_vvec_normalize-kw-vvt-aseq: Incompatible with :NFUN"))
              ((and len (zerop len))                      (list :vvec-type :vvt-nil :len 0))                                                                                                                              ;; 1
              ((and havSESnL (> start end) (plusp  step)) (list :vvec-type :vvt-nil :len 0))                                                                                                                              ;; 2
              ((and havSESnL (< start end) (minusp step)) (list :vvec-type :vvt-nil :len 0))                                                                                                                              ;; 3
              ((and len (minusp len))                     (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: len<0!"))
              ((and len=1 rngR (not rngR=0))              (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: len=0 but start!=end!"))
              ((and len=1 step-supplied-p (not step=0))   (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: len=1, but step!=0!"))
              ((and step=0 len-supplied-p (not len=1))    (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: step=0, but len!=1!"))
              ((and rngR=0 step-supplied-p (not step=0))  (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: start=end, but step!=0!"))
              ((and rngR=0 len-supplied-p (not len=1))    (error "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters: start=end, but len!=1!"))
              ((= ap #b0111)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start (- end rngL) :end end            :step step              :len len                              ) maparg))) ;; 5
              ((= ap #b1011)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start        :end (+ start rngL) :step step              :len len                              ) maparg))) ;; 6

              ((and (= ap #b1101) (= len 1))              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start        :end end            :step 0                 :len len                              ) maparg))) ;; 7.1
              ((= ap #b1101)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start        :end end            :step (/ rngR (1- len)) :len len                              ) maparg))) ;; 7.2

              ((= ap #b1110)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start        :end end            :step step              :len (1+ (/ rngR step))               ) maparg))) ;; 8
              ((= ap #b0011)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start 0                                :step step              :len len                              ) maparg))) ;; 9
              ((= ap #b0101)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start 0            :end end                                    :len len                              ) maparg))) ;; 10
              ((= ap #b0110)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start 0            :end end            :step step                                                    ) maparg))) ;; 11
              ((and (= ap #b1001) (= len 1))              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start                            :step 0                 :len len                              ) maparg))) ;; 12.1
              ((= ap #b1001)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start                            :step 1                 :len len                              ) maparg))) ;; 12.2
              ((= ap #b1010)                              (error "mjr_vvec_normalize-kw-vvt-aseq: Unable to compute missing arithmetic sequence parameters!"))                                                            ;; 13
              ((= ap #b1100)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start start        :end end                                    :len (+ (abs (round (- end start))) 1)) maparg))) ;; 14
              ((= ap #b0001)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start 0                                                        :len len                              ) maparg))) ;; 15
              ((= ap #b0010)                              (error "mjr_vvec_normalize-kw-vvt-aseq: Unable to compute missing arithmetic sequence parameters!"))                                                            ;; 16
              ((= ap #b0100)                              (mjr_vvec_normalize-kw-vvt-aseq (append (list :start 0           :end end                                                                           ) maparg))) ;; 17
              ((= ap #b1000)                              (error "mjr_vvec_normalize-kw-vvt-aseq: Unable to compute missing arithmetic sequence parameters!"))                                                            ;; 18
              ((= ap #b0000)                              (error "mjr_vvec_normalize-kw-vvt-aseq: Unable to compute missing arithmetic sequence parameters!"))                                                            ;; 19
              ((= ap #b1111)                              (progn (if (mjr_cmp_!= rngL rngR)
                                                                     (warn "mjr_vvec_normalize-kw-vvt-aseq: Inconsistent arithmetic sequence parameters (:end fixed)!"))
                                                                 (list :vvec-type vvec-type :start start :end (+ (* (1- len) step) start) :step step :len len :map-fun map-fun))))))))                                    ;; 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-points (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-POINTS -- i.e. a virtual vector explicitly describing by a set of points.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-ASEQ with the following keywords defined: :VVEC-TYPE, :POINTS, :START, :END, :LEN, & :MAP-FUN.
    Note: :START, :END, & :LEN will be consistent."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type points start end len map-fun)
        (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :len :map-fun) kw-vvec 't)
      (let* ((len       (mjr_vvec_check-len len len-supplied-p))
             (plen      (if (and points-supplied-p (or (listp points) (vectorp points))) (length points)))
             (vvec-type (if vvec-type-supplied-p vvec-type :vvt-points))
             (ap        (+ (if start-supplied-p 4 0) (if end-supplied-p 2 0) (if len-supplied-p 1 0))))
        (cond ((not (equalp vvec-type :vvt-points))          (error "mjr_vvec_normalize-kw-vvt-points: :VVEC-TYPE was provided, and was NOT :VVT-POINTS!"))
              (rfun-supplied-p                               (error "mjr_vvec_normalize-kw-vvt-points: Incompatible with :RFUN"))
              (nfun-supplied-p                               (error "mjr_vvec_normalize-kw-vvt-points: Incompatible with :NFUN"))
              (step-supplied-p                               (error "mjr_vvec_normalize-kw-vvt-points: Incompatible with :STEP"))
              ((and len (zerop len))                         (list :vvec-type :vvt-nil :len 0))
              ((and plen (zerop plen))                       (list :vvec-type :vvt-nil :len 0))
              ((not points-supplied-p)                       (error "mjr_vvec_normalize-kw-vvt-points: Requires :POINTS"))
              ((null plen)                                   (error "mjr_vvec_normalize-kw-vvt-points: :POINTS must be a sequence!"))
              ((< plen 1)                                    (error "mjr_vvec_normalize-kw-vvt-points: :POINTS must contain at least 1 point!"))
              ((and len (< len 1))                           (error "mjr_vvec_normalize-kw-vvt-points: :LEN must be greater than 0!"))
              ((and len (not (<= len plen)))                 (error "mjr_vvec_normalize-kw-vvt-points: :LEN provided, but greater than length of :POINTS"))
              ((and end-supplied-p (< end 0))                (error "mjr_vvec_normalize-kw-vvt-points: :END must be non-negative!"))
              ((and end-supplied-p (not (< end plen)))       (error "mjr_vvec_normalize-kw-vvt-points: :END provided, and was too big!"))
              ((and start-supplied-p (< start 0))            (error "mjr_vvec_normalize-kw-vvt-points: :START must be non-negative!"))
              ((and start-supplied-p (not (< start plen)))   (error "mjr_vvec_normalize-kw-vvt-points: :START provided, and was too big!"))
              ('t                                            (progn
                                                               (case ap
                                                                 (#b000  (psetf start 0                end (1- plen)          len plen)              )
                                                                 (#b001  (psetf start 0                end (1- len)           len len)               )
                                                                 (#b010  (psetf start 0                end end                len (1+ end))          )
                                                                 (#b011  (psetf start (1+ (- end len)) end end                len len)               )
                                                                 (#b100  (psetf start start            end (1- plen)          len (- plen start))    )
                                                                 (#b101  (psetf start start            end (1- (+ len start)) len len)               )
                                                                 (#b110  (psetf start start            end end                len (1+ (- end start))))
                                                                 (#b111  (psetf start start            end end                len len)               ))
                                                               (if (and (< 0 len) (= (- end start) (- len 1))) ;; L>0 && E-S=L-1
                                                                   (list :vvec-type :vvt-points :points points :start start :end end :len len :map-fun map-fun)
                                                                   (error "mjr_vvec_normalize-kw-vvt-points: :START, :END, :LEN are inconsistent!")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-rep (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-REP -- i.e. a virtual vector explicitly described by a repeating value, length, and a function map.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-REP with the following keywords defined: :VVEC-TYPE, :START, :LEN, & :MAP-FUN."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type start end len map-fun)
        (mjr_util_get-kwarg-vals '(:vvec-type :start :end :len :map-fun) kw-vvec 't)
      (let* ((len       (mjr_vvec_check-len len len-supplied-p))
             (vvec-type (if vvec-type-supplied-p vvec-type :vvt-rep)))
        (cond ((and len (zerop len))              (list :vvec-type :vvt-nil :len 0))
              ((not (equalp vvec-type :vvt-rep))  (error "mjr_vvec_normalize-kw-vvt-rep: :VVEC-TYPE provided, but was not :VVT-REP!"))
              (rfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-rep: Incompatible with :RFUN"))
              (nfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-rep: Incompatible with :NFUN"))
              (step-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-rep: Incompatible with :STEP"))
              (points-supplied-p                  (error "mjr_vvec_normalize-kw-vvt-rep: Incompatible with :POINTS"))
              ((not (or start-supplied-p
                        end-supplied-p))          (error "mjr_vvec_normalize-kw-vvt-rep: Requires :START or :END"))
              ((and start-supplied-p
                    end-supplied-p
                    (not (= start end)))          (error "mjr_vvec_normalize-kw-vvt-rep: If both :START and :END are provided, they must be equal"))
              ((not len-supplied-p)               (error "mjr_vvec_normalize-kw-vvt-rep: :VVT-REP requires non-NIL :LEN"))
              ((< len 0)                          (error "mjr_vvec_normalize-kw-vvt-rep: :LEN must be greater than 0!"))
              ('t                                 (list :vvec-type vvec-type :start (or start end) :len len :map-fun map-fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-mitch1 (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-MITCH1 -- i.e. a virtual vector described by a rational, Chebyshev-like sequence and map function.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-MITCH1 with the following keywords defined: :VVEC-TYPE, :START, :END, :LEN, & :MAP-FUN."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type start end len map-fun)
        (mjr_util_get-kwarg-vals '(:vvec-type :start :end :len :map-fun) kw-vvec 't)
      (let ((len       (mjr_vvec_check-len len len-supplied-p))
            (vvec-type (if vvec-type-supplied-p vvec-type :vvt-mitch1)))
        (cond ((not (equalp vvec-type :vvt-mitch1)) (error "mjr_vvec_normalize-kw-vvt-mitch1: :VVEC-TYPE was provided, but was not :VVT-MITCH1!"))
              (rfun-supplied-p                      (error "mjr_vvec_normalize-kw-vvt-mitch1: Incompatible with :RFUN"))
              (nfun-supplied-p                      (error "mjr_vvec_normalize-kw-vvt-mitch1: Incompatible with :NFUN"))
              (points-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-mitch1: Incompatible with :POINTS"))
              (step-supplied-p                      (error "mjr_vvec_normalize-kw-vvt-mitch1: Incompatible with :STEP"))
              ((and len (zerop len))                (list :vvec-type :vvt-nil :len 0))
              ((not start-supplied-p)               (error "mjr_vvec_normalize-kw-vvt-mitch1: Requires :START"))
              ((not end-supplied-p)                 (error "mjr_vvec_normalize-kw-vvt-mitch1: Requires :END"))
              ((not len-supplied-p)                 (error "mjr_vvec_normalize-kw-vvt-mitch1: Requires :LEN"))
              ((not (mjr_cmp_< start end))          (error "mjr_vvec_normalize-kw-vvt-mitch1: :START must be less than :END"))
              ((< len 1)                            (error "mjr_vvec_normalize-kw-vvt-mitch1: :LEN must be greater than 1 for :VVT-MITCH1!"))
              ('t                                   (list :vvec-type vvec-type :start start :end end :len len :map-fun map-fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-cheb (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-CHEB -- i.e. a virtual vector described by a Chebyshev sequence and map function.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-CHEB with the following keywords defined: :VVEC-TYPE, :START, :END, :LEN, & :MAP-FUN."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type start end len map-fun)
        (mjr_util_get-kwarg-vals '(:vvec-type :start :end :len :map-fun) kw-vvec 't)
      (let ((len       (mjr_vvec_check-len len len-supplied-p))
            (vvec-type (if vvec-type-supplied-p vvec-type :vvt-cheb)))
      (cond ((not (equalp vvec-type :vvt-cheb))       (error "mjr_vvec_normalize-kw-vvt-cheb: :VVEC-TYPE was provided, but was not :VVT-CHEB!"))
            (rfun-supplied-p                          (error "mjr_vvec_normalize-kw-vvt-cheb: Incompatible with :RFUN"))
            (nfun-supplied-p                          (error "mjr_vvec_normalize-kw-vvt-cheb: Incompatible with :NFUN"))
            (points-supplied-p                        (error "mjr_vvec_normalize-kw-vvt-cheb: Incompatible with :POINTS"))
            (step-supplied-p                          (error "mjr_vvec_normalize-kw-vvt-cheb: Incompatible with :STEP"))
            ((and len (zerop len))                    (list :vvec-type :vvt-nil :len 0))
            ((not start-supplied-p)                   (error "mjr_vvec_normalize-kw-vvt-cheb: Requires :START"))
            ((not end-supplied-p)                     (error "mjr_vvec_normalize-kw-vvt-cheb: Requires :END"))
            ((not len-supplied-p)                     (error "mjr_vvec_normalize-kw-vvt-cheb: Requires :LEN"))
            ((not (mjr_cmp_< start end))              (error "mjr_vvec_normalize-kw-vvt-cheb: :START must be less than :END"))
            ((< len 1)                                (error "mjr_vvec_normalize-kw-vvt-cheb: :LEN must be greater than 1 for :VVT-CHEB!"))
            ('t                                       (list :vvec-type vvec-type :start start :end end :len len :map-fun map-fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-nil (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-NIL -- i.e. a virtual vector with no elements (len=0)

On success, a :VVR-KW virtual vector will be returned of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN"
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p map-fun-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type len)
        (mjr_util_get-kwarg-vals '(:vvec-type :len) kw-vvec 't)
      (let ((len       (mjr_vvec_check-len len len-supplied-p))
            (vvec-type (if vvec-type-supplied-p vvec-type :vvt-nil)))
        (cond ((not (equalp vvec-type :vvt-nil))  (error "mjr_vvec_normalize-kw-vvt-nil: :VVEC-TYPE was provided, but was not :VVT-NIL!"))
              (rfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :RFUN"))
              (nfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :NFUN"))
              (points-supplied-p                  (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :POINTS"))
              (step-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :STEP"))
              (start-supplied-p                   (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :START"))
              (end-supplied-p                     (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :END"))
              (map-fun-supplied-p                 (error "mjr_vvec_normalize-kw-vvt-nil: Incompatible with :MAP_FUN"))
              ((and len (zerop len))              (list :vvec-type :vvt-nil :len 0))
              ('t                                 (error "mjr_vvec_normalize-kw-vvt-nil: Could not normalize!")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-rfun (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-RFUN -- i.e. a virtual vector described by an initial value, recursive, and map function.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-CHEB with the following keywords defined: :VVEC-TYPE, :START, :LEN, :RFUN, & :MAP-FUN."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type start len map-fun rfun)
        (mjr_util_get-kwarg-vals '(:vvec-type :start :len :map-fun :rfun) kw-vvec 't)
      (let ((len       (mjr_vvec_check-len len len-supplied-p))
            (vvec-type (if vvec-type-supplied-p vvec-type :vvt-rfun))
            (start     (typecase start
                         (vector   (concatenate 'list start))
                         (number   (list start))
                         (otherwise start))))
        (cond ((not (equalp vvec-type :vvt-rfun)) (error "mjr_vvec_normalize-kw-vvt-rfun: :VVEC-TYPE was provided and was not :vvt-rfun!"))
              (points-supplied-p                  (error "mjr_vvec_normalize-kw-vvt-rfun: Incompatible with :POINTS"))
              (end-supplied-p                     (error "mjr_vvec_normalize-kw-vvt-rfun: Incompatible with :END"))
              (step-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-rfun: Incompatible with :STEP"))
              (nfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-rfun: Incompatible with :NFUN"))
              ((and len (zerop len))              (list :vvec-type :vvt-nil :len 0))
              ((not rfun-supplied-p)              (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :RFUN"))
              ((not start-supplied-p)             (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :START"))
              ((not len-supplied-p)               (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :LEN"))
              ((not (listp start))                (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :START to be a vector, list, or number"))
              ((not (every #'numberp start))      (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :START must be a number or sequence of numbers"))
              ((some #'complexp start)            (error "mjr_vvec_normalize-kw-vvt-rfun: Requires :START must not contain complex numbers"))
              ('t                                 (list :vvec-type vvec-type :start start :len len :rfun rfun :map-fun map-fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-nfun (kw-vvec)
  "Normalize a :VVR-KW virtual vector of type :VVT-NIL -- i.e. a virtual vector described by an initial value, no-arg, and map function.

On success, a :VVR-KW virtual vector will be returned:
  * Of type :VVT-NIL with the following keywords defined :VVEC-TYPE & :LEN
  * Of type :VVT-CHEB with the following keywords defined: :vvec-type, :len, :nfun, & :map-fun."
  (mjr_vvec_kw-val-type-check kw-vvec)
  (multiple-value-bind (vvec-type-supplied-p points-supplied-p start-supplied-p end-supplied-p step-supplied-p len-supplied-p nfun-supplied-p rfun-supplied-p)
      (mjr_util_get-kwarg-supplied '(:vvec-type :points :start :end :step :len :nfun :rfun) kw-vvec)
    (multiple-value-bind (vvec-type len map-fun nfun)
        (mjr_util_get-kwarg-vals '(:vvec-type :len :map-fun :nfun) kw-vvec 't)
      (let ((len       (mjr_vvec_check-len len len-supplied-p))
            (vvec-type (if vvec-type-supplied-p vvec-type :vvt-nfun)))
        (cond ((not (equalp vvec-type :vvt-nfun)) (error "mjr_vvec_normalize-kw-vvt-nfun: :VVEC-TYPE was provided and was not :vvt-nfun!"))
              (points-supplied-p                  (error "mjr_vvec_normalize-kw-vvt-nfun: Incompatible with :POINTS"))
              (start-supplied-p                   (error "mjr_vvec_normalize-kw-vvt-nfun: Incompatible with :START"))
              (end-supplied-p                     (error "mjr_vvec_normalize-kw-vvt-nfun: Incompatible with :END"))
              (step-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-nfun: Incompatible with :STEP"))
              (rfun-supplied-p                    (error "mjr_vvec_normalize-kw-vvt-nfun: Incompatible with :RFUN"))
              ((and len (zerop len))              (list :vvec-type :vvt-nil :len 0))
              ((not len-supplied-p)               (error "mjr_vvec_normalize-kw-vvt-nfun: Requires :LEN"))
              ((not nfun-supplied-p)              (error "mjr_vvec_normalize-kw-vvt-nfun: Requires :NFUN"))
              ('t                                 (list :vvec-type vvec-type :len len :nfun nfun :map-fun map-fun)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-kw-vvt-any (kw-vvec)
  "Normalize keywords in the given :vvr-kw virtual vector.

On success, a :vvr-kw vvec will be returned with an appropriate set of keywords.

Note that the empty vector has one canonical representation: '(:vvec-type :vvt-aseq :start nil :end nil :step nil :len 0 :map-fun nil)  -- so a vvec may
go into this function with a :vvec-type that differs from what comes out!!

Uses mjr_vvec_kw-type to determine the vvec-type.  If mjr_vvec_kw-type is inconclusive, then :VVT-ASEQ is assumed"
  (case (mjr_vvec_kw-type kw-vvec)
    (:vvt-nil    (mjr_vvec_normalize-kw-vvt-nil    kw-vvec))
    (:vvt-aseq   (mjr_vvec_normalize-kw-vvt-aseq   kw-vvec))
    (:vvt-points (mjr_vvec_normalize-kw-vvt-points kw-vvec))
    (:vvt-rep    (mjr_vvec_normalize-kw-vvt-rep    kw-vvec))
    (:vvt-cheb   (mjr_vvec_normalize-kw-vvt-cheb   kw-vvec))
    (:vvt-mitch1 (mjr_vvec_normalize-kw-vvt-mitch1 kw-vvec))
    (:vvt-rfun   (mjr_vvec_normalize-kw-vvt-rfun   kw-vvec))
    (:vvt-nfun   (mjr_vvec_normalize-kw-vvt-nfun   kw-vvec))
    (otherwise   (mjr_vvec_normalize-kw-vvt-aseq   kw-vvec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_vvec2fi (vvec)
  "Return a Forward Iterator and a length"
  ;; ;; How one might use just the forward iterator without the length
  ;;
  ;; (loop with vvfi  = (mjr_vvec_vvec2fi aVVEC)
  ;;       for i from 1
  ;;       for v = (funcall vvfi)
  ;;       while v
  ;;       do (format 't "~3d:  ~s~%" i v))
  ;;
  ;; ;; How one might use the forward iterator with the length
  ;;
  ;; (loop with (vvfi vvlen) = (multiple-value-list (mjr_vvec_vvec2fi aVVEC))
  ;;       for i from 0 upto (1- vvlen)
  ;;       for v = (funcall vvfi)
  ;;       do (format 't "~3d:  ~s~%" i v))
  ;;
  ;; ;; How one might use the forward iterator with the length to iterate over PAIRS of adjacent values in a vector
  ;; (loop with (vvfi vvlen) = (multiple-value-list (mjr_vvec_vvec2fi (list :points #(0 1 2 3 4 5 6 7 8 9) :start 2)))
  ;;       for i from -1 upto (- vvlen 2)
  ;;       for v0 = nil then v1
  ;;       for v1 = (funcall vvfi)
  ;;       when v0
  ;;       do (format 't "~3d:  ~s ~s~%" i v0 v1))
  (case (mjr_vvec_representation vvec)
    (:vvr-vec            (if (zerop (length vvec))
                             (values (lambda () nil) 0)
                             (values (let ((i -1)) (lambda () (if (< i (1- (length vvec))) (aref vvec (incf i))))) (length vvec))))
    (:vvr-list           (if (null vvec)
                             (values (lambda () nil) 0)
                             (values (lambda () (pop vvec)) (length vvec)))    )
    (:vvr-int            (cond ((zerop  vvec) (values (lambda () nil) 0))
                               ((minusp vvec) (values (let ((i 0))  (lambda () (if (< i (- vvec))  (incf i)))) (- vvec)))
                               ((plusp  vvec) (values (let ((i -1)) (lambda () (if (< i (1- vvec)) (incf i)))) vvec))))
    (:vvr-kw             (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
                             (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun)
                                                      (mjr_vvec_normalize-kw-vvt-any vvec))
                           (let ((fif (case vvec-type
                                        (:vvt-nil    (lambda () nil))
                                        (:vvt-aseq   (let ((i -1)) (lambda () (and (< i (1- len)) (incf i) (+ start (* i step))))))
                                        (:vvt-points (typecase points
                                                       (vector (let ((i (1- start))) (lambda () (if (< i end) (aref points (incf i))))))
                                                       (list   (let ((i 0)
                                                                     (p (nthcdr start points)))
                                                                 (lambda () (and (< i len) (incf i) (pop p)))))))
                                        (:vvt-rep    (let ((i 0)) (lambda () (and (< i len) (incf i) start))))
                                        (:vvt-cheb   (let ((i 0)
                                                           (f1 (/ (+ start end) 2))
                                                           (f2 (/ (- start end) 2)))
                                                       (lambda () (and (< i len)
                                                                       (+ f1 (* f2 (cos (* pi (/ (- (* 2 (incf i)) 1) (* 2 len))))))))))
                                        (:vvt-mitch1 (let ((i 0)
                                                           (f1 (/ (+ start end) 2))
                                                           (f2 (/ (- start end) 2)))
                                                       (lambda () (and (< i len) (let ((x (* 1 (/ (- (* 2 (incf i)) 1) (* 2 len)))))
                                                                                   (+ f1 (* f2 (1+ (* x x (- (* 4 x) 6))))))))))
                                        (:vvt-rfun   (let ((i -1)
                                                           (rord (length start)))
                                                       (lambda () (if (< i (1- rord))
                                                                      (nth (incf i) start)
                                                                      (if (< i (1- len))
                                                                          (let ((v (apply rfun start)))
                                                                            (incf i)
                                                                            (setf start (append (cdr start) (list v)))
                                                                            v))))))
                                        (:vvt-nfun   (let ((i 0)) (lambda () (and (< i len) (incf i) (funcall nfun)))))
                                        (otherwise   (error "mjr_vvec_vvec2fi: Something very bad happened!")))))
                             (if (zerop len)
                                 (values fif 0)
                                 (if map-fun
                                     (values (lambda () (let ((v (funcall fif))) (if v (funcall map-fun v)))) len)
                                     (values fif len))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_map-filter-reduce (result-type-or-reduce-func vvec &key
                                   reduce-start filter-fun
                                   point-fun point-fun-nil-map
                                   pair-fun pair-fun-nil-map)
  "Combine virtual vector generation, map, filter, and reduce into one common calling sequence tuned for mathematical code

The first argument determines the top level 'operation type' (reduce or map) as well as the result form if the operation type is a map operation.

  * 'list .................. Do a 'map' and return the results as a list
  * 'vector ................ Do a 'map' and return the results as a vector
  * NIL .................... Do a 'map' and throw away the result
  * otherwise .............. Do a 'reduce' and assume the first argument is callable via funcall

If PAIR-FUN is provided, then the operation is carried out over pairs of points instead of points.  i.e. Instead of iterating over sequence elements, we
iterate over adjacent PAIRS of elements.  We call this the 'operation mode' (points or pairs) in this documentation -- we say the operation mode is 'points'
when PAIR-FUN is nil and 'pairs' otherwise.

In mathematical code it is common for map operations to depend on the position of the element in the vector, and so the functions (POINT-FUN, PAIR-FUN) take
as a final argument the index of the point or pair:

  * POINT-FUN takes 2 arguments: point & point index.

  * PAIR-FUN takes 5 arguments: left point, right point, POINT-FUN evaluated on left point, POINT-FUN evaluated on right point, and the zero based index of
    the pair.

The FILTER-FUN function is used to suppress the inclusion of a point or pair in the result of a map or reduce.  Depending of the 'operation mode', the
filter-fun will take different arguments:

  * For points (PAIR-FUN is NIL) the FILTER-FUN takes 3 arguments: point, result of POINT-FUN/PAIR-FUN, and point/pair index.

  * For pairs (PAIR-FUN is non-NIL) the FILTER-FUN takes 6 arguments: left point, right point, POINT-FUN evaluated on left point, POINT-FUN evaluated on right
    point, result of PAIR-FUN, and the zero based index of the pair.

It is also common for the reduce operation to depend upon the index of the point or pair, but the number and type of arguments is always the same regardless
of the mode of operation.

  * RESULT-TYPE-OR-REDUCE-FUNC takes three arguments: accumulated value, result of POINT-FUN/PAIR-FUN, and point/pair index.

The REDUCE-START argument is only used for a 'reduce' operation, and is used as the initial value for the reduction.  Note that in the reduce case the
function RESULT-TYPE-OR-REDUCE-FUNC will be called precisely one time for each sequence value (not :LEN-1 times as might be expected).  This is to cope with
possible function side effects the caller may depend upon, but this means that a appropriate REDUCE-START value must almost always be explicitly provided.
The first call to RESULT-TYPE-OR-REDUCE-FUNC will have as its first argument :REDUCE-START, the first point/pair as the second, and 0 as the third.

When the result of POINT-FUN is nil, POINT-FUN-NIL-MAP will be used instead.  Similarly, when the result of PAIR-FUN is nil, PAIR-FUN-NIL-MAP will be used
instead.  This is to provide a way to insure the result will be a numerical vector even when POINT-FUN/PAIR-FUN is unable to return a valid value.  The most
important use case is to provide a 'default value' for POINT-FUN/PAIR-FUN when it is called outside of its mathematical domain.

Much like the calling strategy for RESULT-TYPE-OR-REDUCE-FUNC in reduce operations, the POINT-FUN/PAIR-FUN functions will ALWAYS be called once per
point/pair.  Just as with RESULT-TYPE-OR-REDUCE-FUNC, this is to insure any side effects will occur.

If POINT-FUN is NIL (as in a NIL was passed to this function), then the behavior will be as if #'IDENTITY were passed."

  (let ((op-type (cond ((equalp result-type-or-reduce-func 'vector) :opt-map-vector)
                       ((null result-type-or-reduce-func)           :opt-map-nil)
                       ((equalp result-type-or-reduce-func 'list)   :opt-map-list)
                       ('t                                          :opt-reduce)))
        (op-mode (cond (pair-fun                                    :opm-pairs)
                       ('t                                          :opm-points))))
    (cond
      ((and filter-fun
            (equalp op-type :opt-map-nil))      (error "filter-fun useless for nil result map operations"))
      ((and filter-fun
            (equalp op-type :opt-map-vector))   (error "filter-fun is incompatible with a 'vector result map operation (use 'list) "))
      ((and (equalp op-type :opt-reduce)
            (equalp op-mode :opm-pairs)
            (not pair-fun))                     (error "pair-fun is required for a reduce operation on pairs")))

    (multiple-value-bind (vvfi vvlen)
        (mjr_vvec_vvec2fi vvec)
      (case op-mode
        (:opm-points (case op-type
                       (:opt-map-vector (let ((resv (make-array vvlen)))
                                          (loop for i from 0 upto (1- vvlen)
                                                for v = (funcall vvfi)
                                                for fv = (if point-fun (or (funcall point-fun v i) point-fun-nil-map) v)
                                                do (setf (aref resv i) fv))
                                          resv))
                       (:opt-map-nil    (loop for i from 1 upto vvlen
                                              for v = (funcall vvfi)
                                              for fv = (if point-fun (or (funcall point-fun v i) point-fun-nil-map) v)))
                       (:opt-map-list   (loop for i from 0 upto (1- vvlen)
                                              for v = (funcall vvfi)
                                              for fv = (if point-fun (or (funcall point-fun v i) point-fun-nil-map) v)
                                              when (or (not filter-fun) (funcall filter-fun v fv i))
                                              collect fv))
                       (:opt-reduce     (let ((resv reduce-start))
                                          (loop for i from 0 upto (1- vvlen)
                                                for v = (funcall vvfi)
                                                for fv = (if point-fun (or (funcall point-fun v i) point-fun-nil-map) v)
                                                when (or (not filter-fun) (funcall filter-fun v fv i))
                                                do (setf resv (funcall result-type-or-reduce-func resv fv i)))
                                          resv))))
        (:opm-pairs  (case op-type
                       (:opt-map-vector (let ((resv (make-array (1- vvlen))))
                                          (loop for i from -1 upto (- vvlen 2)
                                                for v0 = nil then v1
                                                for v1 = (funcall vvfi)
                                                for fv0 = nil then fv1
                                                for fv1 = (if point-fun (or (funcall point-fun v1 (1+ i)) point-fun-nil-map) v1)
                                                when v0
                                                do (setf (aref resv i) (or (funcall pair-fun v0 v1 fv0 fv1 i) pair-fun-nil-map)))
                                          resv))
                       (:opt-map-nil    (loop for i from -1 upto (- vvlen 2)
                                              for v0 = nil then v1
                                              for v1 = (funcall vvfi)
                                              for fv0 = nil then fv1
                                              for fv1 = (if point-fun (or (funcall point-fun v1 (1+ i)) point-fun-nil-map) v1)
                                              when v0
                                              do (funcall pair-fun v0 v1 i)))
                       (:opt-map-list   (loop for i from -1 upto (- vvlen 2)
                                              for v0 = nil then v1
                                              for v1 = (funcall vvfi)
                                              for fv0 = nil then fv1
                                              for fv1 = (if point-fun (or (funcall point-fun v1 (1+ i)) point-fun-nil-map) v1)
                                              for fp = nil then (or (funcall pair-fun v0 v1 fv0 fv1 i) pair-fun-nil-map)
                                              when (and v0 (or (not filter-fun) (funcall filter-fun v0 v1 fv0 fv1 fp i)))
                                              collect fp))
                       (:opt-reduce     (let ((resv reduce-start))
                                          (loop for i from -1 upto (- vvlen 2)
                                                for v0 = nil then v1
                                                for v1 = (funcall vvfi)
                                                for fv0 = nil then fv1
                                                for fv1 = (if point-fun (or (funcall point-fun v1 i) point-fun-nil-map) v1)
                                                for fp = nil then (or (funcall pair-fun v0 v1 fv0 fv1 i) pair-fun-nil-map)
                                                when (and v0 (or (not filter-fun) (funcall filter-fun v0 v1 fv0 fv1 fp i)))
                                                do (setf resv (funcall result-type-or-reduce-func resv fp i)))
                                          resv))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_to-vec (vvec)
  "Convert VVEC to a LISP vector.  Returns vvec (not a copy) if it is already a vector.  May return NIL if we can't figure out what to do."
  (case (mjr_vvec_representation vvec)
    (:vvr-kw   (mjr_vvec_map-filter-reduce 'vector vvec))
    (:vvr-vec  vvec)
    (:vvr-list (concatenate 'vector vvec))
    (:vvr-int  (mjr_vvec_map-filter-reduce 'vector vvec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_to-list (vvec)
  "Convert VVEC to a LISP list. Returns vvec (not a copy) if it is already a list of numbers.  May return NIL if we can't figure out what to do."
  (case (mjr_vvec_representation vvec)
    (:vvr-kw       (mjr_vvec_map-filter-reduce 'list vvec))
    (:vvr-vec      (concatenate 'list vvec))
    (:vvr-list     vvec)
    (:vvr-int      (mjr_vvec_map-filter-reduce 'list vvec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_gen-1sim (result-type vvec)
  "Generate and return, as a list or vector, a the sequence of INTERVALS described by the points of the virtual vector.

An interval is a LIST containing an the start and end point of a mathematical interval."
  (if (not (or (eq result-type 'list)
               (eq result-type 'vector))) (error "mjr_vvec_gen-1sim: RESULT-TYPE must be 'VECTOR or 'LIST!"))
  (mjr_vvec_map-filter-reduce result-type vvec :pair-fun (lambda (x0 x1 fx0 fx1 i) (declare (ignore fx0 fx1 i)) (list x0 x1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_map-sum (vvec &key (initial-value 0) pair-fun point-fun filter-fun)
  "Evaluate a function on a sequence and sum the resulting non-NIL values together.

Examples (all ways to add up the same three numbers):
   > (mjr_vvec_map-sum (list :start 4 :end 2 :step -1 :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :start 2 :end 4 :step 1  :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :start 2 :end 4 :len 3   :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :start 4 :end 2 :len 3   :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :start 2 :end 4          :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :start 4 :end 2          :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :points '(2 3 4)         :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :points #(2 3 4)         :map-fun (lambda (x) (* x x))))
   > (mjr_vvec_map-sum (list :points #(4 9 16)))
   > (mjr_vvec_map-sum (list :points '(4 9 16)))
   > (mjr_vvec_map-sum (list :start 2 :end 4 :step 1) :point-fun (lambda (x i) (declare (ignore i)) (* x x)))
Example of adding the odd numbers from 1 up to 10:
   > (mjr_vvec_map-sum (list :start 1 :end 9 :step 2))
   > (mjr_vvec_map-sum (list :start 1 :end 10 :map-fun  (lambda (i) (if (oddp i) i 0))))
   > (mjr_vvec_map-sum (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (and (oddp v) v)))
   > (mjr_vvec_map-sum (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (if (oddp v) v 0)))
   > (mjr_vvec_map-sum (list :start 1 :end 10) :filter-fun (lambda (v fv i) (declare (ignore fv i)) (oddp v)))"
  (mjr_vvec_map-filter-reduce (lambda (a b i) (declare (ignore i)) (+ a b))
                              vvec
                              :reduce-start initial-value
                              :pair-fun-nil-map 0 :point-fun-nil-map 0
                              :pair-fun pair-fun :point-fun point-fun :filter-fun filter-fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_map-prod (vvec &key (initial-value 1) pair-fun point-fun filter-fun)
  "Evaluate a function on a sequence and form the product of the resulting non-NIL values.  The sequence may be an arithmetic sequence specified with START,
END, STEP, & LEN) or an arbitrary sequence specified by POINTS (a vector or list).  See the function MJR_VVEC_ASEQ_ARG_NORMALIZE for details of the arguments
used to specify the sequence.  See the function MJR_VVEC_MAP-SUM for examples of valid argument combinations. "
  (mjr_vvec_map-filter-reduce (lambda (a b i) (declare (ignore i)) (* a b))
                              vvec
                              :reduce-start initial-value
                              :pair-fun-nil-map 1 :point-fun-nil-map 1
                              :pair-fun pair-fun  :point-fun point-fun :filter-fun filter-fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_map-maxi (vvec &key filter-fun point-fun point-fun-nil-map pair-fun pair-fun-nil-map)
  "Evaluate a function on a sequence and return the index and function value of the maximum of the resulting non-NIL values (nil may be mapped to something
else via seq-fun-nil-map)."
  (let ((maxv nil)
        (maxi nil))
    (mjr_vvec_map-filter-reduce (lambda (a v i)
                                  (declare (ignore a))
                                  (if (and v (or (null maxv) (mjr_cmp_> v maxv)))
                                      (setq maxv v
                                            maxi i)))
                                vvec
                                :reduce-start nil :filter-fun filter-fun
                                :point-fun-nil-map point-fun-nil-map  :pair-fun-nil-map pair-fun-nil-map
                                :point-fun point-fun                  :pair-fun pair-fun)
    (values maxi maxv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_map-mini (vvec &key filter-fun point-fun point-fun-nil-map pair-fun pair-fun-nil-map)
  "Evaluate a function on a sequence and return the index, seq value, and function value of the minimum of the resulting non-NIL values (nil may be mapped to
something else via seq-fun-nil-map).  The sequence may be an arithmetic sequence specified with START, END, STEP, & LEN) or an arbitrary sequence specified by
POINTS (a vector or list).  See the function MJR_VVEC_ASEQ_ARG_NORMALIZE for details of the arguments used to specify the sequence.  See the function
MJR_VVEC_MAP-SUM for examples of valid argument combinations."
  (let ((maxv nil)
        (maxi nil))
    (mjr_vvec_map-filter-reduce (lambda (a v i)
                                  (declare (ignore a))
                                  (if (and v (or (null maxv) (mjr_cmp_< v maxv)))
                                      (setq maxv v
                                            maxi i)))
                                vvec
                                :reduce-start nil :filter-fun filter-fun
                                :point-fun-nil-map point-fun-nil-map  :pair-fun-nil-map pair-fun-nil-map
                                :point-fun point-fun                  :pair-fun pair-fun)
    (values maxi maxv)))
