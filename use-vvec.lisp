;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-vvec.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Virtual vectors.@EOL
;; @std       Common Lisp
;; @see       tst-vvec.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2008,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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

           #:mjr_vvec_normalize-vvt-aseq
           #:mjr_vvec_normalize-vvt-points
           #:mjr_vvec_normalize-vvt-mitch1
           #:mjr_vvec_normalize-vvt-cheb
           #:mjr_vvec_normalize-vvt-nfun
           #:mjr_vvec_normalize-vvt-rfun
           #:mjr_vvec_normalize-all

           #:mjr_vvec_get-vvec-rep
           #:mjr_vvec_get-vvec-type

           #:mjr_vvec_convert-rep

           ;; #:mjr_vvec_vvec2rai -- Not implemented.  "Random Access Iterator"
           #:mjr_vvec_vvec2fi

           #:mjr_vvec_map-filter-reduce

           #:mjr_vvec_gen-0sim
           #:mjr_vvec_gen-1sim
           #:mjr_vvec_to-vec-maybe

           #:mjr_vvec_map-sum
           #:mjr_vvec_map-prod
           #:mjr_vvec_map-maxi
           #:mjr_vvec_map-mini
           ))

(in-package :MJR_VVEC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_help ()
  "Help for MJR_VVEC: Virtual VECtor
 
   A 'Virtual Vector' is a numeric vector that may be specified 'virtually' via a list of keyword arguments: 

      - :VVEC-TYPE, :POINTS, :START, :END, :STEP, :LEN, :MAP-FUN, :NFUN, & :RFUN

   If :LEN is a floating point number near an integer, then it is rounded to that integer. If :LEN is a sequence (list or vector), then the length of the
   sequence is used.  Most functions here will directly accept keyword arguments or a list containing keyword arguments.  As an additional convenience
   feature, VVECs may also be described by a single integer or a sequence (list or vector).

   The meanings of the keyword arguments, and other special cases, are defined in the fallowing table:

     |-------------+-----------+------------------+---------------------------------------------+-----------------------------------------------|
     | Type        | Rep       | Description      | Parameters                                  | Sequence                                      |
     |-------------+-----------+------------------+---------------------------------------------+-----------------------------------------------|
     | :VVT-ASEQ   | :vvr-kw   | Arithmetic Seq   | :START # :END # :LEN # :STEP #              | n_i = START+i*STEP                            |
     | :VVT-ASEQ   | :vvr-int  | Zero based Count | positive integer N (Like :LEN N)            | n_i = i                                       |
     | :VVT-ASEQ   | :vvr-int  | One based Count  | Negative integer N (Like :START 1 :LEN N)   | n_i = i+1                                     |
     | :VVT-REP    | :vvr-kw   | Repeat           | :START # :LEN # :VVEC-TYPE :VVT-REP         | n_i = START                                   |
     | :VVT-CHEB   | :vvr-kw   | Chebichev        | :START # :END :LEN # :VVEC-TYPE :VVT-CHEB   | Chebichev interpolation points                |
     | :VVT-MITCH1 | :vvr-kw   | Chebichev-like   | :START # :END :LEN # :VVEC-TYPE :VVT-MITCH1 | Chebichev-like interpolation points           |
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

   The final step of the vector generation process is to map the elements via MAP-FUN -- MAP-FUN may be provided for any :VVEC-TYPE.  Note that map-fun is
   called for each value of the sequence, even for :VVT-REP, to account for possible side effects in MAP-FUN."
  (documentation 'mjr_vvec_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_get-vvec-rep (vvec)
  "Return a symbol for the representation of the vvec (NIL if the representation is unknown)

   As discussed in mjr_vvec_help, VVECs may be described in four ways:

     * :vvr-kw ..... A list vvec keywords and values ... See MJR_VVEC_KW-NORMALIZE for details.  (canonical form)
     * :vvr-int .... A positive integer, $n$ ........... The vector is $[0,1,...,n-1]$
     * :vvr-vec .... An vector of numbers .............. The vector contents define the vector
     * :vvr-list ... An list of numbers ................ The list contents define the vector

   NOTE: This function doesn't check syntax.  For example, if VVEC is a vector, then :vvr-vec is returned -- we don't check to make sure that the vector
   contains only numbers.  As another example, we don't check to make sure keyword arguments are used correctly."
  (typecase vvec
    (vector   (if (< 0 (length vvec))
                  :vvr-vec))
    (integer  :vvr-int)
    (list     (if (and (> 22 (length vvec))
                       (intersection '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec))
                  :vvr-kw
                  (if vvec
                      :vvr-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_get-vvec-type (vvec)
  "Return a symbol for the type (as in :vvec-type) of the vvec (NIL if the type is unknown).

The sequence type is one of the following:
    * :VVT-ASEQ   -- arithmetic sequence
                       Assumed if :STEP is non-NIL or :VVEC-TYPE is :VVT-ASEQ
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
  (multiple-value-bind (vvec-type points step nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :step :nfun :rfun) vvec)
    (cond (vvec-type  vvec-type)
          (points     :vvt-points)
          (nfun       :vvt-nfun)
          (rfun       :vvt-rfun)
          (step       :vvt-aseq)
          ('t         :vvt-aseq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_check-len (len)
  "Convert LEN to integer.  If LEN is valid, an integer is returned, otherwise an error is generated."
  (typecase len
    (integer    len)
    (float      (round len))
    (string     (error "mjr_vvec_check-len: :LEN was a string!"))
    (vector     (length len))
    (null       nil)
    (list       (length len))
    (otherwise  (error "mjr_vvec_check-len: :LEN was an invalid object"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-aseq (vvec)
  "Normalize a group of arguments intended to describe an finite arithmetic sequence.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :start, :end, :step, :len, & :map-fun.  Note that :end may not be the
value that was given -- it may be corrected so that it is consistent with the other values.  Still, :end should NEVER be used for iteration.

An arithmetic sequence may be fully specified by any tree of the following: first element (START), last element (END), the number of terms (LEN), and the
delta between each pair of terms (STEP).  This function is designed to normalize such parameters describing an arithmetic sequence. Intelligent defaults are
provided for cases when fewer than three values are provided.

How the arguments are processed until ERROR or CHECK repeatedly applying the following, 'switch' statement:

  |----+----------+-------+-------+-------+-------+---------------------+----------------------------------+----------------------------------------------|
  |    | case     | START | END   | STEP  | LEN   | Condition           | Action.                          | Action Sequence                              |
  |----+----------+-------+-------+-------+-------+---------------------+----------------------------------+----------------------------------------------|
  | 1* | case 0.0 | N/A   | N/A   | N/A   | 0     |                     | START, END, STEP <- NIL          | EMPTY VECTOR                                 |
  | 2* | case 0.1 | KNOWN | KNOWN | KNOWN | N/A   | START>END & STEP<=0 | LEN   <- NIL                     | case 0.0 => EMPTY VECTOR                     |
  | 3* | case 0.2 | KNOWN | KNOWN | KNOWN | N/A   | START<END & STEP>=0 | LEN   <- NIL                     | case 0.0 => EMPTY VECTOR                     |
  |  4 | case 1.0 | KNOWN | KNOWN | KNOWN | KNOWN |                     | Checked for consistency.         | CHECK                                        |
  |  5 | case 2.1 |       | KNOWN | KNOWN | KNOWN |                     | START <- END-(LEN-1)*STEP        | case 1.0  => CHECK                           |
  |  6 | case 2.2 | KNOWN |       | KNOWN | KNOWN |                     | END   <- START+(LEN-1)*STEP      | case 1.0  => CHECK                           |
  |  7 | case 2.3 | KNOWN | KNOWN |       | KNOWN |                     | STEP  <- (END-START)/(LEN-1)     | case 1.0  => CHECK                           |
  |  8 | case 2.4 | KNOWN | KNOWN | KNOWN |       |                     | LEN   <- (END-START)/STEP+1      | case 1.0  => CHECK                           |
  |  9 | case 3.1 |       |       | KNOWN | KNOWN |                     | START <- 0                       | case 2.2  => case 1.0  => CHECK              |
  | 10 | case 3.2 |       | KNOWN |       | KNOWN |                     | START <- 0                       | case 2.3  => case 1.0  => CHECK              |
  | 11 | case 3.3 |       | KNOWN | KNOWN |       |                     | START <- 0                       | case 2.4  => case 1.0  => CHECK              |
  | 12 | case 3.4 | KNOWN |       |       | KNOWN |                     | STEP  <- 1                       | case 2.2  => case 1.0  => CHECK              |
  | 13 | case 3.5 | KNOWN |       | KNOWN |       |                     | ERROR                            | ERROR                                        |
  | 14 | case 3.6 | KNOWN | KNOWN |       |       |                     | LEN   <- 1+abs(round(end-start)) | case 2.3  => case 1.0  => CHECK              |
  | 15 | case 4.1 |       |       |       | KNOWN |                     | START <- 0                       | case 3.4  => case 2.2  => case 1.0  => CHECK |
  | 16 | case 4.2 |       |       | KNOWN |       |                     | ERROR                            | ERROR                                        |
  | 17 | case 4.3 |       | KNOWN |       |       |                     | START <- 0                       | case 3.6  => case 2.4  => case 1.0  => CHECK |
  | 18 | case 4.4 | KNOWN |       |       |       |                     | ERROR                            | ERROR                                        |
  | 19 | case 4.5 |       |       |       |       |                     | ERROR                            | ERROR                                        |
  |----+----------+-------+-------+-------+-------+---------------------+----------------------------------+----------------------------------------------|

 * -> TODO -- want to add ability to have empty vectors some day.

Note: 'CHECK' in the table above means 'success'

Once the arguments are normalized, an arithmetic sequence can be generated.  We ALWAYS generate the Ith (zero indexed) element with $START+i*STEP$ -- i.e. the
first element is $START+0*STEP=START$.  For only sequential access we could successively add STEP to an initial value; however, we wish to use the same
formula for random and sequential access in order to insure the same results even in the face of round-off error.  We never use END to generate or check for
termination because we wish to avoid length 'twitter' due to round-off error.  An example of a loop that might generate the sequence is as follows:

                (multiple-value-bind (start step len)
                    (mjr_vvec_normalize-all '(:start :step :len) :start 1 :end 10 :len 10)
                  (loop for i from 0 to (1- len)
                        for x = (+ start (* i step))
                        collect x))

This function uses MJR_CMP_!=0 against :START, :END, :STEP, and/or :LEN."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let* ((len       (mjr_vvec_check-len len))
           (ap        (+ (if start 8 0) (if end 4 0) (if step 2 0) (if len 1 0)))
           (rngR      (and start end (- end start)))
           (rngL      (and step len (* step (1- len))))
           (step=0    (and step (mjr_cmp_=0 step 1e-6))) ;; Set this small so we can use tiny steps
           (len=1     (and len (mjr_cmp_= 1 len)))
           (rngR=0    (and rngR (mjr_cmp_=0 rngR)))
           (vvec-type (or vvec-type :vvt-aseq)))
      ;;(format 't "~b ~10s ~10s ~10s ~10s ~%" ap start end step len)
      (cond ((not (equalp vvec-type :vvt-aseq)) (error "mjr_vvec_normalize-vvt-aseq: :VVEC-TYPE must be :VVT-ASEQ!"))
            (points                             (error "mjr_vvec_normalize-vvt-aseq: Incompatible with :POINTS"))
            (rfun                               (error "mjr_vvec_normalize-vvt-aseq: Incompatible with :RFUN"))
            (nfun                               (error "mjr_vvec_normalize-vvt-aseq: Incompatible with :NFUN"))
            ((and len (mjr_cmp_< len 0))        (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: len<0!"))
            ((and len=1 rngR (not rngR=0))      (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: len=0 but start!=end!"))
            ((and len=1  (not step))            (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step 0                 :len len                                :map-fun map-fun)))
            ((and step=0 (not len))             (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step step              :len 1                                  :map-fun map-fun)))
            ((and rngR=0 (not step))            (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step 0                 :len len                                :map-fun map-fun)))
            ((and rngR=0 (not len))             (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step step              :len 1                                  :map-fun map-fun)))
            ((and len=1  (not step=0))          (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: len=1, but step!=0!"))
            ((and step=0 (not len=1))           (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: step=0, but len!=1!"))
            ((and rngR=0 (not step=0))          (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: start=end, but step!=0!"))
            ((and rngR=0 (not len=1))           (error "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters: start=end, but len!=1!"))
            ((= ap #b1111)                      (progn (if (mjr_cmp_!= rngL rngR)
                                                           (warn "mjr_vvec_normalize-vvt-aseq: Inconsistent arithmetic sequence parameters (:end fixed)!"))
                                                       (list :vvec-type vvec-type :start start :end (+ (* (1- len) step) start) :step step :len len :map-fun map-fun)))
            ((= ap #b0111)                      (mjr_vvec_normalize-vvt-aseq (list :start (- end rngL) :end end            :step step              :len len                                :map-fun map-fun)))
            ((= ap #b1011)                      (mjr_vvec_normalize-vvt-aseq (list :start start        :end (+ start rngL) :step step              :len len                                :map-fun map-fun)))
            ((= ap #b1101)                      (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step (/ rngR (1- len)) :len len                                :map-fun map-fun)))
            ((= ap #b1110)                      (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step step              :len (1+ (/ rngR step))                 :map-fun map-fun)))
            ((= ap #b1001)                      (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step 1                 :len len                                :map-fun map-fun)))
            ((= ap #b1100)                      (mjr_vvec_normalize-vvt-aseq (list :start start        :end end            :step step              :len (+ (abs (round (- end start))) 1)  :map-fun map-fun)))
            ((not start)                        (mjr_vvec_normalize-vvt-aseq (list :start 0            :end end            :step step              :len len                :map-fun map-fun)))
            ('t                                 (error "mjr_vvec_normalize-vvt-aseq: Unable to compute missing arithmetic sequence parameters!"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-points (vvec)
  "Normalize a group of arguments intended to describe an vvec described by grid points.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :points, :start, :end, :len, & :map-fun.  Note that :start, :end, & :len
will be consistent."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let* ((len       (mjr_vvec_check-len len))
           (plen      (if (or (listp points) (vectorp points)) (length points)))
           (vvec-type (or vvec-type :vvt-points))
           (ap        (+ (if start 4 0) (if end 2 0) (if len 1 0))))
      (cond ((not (equalp vvec-type :vvt-points))        (error "mjr_vvec_normalize-vvt-points: :VVEC-TYPE was provided, and was NOT :VVT-POINTS!"))
            (rfun                                        (error "mjr_vvec_normalize-vvt-points: Incompatible with :RFUN"))
            (nfun                                        (error "mjr_vvec_normalize-vvt-points: Incompatible with :NFUN"))
            (step                                        (error "mjr_vvec_normalize-vvt-points: Incompatible with :STEP"))
            ((null points)                               (error "mjr_vvec_normalize-vvt-points: Requires :POINTS"))
            ((null plen)                                 (error "mjr_vvec_normalize-vvt-points: :POINTS must be a sequence!"))
            ((< plen 1)                                  (error "mjr_vvec_normalize-vvt-points: :POINTS must contain at least 1 point!"))
            ((and len (< len 1))                         (error "mjr_vvec_normalize-vvt-points: :LEN must be greater than 0!"))
            ((and len (not (<= len plen)))               (error "mjr_vvec_normalize-vvt-points: :LEN provided, but greater than length of :POINTS"))
            ((and end (< end 0))                         (error "mjr_vvec_normalize-vvt-points: :END must be non-negative!"))
            ((and end (not (< end plen)))                (error "mjr_vvec_normalize-vvt-points: :END provided, and was too big!"))
            ((and start (< start 0))                     (error "mjr_vvec_normalize-vvt-points: :START must be non-negative!"))
            ((and start (not (< start plen)))            (error "mjr_vvec_normalize-vvt-points: :START provided, and was too big!"))
            ('t                                          (progn
                                                           (case ap
                                                             (#b000  (psetf start 0                end (1- plen)          len plen)              )               
                                                             (#b001  (psetf start 0                end (1- len)           len len)               )               
                                                             (#b010  (psetf start 0                end end                len (1+ end))               )               
                                                             (#b011  (psetf start (1+ (- end len)) end end                len len)               )               
                                                             (#b100  (psetf start start            end (1- plen)          len (- plen start))    )      
                                                             (#b101  (psetf start start            end (1- (+ len start)) len len)               )               
                                                             (#b110  (psetf start start            end end                len (1+ (- end start))))
                                                             (#b111  (psetf start start            end end                len len)               ))
                                                           (if (and (< 0 len) (= (- end start) (- len 1))) ;; L>0 && E-S=L-1
                                                               (list :vvec-type :vvt-points :points points :start start :end end :len len :map-fun map-fun)
                                                               (error "mjr_vvec_normalize-vvt-points: :START, :END, :LEN are inconsistent!"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-all-rep (vvec)
  "Normalize a group of arguments intended to describe an vvec described by grid points

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :start, :len, & :map-fun."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let* ((len       (mjr_vvec_check-len len))
           (vvec-type (or vvec-type :vvt-rep)))
      (cond ((not (equalp vvec-type :vvt-rep))   (error "mjr_vvec_normalize-all-rep: :VVEC-TYPE provided, but was not :VVT-REP!"))
            (rfun                                (error "mjr_vvec_normalize-all-rep: Incompatible with :RFUN"))
            (nfun                                (error "mjr_vvec_normalize-all-rep: Incompatible with :NFUN"))
            ((not (or start end))                (error "mjr_vvec_normalize-all-rep: Requires :START or :END"))
            ((and start end (not (= start end))) (error "mjr_vvec_normalize-all-rep: If both :START and :END are provided, they must be equal"))
            (step                                (error "mjr_vvec_normalize-all-rep: Incompatible with :STEP"))
            (points                              (error "mjr_vvec_normalize-all-rep: Incompatible with :POINTS"))
            ((null len)                          (error "mjr_vvec_normalize-all-rep: :VVT-REP requires non-NIL :LEN"))
            ((< len 1)                           (error "mjr_vvec_normalize-all-rep: :LEN must be greater than 0!"))
            ('t                                  (list :vvec-type vvec-type :start (or start end) :len len :map-fun map-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-mitch1 (vvec)
  "Normalize a group of arguments intended to describe an vvec described by the mitch1 interpolation scheme.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :start, :end, :len, & :map-fun."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let ((len       (mjr_vvec_check-len len))
          (vvec-type (or vvec-type :vvt-mitch1)))
      (cond ((not (equalp vvec-type :vvt-mitch1))              (error "mjr_vvec_normalize-vvt-mitch1: :VVEC-TYPE was provided, but was not :VVT-MITCH1!"))
            (rfun                                              (error "mjr_vvec_normalize-vvt-mitch1: Incompatible with :RFUN"))
            (nfun                                              (error "mjr_vvec_normalize-vvt-mitch1: Incompatible with :NFUN"))
            (points                                            (error "mjr_vvec_normalize-vvt-mitch1: Incompatible with :POINTS"))
            (step                                              (error "mjr_vvec_normalize-vvt-mitch1: Incompatible with :STEP"))
            ((null start)                                      (error "mjr_vvec_normalize-vvt-mitch1: Requires :START"))
            ((null end)                                        (error "mjr_vvec_normalize-vvt-mitch1: Requires :END"))
            ((null len)                                        (error "mjr_vvec_normalize-vvt-mitch1: Requires :LEN"))
            ((not (mjr_cmp_< start end))                       (error "mjr_vvec_normalize-vvt-mitch1: :START must be less than :END"))
            ((< len 1)                                         (error "mjr_vvec_normalize-vvt-mitch1: :LEN must be greater than 1 for :VVT-MITCH1!"))
            ('t                                                (list :vvec-type vvec-type :start start :end end :len len :map-fun map-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-cheb (vvec)
  "Normalize a group of arguments intended to describe an vvec described by the cheb interpolation scheme.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :start, :end, :len, & :map-fun."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let ((vvec-type (or vvec-type :vvt-mitch1)))
      (cond ((not (equalp vvec-type :vvt-cheb))                (error "mjr_vvec_normalize-vvt-cheb: :VVEC-TYPE was provided, but was not :VVT-CHEB!"))
            (rfun                                              (error "mjr_vvec_normalize-vvt-cheb: Incompatible with :RFUN"))
            (nfun                                              (error "mjr_vvec_normalize-vvt-cheb: Incompatible with :NFUN"))
            (points                                            (error "mjr_vvec_normalize-vvt-cheb: Incompatible with :POINTS"))
            (step                                              (error "mjr_vvec_normalize-vvt-cheb: Incompatible with :STEP"))
            ((null start)                                      (error "mjr_vvec_normalize-vvt-cheb: Requires :START"))
            ((null end)                                        (error "mjr_vvec_normalize-vvt-cheb: Requires :END"))
            ((null len)                                        (error "mjr_vvec_normalize-vvt-cheb: Requires :LEN"))
            ((not (mjr_cmp_< start end))                       (error "mjr_vvec_normalize-vvt-cheb: :START must be less than :END"))
            ((< len 1)                                         (error "mjr_vvec_normalize-vvt-cheb: :LEN must be greater than 1 for :VVT-CHEB!"))
            ('t                                                (list :vvec-type vvec-type :start start :end end :len len :map-fun map-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-rfun (vvec)
  "Normalize a group of arguments intended to describe an vvec described by a recursive function.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :start, :len, :rfun, & :map-fun."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let ((len       (mjr_vvec_check-len len))
          (start     (typecase start
                       (vector   (concatenate 'list start))
                       (number   (list start))
                       (otherwise start)))
          (vvec-type (or vvec-type :vvt-rfun)))
      (cond ((not (equalp vvec-type :vvt-rfun))     (error "mjr_vvec_normalize-vvt-rfun: :VVEC-TYPE was provided and was not :vvt-rfun!"))
            (points                                 (error "mjr_vvec_normalize-vvt-rfun: Incompatible with :POINTS"))
            ((not start)                            (error "mjr_vvec_normalize-vvt-rfun: Requires :START"))
            (end                                    (error "mjr_vvec_normalize-vvt-rfun: Incompatible with :END"))
            (step                                   (error "mjr_vvec_normalize-vvt-rfun: Incompatible with :STEP"))
            ((not len)                              (error "mjr_vvec_normalize-vvt-rfun: Requires :LEN"))
            (nfun                                   (error "mjr_vvec_normalize-vvt-rfun: Incompatible with :NFUN"))
            ((not (listp start))                    (error "mjr_vvec_normalize-vvt-rfun: Requires :START to be a vector, list, or number"))
            ((not (every #'numberp start))          (error "mjr_vvec_normalize-vvt-rfun: Requires :START must be a number or sequence of numbers"))
            ((some #'complexp start)                (error "mjr_vvec_normalize-vvt-rfun: Requires :START must not contain complex numbers"))
            ('t                                     (list :vvec-type vvec-type :start start :len len :rfun rfun :map-fun map-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-vvt-nfun (vvec)
  "Normalize a group of arguments intended to describe an vvec described by a no-arg function.

On success, a :vvr-kw vvec will be returned with the following keywords: :vvec-type, :len, :nfun, & :map-fun."
  (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
      (mjr_util_get-kwarg-vals '( :vvec-type :points :start :end :step :len :map-fun :nfun :rfun) vvec 't)
    (let ((len       (mjr_vvec_check-len len))
          (vvec-type (or vvec-type :vvt-nfun)))
      (cond ((not (equalp vvec-type :vvt-nfun))     (error "mjr_vvec_normalize-vvt-nfun: :VVEC-TYPE was provided and was not :vvt-nfun!"))
            (points                                 (error "mjr_vvec_normalize-vvt-nfun: Incompatible with :POINTS"))
            (start                                  (error "mjr_vvec_normalize-vvt-nfun: Incompatible with :START"))
            (end                                    (error "mjr_vvec_normalize-vvt-nfun: Incompatible with :END"))
            (step                                   (error "mjr_vvec_normalize-vvt-nfun: Incompatible with :STEP"))
            ((not len)                              (error "mjr_vvec_normalize-vvt-nfun: Requires :LEN"))
            (rfun                                   (error "mjr_vvec_normalize-vvt-nfun: Incompatible with :RFUN"))
            ('t                                     (list :vvec-type vvec-type :len len :nfun nfun :map-fun map-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_normalize-all (vvec)
  "Normalize arguments describing a virtual vector.  

On success, a :vvr-kw vvec will be returned with an appropriate set of keywords.

Uses mjr_vvec_get-vvec-type to determine the vvec-type.  If mjr_vvec_get-vvec-type is inconclusive, then :VVT-ASEQ is assumed"
  (case (mjr_vvec_get-vvec-type vvec)
    (:vvt-aseq   (mjr_vvec_normalize-vvt-aseq   vvec))
    (:vvt-points (mjr_vvec_normalize-vvt-points vvec))
    (:vvt-rep    (mjr_vvec_normalize-all-rep    vvec))
    (:vvt-cheb   (mjr_vvec_normalize-vvt-cheb   vvec))
    (:vvt-mitch1 (mjr_vvec_normalize-vvt-mitch1 vvec))
    (:vvt-rfun   (mjr_vvec_normalize-vvt-rfun   vvec))
    (:vvt-nfun   (mjr_vvec_normalize-vvt-nfun   vvec))
    (otherwise   (mjr_vvec_normalize-vvt-aseq   vvec))))

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
  (case (mjr_vvec_get-vvec-rep vvec)
    (:vvr-vec            (values (let ((i -1)) (lambda () (if (< i (1- (length vvec))) (aref vvec (incf i))))) (length vvec)))
    (:vvr-list           (values (lambda () (pop vvec)) (length vvec)))    
    (:vvr-int            (if (< vvec 0)
                             (values (let ((i 0))  (lambda () (if (< i (- vvec))  (incf i)))) (- vvec))
                             (values (let ((i -1)) (lambda () (if (< i (1- vvec)) (incf i)))) vvec)))
    (:vvr-kw             (multiple-value-bind (vvec-type points start end step len map-fun nfun rfun)
                             (mjr_util_get-kwarg-vals '(:vvec-type :points :start :end :step :len :map-fun :nfun :rfun) 
                                                      (mjr_vvec_normalize-all vvec))
                           (let ((fif (case vvec-type
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
                             (if map-fun
                                 (values (lambda () (let ((v (funcall fif))) (if v (funcall map-fun v)))) len)
                                 (values fif len)))))))

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

It is also common for the reduce operation to depend upon the index of the point of pair, but the number and type of arguments is always the same regardless
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
(defun mjr_vvec_gen-0sim (result-type vvec)
  "Interperate VVEC as a list of points (specified by a vvec) that describe a list of 0 simplexes.  Return, as a list or vector, that list of points.

Note: This is one way to transform a vvec into a lisp vector."
  (if (not (or (eq result-type 'list)
               (eq result-type 'vector))) (error "mjr_vvec_gen-0sim: RESULT-TYPE must be 'VECTOR or 'LIST!"))
  (let ((res (mjr_vvec_map-filter-reduce result-type vvec)))
    (typecase res 
      (list    (if (null res)
                   (error "mjr_vvec_gen-0sim: Empty vector generated")))
      (vector  (if (= 0 (length res))
                   (error "mjr_vvec_gen-0sim: Empty vector generated"))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_gen-1sim (result-type vvec)
  "Generate and return, as a list or vector, a the sequence of INTERVALS described by the points of the virtual vector.

An interval is a LIST containing an the start and end point of a mathematical interval."
  (if (not (or (eq result-type 'list)
               (eq result-type 'vector))) (error "mjr_vvec_gen-1sim: RESULT-TYPE must be 'VECTOR or 'LIST!"))
  (let ((res (mjr_vvec_map-filter-reduce result-type vvec :pair-fun (lambda (x0 x1 fx0 fx1 i) (declare (ignore fx0 fx1 i)) (list x0 x1)))))
    (typecase res 
      (list    (if (null res)
                   (error "mjr_vvec_gen-0sim: Empty vector generated")))
      (vector  (if (= 0 (length res))
                   (error "mjr_vvec_gen-0sim: Empty vector generated"))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_convert-rep (vvec vvr-type &optional (error-on-bad-vvr-type 't))
  "Convert VVEC representation to VVR-TYPE (supported: :vvr-kw, :vvr-vec, and :vvr-list)."
  (case vvr-type 
    (:vvr-kw    (case (mjr_vvec_get-vvec-rep vvec)
                  (:vvr-kw       vvec)
                  (:vvr-vec      (list :vvec-type :points :points vvec))
                  (:vvr-list     (list :vvec-type :points :points vvec))
                  (:vvr-int      (list :vvec-type :seq    :len vvec))))
    (:vvr-vec   (case (mjr_vvec_get-vvec-rep vvec)
                  (:vvr-kw       (mjr_vvec_gen-0sim 'vector vvec))
                  (:vvr-vec      vvec)
                  (:vvr-list     (concatenate 'vector vvec))
                  (:vvr-int      (mjr_vvec_gen-0sim 'vector vvec))))
    (:vvr-list  (case (mjr_vvec_get-vvec-rep vvec)
                  (:vvr-kw       (mjr_vvec_gen-0sim 'list vvec))
                  (:vvr-vec      (concatenate 'vector vvec))
                  (:vvr-list     vvec)
                  (:vvr-int      (mjr_vvec_gen-0sim 'list vvec))))
    (otherwise  (if error-on-bad-vvr-type
                    (error "mjr_vvec_convert-rep: Unsupported :VVR-TYPE")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vvec_to-vec-maybe (vvec)
  "Convert VVEC to a :VVR-VEC vvec (a real LISP vector).

If teh convesion was successfull, the LISP vector an 'T are returned, otherwise the return is VVEC and NIL.

Used as a 'filter' for functions that might get a vvec or some other thing entirely."
  (let ((ret (mjr_vvec_convert-rep vvec :vvr-vec nil)))
    (if ret
        (values ret  't)
        (values vvec nil))))

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
