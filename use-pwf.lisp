;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-pwf.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     piecewise function stuff.@EOL
;; @keywords  
;; @std       Common Lisp
;; @see       
;; @copyright 
;;  @parblock
;;  Copyright (c) 2016, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PWF
  (:USE :COMMON-LISP
        :MJR_VVEC
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Univariate Functions over R or C.;")
  (:EXPORT #:mjr_pwf_help
           #:mjr_pwf_eval-linear  #:mjr_pwf_func-linear   
           #:mjr_pwf_eval-step    #:mjr_pwf_func-step     
           #:mjr_pwf_eval-funcs   #:mjr_pwf_func-funcs
           #:mjr_pwf_search-interval-mesh
           ))

(in-package :MJR_PWF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_help ()
  "Help for MJR_PWF: 

Helper utilities to define and evaluate piecewise functions."
  (documentation 'mjr_pwf_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_search-interval-mesh (interval-type breaks x)
  "Return the index of the sub-interval that contains x.  Returns -1 if x is on the left of the interval.  Returns -2 if x is on the right of the interval.

The mesh of the interval $[v_0, v_k]$ is defined by the vector BREAKS $=(v_0, v_1, ..., v_{k-1}, v_k)\\in\\mathbb{R}^{k}$ where $v_i<v_j \\iff i<j$

The sub-intervals are defined in one of two ways depending on the value of the INTERVAL-TYPE argument:

     * :INTERVAL-TYPE-LEFT-CLOSED:    $$\{ I_0=[v_0, v_1), I_1=[v_1, v_2), ..., I_{k-2}=[v_{k-2}, v_{k-1}), I_{k-1}=[v_{k-1}, v_k] \}$$

     * :INTERVAL-TYPE-LEFT-OPEN:      $$\{ I_0=[v_0, v_1], I_1=(v_1, v_2], ..., I_{k-2}=(v_{k-2}, v_{k-1}], I_{k-1}=(v_{k-1}, v_k] \}$$"
  (if (not (member interval-type '(:interval-type-left-closed :interval-type-left-open)))
      (error "Unsupported interval-type"))
  (let* ((len   (length breaks))
         (left  (aref breaks 0))
         (right (aref breaks (1- len))))
    (cond ((< x left)  -1)
          ((> x right) -2)
          ((= x left)   0)
          ((= x right)  (- len 2))
          ('t           (loop with lo = 0
                              with hi = (- len 2)
                              for g = (truncate (+ lo hi) 2)
                              do (if (= lo hi)
                                     (return g))
                              do (if (case interval-type
                                       (:interval-type-left-closed (<  x (aref breaks g)))
                                       (:interval-type-left-open   (<= x (aref breaks g))))
                                     (setf hi (1- g))
                                     (if (case interval-type
                                           (:interval-type-left-closed (>= x (aref breaks (1+ g))))
                                           (:interval-type-left-open   (>  x (aref breaks (1+ g)))))
                                         (setf lo (1+ g))
                                         (return g))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_eval-linear (x xdata ydata &key nil-outside-interval)
  "Evaluate a piecewise linear function defined by a set of points.  

Arguments:
  * X ..................... A real value in [min(XDATA), max(XDATA)]
  * XDATA ................. XDATA is a vector defining an interval mesh (see: MJR_PWF_SEARCH-INTERVAL-MESH).
  * YDATA ................. A vector defining the value of the PL function at the XDATA points.  Same length as XDATA.
  * nil-outside-interval .. If non-NIL, return NIL when X is outside theinterval defined by XDATA.
                            Otherwise pass x, transformed according to :FIXED-DOMAIN, to the left or right segment function.

The value returned when X is outside of the interval will be linearly extrapolated using the nearest (left most or right most) segment.

Example how one might graph a linear PWF:

    (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x) (mjr_pwf_eval-linear x #(1 2 3 6 9) #(2 3 1 6 0)))
                                                  :xdat '(:start -5 :end 15 :len 100)))"
  (if (not (= (length xdata) (length ydata)))
      (error "mjr_pwf_eval-linear: The length of XDATA and YDATA must be the same!"))
  (let* ((i     (mjr_pwf_search-interval-mesh :interval-type-left-closed xdata x))
         (idx-l (case i
                  (-1        (if (not nil-outside-interval)  0))
                  (-2        (if (not nil-outside-interval) (- (length xdata) 2)))
                  (otherwise i)))
         (idx-r (if idx-l (1+ idx-l))))
    (if (numberp idx-l)
        (mjr_numu_2p-linear-interpolate x
                                        (aref xdata idx-l)
                                        (aref xdata idx-r)
                                        (aref ydata idx-l)
                                        (aref ydata idx-r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_func-linear (xdata ydata &key nil-outside-interval)
  "Return a function that evaluates the PW function"
  (lambda (x)
    (mjr_pwf_eval-linear x xdata ydata :nil-outside-interval nil-outside-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_eval-step (x xdata ydata &key (interval-type :interval-type-left-closed) left-tail-value right-tail-value)
  "Evaluate a piecewise step function defined by a set of points.  

Arguments:
  * X ................ A real value in (min(XDATA), max(XDATA)] or [min(XDATA), max(XDATA)) depending on the value of :INTERVAL-TYPE
  * XDATA ............ XDATA is a vector defining an interval mesh (see: MJR_PWF_SEARCH-INTERVAL-MESH).  
  * YDATA ............ A vector of real numbers defineing the value of a step function on the *intervals* defined in XDATA.  Length is one less than XDATA
  * :INTERVAL-TYPE ... See mjr_pwf_search-interval-mesh.
  * LEFT-TAIL-VALUE .. The value to be returned in X is to the left of the interval defined by XDATA.  If nil, then the value of the left segment is returned.
  * RIGHT-TAIL-VALUE . The value to be returned in X is to the right of the interval defined by XDATA.  If nil, then the value of the right segment is returned.

Example how one might graph a linear PWF:
    (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x) (mjr_pwf_eval-step x #(1 2 3 6 9) #(2 3 1 6)))
                                                  :xdat '(:start -5 :end 15 :len 100)))
    (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x) (mjr_pwf_eval-step x #(1 2 3 6 9) #(2 3 1 6) :left-tail-value 0 :right-tail-value 0))
                                                  :xdat '(:start -5 :end 15 :len 100)))"
  (if (not (= (1- (length xdata)) (length ydata)))
      (error "mjr_pwf_eval-step: The length of XDATA must be one more than length of YDATA!"))
  (let ((i (mjr_pwf_search-interval-mesh interval-type xdata x)))
    (case i
      (-1        (or left-tail-value (aref ydata 0)))
      (-2        (or right-tail-value (aref ydata (1- (length ydata)))))
      (otherwise (aref ydata i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_func-step (xdata ydata &key (interval-type :interval-type-left-closed) left-tail-value right-tail-value)
  "Return a function that evaluates the PW function"
  (lambda (x)
    (mjr_pwf_eval-step x xdata ydata
                               :interval-type interval-type
                               :left-tail-value left-tail-value
                               :right-tail-value right-tail-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_eval-funcs (x xdata yfuncs &key (interval-type :interval-type-left-closed) fixed-domain nil-outside-interval)
  "Evaluate a generic piecewise function.

Arguments:
  * X ..................... A real value in (min(XDATA), max(XDATA)] or [min(XDATA), max(XDATA)) depending on the value of :INTERVAL-TYPE
  * XDATA ................. XDATA is a vector defining an interval mesh (see: MJR_PWF_SEARCH-INTERVAL-MESH).  
  * YFUNCS ................ List of functions of length (1- (LENGTH XDATA))
  * :INTERVAL-TYPE ........ See mjr_pwf_search-interval-mesh.
  * :FIXED-DOMAIN ......... Describe the domain of the function components
                              * nil ....... The value of X is directly passed to the component function
                              * List ...... '(a b) : domain of each component function is [a, b]
                              * Vector .... #(a b) : domain of each component function is [a, b]
  * nil-outside-interval .. If non-NIL, return NIL when X is outside theinterval defined by XDATA.
                            Otherwise pass x, transformed according to :FIXED-DOMAIN, to the left or right segment function.

Example how one might graph a PWF:

  (let ((myPW (mjr_pwf_func-funcs #(1 3 5 7) (list (lambda (x) (- x)) (lambda (x) (* x x x)) (lambda (x) (* x x))) :fixed-domain '(-1 1))))
    (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 myPW :xdat '(:start 0 :end 8 :len 1000))))

Another way to do the same thing:

  (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 (lambda (y) (mjr_pwf_eval-funcs y 
                                                                                #(1 3 5 7) 
                                                                                (list (lambda (x) (- x))
                                                                                      (lambda (x) (* x x x))
                                                                                      (lambda (x) (* x x)))
                                                                                :fixed-domain '(-1 1)))
                                                :xdat '(:start 0 :end 8 :len 1000)))"
  (if (not (= (1- (length xdata)) (length yfuncs)))
      (error "mjr_pwf_eval-funcs: The length of XDATA must be one more than length of YFUNCS!"))
  (let* ((i     (mjr_pwf_search-interval-mesh interval-type xdata x))
         (idx-l (case i
                  (-1        (if (not nil-outside-interval)  0))
                  (-2        (if (not nil-outside-interval) (- (length xdata) 2)))
                  (otherwise i))))
    (if (numberp idx-l)
        (if fixed-domain
            (let ((x0 (aref xdata idx-l))
                  (x1 (aref xdata (1+ idx-l)))
                  (d0 (elt fixed-domain 0))
                  (d1 (elt fixed-domain 1)))
              (funcall (elt yfuncs idx-l) (+ d0 (* (/ (- x x0) (- x1 x0)) (- d1 d0)))))
            (funcall (elt yfuncs idx-l) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_pwf_func-funcs (xdata yfuncs &key (interval-type :interval-type-left-closed) fixed-domain nil-outside-interval)
  "Return a function that evaluates the PW function"
  (lambda (x)
    (mjr_pwf_eval-funcs x xdata yfuncs
                               :interval-type interval-type
                               :fixed-domain fixed-domain
                               :nil-outside-interval nil-outside-interval)))
