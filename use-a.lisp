;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-a.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Angle (and time-ish) utilities.@EOL
;; @std       Common Lisp
;; @see       tst-a.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2006,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_A
  (:USE :COMMON-LISP 
        :MJR_STRING
        :MJR_CMP
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Angle (and time-ish) utilities.;")
  (:EXPORT #:mjr_a_help
           #:mjr_a_dms2d #:mjr_a_d2dms
           #:mjr_a_r2d #:mjr_a_d2r
           #:mjr_a_normalize
           ))

(in-package :MJR_A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_help ()
  "Help for MJR_A:  Angle (and time-ish) utilities."
  (documentation 'mjr_a_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_normalize (angle &key (angular-measure :am-degrees) (smallest-abs nil))
  "Return normalized angle

Value of :SMALLEST-ABS changes the definition of 'normalized':
  * SMALLEST-ABS NIL     -- Smallest positive value representing the same angle as ANGLE -- DEFAULT
  * SMALLEST-ABS non-NIL -- Smallest angle in absolute value that represents the same angle as ANGLE

:ANGULAR-MEASURE changes the angular measurement system, and it must be one of:
  * :AM-DEGREES -- result will be same type as ANGLE -- this is the default option
  * :AM-RADIANS -- result will be DOUBLE-FLOAT.
  * :AM-HOURS   -- result will be same type as ANGLE"
  (cond  ((complexp angle)                                       (error "mjr_a_normalize: Input must not be complex!"))
         ((not (numberp angle))                                  (error "mjr_a_normalize: Input must be a number!")))
  (let* ((bconst (case angular-measure
                   (:am-degrees 360)
                   (:am-radians (* 2 pi))
                   (:am-hours   24)
                   (otherwise   (error "mjr_a_normalize: Unsupported ANGULAR-MEASURE!"))))
         (nangle (mod angle bconst)))
    (if (and smallest-abs (mjr_cmp_> (abs nangle) (/ bconst 2)))
        (- nangle bconst)
        nangle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_dms2d (&rest dms-list)
  "Return fractional degs from deg, min, sec.  

If all inputs are rational, then so is the output.  The values may be provided in a list, a string in D:M:S format, or as individual arguments.  The min and
sec are optional, and are assumed to be zero if missing."
  (if (stringp (first dms-list))
      (mjr_a_dms2d (mapcar #'mjr_string_read-as-lisp (mjr_string_split (first dms-list) #\:)))
      (if (not (listp (first dms-list)))
          (mjr_a_dms2d dms-list)
          (let ((the-list (first dms-list)))
            (cond ((not (every #'numberp the-list))  (error "mjr_a_dms2d: Inputs must be numbers!"))
                  ((some #'complexp the-list)        (error "mjr_a_dms2d: Inputs may not be complex!"))
                  ((< 3 (length the-list))           (error "mjr_a_dms2d: Too many inputs provided!"))
                  ((> 1 (length the-list))           (error "mjr_a_dms2d: Too few inputs provided!")))
            (let ((d (first the-list))
                  (m (or (second the-list) 0))
                  (s (or (third the-list) 0)))
              (+ d (/ m 60) (/ s 3600)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_d2dms (df &key return-int-sec)
  "Return degrees, minutes, and seconds from fractional degrees.  

The degrees and minutes returned are integers. :RETURN-INT-SEC is NIL, then the returned seconds will included the fractional seconds.  On the other hand,
if :RETURN-INT-SEC is non-nil, then the returned seconds will also be an integer and an additional fourth return value will will be the fractional seconds.
If the input is rational, then the fractional outputs will be too.  If the input is a float, then the fractional outputs will be DOUBLE-FLOAT."
  (cond  ((complexp df)      (error "mjr_a_dms2d: Input must not be complex!"))
         ((not (numberp df)) (error "mjr_a_dms2d: Input must be a number!")))
  (multiple-value-bind (tmp1 sec-frac) (truncate (* 60 60 (abs (mjr_numu_max-accuracy df))))
    (multiple-value-bind (tmp2 sec)    (truncate tmp1 60)
      (multiple-value-bind (deg min)   (truncate tmp2 60)
        (if return-int-sec
            (values (if (< df 0) (- deg) deg) min sec             sec-frac)
            (values (if (< df 0) (- deg) deg) min (+ sec sec-frac)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_r2d (rad)
  "Convert rads to degrees

Result is DOUBLE-FLOAT unless input is SINGLE-FLOAT (computation performed with DOUBLE-FLOAT)"
  (cond  ((complexp rad)      (error "mjr_a_dms2d: Input must not be complex!"))
         ((not (numberp rad)) (error "mjr_a_dms2d: Input must be numbers!")))
  (if (eq (type-of rad) 'single-float)
      (coerce (/ (* rad 180L0) pi) 'single-float)
    (/ (* rad 180L0) pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_a_d2r (deg)
  "Convert degrees to rads

Result is DOUBLE-FLOAT unless input is SINGLE-FLOAT (computation performed with DOUBLE-FLOAT)"
  (cond  ((complexp deg)      (error "mjr_a_dms2d: Input must not be complex!"))
         ((not (numberp deg)) (error "mjr_a_dms2d: Input must be numbers!")))
  (if (eq (type-of deg) 'single-float)
      (coerce (/ (* deg pi) 180L0) 'single-float)
    (/ (* deg pi) 180L0)))
