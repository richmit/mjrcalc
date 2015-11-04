;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-ia.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     User interface wrapper for :MJR_SIA.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2007,2008,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_IA
  (:USE :COMMON-LISP
        :MJR_SIA
        :MJR_CMP)
  (:DOCUMENTATION "Brief: User interface wrapper for :MJR_SIA.;")
  (:EXPORT #:mjr_ia_i2s #:mjr_ia_s2i #:mjr_ia_rep #:mjr_ia_convert #:mjr_ia_* #:mjr_ia_/ #:mjr_ia_+ #:mjr_ia_- #:mjr_ia_max
           #:mjr_ia_min #:mjr_ia_intersect #:mjr_ia_expt #:mjr_ia_in?  #:mjr_ia_intersect?  #:mjr_ia_< #:mjr_ia_> #:mjr_ia_log
           #:mjr_ia_exp #:mjr_ia_sqrt #:mjr_ia_sq #:mjr_ia_inv #:mjr_ia_sin #:mjr_ia_cos #:mjr_ia_tan #:mjr_ia_left #:mjr_ia_right
           #:mjr_ia_ctr #:mjr_ia_width #:mjr_ia_hwide #:mjr_ia_guess #:mjr_ia_error #:mjr_ia_zapguess #:mjr_ia_left=
           #:mjr_ia_right= #:mjr_ia_guess= #:mjr_ia_ctr= #:mjr_ia_dist #:mjr_ia_same? #:mjr_ia_help
           ))

(in-package :MJR_IA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ia_help ()
  "
Functions:
 |                                  | Interactive API                     | Simple API                               |
 | Binary Operations .............. | mjr_ia_*          mjr_ia_-          | mjr_sia_*            mjr_sia_-           |
 |                                  | mjr_ia_+          mjr_ia_/          | mjr_sia_+            mjr_sia_/           |
 |                                  | mjr_ia_max        mjr_ia_min        | mjr_sia_max          mjr_sia_min         |
 |                                  | mjr_ia_expt       mjr_ia_intersect  | mjr_sia_expt         mjr_sia_intersect   |
 | 1-var math funcs ............... | mjr_ia_cos        mjr_ia_sin        | mjr_sia_cos          mjr_sia_sin         |
 |                                  | mjr_ia_exp        mjr_ia_log        | mjr_sia_exp          mjr_sia_log         |
 |                                  | mjr_ia_sq         mjr_ia_sqrt       | mjr_sia_sq           mjr_sia_sqrt        |
 |                                  | mjr_ia_inv        mjr_ia_tan        | mjr_sia_inv          mjr_sia_tan         |
 | Attribute'esq accessors ........ | mjr_ia_ctr                          | mjr_sia_ctr                              |
 |                                  | mjr_ia_error      mjr_ia_guess      | mjr_sia_error        mjr_sia_guess       |
 |                                  | mjr_ia_hwide      mjr_ia_width      | mjr_sia_hwide        mjr_sia_width       |
 |                                  | mjr_ia_left       mjr_ia_right      | mjr_sia_left         mjr_sia_right       |
 | Attribute'esq modifiers ........ | mjr_ia_ctr=       mjr_ia_guess=     | mjr_sia_ctr=         mjr_sia_guess=      |
 |                                  | mjr_ia_left=      mjr_ia_right=     | mjr_sia_left=        mjr_sia_right=      |
 |                                  | mjr_ia_zapguess                     | mjr_sia_zapguess                         |
 | Containment & order relations .. | mjr_ia_<          mjr_ia_>          | mjr_sia_<            mjr_sia_>           |
 |                                  | mjr_ia_in?        mjr_ia_intersect? | mjr_sia_in?          mjr_sia_intersect?  |
 |                                  | mjr_ia_same?      mjr_ia_dist       | mjr_sia_same?        mjr_sia_dist        |
 | Low level stuff ................ | ................................... | mjr_sia_valid?       mjr_sia_valide      |
 |                                  | ................................... | mjr_sia_apply2ends   mjr_sia_apply2guess |
 |                                  | ................................... | mjr_sia_monfun                           |
 | MJR_IA_* fmts & types .......... | mjr_ia_convert    mjr_ia_rep        | ........................................ |
 |                                  | mjr_ia_i2s        mjr_ia_s2i        | ........................................ |
 | Misc ........................... | ................................... | mjr_sia_ctr=guess?                       |

This library expands upon the MJR_SIA_* functions to provide a more comfortable environment for interactive computation with general intervals and with
intervals derived from error analysis and measurement uncertainty.

Intervals supported:

    |------------------+-----+----------------+--------------------|
    | Name             | SYM | LISP           | Math               |
    |------------------+-----+----------------+--------------------|
    | Interval         | :M  | (list :M  a b) | [a, b]             |
    | Absolute Error   | :AE | (list :AE a e) | [a-e, a+e]         |
    | Fractional Error | :FE | (list :FE a f) | [a-a*f, a+a*f]     |
    | Percentage Error | :PE | (list :PE a p) | [a-a*p, a+a*p]/100 |
    | Interval type    | :S  | (list a b)     | [a, b]             |
    | Number           | :N  | a              | [a, a]             |
    |------------------+-----+----------------+--------------------|

Notes on :AE, :FE, and :PE forms:

  Intervals specified in the :AE, :FE, and :PE forms are defined by a 'best guess' for the true value and an error bound.  This 'best guess' is the center
  point for the defined interval, and the width of the interval is defined by the error.  This is a common format for specifying imprecision or error in
  quantities like physical measurements and survey results.

Notes on the :S and :N forms:

  The :S and :N forms are included for simplicity and for ease of use.  The only time the :S and :N symbols used, are in the conversion functions."
  (documentation 'mjr_ia_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ia_i2s (a)
"Convert ia-type intervals into sia-type intervals."
  (or
   (cond ((numberp a)
          (list a a))
         ((and (listp a) (mjr_sia_valid? a))
          a)
         ((and (listp a)                                                   ;; list
               (>= (length a) 3) (<= (length a) 4)                         ;; length OK
               (symbolp (first a))                                         ;; got a symbol as first element
               (member (first a) '(:m :ae :fe :pe))                        ;; The symbol is one of our important ones
               (numberp (second a)) (numberp (third a))                    ;; interval numbers are numeric
               (not (complexp (second a))) (not (complexp (third a)))      ;; and not complex
               (or (= (length a) 3) (numberp (fourth a)))                  ;; guess is numeric if it exists
               (or (eq (first a) ':m)
                   (and (member (first a) '(:ae :fe :pe)) (<= 0 (third a))))) ;; error bar is non-negative for :ae, :fe, & :pe
          (mjr_sia_valide
           (mjr_sia_guess=
            (cond ((equal ':m  (first a)) (if (<= (second a) (third a)) (list (second a) (third a))))
                  ((equal ':ae (first a)) (let ((d (third a)))                        (list (- (second a) d) (+ (second a) d))))
                  ((equal ':fe (first a)) (let ((d (* (second a) (third a))))         (list (- (second a) d) (+ (second a) d))))
                  ((equal ':pe (first a)) (let ((d (* (second a) (/ (third a) 100)))) (list (- (second a) d) (+ (second a) d)))))
            (if (= 4 (length a)) (fourth a))))))
   (error "mjr_ia_i2s: Bad interval: ~s~&" a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mjr_ia_s2i (a &optional (rep :m))
"Convert sia-type intervals into ia-type intervals"
  (if (and (mjr_sia_valid? a) (symbolp rep))
      (cond ((member rep '(:n :ng))     (mjr_sia_guess a))                 ;; Just return the best guess
            ((equal ':nc rep)           (mjr_sia_ctr a))                   ;; Just return the center
            ((equal ':s  rep)           a)                                 ;; Simple form
            ((member rep '(:fe :pe :ae :m))                                ;; One of our "handy" forms
                     (let ((hformng
                            (if (eq ':m rep)                               ;; Math form
                                (list ':m (mjr_sia_left a) (mjr_sia_right a))
                              (let* ((ctr (mjr_sia_ctr a))                     ;; Ctr +/- Error
                                     (wid (mjr_sia_hwide a)))
                                (cond
                                 ((or (equal ':ae rep) (mjr_cmp_= 0 ctr)) (list ':ae ctr  wid))
                                 ((equal ':fe rep)                        (list ':fe ctr  (/ wid ctr)))
                                 ((equal ':pe rep)                        (list ':pe ctr  (* 100 (/ wid ctr)))))))))
                       (if (mjr_sia_ctr=guess? a)
                           hformng
                         (append hformng (list (mjr_sia_guess a)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ia_rep (a &optional b)
"Return symbol for the representation of the given interval or combination of intervals"
  (if b
      (let* ((aRep (mjr_ia_rep a))
             (bRep (mjr_ia_rep b)))
        (if (and aRep bRep)
            (if (member aRep '(:n :s))
                bRep
              aRep)))
    (cond ((numberp a)                      ':n)
          ((mjr_sia_valid? a)               ':s)
          ((and (listp a) (< 1 (length a))) (first a)))))

;; Convert an interval given as first into representation given by second arg
(defun mjr_ia_convert (a rep) (mjr_ia_s2i (mjr_ia_i2s a) rep))

;; Various 2-var functions
(defun mjr_ia_*         (a b &optional rep) (mjr_ia_s2i (mjr_sia_*         (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_/         (a b &optional rep) (mjr_ia_s2i (mjr_sia_/         (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_+         (a b &optional rep) (mjr_ia_s2i (mjr_sia_+         (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_-         (a b &optional rep) (mjr_ia_s2i (mjr_sia_-         (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_max       (a b &optional rep) (mjr_ia_s2i (mjr_sia_max       (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_min       (a b &optional rep) (mjr_ia_s2i (mjr_sia_min       (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_intersect (a b &optional rep) (mjr_ia_s2i (mjr_sia_intersect (mjr_ia_i2s a) (mjr_ia_i2s b)) (or rep (mjr_ia_rep a b))))
(defun mjr_ia_expt      (a b &optional rep) (mjr_ia_s2i (mjr_sia_expt      (mjr_ia_i2s a) b)          (or rep (mjr_ia_rep a b)))) ;; don't convert second arg!

;; Various 2-var functions returning Boolean
(defun mjr_ia_in?        (a b)  (mjr_sia_in?        (mjr_ia_i2s a) (mjr_ia_i2s b)))
(defun mjr_ia_intersect? (a b)  (mjr_sia_intersect? (mjr_ia_i2s a) (mjr_ia_i2s b)))
(defun mjr_ia_<          (a b)  (mjr_sia_<          (mjr_ia_i2s a) (mjr_ia_i2s b)))
(defun mjr_ia_>          (a b)  (mjr_sia_>          (mjr_ia_i2s a) (mjr_ia_i2s b)))

;; Various 1-var functions returning an interval
(defun mjr_ia_log  (a &optional rep) (mjr_ia_s2i (mjr_sia_log  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_exp  (a &optional rep) (mjr_ia_s2i (mjr_sia_exp  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_sqrt (a &optional rep) (mjr_ia_s2i (mjr_sia_sqrt (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_sq   (a &optional rep) (mjr_ia_s2i (mjr_sia_sq   (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_inv  (a &optional rep) (mjr_ia_s2i (mjr_sia_inv  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_sin  (a &optional rep) (mjr_ia_s2i (mjr_sia_sin  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_cos  (a &optional rep) (mjr_ia_s2i (mjr_sia_cos  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))
(defun mjr_ia_tan  (a &optional rep) (mjr_ia_s2i (mjr_sia_tan  (mjr_ia_i2s a)) (or rep (mjr_ia_rep a))))

;; Various 1-var functions returning numbers
(defun mjr_ia_left     (a) (mjr_sia_left  (mjr_ia_i2s a)))
(defun mjr_ia_right    (a) (mjr_sia_right (mjr_ia_i2s a)))
(defun mjr_ia_ctr      (a) (mjr_sia_ctr   (mjr_ia_i2s a)))
(defun mjr_ia_width    (a) (mjr_sia_width (mjr_ia_i2s a)))
(defun mjr_ia_hwide    (a) (mjr_sia_hwide (mjr_ia_i2s a)))
(defun mjr_ia_guess    (a) (mjr_sia_guess (mjr_ia_i2s a)))
(defun mjr_ia_error    (a) (mjr_sia_error (mjr_ia_i2s a)))

;; Various 1-var, interval modifying functions (they actually return a NEW interval)
(defun mjr_ia_zapguess (a &optional zap-no-matter-what rep) (mjr_ia_s2i (mjr_sia_left=  (mjr_ia_i2s a) zap-no-matter-what) (or rep (mjr_ia_rep a))))
(defun mjr_ia_left=    (a left  &optional rep)              (mjr_ia_s2i (mjr_sia_left=  (mjr_ia_i2s a) left)               (or rep (mjr_ia_rep a))))
(defun mjr_ia_right=   (a right &optional rep)              (mjr_ia_s2i (mjr_sia_right= (mjr_ia_i2s a) right)              (or rep (mjr_ia_rep a))))
(defun mjr_ia_guess=   (a &optional g rep)                  (mjr_ia_s2i (mjr_sia_guess= (mjr_ia_i2s a) g)                  (or rep (mjr_ia_rep a))))
(defun mjr_ia_ctr=     (a &optional c rep)                  (mjr_ia_s2i (mjr_sia_ctr=   (mjr_ia_i2s a) c)                  (or rep (mjr_ia_rep a))))

;; Misc
(defun mjr_ia_dist  (a b &optional method) (mjr_sia_dist  (mjr_ia_i2s a) (mjr_ia_i2s b) method))
(defun mjr_ia_same? (a b &optional method) (mjr_sia_same? (mjr_ia_i2s a) (mjr_ia_i2s b) method))
