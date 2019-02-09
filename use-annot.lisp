;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-annot.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Provide data annotation tools -- DSIMP and DQUAD.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997, 2008, 2010, 2013, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      unit-tests!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_ANNOT
  (:USE :COMMON-LISP
        :MJR_UNITS)
  (:DOCUMENTATION "Brief: Geometric Data Sets: Supporting :MJR_DSIMP and :MJR_DQUAD.;")
  (:EXPORT #:mjr_annot_help ()

           #:mjr_annot_check-ano-typ
           #:mjr_annot_check-ano-nam

           #:mjr_annot_check-ano-key

           #:mjr_annot_make-alist

           #:mjr_annot_get-default-value
           #:mjr_annot_get-value

           #:mjr_annot_ano-typ<

           #:mjr_annot_get-colorpacking #:mjr_annot_get-colorspace

           #:mjr_annot_typ-colorp #:mjr_annot_typ-vectorp #:mjr_annot_typ-numberp #:mjr_annot_typ-nums-real
           ))

(in-package :MJR_ANNOT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_help ()
  "Help for MJR_ANNOT: Annotations for labeling data (for example, in :MJR_DSIMP & :MJR_DQUAD)

Annotations may be collected up into annotation lists -- alists with annotation key (or tag) and value.  Each element may have one
of the following possible annotation keys:

    * :ano-typ -- the element type for the array/array.  Values for the :ano-typ key may be any of the following:

         |------------------+----------------------+---------+-----------+-----------+---------|
         | :ano-typ Value   | Data Type            | MJR_VTK | MJR_GNUPL | MJR_COLOR | MJR_POV |
         |------------------+----------------------+---------+-----------+-----------+---------|
         | :ano-typ-real    | floating             | YES     | YES       | N/A       | YES     |
         | :ano-typ-integer | integer              | YES (1) | YES (1)   | N/A       | YES (1) |
         | :ano-typ-complex | complex              | NO      | NO        | N/A       | NO      |
         |------------------+----------------------+---------+-----------+-----------+---------|
         | :ano-typ-ivec    | vector of integers   | YES (1) | YES (1)   | N/A       | YES (1) |
         | :ano-typ-rvec    | vector of floats     | YES     | YES       | N/A       | YES     |
         | :ano-typ-cvec    | vector of complexs   | NO      | NO        | N/A       | NO      |
         |------------------+----------------------+---------+-----------+-----------+---------|
         | :ano-typ-truint  | :cs-tru & :cp-int8x3 | NO (4)  | YES (2)   | YES       | NO (4)  |
         | :ano-typ-truvec  | :cs-tru & :cp-none   | NO (4)  | YES       | YES       | NO (4)  |
         | :ano-typ-rgbvec  | :cs-rgb & :cp-none   | YES     | YES (2)   | YES       | YES     |
         | :ano-typ-hsvvec  | :cs-hsv & :cp-none   | NO (4)  | YES (2)   | YES       | NO (4)  |
         | :ano-typ-hslvec  | :cs-hsl & :cp-none   | NO (4)  | YES (2)   | YES       | NO (4)  |
         |------------------+----------------------+---------+-----------+-----------+---------|

                   (1) .. Treated as floats
                   (2) .. Automatically converted before used by external tool
                   (3) .. NOP
                   (4) .. Could be supported via conversion -- like (2).  Will probably add support in the future.

    * :ano-nam -- a name for following array.  The value for this key is a string.

    * :ano-units -- a string that can be parsed by MJR_UNITS_CANONIZATION."
  (documentation 'mjr_annot_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_get-default-value (ano-key)
  "Return the default value for an ano-key"
  (case ano-key
    (:ano-typ        :ano-typ-real)
    (otherwise       nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_get-value (ano-key ano-alist)
  "Return the value for the ano-key stored in the ano-alist, or the default value if the alist doesn't contain the key."
  (let ((value (cdr (assoc ano-key  ano-alist))))
    (if value
        (values value                                 't)
        (values (mjr_annot_get-default-value ano-key) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_check-ano-key (ano-key)
  "Error if ano-key is not a supported value (:ano-typ :ano-nam :ano-units)"
  (if (not (member ano-key '(:ano-typ :ano-nam :ano-units)))
      (error "mjr_annot_check-ano-key: :ano-key must be one of :ano-typ, :ano-nam, or :ano-units")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_check-ano-units (ano-units)
  "ERROR if ano-units is non-NIL and invalid."
  (if (and ano-units
           (not (ignore-errors (mjr_units_canonization "m/gm"))))
      (error "mjr_annot_check-ano-units: :ano-units was non-NIL and could not be parsed by mjr_units_canonization!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_check-ano-typ (ano-typ)
  "ERROR if ano-typ is non-NIL and invalid."
  (if ano-typ
      (if (not (member ano-typ '(:ano-typ-real :ano-typ-integer :ano-typ-complex
                                 :ano-typ-truvec :ano-typ-truint :ano-typ-rgbvec :ano-typ-hsvvec :ano-typ-hslvec
                                 :ano-typ-ivec :ano-typ-rvec :ano-typ-cvec)))
          (error "mjr_annot_check-ano-typ: invalid :ano-typ!"))
      (warn "mjr_annot_make-alist: ano-typ is NIL!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_check-ano-nam (ano-nam)
  "ERROR if ano-nam is invalid (NIL is invalid BTW)."
    (cond ((null ano-nam)                (error "mjr_annot_check-ano-nam: :ano-nam must not be NIL"))
          ((not (stringp ano-nam))       (error "mjr_annot_check-ano-nam: :ano-nam must be a string"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_typ-colorp (ano-typ)
  "Return non-NIL if ano-type is a color type."
  (member ano-typ '(:ano-typ-truvec :ano-typ-truint :ano-typ-rgbvec :ano-typ-hsvvec :ano-typ-hslvec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_ano-typ< (key1 key2)
  "A comparison function on possible :ano-typ values.

Order definition:
  :ano-typ-color < :ano-typ-real = :ano-typ-integer < everything else

The order is compatible with the order in which data sets are required to appear in some software (like VisIT)"
  (or (equalp key1 key2)
      (mjr_annot_typ-colorp key1)
      (and (or (equalp key1 :ano-typ-real)
               (equalp key1 :ano-typ-integer))
           (not (mjr_annot_typ-colorp key2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_make-alist (&optional ano-nam ano-typ ano-units)
  "Stuff arguments into an alist."
  (mjr_annot_check-ano-nam ano-nam)
  (mjr_annot_check-ano-typ ano-typ)
  (loop for da-key in (list :ano-nam :ano-typ :ano-units)
        for da-val in (list  ano-nam  ano-typ  ano-units)
        when da-val
        collect (cons da-key da-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_typ-vectorp (ano-typ)
  "Return non-NIL if ano-type is a vector type (not a color)."
  (member ano-typ '(:ano-typ-ivec :ano-typ-rvec :ano-typ-cvec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_typ-numberp (ano-typ)
  "Return non-NIL if ano-type is a vector type (not a color)."
  (member ano-typ '(:ano-typ-real :ano-typ-integer :ano-typ-complex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_typ-nums-real (ano-typ)
  "Return non-NIL if ano-type is a non-color data type that contains real (not complex) numers"
  (member ano-typ '(:ano-typ-real :ano-typ-integer :ano-typ-rvec :ano-typ-ivec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_get-colorspace (ano-typ)
  "Returns one of :cs-tru, :cs-tru, :cs-rgb, :cs-hsv, :cs-hsl, or NIL."
  (case ano-typ
    (:ano-typ-truvec  :cs-tru)
    (:ano-typ-truint  :cs-tru)
    (:ano-typ-rgbvec  :cs-rgb)
    (:ano-typ-hsvvec  :cs-hsv)
    (:ano-typ-hslvec  :cs-hsl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_annot_get-colorpacking (ano-typ)
  "Returns one of :cp-int8x3, :cp-none, or NIL."
  (case ano-typ
    (:ano-typ-truint :cp-int8x3)
    (:ano-typ-truvec :cp-none)
    (:ano-typ-rgbvec :cp-none)
    (:ano-typ-hsvvec :cp-none)
    (:ano-typ-hslvec :cp-none)))
