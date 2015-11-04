;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      pre-annot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Provide data annotation tools.@EOL
;; @Keywords  lisp interactive annot
;; @Std       Common Lisp
;;
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_ANNOT
  (:USE :COMMON-LISP
        :MJR_COLOR
        :MJR_UNITS)
  (:DOCUMENTATION "Brief: Geometric Data Sets: Supporting :MJR_DSIMP and :MJR_DQUAD.;")
  (:EXPORT #:mjr_annot_help ()
           
           #:mjr_annot_check-ano-colorspace
           #:mjr_annot_check-ano-typ
           #:mjr_annot_check-ano-nam

           #:mjr_annot_check-ano-key

           #:mjr_annot_make-alist
           
           #:mjr_annot_get-default-value
           #:mjr_annot_get-value

           #:mjr_annot_ano-typ<

           #:mjr_annot_make-cunpacker
           #:mjr_annot_make-cpacker
           ))

(in-package :MJR_ANNOT)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_help ()
  "Help for MJR_ANNOT: Annotations for labeling data (for example, in :MJR_DSIMP & :MJR_DQUAD)

Annotations may be collected up into annotation lists -- alists with annotation key (or tag) and value.  Each element may have one
of the following possible annotation keys:

    * :ano-typ -- the element type for the array/array.  Values for the :ano-typ key may be any of the following:

         |------------------+-----------------------------+---------------|
         | :ano-typ Value   | Description                 | VTK supported |
         |------------------+-----------------------------+---------------|
         | :ano-typ-real    | real numbers                | YES           |
         | :ano-typ-integer | integers                    | YES'ish       |
         | :ano-typ-complex | complex numbers             | NO            |
         | :ano-typ-color   | A vector representing color | YES           |
         | :ano-typ-ivec    | integer vector              | YES'ish       |
         | :ano-typ-rvec    | real vector                 | YES           |
         | :ano-typ-cvec    | complex vector              | NO            |
         |------------------+-----------------------------+---------------|

      YES'ish for 'VTK supported' means that integers are treated as real numbers by VKT.

    * :ano-colorspace -- May only be present if :ano-typ is :ano-typ-color.  The value of this item indicates the color space used,
      and defaults to :cs-rgb if missing.  If present, it must be a valid colorspace value from the :MJR_COLOR package:

         |-----------------------+----------------+-----------+---------------|
         | :ano-colorspace Value | Component Type | ranges    | VTK Supported |
         |-----------------------+----------------+-----------+---------------|
         | :cs-tru               | integer        | [0,255]^3 | NO            |
         | :cs-rgb               | float          | [0,1]^3   | YES           |
         | :cs-hsv               | float          | [0,1]^3   | NO            |
         | :cs-hsl               | float          | [0,1]^3   | NO            |
         |-----------------------+----------------+-----------+---------------|

    * :ano-cpacking -- Indicates how color values are packed into each data cell.  Note that arbitrary pack/unpack functions are NOT
      supported here as they are in the :MJR_IMG library.  Rather, we er strict the possibilities to a small set of supported
      options. If missing, a value of :CP-IDENTITY is assumed.

         |---------------------+----------+------------+-----------------+----------------|
         | :ano-cpacking Value | Channels | Chan Depth | Colorspace      | VTK Supported  |
         |---------------------+----------+------------+-----------------+----------------|
         | :cp-identity        |      N/A | N/A        | :CS-RGB :CS-TRU | Yes if :CS-RGB |
         | :cp-int8x3          |        3 | 8          | :CS-TRU         | NO             |
         | :cp-int8x4          |        4 | 8          | :CS-TRU         | NO             |
         | :cp-int0x1          |        1 | N/A        | N/A             | NO             |
         |---------------------+----------+------------+-----------------+----------------|

    * :ano-nam -- a name for following array.  The value for this key is a string.

    * :ano-units -- a string that can be parsed by MJR_UNITS_CANONIZATION."
  (documentation 'mjr_annot_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_get-default-value (ano-key)
  "Return the default value for an ano-key"
  (case ano-key
    (:ano-cpacking   :cp-identity)
    (:ano-colorspace :cs-rgb)
    (:ano-typ        :ano-typ-real)
    (otherwise       nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_get-value (ano-key ano-alist)
  "Return the value for the ano-key stored in the ano-alist, or the default value if the alist doesn't contain the key."
  (let ((value (cdr (assoc ano-key  ano-alist))))
    (if value
        (values value                                 't)
        (values (mjr_annot_get-default-value ano-key) nil))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-key (ano-key)
  "Error if ano-key is not a supported value (:ano-typ :ano-colorspace :ano-cpacking :ano-nam :ano-units)"
  (if (not (member ano-key '(:ano-typ :ano-colorspace :ano-cpacking :ano-nam :ano-units)))
      (error "mjr_annot_check-ano-key: :ano-key must be one of :ano-typ, :ano-colorspace, :ano-cpacking, :ano-nam, or :ano-units")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-units (ano-units)
  "ERROR if ano-units is non-NIL and invalid."
  (if (and ano-units
           (not (ignore-errors (mjr_units_canonization "m/gm"))))
      (error "mjr_annot_check-ano-units: :ano-units was non-NIL and could not be parsed by mjr_units_canonization!")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-typ (ano-typ)
  "ERROR if ano-typ is non-NIL and invalid."
  (if ano-typ
      (if (not (member ano-typ '(:ano-typ-real :ano-typ-integer :ano-typ-complex :ano-typ-color :ano-typ-ivec :ano-typ-rvec :ano-typ-cvec)))
          (error "mjr_annot_check-ano-typ: :ano-typ must be one of :ano-typ-real, :ano-typ-integer, :ano-typ-complex, :ano-typ-color, :ano-typ-ivec, :ano-typ-rvec, or :ano-typ-cvec"))
      (warn "mjr_annot_make-alist: ano-typ is NIL!")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-nam (ano-nam)
  "ERROR if ano-nam is invalid (NIL is invalid BTW)."
    (cond ((null ano-nam)                (error "mjr_annot_check-ano-nam: :ano-nam must not be NIL"))
          ((not (stringp ano-nam))       (error "mjr_annot_check-ano-nam: :ano-nam must be a string"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-colorspace (ano-typ ano-colorspace)
  "ERROR if :ano-colorspace is invalid."
  (let ((ano-typ        (or ano-typ
                            (mjr_annot_get-default-value :ano-typ))))
    (if (equalp :ano-typ-color ano-typ)
        (cond ((null ano-colorspace)                              (warn "mjr_annot_check-ano-colorspace: :ano-colorspace should be non-NIL :ano-typ is :ano-typ-color"))
              ((not (member ano-colorspace
                            '(:cs-tru :cs-rgb :cs-hsv :cs-hsl))) (error "mjr_annot_check-ano-colorspace: :ano-colorspace must be one of :cs-tru, :cs-rgb, :cs-hsv, or :cs-hsl")))
        (if ano-colorspace
            (error "mjr_annot_check-ano-colorspace: :ano-colorspace must be NIL if :ano-typ is NOT :ano-typ-color")))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_check-ano-cpacking (ano-typ ano-colorspace ano-cpacking)
  "ERROR if :ano-cpacking is invalid."
  (let ((ano-typ        (or ano-typ
                            (mjr_annot_get-default-value :ano-typ)))
        (ano-colorspace (or ano-colorspace
                            (mjr_annot_get-default-value :ano-colorspace))))
    (if (equalp :ano-typ-color ano-typ)
        (if ano-cpacking
            (if (member ano-cpacking
                        '(:cp-identity :cp-int8x3 :cp-int8x4 :cp-int0x1))
                (if (equalp ano-cpacking :cp-identity)
                    (if (not (or (equalp ano-colorspace :CS-RGB)
                                 (equalp ano-colorspace :CS-TRU)))
                        (error "mjr_annot_check-ano-cpacking: :ano-cpacking & :ano-colorspace have incompatible values!"))
                    (if (or (equalp ano-cpacking :cp-int8x3)
                            (equalp ano-cpacking :cp-int8x4))
                        (if (not (equalp ano-colorspace :CS-TRU))
                            (error "mjr_annot_check-ano-cpacking: :ano-cpacking & :ano-colorspace have incompatible values!"))))
                (error "mjr_annot_check-ano-cpacking: :ano-cpacking must be one of :cp-identity, :cp-int8x3, :cp-int8x4, or :cp-int0x1"))
            (warn "mjr_annot_check-ano-cpacking: :ano-cpacking should be non-NIL when :ano-typ is :ano-typ-color"))
        (if ano-cpacking
            (error "mjr_annot_check-ano-cpacking: :ano-cpacking must be NIL if :ano-typ is NOT :ano-typ-color")))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_ano-typ< (key1 key2)
  "A comparison function on possible :ano-typ values.

Order definition:
  :ano-typ-color < :ano-typ-real = :ano-typ-integer < everything else

The order is compatible with the order in which data sets are required to appear in some software (like VisIT)"
                               (or (equalp key1 key2)
                                   (equalp key1 :ano-typ-color)
                                   (and (or (equalp key1 :ano-typ-real)
                                            (equalp key1 :ano-typ-integer))
                                        (not (equalp key2 :ano-typ-color)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_make-alist (&optional ano-nam ano-typ ano-colorspace ano-cpacking ano-units)
  "Stuff arguments into an alist."
  (mjr_annot_check-ano-nam ano-nam)
  (mjr_annot_check-ano-typ ano-typ)
  (mjr_annot_check-ano-colorspace ano-typ ano-colorspace)
  (mjr_annot_check-ano-cpacking ano-typ ano-colorspace ano-cpacking)
  (loop for da-key in (list :ano-nam :ano-typ :ano-colorspace :ano-cpacking :ano-units)
        for da-val in (list  ano-nam  ano-typ  ano-colorspace  ano-cpacking  ano-units)
        when da-val
        collect (cons da-key da-val)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_make-cunpacker (ano-colorspace ano-cpacking &optional (output-colorspace :cs-rgb))
  "Helper function to unpack a color vector from a value given annotations for colorspace and cpacking"
  (lambda (v) (mjr_color_convert-xxx2xxx (mjr_color_cp-unpack v ano-cpacking) ano-colorspace output-colorspace)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_make-cpacker (ano-colorspace ano-cpacking &optional (input-colorspace :cs-rgb))
  "Helper function to pack a color vector into a value given annotations for colorspace and cpacking"
  (lambda (c) (mjr_color_cp-pack (mjr_color_convert-xxx2xxx c input-colorspace ano-colorspace) ano-cpacking)))


