;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      pre-annot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Support for :MJR_DQUAD and :MJR_DSIMP.@EOL
;; @Keywords  lisp interactive annot
;; @Std       Common Lisp
;;
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_ANNOT
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Geometric Data Sets: Supporting :MJR_DSIMP and :MJR_DQUAD.;")
  (:EXPORT #:mjr_annot_help ()
           
           #:mjr_annot_valid-ano-colorspace
           #:mjr_annot_valid-ano-typ
           #:mjr_annot_valid-meta
           
           #:mjr_annot_ano-typ<
           ))

(in-package :MJR_ANNOT)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_help ()
  "Help for MJR_ANNOT: Meta package supporting :MJR_DSIMP & :MJR_DQUAD

The :MJR_DSIMP and :MJR_DQUAD packages both deal with data sets defined on geometric objects (simplicial sets in the first case and
quadrilateral grids in the second).  As such the have some common needs like meta data.  Meta data objects in both libaries are
alists with the following possible keys:

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

    * :ano-colorspace -- May only be present if :ano-typ is :ano-typ-color.  Note that this means it may never be present in axis-meta.
      The value of this item indicates the color space used, and it must be a valid colorspace value from the :MJR_COLOR package:

         |-----------------------+----------------+-----------+---------------|
         | :ano-colorspace Value | Component Type | ranges    | VTK Supported |
         |-----------------------+----------------+-----------+---------------|
         | :cs-tru               | integer        | [0,255]^3 | NO            |
         | :cs-rgb               | float          | [0,1]^3   | YES           |
         | :cs-hsv               | float          | [0,1]^3   | NO            |
         | :cs-hsl               | float          | [0,1]^3   | NO            |
         |-----------------------+----------------+-----------+---------------|

    * :ano-nam -- a name for following array.  The value for this key is a string."
  (documentation 'mjr_annot_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_valid-ano-colorspace (ano-colorspace)
  "Return non-nil if ano-colorspace is valid."
  (member ano-colorspace '(:cs-tru :cs-rgb :cs-hsv :cs-hsl)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_valid-ano-typ (ano-typ)
  "Return non-nil if ano-typ is valid."
  (member ano-typ '(:ano-typ-real :ano-typ-integer :ano-typ-complex :ano-typ-color :ano-typ-ivec :ano-typ-rvec :ano-typ-cvec)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_annot_valid-meta (meta-alist)
  "Return non-nil if ano-typ is valid."
  (and (listp meta-alist)                                                                  ;; It is a list!
       meta-alist                                                                          ;; It is not empty!
       (assoc :ano-nam meta-alist)                                                         ;; Have a name
       (stringp (car (assoc :ano-nam meta-alist)))                                         ;; Name is string
       (assoc :ano-typ meta-alist)                                                         ;; Have a type
       (mjr_annot_valid-ano-typ (car (assoc :ano-typ meta-alist)))                         ;; Type value is valid
       (if (equalp :ano-typ-color (car (assoc :ano-typ meta-alist)))                        
           (and (assoc :ano-colorspace meta-alist)                                         ;; It is a color and we have a colorspace
                (mjr_annot_valid-ano-colorspace (cdr (assoc :ano-colorspace meta-alist)))) ;; It is a color and the colorspace is valid
           (not (assoc :ano-colorspace meta-alist)))))                                     ;; It is a color and we have no colorspace

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
