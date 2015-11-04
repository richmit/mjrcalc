;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-colorized.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010 by Mitch Richling.  All rights reserved.
;; @brief     Colorization of discrete spaces (Z_n).@EOL
;; @Keywords  lisp interactive color schemes gradient discrete Z
;; @Std       Common Lisp
;;
;;            TODO: Add some color brewer-like schemes (by index and pallet)
;;            TODO: Add some schemes for color blind people (by index and pallet)
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_COLORIZED
  (:USE :COMMON-LISP
        :MJR_COLOR)
  (:DOCUMENTATION "Brief: Colorization of discrete spaces (Z_n).;")
  (:EXPORT #:mjr_colorized_help
           ;; Utilities: Gradients & Pallets
           #:mjr_colorized_ut-tru-from-gradient
           #:mjr_colorized_ut-tru-from-pallet       #:mjr_colorized_ut-pallet-length
           #:mjr_colorized_ut-pallet-from-gradient  #:mjr_colorized_ut-gradient-length
           ;; Colorize via 16-bit povray
           #:mjr_colorized_povray
           ;; Colorize Function Factories: Gradients & Pallets
           #:mjr_colorized_factory-from-gradient
           #:mjr_colorized_factory-from-pallet
           ))

(in-package :MJR_COLORIZED)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_help ()
 "Colorize desecrate spaces of dimensions one (i.e. $Z_n=\{0,1,...,n-1\}$ -- used for visualization.

All colors are returned as :cs-tru (i.e. RGB components are integers in [0,255]).

For real RGB components in [0,1], see :MJR_COLORIZER.
For color theory computations (space conversions, mixing, etc...), see :MJR_COLOR

Such color schemes are frequently based on gradients, and several common gradients included (see ramCanvas for more info):

Several common gradients include:
  * 0GR ....... Povray
  * RYGCBMR ... cmpClrCubeRainbow
  * CR ........ cmpDiagRampCR
  * MG ........ cmpDiagRampMG
  * YB ........ cmpDiagRampYB
  * CMYC ...... cmpConstTwoRamp
  * BRGB ...... cmpConstOneRamp
  * 0W ........ cmpGreyRGB
  * YC ........ cmpUpDownRampBr
  * YM ........ cmpUpDownRampBg
  * MC ........ cmpUpDownRampGr
  * MY ........ cmpUpDownRampGb
  * CM ........ cmpUpDownRampRg
  * CY ........ cmpUpDownRampRb
  * 0RYW ...... cmpSumRampRGB
  * 0BCW ...... cmpSumRampBGR
  * 0GYW ...... cmpSumRampGRB
  * 0GCW ...... cmpSumRampGBR
  * 0BMW ...... cmpSumRampBRG
  * 0RMW ...... cmpSumRampRBG
  * BCGYR ..... cmpColdToHot
  * WCBYR ..... cmpIceToWaterToHot"
  (documentation 'mjr_colorized_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_ut-gradient-length (gradient)
  ""
  (let* ((len (length gradient))
         (sln (if (= len 1)
                  1
                  (- (* 256 (- len 1)) (- len 2)))))
    (values sln len)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_ut-tru-from-gradient (x &optional (gradient "RYGCBMR"))
  ""
  (let ((x (truncate x)))
    (multiple-value-bind (sln len) (mjr_colorized_ut-gradient-length gradient)
      (cond ((>= (+ x 1) sln) (mjr_color_make-tru-from-spec (aref gradient (1- len))))
            ((<= x 0)         (mjr_color_make-tru-from-spec (aref gradient 0)))
            ('t               (let* ((wid  (/ (1- sln) (- len 1)))
                                     (buk  (floor (/ x wid)))
                                     (xn   (* wid buk))
                                     (cn   (mjr_color_make-tru-from-spec (aref gradient buk)))
                                     (cn+1 (mjr_color_make-tru-from-spec (aref gradient (1+ buk))))
                                     (d    (/ (- x xn) wid)))
                                (map 'vector (lambda (c1 c2) (truncate (+ (* c1 (- 1 d)) (* c2 d)))) cn cn+1)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_ut-pallet-from-gradient (gradient)
  ""
  (let ((len (mjr_colorized_ut-gradient-length gradient)))
    (make-array len :initial-contents (loop for i from 0 upto (1- len)
                                            collect (mjr_colorized_ut-tru-from-gradient i gradient)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_ut-pallet-length (pallet)
  ""
  (length pallet))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_ut-tru-from-pallet (i pallet &optional (i-over :clip) (i-under :clip))
  "Return the I'th color from PALLET using the I-OVER/I-UNDER behavior for out of range values of I.

Possible behaviors when index is out of range:
  * :recycle
  * :clip
  * :error"
  (let ((i   (truncate i))
        (len (mjr_colorized_ut-pallet-length pallet)))
    (cond ((< i 0)        (case i-under
                            (:recycle (aref pallet (mod i len)))
                            (:clip    (aref pallet 0))
                            (:error   (error "mjr_colorized_ut-tru-from-pallet: i too small!"))))
          ((> i (1- len)) (case i-over
                            (:recycle (aref pallet (mod i len)))
                            (:clip    (aref pallet (1- len)))
                            (:error   (error "mjr_colorized_ut-tru-from-pallet: i too big!"))))
          ('t             (aref pallet i)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_povray (value)
  "Convert an number in $[0,2^{16}-1]$ into a :cs-tru color representing a povray height."
  (if (integerp value)
      (vector (ldb (byte 8 8) value) (ldb (byte 8 0) value) 0)
      (mjr_colorized_povray (truncate value))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_factory-from-pallet (pallet)
  "Return a function that takes an integer, and returns a color"
  (lambda (i) (mjr_colorized_ut-tru-from-pallet i pallet)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_colorized_factory-from-gradient (gradient)
  "Return a function that takes an integer, and returns a color.
NOTE: The resulting function will be much faster than repeatedly calling mjr_colorized_ut-tru-from-pallet."
    (mjr_colorized_factory-from-pallet (mjr_colorized_ut-pallet-from-gradient gradient)))
