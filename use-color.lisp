;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-color.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Color theory (color space conversions and computations).@EOL
;; @Std       Common Lisp
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_COLOR
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_EPS
        :MJR_CMP
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Color theory (color space conversions and computations).;")
  (:EXPORT #:mjr_color_help
           ;; Color measurement
           #:mjr_color_metrics-rgb
           ;; Color operations
           #:mjr_color_mix-wt-avg #:mjr_color_mix-avg #:mjr_color_mix-max
           ;; Color stuff
           #:mjr_color_to-grey
           ;; Color construction
           #:mjr_color_make-rgb-from-spec #:mjr_color_make-tru-from-spec
           ;; Color space conversions
           ;;                      HSL                         RGB                         HSV                         TRU
           #:mjr_color_convert-rgb2hsl #:mjr_color_convert-hsv2hsl #:mjr_color_convert-tru2hsl
           #:mjr_color_convert-hsl2rgb                             #:mjr_color_convert-hsv2rgb #:mjr_color_convert-tru2rgb
           #:mjr_color_convert-hsl2hsv #:mjr_color_convert-rgb2hsv                             #:mjr_color_convert-tru2hsv
           #:mjr_color_convert-hsl2tru #:mjr_color_convert-rgb2tru #:mjr_color_convert-hsv2tru
           ;; Generic color conversion
           #:mjr_color_convert-xxx2xxx
           ;; Color packers & unpackers
           #:mjr_color_cp-pack-int8x3-int24   #:mjr_color_cp-pack-int8x4-int32   #:mjr_color_cp-pack-int0x1-int   #:mjr_color_cp-pack
           #:mjr_color_cp-unpack-int8x3-int24 #:mjr_color_cp-unpack-int8x4-int32 #:mjr_color_cp-unpack-int0x1-int #:mjr_color_cp-unpack          
         ))

(in-package :MJR_COLOR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_help ()
  "Color theory computations

For color schemes (gradients and other schemes), see :MJR_COLORIZE.  Functionality is broken up into the following categories:

A 'color' is usually a vector of channel values -- like Red, Green, and Blue.  Depending on the color space, the components may have different meanings -- RGB
vs HSL for example.  Most functions here take such vectors as colors, and some will also accept the components as arguments.  A few functions, the
mjr_color_make-*-from-spec and mjr_color_cp-unpack-*, take objects representing colors that may, or may not, be in the vector form just described -- see the
documentation about them below.

Color spaces for which special support is provided include:

  |------+---------+--------------+-----------+----------+----------------------------------------------------------------------|
  | name | symbol  | channel type | ranges    | black    | notes                                                                |
  |------+---------+--------------+-----------+----------+----------------------------------------------------------------------|
  | tru  | :cs-tru | Integer      | [0,255]^3 | #(0,0,0) | TRU == 24-bit TRUecolor                                              |
  | rgb  | :cs-rgb | Real         | [0,1]^3   | #(0,0,0) |                                                                      |
  | hsv  | :cs-hsv | Real         | [0,1]^3   |          | Equivalent to HSB; Hue is a float in $[0,1]$ and not in $[0,359]$!   |
  | hsl  | :cs-hsl | Real         | [0,1]^3   |          | $V$ is normalized to a maximum of $1$, and is not always $V<S$       |
  |------+---------+--------------+-----------+----------+----------------------------------------------------------------------|

This library provides functionality broken up into several basic categories:

 * Color measurement

 * Color operations on one or more colors (color mixing)

    - Weighted average, average, and max

 * Color stuff

    - mjr_color_to-grey

 * Color construction

    - Construct :cs-tru & :cs-rgb from color specifications
    - For how a color may be specified, see the appropriate function (mjr_color_make-XXX-from-spec)

 * Color space conversions

    - tru, rgb, hsv, & hsl

 * Generic color conversion

    Conversion functions look like: mjr_color_convert-XXX2YYY, where XXX & YYY are one of the color space names above.  Note that the function explicitly
    named MJR_COLOR_CONVERT-XXX2XXX can convert between any two color spaces.

 * Color packers & unpackers

    Many traditional image formats use a 24-bit integer to store three 8-bit channels of image information per pixel -- we say that the three, 8-bit numbers
    are 'packed' into the 24-bit integer.  The plug-in pack/unpack functions allow this library to move beyond integers, and employ arbitrary objects for the
    same task.  Note that FIXNUMs will normally be the fastest option, and provide more compatibility with external image tools.  That said, things like
    vectors of color components, HTML color codes, or even color names can prove useful for some applications.

      - For integer packing, we have three sets of functions:

          * mjr_color_cp-pack-int8x3-int24 & mjr_color_cp-unpack-int8x3-int24 ---- 3 channels each with 8-bits -- tru rgb
          * mjr_color_cp-pack-int8x4-int32 & mjr_color_cp-unpack-int8x4-int32 ---- 4 channels each with 8-bits -- tru rgba
          * mjr_color_cp-pack-int0x1-int   & mjr_color_cp-unpack-int0x1-int   ---- 1 channel with n-bits       -- greyscale

      - The IDENTITY function can be used to 'pack' color vectors into an array -- useful for scientific applications.

      - mjr_color_make-rgb-from-spec & mjr_color_make-tru-from-spec can be used to 'unpack' color specs -- handy for converting from awkward color formats
        like HTML color codes or X11 color names.

    A generic interface exists, but is a bit slower, for common packing schemes via the mjr_color_cp_pack & mjr_color_cp_unpack functions.

         |--------------+----------+------------+-----------------+-------------------------------------------------------------------|
         | Symbol       | Channels | Chan Depth | Colorspace      | pack & unpack functions                                           |
         |--------------+----------+------------+-----------------+-------------------------------------------------------------------|
         | :cp-identity |      N/A | N/A        | :CS_RGB :CS_TRU | identity                       & identity                         |
         | :cp-int8x3   |        3 | 8          | :CS_TRU         | mjr_color_cp-pack-int8x3-int24 & mjr_color_cp-unpack-int8x3-int24 |
         | :cp-int8x4   |        4 | 8          | :CS_TRU         | mjr_color_cp-pack-int8x4-int32 & mjr_color_cp-unpack-int8x4-int32 |
         | :cp-int0x1   |        1 | N/A        | N/A             | mjr_color_cp-pack-int0x1-int   & mjr_color_cp-unpack-int0x1-int   |
         |--------------+----------+------------+-----------------+-------------------------------------------------------------------|"
  (documentation 'mjr_color_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_metrics-rgb (r-or-rgb &optional g-or-nil b-or-nil)
  "Fastest way to get more than one of maxv, minv, maxi, mini, chroma, hue, brightness, or lightness."
  (multiple-value-bind (r g b) (mjr_util_get-all-elements-or-args r-or-rgb g-or-nil b-or-nil)
    (multiple-value-bind (maxv minv maxi mini) (mjr_numu_tuple-max-min r g b)
      (let* ((chroma         (- maxv minv))
             (hp             (cond ((mjr_cmp_=0 chroma)          0) ;; CAREFUL
                                   ;; MJR TODO NOTE <2011-10-06 08:39:29 > mjr_color_metrics-rgb: H should be nil if chroma==0
                                   ((= maxi 0)                  (mod (/ (- g b) chroma) 6))
                                   ((= maxi 1)                  (+ (/ (- b r) chroma) 2))
                                   ((= maxi 2)                  (+ (/ (- r g) chroma) 4))
                                   ('t                          0)))
             (hue            (/ hp 6))
             (brightness     maxv) ;; also called "value"
             (intensity      (/ (+ r g b) 3))
             (lightness      (/ (+ maxv minv) 2))  ;; Y'
             (luma           (+ (* 0.30 r) (* 0.59 g) (* b 0.11))))
        (values maxv minv maxi mini chroma brightness lightness intensity luma hue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-rgb2hsv (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSV"
  (multiple-value-bind (maxv minv maxi mini chroma brightness lightness intensity luma hue)
      (mjr_color_metrics-rgb r-or-rgb g-or-nil b-or-nil)
    (declare (ignore maxv minv maxi mini lightness intensity luma))
    (vector hue
            (if (= chroma 0) 0 (/ chroma brightness))
            brightness)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-rgb2hsl (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSL"
  (multiple-value-bind (maxv minv maxi mini chroma brightness lightness intensity luma hue)
      (mjr_color_metrics-rgb r-or-rgb g-or-nil b-or-nil)
    (declare (ignore maxv minv maxi mini brightness intensity luma))
    (vector hue
            (if (= chroma 0) 0 (/ chroma (- 1 (abs (- (* 2 lightness) 1)))))
            lightness)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-rgb2tru (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> TRU"
  (let ((color  (if g-or-nil (vector r-or-rgb g-or-nil b-or-nil) r-or-rgb)))
    (map 'vector (lambda (cc) (floor (* 255 cc))) color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-tru2rgb (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert TRU -> RGB"
  (let ((color  (if g-or-nil (vector r-or-rgb g-or-nil b-or-nil) r-or-rgb)))
    (map 'vector (lambda (cc) (float (+ 0/510 (/ cc 255.0)))) color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsv2rgb (h-or-hsv &optional s-or-nil v-or-nil)
  "Convert HSV -> RGB"
  (multiple-value-bind (h s v) (mjr_util_get-all-elements-or-args h-or-hsv s-or-nil v-or-nil)
    (if (mjr_eps_=0 s 2.3d-10) ;; Anything smaller is black in 96bit color
        (vector v v v)
        (let* ((h (* h 6))
               (i (floor h))
               (f (- h i))
               (p (* v (- 1 s )))
               (q (* v (- 1 (* s f))))
               (u (* v (- 1 (* s (- 1 f))))))
          (cond ((= i 0) (vector  v u p))
                ((= i 1) (vector  q v p))
                ((= i 2) (vector  p v u))
                ((= i 3) (vector  p q v))
                ((= i 4) (vector  u p v))
                ('t      (vector  v p q)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsl2rgb (h-or-hsl &optional s-or-nil l-or-nil)
  "Convert HSL -> RGB"
  (multiple-value-bind (h s l) (mjr_util_get-all-elements-or-args h-or-hsl s-or-nil l-or-nil)
    (if (mjr_eps_=0 s 2.3d-10) ;; Anything smaller is black in 96bit color
        (vector l l l)
        (if (mjr_eps_=0 l 2.3d-10) ;; Anything smaller is black in 96bit color
            (vector 0 0 0)
            (let* ((v  (if (<= l 1/2)
                           (* l (+ 1 s))
                           (- (+ l s) (* l s))))
                   (m  (- (* 2 l) v))
                   (sv (/ (- v m ) v))
                   (h  (* h 6))
                   (i  (floor h)) ;;(truncate h))
                   (f  (- h i))
                   (p  (* v sv f))
                   (q  (+ m p))
                   (u  (- v p)))
              (cond ((= i 1) (vector u v m))
                    ((= i 2) (vector m v q))
                    ((= i 3) (vector m u v))
                    ((= i 4) (vector q m v))
                    ((= i 5) (vector v m u))
                    ('t      (vector v q m))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsv2tru (h-or-hsv &optional s-or-nil v-or-nil)
  "Convert HSV -> TRU"
  (mjr_color_convert-rgb2tru (mjr_color_convert-hsv2rgb h-or-hsv s-or-nil v-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsl2hsv  (h-or-hsv &optional s-or-nil l-or-nil)
  "Convert HSL -> HSV"
  (mjr_color_convert-rgb2hsv (mjr_color_convert-hsl2rgb h-or-hsv s-or-nil l-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsl2tru  (h-or-hsl &optional s-or-nil l-or-nil)
  "Convert HSL -> TRU"
  (mjr_color_convert-rgb2tru (mjr_color_convert-hsl2rgb h-or-hsl s-or-nil l-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-hsv2hsl  (h-or-hsv &optional s-or-nil v-or-nil)
  "Convert HSV -> HSL"
  (mjr_color_convert-rgb2hsl (mjr_color_convert-hsv2rgb h-or-hsv s-or-nil v-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-tru2hsl  (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert TRU -> HSL"
  (mjr_color_convert-rgb2hsl (mjr_color_convert-tru2rgb r-or-rgb g-or-nil b-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-tru2hsv  (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSV"
  (mjr_color_convert-rgb2hsv (mjr_color_convert-tru2rgb r-or-rgb g-or-nil b-or-nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_convert-xxx2xxx (color in-color-space out-color-space)
  "Convert the COLOR from IN-COLOR-SPACE to OUT-COLOR-SPACE"
  (if (or (not out-color-space)
          (not in-color-space)
          (equal out-color-space in-color-space))
      color
      (case in-color-space
        (:cs-tru (case out-color-space
                   (:cs-rgb   (mjr_color_convert-tru2rgb color))
                   (:cs-hsl   (mjr_color_convert-tru2hsl color))
                   (:cs-hsv   (mjr_color_convert-tru2hsv color))
                   (otherwise (error "mjr_color_convert-xxx2xxx: Unknown value for :OUT-COLOR-SPACE!"))))
        (:cs-rgb (case out-color-space
                   (:cs-tru   (mjr_color_convert-rgb2tru color))
                   (:cs-hsl   (mjr_color_convert-rgb2hsl color))
                   (:cs-hsv   (mjr_color_convert-rgb2hsv color))
                   (otherwise (error "mjr_color_convert-xxx2xxx: Unknown value for :OUT-COLOR-SPACE!"))))
        (:cs-hsv (case out-color-space
                   (:cs-tru   (mjr_color_convert-hsv2tru color))
                   (:cs-rgb   (mjr_color_convert-hsv2rgb color))
                   (:cs-hsl   (mjr_color_convert-hsv2hsl color))
                   (otherwise (error "mjr_color_convert-xxx2xxx: Unknown value for :OUT-COLOR-SPACE!"))))
        (:cs-hsl (case out-color-space
                   (:cs-tru   (mjr_color_convert-hsl2tru color))
                   (:cs-rgb   (mjr_color_convert-hsl2rgb color))
                   (:cs-hsv   (mjr_color_convert-hsl2hsv color))
                   (otherwise (error "mjr_color_convert-xxx2xxx: Unknown value for :OUT-COLOR-SPACE!")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_make-rgb-from-spec (red-or-color-spec &optional green-or-nil blue-or-nil)
  "Return a vector of floats (RGB)

If three values are provided, then they represent Red, Green, and Blue components of the color.
  * numbers .............. Real in [0,1]

If only one argument is provided, then it must be a COLOR-SPEC:
  * color name string .. Case ignored.  Note: 0=black, b=blue, & bl=blue
  * character  ......... Case ignored. Note: #\0=black, #\1=white
  * numeric vector ..... Directly specifies the color (returned as-is)
  * list ............... Converted to vector and fed back to MJR_COLOR_MAKE-RGB-FROM-SPEC"
  (if (or green-or-nil blue-or-nil)
      (if (and green-or-nil blue-or-nil)
          (mjr_color_make-rgb-from-spec (vector red-or-color-spec green-or-nil blue-or-nil))
          (error "mjr_color_make-rgb-from-spec: One or three inputs are required -- not two!"))
      (typecase red-or-color-spec
        (character  (cond ((equalp red-or-color-spec #\0)    #(0 0 0))
                          ((equalp red-or-color-spec #\1)    #(1 1 1))
                          ((equalp red-or-color-spec #\R)    #(1 0 0))
                          ((equalp red-or-color-spec #\G)    #(0 1 0))
                          ((equalp red-or-color-spec #\B)    #(0 0 1))
                          ((equalp red-or-color-spec #\Y)    #(1 1 0))
                          ((equalp red-or-color-spec #\C)    #(0 1 1))
                          ((equalp red-or-color-spec #\M)    #(1 0 1))))
        (string     (let ((red-or-color-spec (string-upcase red-or-color-spec))
                          (red-or-color-spec-len (length red-or-color-spec)))
                      (cdr (assoc-if (lambda (s) (let ((send (min red-or-color-spec-len (length s))))
                                                   (string= red-or-color-spec s :end1 send :end2 send)))
                                     '(("0"       . #(0 0 0))
                                       ("1"       . #(1 1 1))
                                       ("WHITE"   . #(1 1 1))
                                       ("RED"     . #(1 0 0))
                                       ("GREEN"   . #(0 1 0))
                                       ("BLUE"    . #(0 0 1))
                                       ("BLACK"   . #(0 0 0))
                                       ("YELLOW"  . #(1 1 0))
                                       ("CYAN"    . #(0 1 1))
                                       ("MAGENTA" . #(1 0 1)))))))
        (vector     red-or-color-spec)
        (list       (mjr_color_make-rgb-from-spec (make-array 3 :initial-contents red-or-color-spec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_make-tru-from-spec (red-or-color-spec &optional green-or-nil blue-or-nil)
  "Return a vector of unsigned-byte (TRU)

If three values are provided, then they represent Red, Green, and Blue components of the color.
  * integer ............ Integer in [0,255]
  * HTML HEX color ..... HEX string: (i.e. a string with two hex digits)
  * float .............. Real in [0,1]

If only one argument is provided, then it must be a COLOR-SPEC:
  * integer ............ Packed integer (LSB=red)
  * HTML HEX color ..... HTML HEX string: (i.e. #rrggbb)
  * color name string .. Case ignored.  Note: 0=black, b=blue, & bl=blue
  * character .......... Case ignored. Note: #\0=black, #\1=white
  * int vector ......... Directly specifies the color (returned as-is)
  * float vector ....... [0,1] transformed to [0,255]
  * list ............... Converted to vector and fed back to MJR_COLOR_MAKE-TRU-FROM-SPEC"
  (if (or green-or-nil blue-or-nil)
      (if (and green-or-nil blue-or-nil)
          (mjr_color_make-tru-from-spec (vector red-or-color-spec green-or-nil blue-or-nil))
          (error "mjr_color_make-tru-from-spec: One or three inputs are required -- not two!"))
      (typecase red-or-color-spec
        (character  (cond ((equalp red-or-color-spec #\0)    #(0   0   0))
                          ((equalp red-or-color-spec #\1)    #(255 255 255))
                          ((equalp red-or-color-spec #\R)    #(255 0   0))
                          ((equalp red-or-color-spec #\G)    #(0   255 0))
                          ((equalp red-or-color-spec #\B)    #(0   0   255))
                          ((equalp red-or-color-spec #\Y)    #(255 255 0))
                          ((equalp red-or-color-spec #\C)    #(0   255 255))
                          ((equalp red-or-color-spec #\M)    #(255 0   255))))
        (string     (let ((red-or-color-spec (string-upcase red-or-color-spec)))
                      (if (string= "#" (subseq red-or-color-spec 0 1))
                          (vector (read-from-string (concatenate 'string "#x" (subseq red-or-color-spec 1 3)))
                                  (read-from-string (concatenate 'string "#x" (subseq red-or-color-spec 3 5)))
                                  (read-from-string (concatenate 'string "#x" (subseq red-or-color-spec 5 7))))
                          (let ((red-or-color-spec-len (length red-or-color-spec)))
                            (cdr (assoc-if (lambda (s) (let ((send (min red-or-color-spec-len (length s))))
                                                         (string= red-or-color-spec s :end1 send :end2 send)))
                                           '(("0"       . #(0   0   0))
                                             ("1"       . #(255 255 255))
                                             ("WHITE"   . #(255 255 255))
                                             ("RED"     . #(255 0   0))
                                             ("GREEN"   . #(0   255 0))
                                             ("BLUE"    . #(0   0   255))
                                             ("BLACK"   . #(0   0   0))
                                             ("YELLOW"  . #(255 255 0))
                                             ("CYAN"    . #(0   255 255))
                                             ("MAGENTA" . #(255 0   255)))))))))
        (integer    (vector (ldb (byte 8 0) red-or-color-spec) (ldb (byte 8 8) red-or-color-spec) (ldb (byte 8 16) red-or-color-spec)))
        (vector     (if (every #'integerp red-or-color-spec)
                        red-or-color-spec
                        (map 'vector (lambda (x) (max 0 (min 255 (truncate (* 255 x))))) red-or-color-spec)))
        (list       (mjr_color_make-tru-from-spec (make-array 3 :initial-contents red-or-color-spec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_mix-wt-avg (color1 color2 weight)
  "Weighted average (1-weight) for the color1 and weight for color2."
  (map 'vector (lambda (c1 c2) (+ (* c1 (- 1 weight)) (* c2 weight))) color1 color2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_mix-avg (&rest rest)
  "Average the components in each channel."
  (let ((len (length rest)))
    (map 'vector (lambda (x) (/ x len))
         (apply #'map 'vector #'+ rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_mix-max (&rest rest)
  "Maximum in each channel."
  (apply #'map 'vector #'max rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_to-grey (color)
  "Convert an RGB color to a single gray value (equal weights on all channels)"
  (let ((len (length color)))
    (/ (reduce #'+ color) len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-pack-int8x3-int24 (color)
  "Pack an 8-bit per pixel RGBA color into a 24bit integer"
  (declare (type (simple-vector 3) color))
  (+ (aref color 0)
     (* 256 (aref color 1))
     (* 256 256 (aref color 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-unpack-int8x3-int24 (value)
  "Unpack an 8-bit per pixel RGB color from a 24bit integer"
  #-ecl (declare ((unsigned-byte 24) value))
  (vector (ldb (byte 8  0) value)
          (ldb (byte 8  8) value)
          (ldb (byte 8 16) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-pack-int8x4-int32 (color)
  "Pack an 8-bit per pixel RGBA color into a 32bit integer"
  (declare (type (simple-vector 4) color))
  (+ (aref color 0)
     (* 256 (aref color 1))
     (* 256 256 (aref color 2))
     (* 256 256 256 (aref color 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-unpack-int8x4-int32 (value)
  "Unpack an 8-bit per pixel RGBA color from a 32bit integer"
  #-ecl (declare ((unsigned-byte 32) value))
  (vector (ldb (byte 8  0) value)
          (ldb (byte 8  8) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 24) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-pack-int0x1-int (color)
  "Unpack an n-bit per pixel greyscale color into an integer"
  (declare (type (simple-vector 1) color))
  (aref color 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-unpack-int0x1-int (value)
  "Unpack an n-bit per pixel greyscale color from an integer"
  (declare (fixnum value))
  (vector value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-pack (color &optional (packing :cp-identity))
  "Pack a color into a packed color value -- see mjr_color_help for details on the packing argument."
  (case packing
    (:cp-identity color)
    (:cp-int8x3   (mjr_color_cp-pack-int8x3-int24 color))
    (:cp-int8x4   (mjr_color_cp-pack-int8x4-int32 color))
    (:cp-int0x1   (mjr_color_cp-pack-int0x1-int   color))
    (otherwise    (error "mjr_color_cp-pack: Value for packing is not supported!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_color_cp-unpack (value &optional (packing :cp-identity))
  "Unpack a packed color value into a color -- see mjr_color_help for details on the packing argument."
  (case packing
    (:cp-identity value)
    (:cp-int8x3   (mjr_color_cp-unpack-int8x3-int24 value))
    (:cp-int8x4   (mjr_color_cp-unpack-int8x4-int32 value))
    (:cp-int0x1   (mjr_color_cp-unpack-int0x1-int   value))
    (otherwise    (error "mjr_color_cp-pack: Value for packing is not supported!"))))
