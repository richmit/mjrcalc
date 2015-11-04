;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-color.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Color theory (color space conversions and computations).@EOL
;; @Keywords  lisp interactive color space theory processing
;; @Std       Common Lisp
;;
;;
;;

;;----------------------------------------------------------------------------------------------------------------------------------
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
           ))

(in-package :MJR_COLOR)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_help ()
  "Color theory computations (space conversions, mixing, etc...)

For color schemes (gradiants and other schemes), see :MJR_COLORIZED

Colors are vectors with three components.  These components may represent different things depending on the color space:

    name  symbol  ranges     black    notes
    tru   :cs-tru [0,255]^3  #(0,0,0) TRU == 24-bit TRUecolor; coordinates are integers
    rgb   :cs-rgb [0,1]^3    #(0,0,0)
    hsv   :cs-hsv [0,1]^3             This space is the same as HSB; The hue is a float in $[0,1]$ and not in $[0,359]$!
    hsl   :cs-hsl [0,1]^3             Note that $V$ is normalized to a maximum value of $1$, and is not always $V<S$

Conversions functions look like: mjr_color_convert-XXX2YYY, where XXX & YYY are one of the color spaces above."
  (documentation 'mjr_color_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-rgb2hsv (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSV"
  (multiple-value-bind (maxv minv maxi mini chroma brightness lightness intensity luma hue)
      (mjr_color_metrics-rgb r-or-rgb g-or-nil b-or-nil)
    (declare (ignore maxv minv maxi mini lightness intensity luma))
    (vector hue
            (if (= chroma 0) 0 (/ chroma brightness))
            brightness)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-rgb2hsl (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSL"
  (multiple-value-bind (maxv minv maxi mini chroma brightness lightness intensity luma hue)
      (mjr_color_metrics-rgb r-or-rgb g-or-nil b-or-nil)
    (declare (ignore maxv minv maxi mini brightness intensity luma))
    (vector hue
            (if (= chroma 0) 0 (/ chroma (- 1 (abs (- (* 2 lightness) 1)))))
            lightness)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-rgb2tru (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> TRU"
  (let ((color  (if g-or-nil (vector r-or-rgb g-or-nil b-or-nil) r-or-rgb)))
    (map 'vector (lambda (cc) (floor (* 255 cc))) color)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-tru2rgb (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert TRU -> RGB"
  (let ((color  (if g-or-nil (vector r-or-rgb g-or-nil b-or-nil) r-or-rgb)))
    (map 'vector (lambda (cc) (float (+ 0/510 (/ cc 255.0)))) color)))

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-hsv2tru (h-or-hsv &optional s-or-nil v-or-nil)
  "Convert HSV -> TRU"
  (mjr_color_convert-rgb2tru (mjr_color_convert-hsv2rgb h-or-hsv s-or-nil v-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-hsl2hsv  (h-or-hsv &optional s-or-nil l-or-nil)
  "Convert HSL -> HSV"
  (mjr_color_convert-rgb2hsv (mjr_color_convert-hsl2rgb h-or-hsv s-or-nil l-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-hsl2tru  (h-or-hsl &optional s-or-nil l-or-nil)
  "Convert HSL -> TRU"
  (mjr_color_convert-rgb2tru (mjr_color_convert-hsl2rgb h-or-hsl s-or-nil l-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-hsv2hsl  (h-or-hsv &optional s-or-nil v-or-nil)
  "Convert HSV -> HSL"
  (mjr_color_convert-rgb2hsl (mjr_color_convert-hsv2rgb h-or-hsv s-or-nil v-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-tru2hsl  (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert TRU -> HSL"
  (mjr_color_convert-rgb2hsl (mjr_color_convert-tru2rgb r-or-rgb g-or-nil b-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-tru2hsv  (r-or-rgb &optional g-or-nil b-or-nil)
  "Convert RGB -> HSV"
  (mjr_color_convert-rgb2hsv (mjr_color_convert-tru2rgb r-or-rgb g-or-nil b-or-nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_convert-xxx2xxx (color in-color-space out-color-space)
  "Conver the COLOR from IN-COLOR-SPACE to OUT-COLOR-SPACE"
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

;;----------------------------------------------------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_mix-wt-avg (color1 color2 weight)
  ""
  (map 'vector (lambda (c1 c2) (+ (* c1 (- 1 weight)) (* c2 weight))) color1 color2))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_mix-avg (&rest rest)
  ""
  (let ((len (length rest)))
    (map 'vector (lambda (x) (/ x len))
         (apply #'map 'vector #'+ rest))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_mix-max (&rest rest)
  ""
  (apply #'map 'vector #'max rest))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_color_to-grey (color)
  "Convert an RGB color to a single gray value (equal weights on all channels)"
  (let ((len (length color)))
    (/ (reduce #'+ color) len)))

;;----------------------------------------------------------------------------------------------------------------------------------
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
