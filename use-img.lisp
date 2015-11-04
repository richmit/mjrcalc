;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-img.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,1997,2008,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     Raster image stuff.@EOL
;; @Keywords  lisp interactive graphics files tga image processing rgb raster
;; @Std       Common Lisp
;;
;;            TODO: Geometric transforms (rotate, scale, etc...)
;;            TODO: Fast convolution
;;            TODO: FFT by channel
;;            TODO: Macro to iterate over pixels (for-pixels-values  & (for-pixel-idxs
;;            TODO: DRAW: hline, vline, rect
;;            TODO: image metrics: histogram, mean, sd, etc...
;;            TODO: Think about image files and other formats beyond TGA.  Should that be in this lib or another?
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_IMG
  (:USE :COMMON-LISP
        :MJR_COLOR
        :MJR_COLORIZE
        :MJR_COMBC
        :MJR_VVEC
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Raster image stuff.;")
  (:EXPORT #:mjr_img_help
           ;; Image query
           #:mjr_img_size #:mjr_img_num-chan #:mjr_img_get-format #:mjr_img_stacked?
           ;; Pixel access
           #:mjr_img_set-px-value #:mjr_img_set-px-color #:mjr_img_set-px-rgb-color-spec
           #:mjr_img_set-px-tru-color-spec #:mjr_img_get-px-value #:mjr_img_get-px-color #:mjr_img_clear
           ;; Sub-Images
           #:mjr_img_get-sub-image #:mjr_img_set-sub-image
           ;; Image create
           #:mjr_img_make #:mjr_img_make-from-gndata #:mjr_img_make-from-func #:mjr_img_make-from-func-r2-r1
           ;; Image Processing
           #:mjr_img_homo-filter-color #:mjr_img_homo-filter-channel #:mjr_img_homo-filter-value
           #:mjr_img_convolution-filter-channel
           ;; Real Coordinates
           #:mjr_img_coord-usr2int #:mjr_img_coord-int2usr #:mjr_img_coord-draw-point
           ;; image files
           #:mjr_img_tga-write #:mjr_img_tga-read
           ;; Color packers & unpackers
           #:mjr_img_cp-pack-int8x3-int24    #:mjr_img_cp-pack-int8x4-int32    #:mjr_img_cp-pack-int0x1-int
           #:mjr_img_cp-unpack-int8x3-int24  #:mjr_img_cp-unpack-int8x4-int32  #:mjr_img_cp-unpack-num0x1-int
           ))

(in-package :MJR_IMG)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_help ()
  "Image manipulation

Images are arrays with (aref image x y ...) being the pixel that is x pixels from the left of the image and y pixels from the top of
the image.  That is to say (0,0) is the upper left pixel, and (width-1, height-1) is the lower right pixel.

Image formats:
  :img-stacked ......... 3D array of '(unsigned-byte 64) or '(real 0 1) Channels are third index
                         - The type for the value can be adjusted.
                         - Size of 3rd dimension is the number of channels.  First channel is (aref img x y 0).
                         - This format is intended for scientific applications requiring very high color depth,
                           floating point RGB values, or a large number of channels.
  :img-packed .......... 2D array of objects that can be 'unpacked' into a color.
                         - Usually the objects are FIXNUMs (but a vector might also be used).
                         - When using a FIXNUM, this is the FASTEST format and most resembles tridational image formats
                         - The default packing is TrueColor (3 channels of 8 bits representing Red, Blue, and Green)
                         - This format directly represents the most common method of encoding images in every day life,
                           and is useful for such applications.

For :img-packed images, three sets of standard packer/unpackers are provided:
  * mjr_img_cp-pack-int8x3-int24 & mjr_img_cp-unpack-int8x3-int24 ---- 3 channels each with 8-bits -- tru rgb
  * mjr_img_cp-pack-int8x4-int32 & mjr_img_cp-unpack-int8x4-int32 ---- 4 channels each with 8-bits -- tru rgba
  * mjr_img_cp-pack-int0x1-int   & mjr_img_cp-unpack-num0x1-int   ---- 1 channels each with n-bits -- greyscale"
  (documentation 'mjr_img_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-pack-int8x3-int24 (color)
  "Pack an 8-bit per pixel RGBA color into a 24bit integer"
  (declare (type (simple-vector 3) color))
  (+ (aref color 0)
     (* 256 (aref color 1))
     (* 256 256 (aref color 2))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-unpack-int8x3-int24 (value)
  "Unpack an 8-bit per pixel RGB color from a 24bit integer"
  #-ecl (declare ((unsigned-byte 24) value))
  (vector (ldb (byte 8  0) value)
          (ldb (byte 8  8) value)
          (ldb (byte 8 16) value)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-pack-int8x4-int32 (color)
  "Pack an 8-bit per pixel RGBA color into a 32bit integer"
  (declare (type (simple-vector 4) color))
  (+ (aref color 0)
     (* 256 (aref color 1))
     (* 256 256 (aref color 2))
     (* 256 256 256 (aref color 3))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-unpack-int8x4-int32 (value)
  "Unpack an 8-bit per pixel RGBA color from a 32bit integer"
  #-ecl (declare ((unsigned-byte 32) value))
  (vector (ldb (byte 8  0) value)
          (ldb (byte 8  8) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 24) value)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-pack-int0x1-int (color)
  "Unpack an n-bit per pixel greyscale color into an integer"
  (declare (type (simple-vector 1) color))
  (aref color 0))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_cp-unpack-num0x1-int (value)
  "Unpack an n-bit per pixel greyscale color from an integer"
  (declare (fixnum value))
  (vector value))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_stacked? (img)
  ""
  (< 2 (array-rank img)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_size (img)
  "List with width & height"
  (subseq (array-dimensions img) 0 2))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_num-chan (img &optional (color-unpacker #'mjr_img_cp-unpack-int8x3-int24))
  "Number of channels"
  (if (mjr_img_stacked? img)
      (array-dimension img 2)
      (length (funcall color-unpacker (aref img 0 0)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_get-format (img)
  "Return the image format.  It will be :img-stacked or :img-packed."
  (if (mjr_img_stacked? img)
      :img-stacked
      :img-packed))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_get-px-value (img x y)
  "Get the pixel value at the coordinates (x, y). No conversions, but vectors are reutnred for :img-stacked"
  (if (mjr_img_stacked? img)
      (let* ((nchan (array-dimension img 2))
             (value (make-array nchan)))
        (dotimes (i nchan value)
          (setf (aref value i) (aref img x y i))))
      (aref img x y)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_get-px-color (img x y &optional (color-unpacker #'mjr_img_cp-unpack-int8x3-int24))
  "Get the color at the coordinates (x, y)"
  (if (mjr_img_stacked? img)
      (let* ((nchan (array-dimension img 2))
             (value (make-array nchan)))
        (dotimes (i nchan value)
          (setf (aref value i) (aref img x y i))))
      (mjr_util_funcall-one-if color-unpacker (aref img x y))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_set-px-value (img x y value)
  "Set the value for the given pixel -- no conversions are made."
  (if (mjr_img_stacked? img)
      (loop for i from 0
            for v across value
            do (setf (aref img x y i) v))
      (setf (aref img x y) value)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_set-px-color (img x y color &optional (color-packer #'mjr_img_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array) -- Pseudo-Color images are not supported."
  (if (mjr_img_stacked? img)
      (loop for i from 0
            for v across color
            do (setf (aref img x y i) v))
      (setf (aref img x y) (mjr_util_funcall-one-if color-packer color))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_clear (img color &optional (color-packer #'mjr_img_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array) -- Pseudo-Color images are not supported."
  (let ((value (if color-packer
                   (funcall color-packer color)
                   color)))
    (destructuring-bind (x-wid y-wid) (mjr_img_size img)
      (loop for x from 0 upto (1- x-wid)
            finally (return img)
            do (loop for y from 0 upto (1- y-wid)
                     do (mjr_img_set-px-value img x y value))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_set-px-tru-color-spec (img x y spec &optional (color-packer #'mjr_img_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array)."
  (mjr_img_set-px-color img x y (mjr_color_make-tru-from-spec spec) color-packer))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_set-px-rgb-color-spec (img x y spec &optional (color-packer #'mjr_img_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array)."
  (mjr_img_set-px-color img x y (mjr_color_make-rgb-from-spec spec) color-packer))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_homo-filter-channel (img chan-filter &optional (color-packer #'mjr_img_cp-pack-int8x3-int24) (color-unpacker #'mjr_img_cp-unpack-int8x3-int24))
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-color img x y (map 'vector chan-filter (mjr_img_get-px-color img x y color-unpacker)) color-packer)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_homo-filter-color (img color-filter &optional (color-packer #'mjr_img_cp-pack-int8x3-int24) (color-unpacker #'mjr_img_cp-unpack-int8x3-int24))
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-color img x y (funcall color-filter (mjr_img_get-px-color img x y color-unpacker) color-packer))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_homo-filter-value (img value-filter)
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-value img x y (funcall value-filter (mjr_img_get-px-value img x y)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_convolution-filter-channel (img chan-filter &optional (color-packer #'mjr_img_cp-pack-int8x3-int24) (color-unpacker #'mjr_img_cp-unpack-int8x3-int24))
  ;; MJR TODO NOTE <2012-08-02 00:18:29 CDT> mjr_img_convolution-filter-channel: IMPLEMENT!!
  "Apply a convolution filter to the image"
  (declare (ignore img chan-filter color-packer color-unpacker)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_make (x-wid y-wid &key img-format num-chan chan-depth color-space chan-type)
  "Return a data structure for an image with the given size and img-format

The kind of image created is determined as follows:
  * If all arguments are nil, then :img-packed 24-bit images are constructed.
  * color-space overrides all parameters.
  * img-format is checked:
    * :img-packed
      If chan-type is provided, then chan-depth and num-chan are ignored.
      else if num-chan & chan-depth are provided, then chan-type is set to (list 'unsigned-byte (* num-chan chan-depth))
      else the pixel type is not locked down -- i.e. can be anything.
    * :img-stacked
      If chan-type is provided, then chan-depth and num-chan are ignored.
      else if num-chan are chan-depth provided, then num-chan is used for array size & chan-type is (list 'unsigned-byte chan-depth)
      else the pixel type is not locked down -- i.e. can be anything."
  (if color-space
      (mjr_img_make x-wid
                    y-wid
                    :img-format (cond ((equal color-space :cs-tru) :img-packed)
                                      ((equal color-space :cs-rgb) :img-stacked)
                                      (img-format                  img-format)
                                      ('t                          :img-packed))
                    :num-chan 3
                    :chan-depth (if (equal color-space :cs-tru) 8)
                    :chan-type (if (equal color-space :cs-rgbu) chan-type '(real 0 1)))
      (let ((img-format (or img-format
                            :img-packed))
            (num-chan   (or num-chan
                            3))
            (chan-depth (or chan-depth
                            8)))
        (case img-format
          (:img-packed  (cond (chan-type       (make-array (list x-wid y-wid) :element-type chan-type))
                              ((and chan-depth
                                    num-chan)  (make-array (list x-wid y-wid) :element-type (list 'unsigned-byte (* num-chan chan-depth))))
                              ('t              (make-array (list x-wid y-wid)))))
          (:img-stacked (cond (chan-type     (make-array (list x-wid y-wid num-chan) :element-type chan-type))
                              (chan-depth    (make-array (list x-wid y-wid num-chan) :element-type (list 'unsigned-byte chan-depth)))
                              ('t            (make-array (list x-wid y-wid num-chan)))))
          (otherwise   (error "mjr_img_make: img-format unsupported!"))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_coord-usr2int (img img-coord x y)
  "Convert x/y coords."
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (let* ((x0 (aref img-coord 0))
           (x1 (aref img-coord 1))
           (y0 (aref img-coord 2))
           (y1 (aref img-coord 3))
           (xw (/ (- x1 x0) x-wid))
           (yw (/ (- y1 y0) y-wid)))
      (values (truncate (- x x0) xw)
              (truncate (- y1 y) yw)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_coord-int2usr (img img-coord x y)
  "Convert x/y coords."
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (let* ((x0 (aref img-coord 0))
           (x1 (aref img-coord 1))
           (y0 (aref img-coord 2))
           (y1 (aref img-coord 3))
           (xw (/ x-wid (- x1 x0)))
           (yw (/ y-wid (- y1 y0))))
      (values (+ (* x xw) x0)
              (+ (* y yw) y0)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_coord-draw-point (img x y color img-coord &optional (color-packer #'mjr_img_cp-pack-int8x3-int24))
  "Draw a point at the given 'usr' coords."
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (let* ((x0 (aref img-coord 0))
           (x1 (aref img-coord 1))
           (y0 (aref img-coord 2))
           (y1 (aref img-coord 3))
           (xw (/ x-wid (- x1 x0)))
           (yw (/ y-wid (- y1 y0))))
      (mjr_img_set-px-color img (/ (- x x0) xw) (/ (- y y0) yw) color color-packer))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_make-from-func (f &key xdat ydat arg-mode
                               img-format num-chan chan-depth chan-type
                               (color-packer #'mjr_img_cp-pack-int8x3-int24) show-progress)
  "Sample a function (f:RxR->COLOR) on a regular 2D grid, and create an image.

Image is created with MJR_IMG_FROM-NIL, and the color returned by F must be compatible with the image created."
  (if show-progress
      (format 't "PROGRESS: mjr_img_make-from-func: Computing function values~%"))
  (let* ((xdat (mjr_vvec_to-vec-maybe xdat))
         (ydat (mjr_vvec_to-vec-maybe ydat))
         (xwid (length xdat))
         (ywid (length ydat))
         (img  (mjr_img_make xwid ywid :img-format img-format :num-chan num-chan :chan-depth chan-depth :chan-type chan-type)))
    (loop for xi from 0
          for x across xdat
          finally (return img)
          do (loop for yi from 0
                   for y across ydat
                   for fv = (mjr_util_fun-adapt-eval-v f (vector x y) arg-mode)
                   do (mjr_img_set-px-color img xi yi fv color-packer)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_make-from-gndata (zdat &key xdat ydat auto-scale cm-vars color-method color-space max-color data-range show-progress)
  "Convert a 2D array of numbers into an image of the same size.

 * Data: xdat, ydat, & zdat
   xdat & ydat Default to Z_(width) and Z_(height)
 * If :data-range is a list, then it will be used directly.  If it is 't, then the ranges will be computed.
 * cm-vars ..... Which variables to colorize
     * :in ..... The domain variables.  i.e. in f(x,y)->z, the :in vars are x & y
     * :out .... The range variables.  i.e. in f(x,y)->z, the :out var is z  DEFAULT
     * :in-out . Both :in and :out vars.  i.e. in f(x,y)->z, the :out var is x, y, & z
 * The :color-space argument is used by mjr_img_make and mjr_colorize_make-colorize-function (Default: :cs-tru)
 * mjr_colorize_make-colorize-function uses :z-color-method, :xyz-color-method, :max-color, & :color-space."
  (let* ((xwid        (array-dimension zdat 0))
         (ywid        (array-dimension zdat 1))
         (cm-vars     (or cm-vars :out))
         (color-space (or color-space :cs-tru))
         (data-range2 (or data-range
                          (make-array (case cm-vars
                                        (:out    2)
                                        (:in     4)
                                        (:in-out 6))))))
    (if (and auto-scale (null data-range))
        (progn (if show-progress
                   (format 't "PROGRESS: mjr_img_make-from-gndata: Computing max/min values~%"))

               (if (or (eq cm-vars :out) (eq cm-vars :in-out))
                   (loop for z across (make-array (apply #'* (array-dimensions zdat)) :displaced-to zdat)
                         maximize z into zmax
                         minimize z into zmin
                         finally (setf (aref data-range2 (if (eq cm-vars :out) 0 4)) zmin
                                       (aref data-range2 (if (eq cm-vars :out) 1 5)) zmax)))
               (if (or (eq cm-vars :in) (eq cm-vars :in-out))
                   (loop for yi from 0 upto (1- ywid)
                         for y = (if ydat (aref ydat yi) yi)
                         maximize y into ymax
                         minimize y into ymin
                         finally (setf (aref data-range2 2) ymin
                                       (aref data-range2 3) ymax)))
               (if (or (eq cm-vars :in) (eq cm-vars :in-out))
                   (loop for xi from 0 upto (1- xwid)
                         for x = (if xdat (aref xdat xi) xi)
                         maximize x into xmax
                         minimize x into xmin
                         finally (setf (aref data-range2 0) xmin
                                       (aref data-range2 1) xmax)))))
    (if (and show-progress auto-scale)
        (format 't "PROGRESS: mjr_img_make-from-gndata: data-range: ~a~%" data-range2))
    (if show-progress
        (format 't "PROGRESS: mjr_img_make-from-gndata: Colorize.~%"))
    (let ((img  (mjr_img_make xwid ywid :color-space color-space))
          (colf (mjr_colorize_make-colorize-function color-method color-space max-color data-range2 auto-scale)))
      (loop for yi from 0 upto (1- ywid)
            for y = (if ydat (aref ydat yi) yi)
            do (loop for xi from 0 upto (1- xwid)
                     for x = (if xdat (aref xdat xi) xi)
                     for z  = (aref zdat xi yi)
                     for c = (case cm-vars
                               (:out    (funcall colf z))
                               (:in     (funcall colf x y))
                               (:in-out (funcall colf x y z)))
                     do (mjr_img_set-px-color img xi yi c #'mjr_img_cp-pack-int8x3-int24)))
      img)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_make-from-func-r2-r1 (f &key arg-mode xdat ydat auto-scale cm-vars color-method color-space max-color show-progress)
  "Sample a function (f:RxR->R) on a regular 2D grid, and create an image.
The argument :arg-mode defaults to :arg-number. See mjr_img_make-from-gndata for info on the remaining keyword arguments."
  (or arg-mode xdat ydat auto-scale cm-vars color-method color-space max-color show-progress)
  (let* ((xdat (mjr_vvec_to-vec-maybe xdat))
         (ydat (mjr_vvec_to-vec-maybe ydat))
         (zdat (mjr_combc_gen-all-cross-product (list xdat ydat) :collect-value f :result-type :array :arg-mode (or arg-mode :arg-number))))
    (mjr_img_make-from-gndata zdat
                              :auto-scale   auto-scale   :xdat        xdat        :ydat      ydat      :cm-vars       cm-vars
                              :color-method color-method :color-space color-space :max-color max-color :show-progress show-progress)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_tga-write (out-file img &key
                          x-min x-max y-min y-max
                          (color-unpacker #'mjr_img_cp-unpack-int8x3-int24) (color-space :cs-tru)
                          show-progress)
  "Write IMG to a 24-bit TGA image file named OUT-FILE.

The x-min x-max y-min y-max arguments specify the part of the image to write to the file."
  (if show-progress
      (format 't "PROGRESS: mjr_img_tga-write: Begin TGA file write~%"))
  (destructuring-bind (img-wid img-tal) (mjr_img_size img)
    (let* ((x-max (or x-max (1- img-wid)))
           (y-max (or y-max (1- img-tal)))
           (x-min (or x-min 0))
           (y-min (or y-min 0))
           (x-wid (1+ (- x-max x-min)))
           (y-wid (1+ (- y-max y-min))))
      (with-open-file (out-d out-file :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-byte 0 out-d)                              ;; TGA header:  8-bit id length
        (write-byte 0 out-d)                              ;; TGA header:  8-bit color map type
        (write-byte 2 out-d)                              ;; TGA header:  8-bit data type code
        (write-byte 0 out-d) (write-byte 0 out-d)         ;; TGA header: 16-bit colormap origin
        (write-byte 0 out-d) (write-byte 0 out-d)         ;; TGA header: 16-bit colormap length
        (write-byte 0 out-d)                              ;; TGA header:  8-bit colormap depth
        (write-byte 0 out-d) (write-byte 0 out-d)         ;; TGA header: 16-bit x_origin
        (write-byte 0 out-d) (write-byte 0 out-d)         ;; TGA header: 16-bit y_origin
        (write-byte (ldb (byte 8 0) x-wid) out-d)         ;; LSB x-wid
        (write-byte (ldb (byte 8 8) x-wid) out-d)         ;; MSB x-wid
        (write-byte (ldb (byte 8 0) y-wid) out-d)         ;; LSB y-wid
        (write-byte (ldb (byte 8 8) y-wid) out-d)         ;; MSB y-wid
        (write-byte 24 out-d)                             ;; bits per pixel
        (write-byte 0 out-d)                              ;; image descriptor
        (loop for y from y-max downto y-min
              for i from 0
              do (if (and show-progress (zerop (mod i (if (numberp show-progress) show-progress 200))))
                     (format 't "PROGRESS: mjr_tga_from-func: written line ~5d of ~d~%"i y-wid))
              do (loop for x from x-min upto x-max
                       for color24 = (mjr_color_convert-xxx2xxx (mjr_img_get-px-color img x y color-unpacker) color-space :cs-tru)
                       do (progn (write-byte (aref color24 2) out-d)      ;; blue
                                 (write-byte (aref color24 1) out-d)      ;; green
                                 (write-byte (aref color24 0) out-d)))))) ;; red
    (if show-progress
        (format 't "PROGRESS: mjr_img_tga-write: Completed TGA file write~%"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_tga-read (file-name &key show-progress)
  "Read a 24-bit TGA file, and return an :img-packed image (See: mjr_img_help).

The resulting image will have an :img-format of :img-packed.

The full TGA specification is not supported -- the intent is that this function can read files written by MJR_IMG_TGA-WRITE.
Supported images must be truecolor (24-bit, type 2), must not have a colormap or ID, and must have all color map header values equal
to zero."
  (with-open-file (stream file-name :direction :input :element-type '(unsigned-byte 8) :if-exists :supersede :if-does-not-exist :create)
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: id length must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: colourmap type must be 0"))
    (if (not (= 2 (read-byte stream))) (error "mjr_img_tga-read: TGA header: data type code must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit colourmap origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit colourmap origin MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: colurmap length LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: colurmap length MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: colormap depth must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit x_origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit x_origin MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit y_origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_img_tga-read: TGA header: 16-bit y_origin MSB must be 0"))
    (let ((x-wid-lsb (read-byte stream))           ;; LSB x-wid
          (x-wid-msb (read-byte stream))           ;; MSB x-wid
          (y-wid-lsb (read-byte stream))           ;; LSB y-wid
          (y-wid-msb (read-byte stream)))          ;; MSB y-wid
      (if (not (= 24 (read-byte stream))) (error "mjr_img_tga-read: TGA header: bits per pixel must be 24"))
      (if (not (= 0  (read-byte stream))) (error "mjr_img_tga-read: TGA header: image descriptor must be 0"))
      (let* ((x-wid (+ x-wid-lsb (* 256 x-wid-msb)))
             (y-wid (+ y-wid-lsb (* 256 y-wid-msb)))
             (img   (mjr_img_make x-wid y-wid :img-format :img-packed :num-chan 3 :chan-depth 8)))
        (loop for y downfrom (1- y-wid) to 0
              for i from 1
              finally (return img)
              do (if (and show-progress (zerop (mod i (if (numberp show-progress) show-progress 200))))
                     (format 't "PROGRESS: mjr_img_tga-read: read line ~5d of ~d~%"i y-wid))
              do (loop for x from 0 upto (1- x-wid)
                       do (let* ((b (read-byte stream))
                                 (g (read-byte stream))
                                 (r (read-byte stream)))
                            (mjr_img_set-px-color img x y (vector r g b) #'mjr_img_cp-pack-int8x3-int24))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_ppm-write (out-file img &key
                          (x-min 0) (x-max nil) (y-min 0) (y-max nil)
                          (color-unpacker #'mjr_img_cp-unpack-int8x3-int24) (color-space :cs-tru)
                          show-progress)
  "Write IMG to a 24-bit PPM image file named OUT-FILE."
  (if show-progress
      (format 't "PROGRESS: mjr_img_ppm-write: Begin PPM file write~%"))
  (destructuring-bind (img-wid img-tal) (mjr_img_size img)
    (let* ((x-max    (or x-max (1- img-wid)))
           (y-max    (or y-max (1- img-tal)))
           (x-min    (or x-min 0))
           (y-min    (or y-min 0))
           (x-wid    (1+ (- x-max x-min)))
           (y-wid    (1+ (- y-max y-min))))
      (with-open-file (out-d out-file :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede :if-does-not-exist :create)
        (loop for c across (format nil "P6~%~d ~d~%255~%" x-wid y-wid)
              for b = (char-int c) ;; only works on ASCII LISP systems
              do (write-byte b out-d))
        (loop for y from y-min upto y-max
              for i from 1
              do (if (and show-progress (zerop (mod i (if (numberp show-progress) show-progress 200))))
                     (format 't "PROGRESS: mjr_img_ppm-write: written line ~5d of ~d~%"i y-wid))
              do (loop for x from x-min upto x-max
                       for color24 = (mjr_color_convert-xxx2xxx (mjr_img_get-px-color img x y color-unpacker) color-space :cs-tru)
                       do (write-byte (aref color24 0) out-d)   ;; red
                       do (write-byte (aref color24 1) out-d)   ;; green
                       do (write-byte (aref color24 2) out-d))) ;; blue
              (if show-progress
                  (format 't "PROGRESS: mjr_img_ppm-write: Completed PPM file write~%"))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_get-sub-image (img x-min x-max y-min y-max)
  ""
  (let* ((x-wid    (1+ (- x-max x-min)))
         (y-wid    (1+ (- y-max y-min)))
         (oimg     (make-array (list x-wid y-wid) :element-type (array-element-type img))))
    (loop for xi from x-min upto x-max
          for xo from 0
          finally (return oimg)
          do (loop for yi from y-min upto y-max
                   for yo from 0
                   do (mjr_img_set-px-value oimg xo yo (mjr_img_get-px-value img xi yi))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_img_set-sub-image (img simg x0 y0)
  ""
  (destructuring-bind (img-x-wid img-y-wid) (mjr_img_size img)
    (destructuring-bind (sub-x-wdi simg-y-wid) (mjr_img_size simg)
      (let* ((x-min x0)
             (y-min y0)
             (x-max (min (1- (+ x0 sub-x-wdi))
                         (1- img-x-wid)))
             (y-max (min (1- (+ y0 simg-y-wid))
                         (1- img-y-wid))))
        (loop for x-img from x-min upto x-max
              for x-simg from 0
              finally (return img)
              do (loop for y-img from y-min upto y-max
                       for y-simg from 0
                       do (mjr_img_set-px-value img x-img y-img (mjr_img_get-px-value img x-simg y-simg))))))))
