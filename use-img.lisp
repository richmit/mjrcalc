;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-img.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Raster image stuff.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2010,2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      Reconsider base functionality in terms of dquad package.@EOL@EOL
;; @todo      UNIT TESTS!!!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_IMG
  (:USE :COMMON-LISP
        :MJR_COLOR
        :MJR_COLORIZE
        :MJR_COMBC
        :MJR_VVEC
        :MJR_UTIL
        :MJR_DQUAD)
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
           #:mjr_img_make
           ;; Image Processing
           #:mjr_img_homo-filter-color #:mjr_img_homo-filter-channel #:mjr_img_homo-filter-value
           ;; Real Coordinates
           #:mjr_img_coord-usr2int #:mjr_img_coord-int2usr #:mjr_img_coord-draw-point
           ;; image files
           #:mjr_img_tga-write #:mjr_img_tga-read
           ))

(in-package :MJR_IMG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_help ()
  "MJR_IMG: simple image manipulation

The primary use case is image synthesis from mathematical data (scalar fields for example).  For this application we support mapping pixel coordinates to real
coordinate systems, simple pixel level drawing, homogeneous image transformations, and sub-image extraction.  Also supported is the reading and writing of
simple TGA files.

Images are arrays with (aref image x y ...) being the pixel that is x pixels from the left of the image and y pixels from the top of the image.  That is to
say (0,0) is the upper left pixel, and (width-1, height-1) is the lower right pixel.

Image formats:
  :img-stacked ......... 3D array of '(unsigned-byte 64) or '(real 0 1) Channels are third index
                         - The type for the value can be adjusted.
                         - Size of 3rd dimension is the number of channels.  First channel is (aref img x y 0).
                         - This format is intended for scientific applications requiring very high color depth, floating point RGB values, or a large 
                           number of channels.
  :img-packed .......... 2D array of objects that can be 'unpacked' into a color.
                         See the section regarding color pack/unpack in MJR_COLOR_HELP for details"
  (documentation 'mjr_img_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_stacked? (img)
  ""
  (< 2 (array-rank img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_size (img)
  "List with width & height"
  (subseq (array-dimensions img) 0 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_num-chan (img &optional (color-unpacker #'mjr_color_cp-unpack-int8x3-int24))
  "Number of channels"
  (if (mjr_img_stacked? img)
      (array-dimension img 2)
      (length (funcall color-unpacker (aref img 0 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_get-format (img)
  "Return the image format.  It will be :img-stacked or :img-packed."
  (if (mjr_img_stacked? img)
      :img-stacked
      :img-packed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_get-px-value (img x y)
  "Get the pixel value at the coordinates (x, y). No conversions, but vectors are returned for :img-stacked"
  (if (mjr_img_stacked? img)
      (let* ((nchan (array-dimension img 2))
             (value (make-array nchan)))
        (dotimes (i nchan value)
          (setf (aref value i) (aref img x y i))))
      (aref img x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_get-px-color (img x y &optional (color-unpacker #'mjr_color_cp-unpack-int8x3-int24))
  "Get the color at the coordinates (x, y)"
  (if (mjr_img_stacked? img)
      (let* ((nchan (array-dimension img 2))
             (value (make-array nchan)))
        (dotimes (i nchan value)
          (setf (aref value i) (aref img x y i))))
      (mjr_util_funcall-one-if color-unpacker (aref img x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_set-px-value (img x y value)
  "Set the value for the given pixel -- no conversions are made."
  (if (mjr_img_stacked? img)
      (loop for i from 0
            for v across value
            do (setf (aref img x y i) v))
      (setf (aref img x y) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_set-px-color (img x y color &optional (color-packer #'mjr_color_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array) -- Pseudo-Color images are not supported."
  (if (mjr_img_stacked? img)
      (loop for i from 0
            for v across color
            do (setf (aref img x y i) v))
      (setf (aref img x y) (mjr_util_funcall-one-if color-packer color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_clear (img color &optional (color-packer #'mjr_color_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array) -- Pseudo-Color images are not supported."
  (let ((value (if color-packer
                   (funcall color-packer color)
                   color)))
    (destructuring-bind (x-wid y-wid) (mjr_img_size img)
      (loop for x from 0 upto (1- x-wid)
            finally (return img)
            do (loop for y from 0 upto (1- y-wid)
                     do (mjr_img_set-px-value img x y value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_set-px-tru-color-spec (img x y spec &optional (color-packer #'mjr_color_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array)."
  (mjr_img_set-px-color img x y (mjr_color_make-tru-from-spec spec) color-packer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_set-px-rgb-color-spec (img x y spec &optional (color-packer #'mjr_color_cp-pack-int8x3-int24))
  "Set the image pixel.  Color MUST be an valid color (i.e. an array)."
  (mjr_img_set-px-color img x y (mjr_color_make-rgb-from-spec spec) color-packer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_homo-filter-channel (img chan-filter &optional (color-packer #'mjr_color_cp-pack-int8x3-int24) (color-unpacker #'mjr_color_cp-unpack-int8x3-int24))
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-color img x y (map 'vector chan-filter (mjr_img_get-px-color img x y color-unpacker)) color-packer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_homo-filter-color (img color-filter &optional (color-packer #'mjr_color_cp-pack-int8x3-int24) (color-unpacker #'mjr_color_cp-unpack-int8x3-int24))
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-color img x y (funcall color-filter (mjr_img_get-px-color img x y color-unpacker) color-packer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_homo-filter-value (img value-filter)
  "Apply a filter to each pixel of the image"
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (dotimes (y y-wid img)
      (dotimes (x x-wid)
        (mjr_img_set-px-value img x y (funcall value-filter (mjr_img_get-px-value img x y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_make (x-wid y-wid &key img-format num-chan chan-depth color-space chan-type)
  "Return a data structure for an image with the given size and img-format

The kind of image created is determined as follows:
  * If all arguments are nil, then :img-packed 24-bit images are constructed.
  * color-space overrides all parameters.
  * img-format is checked:
    * :img-packed
      If chan-type is provided, then chan-depth and num-chan are ignored.  If num-chan & chan-depth are provided, then chan-type is set to (list
      'unsigned-byte (* num-chan chan-depth)) else the pixel type is not locked down -- i.e. can be anything.
    * :img-stacked
      If chan-type is provided, then chan-depth and num-chan are ignored. If num-chan and chan-depth provided, then num-chan is used for array size &
      chan-type is (list 'unsigned-byte chan-depth) else the pixel type is not locked down -- i.e. can be anything."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_coord-draw-point (img x y color img-coord &optional (color-packer #'mjr_color_cp-pack-int8x3-int24))
  "Draw a point at the given 'usr' coords."
  (destructuring-bind (x-wid y-wid) (mjr_img_size img)
    (let* ((x0 (aref img-coord 0))
           (x1 (aref img-coord 1))
           (y0 (aref img-coord 2))
           (y1 (aref img-coord 3))
           (xw (/ x-wid (- x1 x0)))
           (yw (/ y-wid (- y1 y0))))
      (mjr_img_set-px-color img (/ (- x x0) xw) (/ (- y y0) yw) color color-packer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_tga-write (out-file img &key
                          x-min x-max y-min y-max
                          (color-unpacker #'mjr_color_cp-unpack-int8x3-int24) (color-space :cs-tru)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_tga-read (file-name &key show-progress)
  "Read a 24-bit TGA file, and return an :img-packed image (See: mjr_img_help).

The resulting image will have an :img-format of :img-packed.

The full TGA specification is not supported -- the intent is that this function can read files written by MJR_IMG_TGA-WRITE.  Supported images must be
truecolor (24-bit, type 2), must not have a colormap or ID, and must have all color map header values equal to zero."
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
                            (mjr_img_set-px-color img x y (vector r g b) #'mjr_color_cp-pack-int8x3-int24))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_img_ppm-write (out-file img &key
                          (x-min 0) (x-max nil) (y-min 0) (y-max nil)
                          (color-unpacker #'mjr_color_cp-unpack-int8x3-int24) (color-space :cs-tru)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
