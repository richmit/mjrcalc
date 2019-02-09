;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-tga.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Raster image stuff.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,2008,2010,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_TGA
  (:USE :COMMON-LISP
        :MJR_COLOR
        :MJR_ANNOT
        :MJR_DQUAD)
  (:DOCUMENTATION "Brief: Read and write TGA files.;")
  (:EXPORT #:mjr_tga_help
           #:mjr_tga_from-array
           #:mjr_tga_to-array
           #:mjr_tga_from-dquad
           ))

(in-package :MJR_TGA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_tga_help ()
  "MJR_TGA: Read and write TGA files."
  (documentation 'mjr_tga_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_tga_from-array (out-file img-array &key color-space color-packing x-min x-max y-min y-max show-progress)
  "Write array containing colors or packed colors to a 24-bit TGA image file named OUT-FILE.

The x-min x-max y-min y-max arguments specify the part of the image to write to the file."
  (if show-progress
      (format 't "PROGRESS: mjr_tga_from-array: Begin TGA file write~%"))
  (let* ((cuc     (mjr_color_make-unpacker-color-space-converter-and-packer color-packing color-space :cs-tru :cp-none))
         (img-wid (array-dimension img-array 0))
         (img-tal (array-dimension img-array 1))
         (x-max   (or x-max (1- img-wid)))
         (y-max   (or y-max (1- img-tal)))
         (x-min   (or x-min 0))
         (y-min   (or y-min 0))
         (x-wid   (1+ (- x-max x-min)))
         (y-wid   (1+ (- y-max y-min))))
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
                     for color24 = (funcall cuc (aref img-array x y))
                     do (progn (write-byte (aref color24 2) out-d)      ;; blue
                               (write-byte (aref color24 1) out-d)      ;; green
                               (write-byte (aref color24 0) out-d))))) ;; red
    (if show-progress
        (format 't "PROGRESS: mjr_tga_from-array: Completed TGA file write~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_tga_to-array (file-name &key (ano-typ :ano-typ-truint) show-progress)
  "Read a 24-bit TGA file, and return an array sutiable for inclusion into a dquad."
  (with-open-file (stream file-name :direction :input :element-type '(unsigned-byte 8) :if-exists :supersede :if-does-not-exist :create)
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: id length must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: colourmap type must be 0"))
    (if (not (= 2 (read-byte stream))) (error "mjr_tga_to-array: TGA header: data type code must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit colourmap origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit colourmap origin MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: colurmap length LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: colurmap length MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: colormap depth must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit x_origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit x_origin MSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit y_origin LSB must be 0"))
    (if (not (= 0 (read-byte stream))) (error "mjr_tga_to-array: TGA header: 16-bit y_origin MSB must be 0"))
    (let ((x-wid-lsb (read-byte stream))           ;; LSB x-wid
          (x-wid-msb (read-byte stream))           ;; MSB x-wid
          (y-wid-lsb (read-byte stream))           ;; LSB y-wid
          (y-wid-msb (read-byte stream)))          ;; MSB y-wid
      (if (not (= 24 (read-byte stream))) (error "mjr_tga_to-array: TGA header: bits per pixel must be 24"))
      (if (not (= 0  (read-byte stream))) (error "mjr_tga_to-array: TGA header: image descriptor must be 0"))
      (let* ((x-wid (+ x-wid-lsb (* 256 x-wid-msb)))
             (y-wid (+ y-wid-lsb (* 256 y-wid-msb)))
             (img   (make-array (list x-wid y-wid)))
             (cuc     (mjr_color_make-unpacker-color-space-converter-and-packer :cp-none
                                                                                :cs-tru
                                                                                (mjr_annot_get-colorspace   ano-typ)
                                                                                (mjr_annot_get-colorpacking ano-typ))))
        (loop for y downfrom (1- y-wid) to 0
              for i from 1
              finally (return img)
              do (if (and show-progress (zerop (mod i (if (numberp show-progress) show-progress 200))))
                     (format 't "PROGRESS: mjr_tga_to-array: read line ~5d of ~d~%"i y-wid))
              do (loop for x from 0 upto (1- x-wid)
                       do (let* ((b (read-byte stream))
                                 (g (read-byte stream))
                                 (r (read-byte stream)))
                            (setf (aref img x y) (funcall cuc (vector r g b))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_tga_from-dquad (out-file dquad &key data x-min x-max y-min y-max show-progress)
  "Write IMG in a dquad to a 24-bit TGA image file named OUT-FILE.

The x-min x-max y-min y-max arguments specify the part of the image to write to the file."
  (if show-progress
      (format 't "PROGRESS: mjr_tga_from-dquad: Begin TGA file write~%"))
  (let* ((data          (or data
                            (car (loop for idx from 0 upto (1- (mjr_dquad_axis-count dquad))
                                       when (mjr_annot_typ-colorp (mjr_dquad_get-data-ano dquad idx :ano-typ))
                                       collect idx))))
         (datt          (mjr_dquad_get-data-ano dquad data :ano-typ))
         (img-array     (mjr_dquad_get-data-array dquad data))
         (color-packing (mjr_annot_get-colorpacking datt))
         (color-space   (mjr_annot_get-colorspace   datt)))
    (mjr_tga_from-array out-file img-array :color-space color-space :color-packing color-packing :x-min x-min :x-max x-max :y-min y-min :y-max y-max :show-progress show-progress)))
