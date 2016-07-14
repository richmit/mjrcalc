;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-Life.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Read in a life RLE file, and iterate (dumping a TGA at each step) until the pattern cycles.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @filedetails
;;
;;  Render a movie like this:
;;     convert exp-Life-OUT-*.tga -sample 800% exp-Life-ART.gif  
;;     convert -define gif:size=200x200 exp-Life-ART.gif -thumbnail '200x200>' -background white -gravity Center -extent 190x190 exp-Life-ART-t.gif
;;     rm exp-Life-OUT-???.tga
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time (let* ((shop  't)
             (padn  10)
             (shap  (with-open-file (in-file "exp-Life-IN.rle" :direction :input)
                      (let* ((header  (loop for line = (read-line in-file nil)
                                            while line
                                            when (not (equal #\# (aref line 0)))
                                            do (return line)))
                             (rledata (with-output-to-string (str-strm)
                                        (loop for char = (read-char in-file nil)
                                              while char
                                              when (mjr_char_in-class char "0123456789bo$!")
                                              do (format str-strm  "~a" char)))))
                        (if shop (format 't "HEADER: ~a~%" header))
                        (if shop (format 't "   RLE: ~a~%" rledata))
                        (multiple-value-bind (xwid pos1) (parse-integer header :start (position-if #'mjr_char_digitsp header) :junk-allowed 't)
                          (multiple-value-bind (ywid) (parse-integer header :start (position-if #'mjr_char_digitsp header :start pos1) :junk-allowed 't)
                            (if shop (format 't "     X: ~a~%" xwid))
                            (if shop (format 't "     Y: ~a~%" ywid))
                            (let ((x   0)
                                  (y   0)
                                  (len 1)
                                  (p   0)
                                  (img (make-array (list xwid ywid) :initial-element 0)))
                              (flet ((don (live num)
                                       (loop for i from 1 upto num
                                             do (setf (aref img x y) (if live 1 0))
                                             do (incf x))
                                       (setf len 1)
                                       (incf p)))
                                (loop for c = (aref rledata p)
                                      do (if shop (format 't "~5a ~5a ~5a ~5a ~5a ~15a ~%" x y p c len (subseq rledata p (min (+ 15 p) (1- (length rledata))))))
                                      do (cond ((equal c #\b)       (don nil len))
                                               ((equal c #\o)       (don 't len))
                                               ((equal c #\$)       (progn (setf x 0) (incf y len) (incf p) (setf len 1)))
                                               ((mjr_char_digitsp c) (multiple-value-bind (nlen npos) (parse-integer rledata :start p :junk-allowed 't)
                                                                       (setf p   npos
                                                                             len nlen))))
                                      until (equal c #\!)
                                      finally (return img)))))))))
             (xmax  (+ padn padn (first (array-dimensions shap))))
             (ymax  (+ padn padn (second (array-dimensions shap))))
             (seed  (make-array (list xmax ymax)))
             (img1  (make-array (list xmax ymax)))
             (img2  (make-array (list xmax ymax))))
        (declare (fixnum xmax ymax))
        (dotimes (y (- ymax padn padn))
          (dotimes (x (- xmax padn padn))
            (setf (aref seed (+ padn x) (+ padn y)) (aref shap x y)
                  (aref img1 (+ padn x) (+ padn y)) (aref shap x y))))

        (mjr_tga_from-array "exp-Life-OUT-000.tga" seed :color-space :cs-bit :color-packing :cp-none)
        
        (loop for i from 1 upto 200
              for old-img = (if (evenp i) img2 img1)
              for new-img = (if (evenp i) img1 img2)
              do (if shop (format 't "CYCLE: ~10a~%" i))
              do (loop for y fixnum from 0 upto (1- ymax)
                       do (loop for x fixnum from 0 upto (1- xmax)
                                for nc = (loop for xd in '(-1  0  1 -1 1 -1 0 1)
                                               for yd in '(-1 -1 -1  0 0  1 1 1)
                                               count (not (zerop (aref old-img (mod (+ x xd) xmax) (mod (+ y yd) ymax)))))
                                do (if (zerop (aref old-img x y))
                                       (if (= nc 3)
                                           (setf (aref new-img x y) 1)
                                           (setf (aref new-img x y) 0))
                                       (if (or (< nc 2) (> nc 3))
                                           (setf (aref new-img x y) 0)
                                           (setf (aref new-img x y) 1)))))
              do (mjr_tga_from-array (format nil "exp-Life-OUT-~3,'0d.tga" i) new-img :color-space :cs-bit :color-packing :cp-none)
              until (if (equalp new-img old-img)
                        (progn (format 't "STABLE STATE FOUND: ~d~%" i)
                               't))
              until (if (equalp new-img seed)
                        (progn (format 't "PERIOD FOUND: ~d~%" i)
                               't)))))
