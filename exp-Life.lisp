;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-Life.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Read in a life RLE file, and iterate (dumping a TGA at each step) until the pattern cycles.@EOL
;; @Keywords  
;; @Std       Common Lisp, Life RLE
;;
;;            Render a movie like this:
;;               for f in exLife-OUT-???.tga; do convert -colorspace RGB -sample 400% -type TrueColor $f `echo $f | sed 's/.tga$/.png/'`; done
;;               ffmpeg -pix_fmt rgb24 -t 72 -r 12 -loop_output 0 -pix_fmt rgb24 -i exLife-OUT-%03d.png exLife-ART.gif
;;               mplayer exLife-ART.gif
;;               convert -define gif:size=200x200 exLife-ART.gif -thumbnail '200x200>' -background white -gravity Center -extent 190x190 exLife-ART-t.gif
;;               rm exLife-exLife-OUT-???d.png
;;

;;----------------------------------------------------------------------------------------------------------------------------------

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
                                  (img (mjr_img_make xwid ywid)))
                              (flet ((don (live num)
                                       (loop for i from 1 upto num
                                             do (setf (aref img x y) (if live #xFFFFFF #x000000))
                                             do (incf x))
                                       (setf len 1)
                                       (incf p)))

                                (loop for c = (aref rledata p)
                                      do (if shop (format 't "~5a ~5a ~5a ~5a ~5a ~15a ~%" x y p c len (subseq rledata p (min (+ 15 p) (1- (length rledata))))))
                                      do (cond ((equal c #\b)       (don nil len))
                                               ((equal c #\o)       (don 't len))
                                               ((equal c #\$)       (progn (setf x 0) (incf y) (incf p)))
                                               ((mjr_char_digitsp c) (multiple-value-bind (nlen npos) (parse-integer rledata :start p :junk-allowed 't)
                                                                       (setf p   npos
                                                                             len nlen))))
                                      until (equal c #\!)
                                      finally (return img)))))))))
             (xmax  (+ padn padn (first (mjr_img_size shap))))
             (ymax  (+ padn padn (second (mjr_img_size shap))))
             (seed  (mjr_img_make xmax ymax :color-space :cs-tru))
             (img1  (mjr_img_make xmax ymax :color-space :cs-tru))
             (img2  (mjr_img_make xmax ymax :color-space :cs-tru)))
        (declare (fixnum xmax ymax))
        (dotimes (y (- ymax padn padn))
          (dotimes (x (- xmax padn padn))
            (setf (aref seed (+ padn x) (+ padn y)) (aref shap x y)
                  (aref img1 (+ padn x) (+ padn y)) (aref shap x y))))
        (loop with imgeq = nil
              for i from 1 upto 200
              for old-img = (if (evenp i) img2 img1)
              for new-img = (if (evenp i) img1 img2)
              do (setq imgeq 't)
              do (if shop (format 't "CYCLE: ~10a~%" i))
              do (loop for y fixnum from 0 upto (1- ymax)
                       do (loop for x fixnum from 0 upto (1- xmax)
                                for nc = (loop for xd in '(-1  0  1 -1 1 -1 0 1)
                                               for yd in '(-1 -1 -1  0 0  1 1 1)
                                               count (not (zerop (aref old-img (mod (+ x xd) xmax) (mod (+ y yd) ymax)))))
                                do (if (zerop (aref old-img x y))
                                       (if (= nc 3)
                                           (setf (aref new-img x y) #xFFFFFF)
                                           (setf (aref new-img x y) #x000000))
                                       (if (or (< nc 2) (> nc 3))
                                           (setf (aref new-img x y) #x000000)
                                           (setf (aref new-img x y) #xFFFFFF)))
                                do (if (not (equal (aref new-img x y) (aref seed x y)))
                                       (setq imgeq nil))))                                       
              do (mjr_img_tga-write (format nil "exp-Life-OUT-~3,'0d.tga" i) new-img)
              until imgeq)))
