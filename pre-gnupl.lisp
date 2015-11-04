;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      pre-gnupl.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Plotting dquads with GNUPlot.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright 1997,1998,2004,2008,2013,2015 by Mitch Richling.  All rights reserved.
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
;; @todo      Support vector fields: plot 'file.dat' using 1:2:3:4 with vectors head filled lt 2.@EOL@EOL
;; @todo      unit-tests!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GNUPL
  (:USE :COMMON-LISP
        :MJR_UTIL
        :MJR_COLOR
        :MJR_DQUAD
        :MJR_VVEC
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Plotting dquads with GNUPlot.;")
  (:EXPORT #:mjr_gnupl_help
           #:mjr_gnupl_connect
           #:mjr_gnupl_disconnect
           #:mjr_gnupl_send-command
           #:mjr_gnupl_send-reset
           #:mjr_gnupl_dquad
           #:*mjr_gnupl_gnuplot-stream-echo*
           ;; Global variables (NOT EXPORTED)
           ;; #:*mjr_gnupl_gnuplot-stream*
           ;; #:*mjr_gnupl_gnuplot-fifo*
           ))

(in-package :MJR_GNUPL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_help ()
  "Help for MJR_GNUPL: Plotting dquads with GNUPlot

This package provides a simple interface for quickly plotting dquad lists via GNUPlot."
  (documentation 'mjr_gnupl_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-stream-echo* nil
  "Duplicate all gnuplot commands to standard out for debug")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-fifo*
  (namestring (merge-pathnames (user-homedir-pathname) ".mjrcalc-mjr_gunpl-fifo"))
  "Set this to a FIFO file connected to a running gnuplot process.  

See the gnuplotGO.sh script for one way to make sure that a gnuplot process is always running and listening to the FIFO.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_connect ()
  "If not only open, then open the GNUplot FIFO stream."
  (or *mjr_gnupl_gnuplot-stream*
      (progn (if (not *mjr_gnupl_gnuplot-fifo*)
                 (error "mjr_gnupl_send-command: No gnuplot FIFO configured (See: *MJR_GNUPL_GNUPLOT-FIFO*)!")
                 (progn (print "If the reader prompt hasn't returned, then start-up a listening gnuplot!!")
                        (format *mjr_gnupl_gnuplot-stream* "unset output~%")
                        (setq *mjr_gnupl_gnuplot-stream* (open *mjr_gnupl_gnuplot-fifo* :direction :output :if-exists  :overwrite))
                        (format *mjr_gnupl_gnuplot-stream* "set term x11~%")
                        (format *mjr_gnupl_gnuplot-stream* "reset~%")
                        (force-output *mjr_gnupl_gnuplot-stream*)
                        *mjr_gnupl_gnuplot-stream*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_disconnect ()
  "If it is open, thne we close the GNUplot FIFO stream"
  (if *mjr_gnupl_gnuplot-stream*
      (progn (ignore-errors (close *mjr_gnupl_gnuplot-stream*))
             (setq *mjr_gnupl_gnuplot-stream* nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_send-command (command &optional alternate-gnuplot-stream)
  "Send a command to gnuplot to the *MJR_GNUPL_GNUPLOT-FIFO* -- or ALTERNATE-GNUPLOT-STREAM if it is non-NIL."
  (if alternate-gnuplot-stream
      (progn (format alternate-gnuplot-stream "~a ~%" command)
             (force-output alternate-gnuplot-stream))
      (if (not (mjr_gnupl_connect))
          (error "mjr_gnupl_send-command: Could not communicate with gnuplot")
          (progn (format *mjr_gnupl_gnuplot-stream* "~a ~%" command)
                 (if *mjr_gnupl_gnuplot-stream-echo*
                     (format 't "~a ~%" command))
                 (force-output *mjr_gnupl_gnuplot-stream*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_send-reset ( &optional alternate-gnuplot-stream)
  (mjr_gnupl_send-command "unset output" alternate-gnuplot-stream)
  (mjr_gnupl_send-command "set term x11" alternate-gnuplot-stream)
  (mjr_gnupl_send-command "reset"        alternate-gnuplot-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_dquad (dquad &key data-arrays
                                (type :l) (pal "default") col
                                title xlab ylab zlab main
                                xlim ylim zlim
                                alternate-gnuplot-stream)
  "Plot a dquad object with GNUplot.

If :TITLE is NIL, then the :ANO-NAM values in the DQUAD will be usd.  To suppress titles entirely, set :TITLE to '(NIL) -- a list containing a NIL."
  (let* ((data-idxs   (or (mjr_util_non-list-then-list data-arrays)
                                  (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_data-count dquad)))))
         (data-arrays (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x       )) data-idxs))
         (data-types  (mapcar (lambda (x) (mjr_dquad_get-data-ano dquad x :ano-typ)) data-idxs))
         (data-names  (mapcar (lambda (x) (mjr_dquad_get-data-ano dquad x :ano-nam)) data-idxs))
         (num-plots   (length data-arrays))
         (title       (mjr_util_non-list-then-list title))
         (type        (mjr_util_non-list-then-list type))
         (rng-dim     (loop for plt-idx from 0 upto (1- num-plots)
                            for daElm = (mjr_arr_aref-col-major (mjr_util_elt-mod data-arrays plt-idx) 0)
                            for dtyp in data-types
                            for ptyp = (mjr_util_elt-mod type plt-idx)
                            collect (case dtyp
                                      (:ano-typ-real    1)
                                      (:ano-typ-integer 1)
                                      (:ano-typ-complex 2)
                                      (:ano-typ-color   (if (string-equal ptyp :rgb) (length daElm)))
                                      (:ano-typ-ivec    (length daElm))
                                      (:ano-typ-rvec    (length daElm))
                                      (:ano-typ-cvec    (error "mjr_gnupl_dquad: Complex vector range not supported"))
                                      (otherwise        (error "mjr_gnupl_dquad: Invalid range data type (~a)" dtyp)))))
         (axes        (mapcar (lambda (x) (mjr_dquad_get-axis-vector dquad x))
                              (concatenate 'list (mjr_vvec_to-vec-maybe (mjr_dquad_axis-count dquad)))))
         (col         (mjr_util_non-list-then-list col))
         (num-plt     (length data-arrays))
         (dat3d       (or (< 1 (length axes))
                          (some (lambda (x) (= 3 x)) rng-dim)))
         (plt3d       (and dat3d
                           (every (lambda (x) (not (or (string-equal :i x) (string-equal :rgb x)))) type))))
    ;; Setup the plot
    (mjr_gnupl_send-reset)
    (mjr_gnupl_send-command (format nil "set xlabel \"~a\"" (or xlab "")) alternate-gnuplot-stream)
    (mjr_gnupl_send-command (format nil "set ylabel \"~a\"" (or ylab "")) alternate-gnuplot-stream)
    (mjr_gnupl_send-command (format nil "set zlabel \"~a\"" (or zlab "")) alternate-gnuplot-stream)
    (mjr_gnupl_send-command (format nil "set title \"~a\""  (or main "")) alternate-gnuplot-stream)
    (mjr_gnupl_send-command "set zeroaxis" alternate-gnuplot-stream)
    (mjr_gnupl_send-command "set autoscale" alternate-gnuplot-stream)
    (mjr_gnupl_send-command "set autoscale fix" alternate-gnuplot-stream)
    (if xlim (mjr_gnupl_send-command (format nil "set xrange [~f:~f]" (first xlim) (second xlim)) alternate-gnuplot-stream))
    (if ylim (mjr_gnupl_send-command (format nil "set yrange [~f:~f]" (first ylim) (second ylim)) alternate-gnuplot-stream))
    (if zlim (mjr_gnupl_send-command (format nil "set zrange [~f:~f]" (first zlim) (second zlim)) alternate-gnuplot-stream))
    (if (and dat3d pal)
        (progn (mjr_gnupl_send-command 
                (concatenate 'string 
                             "set palette rgbformulae "
                             (or (cdr (assoc pal '(("default"   . "7,5,15")   ("ocean"   . "23,28,3")  ("hot"  . "21,22,23")
                                                   ("printable" . "30,31,32") ("rainbow" . "33,13,10") ("afm"  . "34,35,36")
                                                   ("hsv"       . "3,2,2")    ("gray"    . "3,3,3")    ("grey" . "3,3,3"))
                                             :test #'string=))
                                 pal)) alternate-gnuplot-stream)
               (mjr_gnupl_send-command "set  hidden3d" alternate-gnuplot-stream)
               ;;(mjr_gnupl_send-command "set pm3d depthorder")
               (mjr_gnupl_send-command "set style fill solid 1.00" alternate-gnuplot-stream)))
      ;; The plot command
      (mjr_gnupl_send-command  
       (with-output-to-string (ss)
         (format ss (if plt3d "splot " "plot "))
         (dotimes (plt-idx num-plt)
           (format ss "'-' using ~a ~a with ~a ~a ~a" 
                   (cond ((string-equal (mjr_util_elt-mod type plt-idx) :rgb) "1:2:3:4:5")
                         (dat3d                    "1:2:3")
                         ('t                       "1:2" ))
                   (let ((ctitle (mjr_util_elt-mod (or title data-names) plt-idx)))
                     (if ctitle
                         (format nil "title \"~a\"" ctitle)
                         (format nil "notitle")))
                   (cdr (assoc (mjr_util_elt-mod type plt-idx)
                               '((:l . "lines")    (:i . "image")  (:d . "dots") (:b   . "linespoints")
                                 (:h . "impulses") (:p . "points") (:f . "pm3d") (:rgb . "rgbimage"))
                               :test #'string-equal))
                   (if (and col (mjr_util_elt-mod col plt-idx))
                       (format nil "linecolor rgb \"~a\"" (mjr_util_elt-mod col plt-idx))
                       "")
                   (if (= plt-idx (1- num-plt)) "" ", ")))) alternate-gnuplot-stream)
      ;; Send the data
      (let* ((xaxis (first axes))
             (yaxis (second axes))
             (ddim  (if yaxis 2 1)))
        (case ddim
          (1 (dotimes (plt-idx num-plt)
               (let* ((rdim (elt rng-dim    plt-idx))
                      (dtyp (elt data-types plt-idx)))
                 (case rdim
                   (1 (loop for x across xaxis
                            for d across (nth plt-idx data-arrays)
                            do (mjr_gnupl_send-command (format nil "~F ~F " x d) alternate-gnuplot-stream)))
                   (2 (case dtyp
                        (:ano-typ-complex (loop for d across (nth plt-idx data-arrays)
                                                do (mjr_gnupl_send-command (format nil "~F ~F " (realpart d) (imagpart d)) alternate-gnuplot-stream)))
                        (otherwise        (loop for d across (nth plt-idx data-arrays)
                                                do (mjr_gnupl_send-command (format nil "~F ~F " (aref d 0) (aref d 1)) alternate-gnuplot-stream)))))
                   (3 (loop for d across (nth plt-idx data-arrays)
                            do (mjr_gnupl_send-command (format nil "~F ~F ~F " (aref d 0) (aref d 1) (aref d 2)) alternate-gnuplot-stream)))
                   (otherwise (error "mjr_gnupl_dquad: Range dimension ~d is not support with domain dimension of 1" rdim)))
                 (mjr_gnupl_send-command  "e" alternate-gnuplot-stream))))
          (2 (dotimes (plt-idx num-plt)
               (let* ((rdim (elt rng-dim plt-idx))
                      (cdat (elt data-arrays plt-idx)))
                 (case rdim
                   (1 (loop for xi from 0
                            for x across xaxis
                            do (loop for yi from 0
                                     for y across yaxis
                                     for d = (aref cdat xi yi)
                                     do (mjr_gnupl_send-command
                                         (if (string-equal (mjr_util_elt-mod type plt-idx) :rgb)
                                             (let ((pcol (if (vectorp d)
                                                             d
                                                             (mjr_color_cp-unpack-int8x3-int24 d))))
                                               (format nil "~F ~F ~F ~F ~F " x y (aref pcol 0) (aref pcol 1) (aref pcol 2)))
                                             (format nil "~F ~F ~F " x y d)) alternate-gnuplot-stream))
                            do (mjr_gnupl_send-command "" alternate-gnuplot-stream)))
                   (3 (loop for xi from 0
                            for x across xaxis
                            do (loop for yi from 0
                                     for y across yaxis
                                     for d = (aref cdat xi yi)
                                     do (mjr_gnupl_send-command (format nil "~F ~F ~F " (aref d 0) (aref d 1) (aref d 2)) alternate-gnuplot-stream))
                            do (mjr_gnupl_send-command "" alternate-gnuplot-stream)))
                   (otherwise (error "mjr_gnupl_dquad: Range dimension ~d is not support with domain dimension of 2" rdim)))
                 (mjr_gnupl_send-command  "e" alternate-gnuplot-stream))))
          (otherwise (error "mjr_gnupl_dquad: Domain dimension of ~d is not supported!" ddim))))))
