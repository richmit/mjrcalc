;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-gnupl.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Plotting dquads with GNUPlot.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright 1997,1998,2004,2008,2013,2015,2019 by Mitch Richling.  All rights reserved.
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
;; @todo      Add support for NaN values in dquads!!!  set datafile missing "NaN"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GNUPL
  (:USE :COMMON-LISP
        :MJR_UTIL
        :MJR_COLOR
        :MJR_DQUAD
        :MJR_DSIMP
        :MJR_ANNOT
        :MJR_VVEC
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Plotting dquads with GNUPlot.;")
  (:EXPORT #:mjr_gnupl_help
           #:mjr_gnupl_connect
           #:mjr_gnupl_disconnect
           #:mjr_gnupl_send-command
           #:mjr_gnupl_send-reset
           #:mjr_gnupl_dquad
           #:mjr_gnupl_dsimp
           #:*mjr_gnupl_gnuplot-stream-echo*
           #:*mjr_gnupl_gnuplot-connect-method*
           ;; Global variables (NOT EXPORTED)
           ;; #:*mjr_gnupl_gnuplot-term* 
           ;; #:*mjr_gnupl_gnuplot-stream*
           ;; #:*mjr_gnupl_gnuplot-fifo-name*
           ;; #:*mjr_gnupl_gnuplot-exec-path*
           ))

(in-package :MJR_GNUPL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_help ()
  "Help for MJR_GNUPL: Plotting data (DQUADS, DSIMP, BQTREE) with GNUPlot

This package provides a simple interface for quickly plotting dquad lists via GNUPlot."
  (documentation 'mjr_gnupl_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-stream-echo* nil
  "Duplicate all gnuplot commands to standard out for debug")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-connect-method* :mjr_gnupl_use-exec
  "Method to use for connecting to gnuplot.  
      * :mjr_gnupl_use-exec -- start a gnuplot process and connect to it
      * :mjr_gnupl_use-fifo -- connect to a fifo that is already connected to a running gnuplot")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-term* (if (find :unix *features*) "x11")
  "Set to the gnuplot terminal")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-exec-path* (find-if #'probe-file '("/usr/local/bin/gnuplot"
                                                              "/usr/bin/gnuplot"
                                                              "/bin/gnuplot"
                                                              "/opt/local/bin/gnuplot"
                                                              "/opt/bin/gnuplot"
                                                              "C:\\msys64\\mingw64\\bin\\gnuplot.exe"
                                                              "C:\\msys32\\mingw64\\bin\\gnuplot.exe"))
    "Set this to a the fully qualified filename of the gnuplot executable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mjr_gnupl_gnuplot-fifo-name* (if (probe-file (user-homedir-pathname))
                                          (namestring (merge-pathnames (user-homedir-pathname) ".mjrcalc-mjr_gunpl-fifo")))
  "Set this to a FIFO file name connected to a running gnuplot process.

See the gnuplotGO.sh script for one way to make sure that a gnuplot process is always running and listening to the FIFO.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_connect ()
  "If not already open, then open the GNUplot FIFO stream."
  (or *mjr_gnupl_gnuplot-stream*
      (case *mjr_gnupl_gnuplot-connect-method*
        (:mjr_gnupl_use-fifo (if (not (and *mjr_gnupl_gnuplot-fifo-name* (probe-file *mjr_gnupl_gnuplot-fifo-name*)))
                                 (error "mjr_gnupl_connect: Can't use *mjr_gnupl_gnuplot-fifo-name*")
                                 (progn (print "If the reader prompt hasn't returned, then start-up a listening gnuplot!!")
                                        (format *mjr_gnupl_gnuplot-stream* "unset output~%")
                                        (setq *mjr_gnupl_gnuplot-stream* (open *mjr_gnupl_gnuplot-fifo-name* :direction :output :if-exists  :overwrite))
                                        (if *mjr_gnupl_gnuplot-term*
                                            (format *mjr_gnupl_gnuplot-stream* "set term ~s~%" *mjr_gnupl_gnuplot-term*))
                                        (format *mjr_gnupl_gnuplot-stream* "reset~%")
                                        (force-output *mjr_gnupl_gnuplot-stream*)
                                        *mjr_gnupl_gnuplot-stream*)))
        (:mjr_gnupl_use-exec (if (not *mjr_gnupl_gnuplot-exec-path*)
                                 (error "mjr_gnupl_connect: Can's use *mjr_gnupl_gnuplot-exec-path* -- NOT SET!")
                                 (if (not (probe-file *mjr_gnupl_gnuplot-exec-path*))
                                     (error "mjr_gnupl_connect: Can's use *mjr_gnupl_gnuplot-exec-path* -- FILE NOT FOUND (~s)!" *mjr_gnupl_gnuplot-exec-path*)
                                     (progn
                                       #+sbcl      (setq *mjr_gnupl_gnuplot-stream* (sb-ext:process-input  (sb-ext:run-program          *mjr_gnupl_gnuplot-exec-path* nil :input :stream :output nil :wait nil :search t)))
                                       #+clisp     (setq *mjr_gnupl_gnuplot-stream*                        (ext:make-pipe-output-stream *mjr_gnupl_gnuplot-exec-path*))
                                       #+ecl       (setq *mjr_gnupl_gnuplot-stream*                        (ext:run-program             *mjr_gnupl_gnuplot-exec-path* nil :input :stream :output t   :wait nil :error :output))
                                       #+abcl      (setq *mjr_gnupl_gnuplot-stream* (system::process-input (system::run-program         *mjr_gnupl_gnuplot-exec-path* nil                            :wait nil)))
                                       #-(or clisp sbcl ecl abcl) (error "mjr_gnupl_connect: Can't figure out how to run GNUplot.")
                                       (if *mjr_gnupl_gnuplot-term*
                                           (format *mjr_gnupl_gnuplot-stream* "set term ~s~%" *mjr_gnupl_gnuplot-term*))
                                       (format *mjr_gnupl_gnuplot-stream* "reset~%")
                                       (force-output *mjr_gnupl_gnuplot-stream*)
                                       *mjr_gnupl_gnuplot-stream*))))
        (otherwise           (warn "mjr_gnupl_connect: *mjr_gnupl_gnuplot-connect-method* not set correctly!!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_disconnect ()
  "If it is open, thne we close the GNUplot FIFO stream"
  (if *mjr_gnupl_gnuplot-stream*
      (progn (ignore-errors (close *mjr_gnupl_gnuplot-stream*))
             (setq *mjr_gnupl_gnuplot-stream* nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_send-command (command &optional alternate-gnuplot-stream)
  "Send a command to gnuplot to the *MJR_GNUPL_GNUPLOT-FIFO-NAME* -- or ALTERNATE-GNUPLOT-STREAM if it is non-NIL."
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
  (if *mjr_gnupl_gnuplot-term*
      (mjr_gnupl_send-command (format nil "set term ~s~%" *mjr_gnupl_gnuplot-term*) alternate-gnuplot-stream))
  (mjr_gnupl_send-command "reset"        alternate-gnuplot-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_dquad (dquad &key data
                                (type :l) (pal "default") col
                                title xlab ylab zlab main
                                xlim ylim zlim
                                alternate-gnuplot-stream)
  "Plot a dquad object with GNUplot.

If :TITLE is NIL, then the :ANO-NAM values in the DQUAD will be usd.  To suppress titles entirely, set :TITLE to '(NIL) -- a list containing a NIL."
  (let* ((data-idxs   (or (mjr_util_non-list-then-list data)
                                  (concatenate 'list (mjr_vvec_to-vec (mjr_dquad_data-count dquad)))))
         (data (mapcar (lambda (x) (mjr_dquad_get-data-array dquad x       )) data-idxs))
         (data-types  (mapcar (lambda (x) (mjr_dquad_get-data-ano dquad x :ano-typ)) data-idxs))
         (data-names  (mapcar (lambda (x) (mjr_dquad_get-data-ano dquad x :ano-nam)) data-idxs))
         (num-plots   (length data))
         (title       (mjr_util_non-list-then-list title))
         (type        (mjr_util_non-list-then-list type))
         (rng-dim     (loop for plt-idx from 0 upto (1- num-plots)
                            for daElm = (mjr_arr_aref-col-major (mjr_util_elt-mod data plt-idx) 0)
                            for dtyp in data-types
                            for ptyp = (mjr_util_elt-mod type plt-idx)
                            for dim  = (cond ((equalp dtyp :ano-typ-complex)         2)
                                             ((and (mjr_annot_typ-numberp dtyp))     1)
                                             ((and (mjr_annot_typ-vectorp dtyp)
                                                   (mjr_annot_typ-nums-real dtyp))   (length daElm))
                                             ((mjr_annot_typ-colorp dtyp)            1))
                            collect (or dim (error "mjr_gnupl_dquad: Unsupported range data type (~a)" dtyp))))
         (axes        (mapcar (lambda (x) (mjr_dquad_get-axis-vector dquad x))
                              (concatenate 'list (mjr_vvec_to-vec (mjr_dquad_axis-count dquad)))))
         (col         (mjr_util_non-list-then-list col))
         (num-plt     (length data))
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
               (mjr_gnupl_send-command "set hidden3d" alternate-gnuplot-stream)
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
                            for d across (nth plt-idx data)
                            do (mjr_gnupl_send-command (format nil "~F ~F " x d) alternate-gnuplot-stream)))
                   (2 (case dtyp
                        (:ano-typ-complex (loop for d across (nth plt-idx data)
                                                do (mjr_gnupl_send-command (format nil "~F ~F " (realpart d) (imagpart d)) alternate-gnuplot-stream)))
                        (otherwise        (loop for d across (nth plt-idx data)
                                                do (mjr_gnupl_send-command (format nil "~F ~F " (aref d 0) (aref d 1)) alternate-gnuplot-stream)))))
                   (3 (loop for d across (nth plt-idx data)
                            do (mjr_gnupl_send-command (format nil "~F ~F ~F " (aref d 0) (aref d 1) (aref d 2)) alternate-gnuplot-stream)))
                   (otherwise (error "mjr_gnupl_dquad: Range dimension ~d is not support with domain dimension of 1" rdim)))
                 (mjr_gnupl_send-command  "e" alternate-gnuplot-stream))))
          (2 (dotimes (plt-idx num-plt)
               (let* ((rdim (elt rng-dim plt-idx))
                      (cdat (elt data plt-idx))
                      (datt (elt data-types plt-idx))
                      (colp (string-equal (mjr_util_elt-mod type plt-idx) :rgb))
                      (cuc  (if colp
                                (mjr_color_make-unpacker-color-space-converter-and-packer (mjr_annot_get-colorpacking datt)
                                                                                          (mjr_annot_get-colorspace datt)
                                                                                          :cs-tru
                                                                                          :cp-none))))
                 (case rdim
                   (1 (loop for xi from 0
                            for x across xaxis
                            do (loop for yi from 0
                                     for y across yaxis
                                     for d = (aref cdat xi yi)
                                     do (mjr_gnupl_send-command
                                         (if colp
                                             (let ((pcol  (funcall cuc d)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_gnupl_dsimp (dsimp &key
                                (dimx 0) (dimy 1) (dimz nil)
                                (simplices 1)
                                (type :l) col
                                xlab ylab zlab main
                                xlim ylim zlim
                                alternate-gnuplot-stream)  
  "Plot the points and/or edges of 0, 1, or 2 simplices of a dsimp object with GNUplot.

No attempt is made to draw a point or segment only one time -- i.e. when drawing 2-simplices forming a surface mesh, most edges will be draw twice and most
vertexes will be drawn three times.  For best performance, use :simplices 0 when using :type :p.  When available, use 1-simplices for edge drawing instead of
2-simplices."
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
  ;; The plot command
  (mjr_gnupl_send-command (format nil "~a '-' using ~a notitle with ~a ~a ~%"
                                  (if dimz "splot " "plot ")
                                  (if dimz "1:2:3 " "1:2 ")
                                  (or (cdr (assoc type
                                                  '((:l . "lines")
                                                    (:d . "dots")
                                                    (:b . "linespoints")
                                                    (:h . "impulses")
                                                    (:p . "points"))
                                                  :test #'string-equal))
                                      "lines")
                                  (if col
                                      (format nil "linecolor rgb \"~a\"" col)
                                      ""))
                          alternate-gnuplot-stream)
  ;; Send the data
  (flet ((fmt-pnt (p) (with-output-to-string (ss)
                        (format ss "~F ~F" (aref p dimx) (aref p dimy))
                        (if dimz (format ss " ~F" (aref p dimz))))))
    (cond ((= 0 simplices) (loop for point across (mjr_dsimp_get-simplex-array dsimp 0)
                                 do (mjr_gnupl_send-command (fmt-pnt point))))
          ((= 1 simplices) (loop with points = (mjr_dsimp_get-simplex-array dsimp 0)
                                 for simplex across (mjr_dsimp_get-simplex-array dsimp 1)
                                 for p0 = (aref points (aref simplex 0))
                                 for p1 = (aref points (aref simplex 1))
                                 do (mjr_gnupl_send-command (format nil (if dimz "~a~%~a~%~%" "~a~%~a~%") (fmt-pnt p0) (fmt-pnt p1)))))
          ((= 2 simplices) (loop with points = (mjr_dsimp_get-simplex-array dsimp 0)
                                 for simplex across (mjr_dsimp_get-simplex-array dsimp 2)
                                 for p0 = (aref points (aref simplex 0))
                                 for p1 = (aref points (aref simplex 1))
                                 for p2 = (aref points (aref simplex 2))                                 
                                 do (mjr_gnupl_send-command (format nil (if dimz "~a~%~a~%~%" "~a~%~a~%") (fmt-pnt p0) (fmt-pnt p1)))
                                 do (mjr_gnupl_send-command (format nil (if dimz "~a~%~a~%~%" "~a~%~a~%") (fmt-pnt p1) (fmt-pnt p2)))
                                 do (mjr_gnupl_send-command (format nil (if dimz "~a~%~a~%~%" "~a~%~a~%") (fmt-pnt p2) (fmt-pnt p0)))))
          ('t              (error "mjr_gnupl_dsimp: Only 0, 1, and 2 simplexes are supported!")))
    (mjr_gnupl_send-command  "e" alternate-gnuplot-stream)))
