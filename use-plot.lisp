;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-plot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     Plotting with backend interface to GNUPlot.@EOL
;; @Keywords  lisp interactive plot gnuplot
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_PLOT
  (:USE :COMMON-LISP
        :MJR_POLY
        :MJR_UTIL
        :MJR_IMG
        :MJR_VVEC
        :MJR_MXP
        :MJR_COMBC
        :MJR_ARR)
  (:DOCUMENTATION "Brief: Plotting with backend interface to GNUPlot.;")
  (:EXPORT #:mjr_plot_disconnect
           #:mjr_plot_data
           #:mjr_plot_func-r1-r1 #:mjr_plot_func-r1-r2 #:mjr_plot_func-r1-r3 #:mjr_plot_func-r2-r1 #:mjr_plot_func-r2-r3
           #:mjr_plot_func-r1-c1                       #:mjr_plot_func-c1-r1
           #:mjr_plot_poly-r1-r1
           #:mjr_plot_poly-n-dif-r1-r1 
           #:mjr_plot_hist
           ;; GNUPLOT Driver Stuff (NOT EXPORTED)
           ;; #:*mjr_plot_drv-gnup-stream-echo*
           ;; #:*mjr_plot_drv-gnup-cmd-or-fifo*
           ;; #:mjr_plot_drv-gnup-disconnect
           ;; #:mjr_plot_drv-gnup-send-command
           ;; #:mjr_plot_drv-gnup-plot-data
           ;; Helper stuff
           ;; #:mjr_plot_dat-from-dat-or-lim
           ))

(in-package :MJR_PLOT)

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_plot_drv_gnup_stream* nil)

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_plot_drv-gnup-stream-echo* nil
  "Duplicate all gnuplot commands to standard out for debug")

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_plot_drv-gnup-cmd-or-fifo*
  (cond ((and (member :WIN32 *features*) (probe-file "C:\\PROGRA~1\\gnuplot\\binary\\WGNUPL~1.EXE"))
         "C:\\PROGRA~1\\gnuplot\\binary\\pgnuplot.exe")
        ((user-homedir-pathname)
         (namestring (merge-pathnames (user-homedir-pathname) ".lispy-gnuplot-fifo"))))
  "How to communicate with gnuplot.  If set to a string containing 'fifo', then this value is assumed to be a
FIFO connected to a running gnuplot process.  See the gnuplotGO.sh script for one way to make sure that a gnuplot
process is always running and listening to the FIFO. Otherwise, it is assumed that this value is the name of the
gnuplot command.  In this case, a gnuplot process is spawned and then used for plotting.")

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_drv-gnup-send-command (command)
  "Send a command to gnuplot.
If an active stream is not open, then create one."
  (if (not *mjr_plot_drv-gnup-cmd-or-fifo*)
      (error "mjr_plot_drv-gnup-send-command: No gnuplot command or FIFO configured (See: *MJR_PLOT_DRV-GNUP-CMD-OR-FIFO*)!"))
  (if (not *mjr_plot_drv_gnup_stream*) ;; If no existing stream, then create one
      (progn 
        (if (search "fifo" *mjr_plot_drv-gnup-cmd-or-fifo*)
            (if (member :WIN32 *features*)
                (error "mjr_plot_drv-gnup-send-command: FIFO mode not suported on native windows -- use POPEN mode or use under cygwin instead...")
                (progn
                  (print "If the reader prompt hasn't returned, then start-up a listening gnuplot!!")
                  (setq *mjr_plot_drv_gnup_stream* (open *mjr_plot_drv-gnup-cmd-or-fifo* :direction :output :if-exists  :overwrite))))
            (progn
              #+CLISP (setq *mjr_plot_drv_gnup_stream* (ext:make-pipe-output-stream *mjr_plot_drv-gnup-cmd-or-fifo*))
              #+GCL   (setq *mjr_plot_drv_gnup_stream* (open (concatenate 'string "| " *mjr_plot_drv-gnup-cmd-or-fifo*) :direction :output))
              #+CMU   (setq *mjr_plot_drv_gnup_stream* (ext:process-input    (ext:run-program    *mjr_plot_drv-gnup-cmd-or-fifo* nil :input :stream :output nil :wait nil)))
              #+SBCL  (setq *mjr_plot_drv_gnup_stream* (sb-ext:process-input (sb-ext:run-program *mjr_plot_drv-gnup-cmd-or-fifo* nil :input :stream :output nil :wait nil :search t)))
              #+SCL   (setq *mjr_plot_drv_gnup_stream* (ext:process-input    (ext:run-program    *mjr_plot_drv-gnup-cmd-or-fifo* nil :input :stream :output nil :wait nil)))
              #-(or CLISP CMU SBCL GCL SCL)            (error "mjr_plot_drv-gnup-send-command: No support for popen on this LISP!!")
              ))
        (if *mjr_plot_drv_gnup_stream*;; If it worked, then initialize the gnuplot
            (progn
              (mjr_plot_drv-gnup-send-command "unset output")
              (if (member :WIN32 *features*)
                  (mjr_plot_drv-gnup-send-command "set term wxt")
                  (mjr_plot_drv-gnup-send-command "set term x11"))
              (mjr_plot_drv-gnup-send-command "reset")))))
  (if (not *mjr_plot_drv_gnup_stream*)
      (error "mjr_plot_drv-gnup-send-command: Could not communicate with gnuplot")
      (progn (format *mjr_plot_drv_gnup_stream* "~a ~%" command)
             (if *mjr_plot_drv-gnup-stream-echo*
                 (format 't "~a ~%" command))
             (force-output *mjr_plot_drv_gnup_stream*))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_drv-gnup-disconnect ()
  (if *mjr_plot_drv_gnup_stream*
      (progn
        (ignore-errors (close *mjr_plot_drv_gnup_stream*))
        (setq *mjr_plot_drv_gnup_stream* nil))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_disconnect ()
  "Close down resrouces usesd by the lower level drivers"
  (mjr_plot_drv-gnup-disconnect))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_drv-gnup-plot-data (&key xdat ydat zdat dat datcols span type title xlab ylab zlab main xlim ylim zlim pal col)
  "GNUplot driver for plot-data"
  (let* ((num-xd   (length xdat))
         (num-yd   (length ydat))
         (num-zd   (length zdat))
         (num-dat  (length dat))
         (num-dc   (length datcols))
         (num-span (length span))
         (num-plt  (reduce #'max (list num-xd num-yd num-zd num-dat num-dc)))
         (num-typ  (length type))
         (dat3d    (or zdat (and datcols (find 3 (mapcar #'length datcols)))))
         (plt3d    (and dat3d
                        (notany (lambda (x) (or (equal :i x) (equal :rgb x))) type))))
    (flet ((get-m-elt (the-dat row col-spec) (if (numberp col-spec)
                                                 (aref the-dat row col-spec)
                                                 (apply (first col-spec) (loop for idx in (cdr col-spec) 
                                                                               collect (aref the-dat row idx))))))
      ;; Autoscale for single image plots, no xlim, no ylim, no x-data or y-data -- i.e. "just draw this matrix as an image"
      (if (and (not ydat) (not xdat) (and zdat span) (not xlim) (not ylim) (= 1 num-plt) (or (equal (first type) :rgb) (equal (first type) :i)))
          (let* ((rows (if dat (first span)                            (mjr_arr_num-rows (first zdat))))
                 (cols (if dat (/ (mjr_arr_num-rows (first dat)) (first span)) (mjr_arr_num-cols (first zdat)))))
            (setq xlim (or xlim (list -0.5 (+ -0.5 cols))))
            (setq ylim (or ylim (list -0.5 (+ -0.5 rows))))))
      ;; Setup the plot
      (mjr_plot_drv-gnup-send-command (format nil "set xlabel \"~a\"" (or xlab "")))
      (mjr_plot_drv-gnup-send-command (format nil "set ylabel \"~a\"" (or ylab "")))
      (mjr_plot_drv-gnup-send-command (format nil "set zlabel \"~a\"" (or zlab "")))
      (mjr_plot_drv-gnup-send-command (format nil "set title \"~a\""  (or main "")))
      (mjr_plot_drv-gnup-send-command "set zeroaxis")
      (mjr_plot_drv-gnup-send-command "set autoscale")
      (if xlim (mjr_plot_drv-gnup-send-command (format nil "set xrange [~f:~f]" (nth 0 xlim) (nth 1 xlim))))
      (if ylim (mjr_plot_drv-gnup-send-command (format nil "set yrange [~f:~f]" (nth 0 ylim) (nth 1 ylim))))
      (if zlim (mjr_plot_drv-gnup-send-command (format nil "set zrange [~f:~f]" (nth 0 zlim) (nth 1 zlim))))
      (if dat3d
          (progn 
            (mjr_plot_drv-gnup-send-command 
            (concatenate 'string 
                         "set palette rgbformulae "
                         (or
                          (cdr (assoc pal '(("default" . "7,5,15")     ("ocean" . "23,28,3")    ("hot" . "21,22,23")
                                            ("printable" . "30,31,32") ("rainbow" . "33,13,10") ("afm"   . "34,35,36")
                                            ("hsv" . "3,2,2")          ("gray"      . "3,3,3"))
                                      :test #'string=))
                          pal)))
            (if (some (lambda (x) (string= "F" x)) type)
            (progn (mjr_plot_drv-gnup-send-command "unset hidden3d")
                   (mjr_plot_drv-gnup-send-command "set pm3d depthorder"))
            (progn (mjr_plot_drv-gnup-send-command "set hidden3d")))))
      ;; The plot command
      (mjr_plot_drv-gnup-send-command  
       (with-output-to-string (ss)
         (format ss (if plt3d "splot " "plot "))
         (dotimes (plt-idx num-plt)
           (let ((cur-type (mjr_util_elt-mod type plt-idx)))
             (format ss "'-' using ~a ~a with ~a linecolor rgb \"~a\" ~a" 
                     (cond ((equal cur-type :rgb) "1:2:3:4:5")
                           (dat3d                 "1:2:3")
                           ('t                    "1:2" ))
                     (if title (format nil "title \"~a\"" (mjr_util_elt-mod title plt-idx)) "notitle")
                     (cdr (assoc cur-type
                                 '((:l . "lines")    (:i . "image")  (:d . "dots") (:b   . "linespoints")
                                   (:h . "impulses")   (:p . "points") (:f . "pm3d") (:rgb . "rgbimage"))
                                 :test #'equal))
                     (mjr_util_elt-mod col plt-idx)
                     (if (= plt-idx (1- num-plt)) "" ", "))))))  
      ;; Send the data
      (dotimes (plt-idx num-plt)
        (let* ((cur-xdat (if xdat    (mjr_util_elt-mod xdat    plt-idx)))
               (cur-ydat (if ydat    (mjr_util_elt-mod ydat    plt-idx)))
               (cur-zdat (if zdat    (mjr_util_elt-mod zdat    plt-idx)))
               (cur-dat  (if dat     (mjr_util_elt-mod dat     plt-idx)))
               (cur-type (if type    (nth (mod plt-idx num-typ) type)))
               (cur-dc   (if datcols (mjr_util_elt-mod datcols plt-idx)))
               (have-x   (if cur-dat (<= 2 (length cur-dc)) cur-xdat))
               (have-y   (if cur-dat (<= 1 (length cur-dc)) cur-ydat))
               (have-z   (if cur-dat (<= 3 (length cur-dc)) cur-zdat))
        (cur-span (if span
                      (if (or (null (nth (mod plt-idx num-span) span)) (< 0 (nth (mod plt-idx num-span) span)))
                          (nth (mod plt-idx num-span) span)
                          (mjr_arr_num-rows cur-zdat))))
        (num-pts  (cond (cur-dat                      (mjr_arr_num-rows cur-dat))
                        ((mjr_arr_rank-is 2 cur-zdat) (array-total-size cur-zdat))
                        ('t                           (length cur-ydat)))))
          (dotimes (pt-idx num-pts)
          (let ((r-idx (if cur-span (mod pt-idx cur-span)))
                (c-idx (if cur-span (truncate (/ pt-idx cur-span)))))
            (mjr_plot_drv-gnup-send-command (concatenate 'string 
                                                (format nil "~F " (if have-x
                                                                      (if dat
                                                                          (get-m-elt cur-dat pt-idx (elt cur-dc 0))
                                                                          (elt cur-xdat (if cur-span r-idx pt-idx)))
                                                                      (if cur-span r-idx pt-idx)))
                                                (format nil "~F " (if have-y
                                                                      (if dat
                                                                          (get-m-elt cur-dat pt-idx (elt cur-dc (if have-x 1 0)))
                                                                          (elt cur-ydat (if cur-span c-idx pt-idx)))
                                                                      (if cur-span c-idx pt-idx)))
                                                (if dat3d (let ((val (if have-z
                                                                        (if dat
                                                                            (get-m-elt cur-dat pt-idx (elt cur-dc 2))
                                                                            (if cur-span
                                                                                (aref cur-zdat r-idx c-idx)
                                                                                (elt cur-zdat pt-idx)))
                                                                        0)))
                                                           (if (equal cur-type :rgb)
                                                               (let ((pcol (if (vectorp val)
                                                                               val
                                                                               (mjr_img_cp-unpack-int8x3-int24 val))))
                                                                 (format nil "~F ~F ~F " (aref pcol 0) (aref pcol 1) (aref pcol 2)))
                                                               (format nil "~F " val))))))
            (if (and cur-span (zerop (mod (1+ pt-idx) cur-span))) (mjr_plot_drv-gnup-send-command ""))))
          (mjr_plot_drv-gnup-send-command  "e"))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_data (&key xdat ydat zdat
                           dat datcols span
                           (type :l) title xlab ylab zlab main xlim ylim zlim (pal "default")
                           (col (list "black" "red" "green" "blue" "purple" "cyan" "brown" "orange" "violet")))
  "Plot single and/or two parameter data in 2D or 3D:
    * 1dof curves like <x, f(x)>, <x(t), y(t)>, <x(t), y(t), z(t)>, etc...
    * 2dof objects like images <x, y, c(x,y)> and surfaces <x, y, z(x,y)>

PROVIDING DATA

Data may be provided in one of two different ways:
  * Via named variables :xdat, :ydat, :zdat, and/or :span
    The :xdat & :ydat arguments take the form of a sequence, and :zdat may be a sequence or a matrix (for 2dof)
  * Via a matrix (in :dat), a column list (in :datcols), and/or a span (in :span).  The :dat argument takes
    the form of a 'table' matrix (see mjr_combc_gen-all-cross-product)

Space curves (in 3D space) may be drawn by providing sequences for :xdat, :ydat, & :zdat or providing a matrix for :dat with
a :datcols value containing three indexes with no :span.  Parametric, plane curves (in 2D space) may be drawn by providing
just :xdat & :ydat or providing :dat with a :datcols value containing two indexes and no :span.  Sequence curves (in 2D
space), i.e. <x, (f(x)>, may be drawn by providing just :ydat or by providing :dat with a :datcols value containing ONE index
and no :span.  In this last case, the :xdat will be a manufactured integer sequence beginning with the number 0.  Surfaces in
3D space and images in 2D space may be drawn by providing a :zdat as a matrix and :span (set to the number of rows in :zdat
or to -1).  In this case, :xdat & :ydat may be sequences providing x/y coordinate information for the regular grid upon
which the :zdat was sampled. Alternately, :dat may be used with a :datcols value containing three indexes and a :span filling
the same roll as :len in mjr_combc_gen-all-cross-product.

MULTIPLE PLOTS

Multiple objects may be plotted by providing lists of the objects discussed previously to the data arguments.  When using
:xdat, :ydat, and/or :zdat, multiple curves will result when :ydat is a list of vectors, a list of lists of numbers, or a
matrix with no :span (the columns will be used).  Similarly, :dat may be a list of matrices each specifying a different
object.  Finally, :dat may be a single matrix while :datcols contains a list of sequences identifying multiple columns for
each different object.  In this last case, each sequence contained in :datcols must be the same length -- i.e.  all space
curves, 2D parametric curves, all sequence curves, etc...  When plotting multiple objects, supporting data elements will be
reused as required.  For example, if :ydat is a list of several sequences and :xdat has a smaller list, then the :xdat values
will be 'reused' as required.

TRANSFORMATION

Relatively generic data transformation capabilities are provided that increase flexibility and ease of use.  In the discussion
below, complex numbers are the subject of the examples, but the utility of the transformation capabilities go far beyond this
simple application.

Columns for :dat may be transformed or combined into new values in a relatively generic way by specifying a function, and the
columns it will take as arguments, in the :datcols parameter.  Instead of a simple index number, provide a list with a first
element a function or symbol, .i.e. (list FUNC IDX1 IDX2 ...).  Be carefully with quoting:
    :datcols '((realpart 1) (imagpart 1)))
    :datcols (list (list #'realpart 1) (list #'imagpart 1)))
but NOT
    :datcols '((#'realpart 1) (#'imagpart 1)))
Example:
    :dat m :datcols '((realpart 1) (imagpart 1)))
This will plot a parametric curve of Re vs. Im for column 1 of matrix m.  The most common application is dealing with complex
quantities via #'realpart, #'imagpart, #'phase, and #'abs to reduce complex columns to real values to be plotted.

This functionality may be used to combine columns as well.  For example, one might plot the magnitude of a group of columns as a
vector, or the ratio of two columns.

Data provided via :xdat, :ydat, & :zdat may be transformed too.  The syntax is somewhat similar to the syntax for :datcols.
Instead of providing a sequence, provide a list containing a function or symbol bound to a function followed by numeric
sequence(s) and/or symbols representing sequences the other data sequences (i.e. :xdat, :ydat, or :zdat).  The symbols evaluate
to the the numeric sequence originally passed in as an argument or to the LAST element of the list.  For example, one might plot a
complex number sequence with Re vs. Im like so:
    :xdat (list #'realpart :ydat) :ydat (list #'imagpart #C(x y))
Note that the :ydat in the :xdat argument corresponds to the LAST element in the :ydat list -- the complex sequence.  The
computations are all done in parallel to make this work.  Dependencies can not be chained (i.e. :xdat depends on :ydat which
depends on :zdat).

EFFICIENCY

When using :xdat, :ydat, & :zdat the most efficient input for this function will be a list of arrays.  For large,
multi-object graphs, the most efficient input is a single :dat matrix.

GRAPH APPEARANCE OPTIONS

Many optional arguments (:type :xlab :ylab :zlab :main :xlim :ylim :zlim :col) modify the plots. They are named to mirror
many of the options available in the R system.  Parameters lie :col and :type may be given string values just like R, or they
may be given more LISP-like symbols.  Like R, the :col parameter may be a single value or a list; however, each color
provided is used for an ENTIRE SEQUENCE not point-wise like R.  Unlike R, the default value for :col is a list that works for
as many as 9 different curves.  If the :col list is too short, colors will be will be reused.  In addition to the :h, :b, :p,
& :l values for :type, the parameter may also be set to :d is much like type='p' with pch='.' in R.  For 2dof plots, the
:type parameter accepts :i (image) and :f (filled 3D plot) values.  Like the :col option, the :type option may be a list with
each element use for a sequence. The :pal parameter describes the pallet to be used when the parameter :type is :i or :f. The
syntax for :pal is the integer tuple syntax GNUPLOT uses for rgbformulae or one of several pre-defined names including:
default, ocean, hot, printable, rainbow, fm, hsv, and gray. For SINGLE image plots, when :type is :i, the :xlim and :ylim
parameters have defaults that correspond to a square the precise size of the image.

; MJR TODO NOTE mjr_plot_data: 2D & 3D vector fields
; MJR TODO NOTE mjr_plot_data: 2D slope fields

EXAMPLES

Example (just some random data)
   (mjr_plot_data :ydat (mjr_prng_vector 100 #'mjr_prng_float-co -1 1))
Example (a nice spiral)
   (let* ((t1 (mjr_vec_make-seq :start 0 :end 7.0 :len 5000))
          (x1 (mjr_vec_ewuo t1 (lambda (x) (* x (cos x)))))
          (y1 (mjr_vec_ewuo t1 (lambda (x) (* x (sin x))))))
     (mjr_plot_data :xdat x1 :ydat y1))
Example (the canonical Matlab example)
   (mjr_plot_data :type :f :span -1
     :zdat (mjr_mat_make-from-func (lambda (i j) (let ((r (sqrt (+ (* i i) (* j j))))) (/ (sin r) r))) :rows 100 :cols 100 :rstart -8 :cstart -8 :rend 8 :cend 8))
Example (with some x/y data)
   (let* ((x (mjr_vec_make-seq :start -8 :end 8 :len 100))
          (y (mjr_vec_make-seq :start -8 :end 8 :len 100))
          (z (mjr_mat_make-from-func (lambda (i j) (let* ((r (sqrt (+ (expt (aref x i) 2) (expt (aref y j) 2))))) (/ (sin r) r))) :rows 100 :cols 100)))
     (mjr_plot_data :zdat z :xdat x :ydat y :span -1 :type :f))"
  (cond ((not (or xdat ydat zdat dat))                 (error "mjr_plot_data: No data provided via :dat, :xdat, :ydat, or :zdat!"))
        ((and (or xdat ydat zdat) dat)                 (error "mjr_plot_data: Data may not be provided via both :xdat/ydat/zdat and :dat!"))
        ((and datcols (or xdat ydat zdat))             (error "mjr_plot_data: Data :datcols may not be used with :xdat, :ydat, or :zdat!"))
        ((and zdat datcols)                            (error "mjr_plot_data: Data :datcols may not be used with :zdat!"))
        )
  (flet ((obj-to-list-of-xnumber-sequences (obj)
           (flet ((sonp (obj) (or (and (vectorp obj) (numberp (elt obj 0))) (and (listp obj) (car obj) (numberp (car obj)))))) ; SeqOfNumbers?
             (cond ((numberp obj)                      (list (make-array 1 :initial-element obj)))
                   ((mjr_arr_rank-is 2 obj)            (mjr_arr_get-cols obj))
                   ((sonp obj)                         (list obj))
                   ((and (listp obj)
                         (cdr obj)
                         (or (functionp (car obj)) 
                             (symbolp (car obj)))
                         (or (sonp (cdr obj))
                             (sonp (second obj))
                             (symbolp (second obj))))  (list obj))
                   ('t                                 obj)))))
    (let* ((xdat-r   (obj-to-list-of-xnumber-sequences xdat))
           (ydat-r   (obj-to-list-of-xnumber-sequences ydat))
           (zdat-r   (if (and zdat span)
                         (if (mjr_arr_rank-is 2 zdat) (list zdat) zdat)
                         (obj-to-list-of-xnumber-sequences zdat)))
           (dat      (if (mjr_arr_rank-is 2 dat) (list dat) dat))
           (datcols  (let ((tlist (obj-to-list-of-xnumber-sequences datcols)))
                       (if (and (car tlist) (listp (car tlist)) (caar tlist) (or (functionp (caar tlist)) (symbolp (caar tlist))))
                           (list tlist)
                         tlist)))
           (span     (if (numberp span) (list span) span))
           (col      (mapcar (lambda (x) (string-downcase (if (symbolp x) (symbol-name x) x))) (if (listp col) col (list col))))
           (title    (if (and (not (stringp title)) (or (vectorp title) (listp title))) title (list title)))
           (type     (if (listp type) type (list type))))
      (flet ((ser-xform (seq xd yd zd) (if (and seq (listp seq) (or (functionp (first seq)) (symbolp (first seq))))
                                           (apply #'map 'vector (car seq) 
                                                  (loop for targ in (cdr seq)
                                                        collect (cond ((eq targ :xdat) (if (and (listp xd) (functionp (first xd))) (car (last xd)) xd))
                                                                      ((eq targ :ydat) (if (and (listp xd) (functionp (first yd))) (car (last yd)) yd))
                                                                      ((eq targ :zdat) (if (and (listp xd) (functionp (first zd))) (car (last zd)) zd))
                                                                      ('t              targ))))
                                           seq)))

        (let ((xdat (if xdat-r (loop for i from 0 for v in xdat-r collect (ser-xform v (nth i xdat-r) (nth i ydat-r) (nth i zdat-r)))))
              (ydat (if ydat-r (loop for i from 0 for v in ydat-r collect (ser-xform v (nth i xdat-r) (nth i ydat-r) (nth i zdat-r)))))
              (zdat (if zdat-r (loop for i from 0 for v in zdat-r collect (ser-xform v (nth i xdat-r) (nth i ydat-r) (nth i zdat-r))))))
          (mjr_plot_drv-gnup-plot-data :xdat xdat :ydat ydat :zdat zdat :dat dat :datcols datcols :span span :type type :title title
                              :xlab xlab :ylab ylab :zlab zlab :main main :xlim xlim :ylim ylim :zlim zlim :pal pal :col col))))))


;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_dat-from-dat-or-lim (dat lim dflt-start dflt-end dflt-len)
  "Helper function to generate sequences based on plot limits or aseq-type arguments"
  (mjr_vvec_to-vec-maybe (or dat
                             (and lim (list :start (first lim) :end (second lim) :len dflt-len))
                             (list :start dflt-start :end dflt-end :len dflt-len))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r1-r1 (func &rest kw-arg &key xdat type title (ylab "y") (xlab "x") main xlim ylim col)
  "Plot a function (or list of functions) on the range :XDAT (default [-7,7]) using N (default 100) sample points.
When plotting a list of functions, they all share :xdat, :main, :xlab, :xlim, and :ylim.
FUNC may be a lambda, function symbol, a string containing an expression of X in infix notation, or a list of these items.

Example:
   (mjr_plot_func-r1-r1 #'sin)
Example (see mjr_plot_poly-n-dif-r1-r1 for an easy way to do this):
   (mjr_plot_func-r1-r1 (lambda (x) (mjr_poly_eval #(2310 -7201 8949 -5541 1709 -210) x)) :xlim '(0.45L0 0.75L0) :ylim '(-0.002 0.015))"
  (declare (ignore type ylim main col title)) ;; Variables not directly used
  (let ((func (mjr_mxp_string-or-func-to-list-o-lambda func "x"))
        (xdat (mjr_plot_dat-from-dat-or-lim xdat xlim -7 7 100)))
    (apply
     #'mjr_plot_data
     :xdat xdat
     :ydat (mapcar (lambda (f) (map 'vector f xdat)) func)
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :xdat :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_poly-r1-r1 (poly &rest kw-arg &key xdat type title ylab xlab main xlim ylim col)
  "Plot a polynomial, or list of polynomials, on the range :XLIM (default [-7,7]) using N (default 100) sample points.

Example:
   (mjr_plot_poly-r1-r1 "
  (declare (ignore type ylim main col title)) ;; Variables not directly used
  (let ((poly (if (listp poly) poly (list poly)))
        (xdat (mjr_plot_dat-from-dat-or-lim xdat xlim -7 7 100)))
    (apply
     #'mjr_plot_data
     :dat (mjr_combc_gen-all-cross-product (list xdat)
                                           :collect-value (lambda (x) (loop for p in poly
                                                                            collect (mjr_poly_eval p x)))
                                           :arg-mode :arg-number
                                           :result-type :table)
     :datcols (loop for i from 1 upto (length poly)
                    collect (list 0 i))
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :xdat :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r2-r1 (func &rest kw-arg &key xdat ydat type title zlab ylab xlab main xlim ylim zlim pal col (arg-mode :arg-number))
  "Plot a two argument function (or list of functions), FUNC, on the range :XLIM x :YLIM (default [-7,7]x[-7,7]) using NxM (default 20x20) sample points.
When plotting a list of functions, they all share :n, :m, :xlim, and :ylim.
FUNC may be a lambda, function symbol, a string containing an expression of X & Y in infix notation, or a list of these items.

Example:
   (mjr_plot_func-r2-r1 (lambda (x y) (+ (sin x) (cos y))) :type :f)"
  (declare (ignore type pal zlim main col title)) ;; Variables not directly used
  (let ((func (mjr_mxp_string-or-func-to-list-o-lambda func "x" "y"))
        (xdat (mjr_plot_dat-from-dat-or-lim xdat xlim -7 7 20))   ;(if (or xdat ydat) xdat (or ydat)) (if (or xdat ydat) xlim (or ylim))
        (ydat (mjr_plot_dat-from-dat-or-lim ydat ylim -7 7 20)))
    (apply
     #'mjr_plot_data
     :xdat xdat
     :ydat ydat
     :zdat (mapcar (lambda (f) (mjr_combc_gen-all-cross-product (list xdat ydat) :collect-value f :arg-mode arg-mode :result-type :array)) func)
     :span 0
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     :zlab (or zlab "z")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :arg-mode :xdat :ydat :zlab :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-c1-r1 (func &rest kw-arg &key rdat idat type title zlab rlab ilab main rlim ilim zlim pal col)
  "Plot a one complex argument function (or list of functions), FUNC on a default range determined by MJR_PLOT_FUNC-R2-R1.
When plotting a list of functions, they all share :rdat, :idat, :xlim, and :ylim.
FUNC may be a lambda, function symbol, a string containing an expression of Z in infix notation, or a list of these items.

Example:
   (mjr_plot_func-c1-r1 (list (lambda (x) (realpart x)) (lambda (x) (imagpart x))) :rlim '(-3 3) :ilim '(-3 3):type :f)
   (mjr_plot_func-r2-r1 (lambda (z) (abs (sin z))) :type :f)"
  (declare (ignore type pal zlim zlab main col title)) ;; Variables not directly used
  (let ((func (mjr_mxp_string-or-func-to-list-o-lambda func "z")))
    (apply #'mjr_plot_func-r2-r1
           (mapcar (lambda (f) (lambda (x y) (funcall f (complex x y)))) func)
           :xdat rdat
           :ydat idat
           :xlab (or rlab "real")
           :ylab (or ilab "imag")
           :xlim rlim
           :ylim ilim
           (mjr_util_strip-kwarg kw-arg :strip-list (list :rdat :idat :rlim :ilim :rlab :ilab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r1-r2 (func &rest kw-arg &key udat type title ylab xlab main xlim ylim col)
  "Plot a parametric function func on the range :UDAT (default [0,1]) using 100 sample points).
The func should return a list or vector with two elements.
FUNC may be a lambda, function symbol, a string containing an expression of U in infix notation, or a list of these items.

Example:
   (mjr_plot_func-r1-r2 (lambda (x) (list (cos x) (sin x))))
Example
   (mjr_plot_func-r1-r2 (lambda (x) (list (cos x) (sin x))) :udat (list :start 0 :end (* 2 pi) :len 5000))"
  (declare (ignore type main xlim ylim col title)) ;; Variables not directly used
  (let ((func (mjr_mxp_string-or-func-to-list-o-lambda func "u"))
        (udat (mjr_plot_dat-from-dat-or-lim udat nil 0 1 100)))
    (apply
     #'mjr_plot_data
     :dat (mapcar (lambda (f) (mjr_combc_gen-all-cross-product (list udat) :collect-value f :arg-mode :arg-number :result-type :table)) func)
     :datcols (list 1 2)
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :udat :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r1-r3 (func &rest kw-arg &key udat type title zlab ylab xlab main xlim ylim col)
  "Plot a parametric function func on the range :ULIM (default [0,1]) using N (default 100) sample points.
The func should return a list or vector with three elements.
FUNC may be a lambda, function symbol, a string containing an expression of U in infix notation, or a list of these items.

Example:
   (mjr_plot_func-r1-r3 (lambda (x) (list (cos x) (sin x) x))  :udat '(:start 0 :end 17 :len 100))"
  (declare (ignore type main xlim ylim col title)) ;; Variables not directly used
  (let ((func (mjr_mxp_string-or-func-to-list-o-lambda func "u"))
        (udat (mjr_plot_dat-from-dat-or-lim udat nil 0 1 100)))
    (apply
     #'mjr_plot_data
     :dat (mapcar (lambda (f) (mjr_combc_gen-all-cross-product (list udat) :collect-value f :arg-mode :arg-number :result-type :table)) func)
     :datcols (list 1 2 3)
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     :zlab (or zlab "z")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :udat :zlab :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r2-r3 (func &rest kw-arg &key udat vdat type title zlab ylab xlab main xlim ylim zlim col arg-mode)
  "Plot a parametric function func on the range :ULIM and :VLIM (default [0,1]) using N (default 20) sample points.
The func should return a list or vector with three elements.
FUNC may be a lambda, function symbol, a string containing an expression of U & V in infix notation, or a list of these items.


Example (a sphere & pinched sphere):
  (mjr_plot_func-r2-r3 (list (lambda (u v) (vector (* 5 (sin u) (cos v)) (* 5 (sin u) (sin v)) (* 5 (cos u))))
                                    (lambda (u v) (vector (* 5 (sin (* 2 u)) (cos v)) (* 7 (sin u) (sin v)) (* 5 (cos u)))))
                       :udat (list :start 0 :end pi :len 20)
                       :vdat (list :start 0 :end (* 2 pi) :len 20))
Example (a sphere):
  (mjr_plot_func-r2-r3 (lambda (u v) (vector (* 5 (sin u) (cos v)) (* 5 (sin u) (sin v)) (* 5 (cos u))))
                       :udat (list :start 0 :end pi :len 20)
                       :vdat (list :start 0 :end (* 2 pi) :len 20))"
  (declare (ignore type main xlim ylim zlim col title)) ;; Variables not directly used
  (let ((arg-mode (or arg-mode :arg-number))
        (func (mjr_mxp_string-or-func-to-list-o-lambda func "u" "v"))
        (udat (mjr_plot_dat-from-dat-or-lim udat nil 0 1 20))
        (vdat (mjr_plot_dat-from-dat-or-lim vdat nil 0 1 20)))
    (apply
     #'mjr_plot_data
     :dat (mapcar (lambda (f) (mjr_combc_gen-all-cross-product (list udat vdat) :collect-value f :arg-mode arg-mode :result-type :table)) func)
     :datcols (list 2 3 4)
     :span (length udat)
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     :zlab (or zlab "z")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :udat :vdat :arg-mode :zlab :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_func-r1-c1 (func &rest kw-arg &key udat type title ylab xlab main xlim ylim col)
  "Plot a complex function func on the range :ULIM (default [0,1]) using N (default 100) sample points.
FUNC may be a lambda, function symbol, or a string containing an expression of U in infix notation.

Example:
   (mjr_plot_func-r1-c1 (lambda (tim) (complex (cos tim) (sin tim))) :udat '(:start 0 :end 7 :len 100))"
  (declare (ignore type main xlim ylim col title)) ;; Variables not directly used
  (let ((func (car (mjr_mxp_string-or-func-to-list-o-lambda func "u")))
        (udat (mjr_plot_dat-from-dat-or-lim udat nil 0 1 100)))
    (apply
     #'mjr_plot_data
     :xdat (list #'realpart (mjr_vvec_gen-0sim 'vector (list :map-fun func :points udat)))
     :ydat (list #'imagpart :xdat)
     :xlab (or xlab "real")
     :ylab (or ylab "imag")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :udat :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_poly-n-dif-r1-r1 (poly &rest kw-arg &key (order 0) xdat type title ylab xlab main xlim ylim col)
  "Plot a polynomial and ORDER derivatives on the range :XLIM (default [-7,7]) using N (default 100) sample points.

Example:
   (mjr_plot_poly-n-dif-r1-r1 #(2310 -7201 8949 -5541 1709 -210) :xlim '(0.45L0 0.75L0) :ylim '(-0.002 0.015))"
  (declare (ignore type ylim main col title)) ;; Variables not directly used
  (let ((xdat (mjr_plot_dat-from-dat-or-lim xdat xlim -7 7 100)))
    (apply
     #'mjr_plot_data
     :dat (mjr_combc_gen-all-cross-product (list xdat)
                                           :collect-value (lambda (x) (multiple-value-list (mjr_poly_eval-poly-and-first-n-derivatives poly x order)))
                                           :arg-mode :arg-number :result-type :table)
     :datcols (loop for i from 0 upto order
                    collect (list 0 (1+ i)))
     :xlab (or xlab "x")
     :ylab (or ylab "y")
     (mjr_util_strip-kwarg kw-arg :strip-list (list :order :xdat :ylab :xlab)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_plot_hist (hist &rest kw-arg &key title ylab xlab main xlim ylim col)
  "Plots a histogram from a vector of counts and a vector of breaks.

The first argument is assumed to be in the format returned by MJR_STATS_HIST."
  (declare (ignore title ylab xlab main xlim ylim col)) ;; Variables not directly used
      (let* ((counts (first  hist))
             (breaks (second hist))
             (ctlen  (length counts))
             (ctrs   (make-array ctlen)))
        ;; Compute centers
        (loop for i from 0 upto (1- ctlen)
              do (setf (aref ctrs i) (/ (+ (aref breaks (1+ i)) (aref breaks i)) 2)))
        ;; Draw it
        (apply #'mjr_plot_data
               :xdat ctrs
               :ydat counts
               :type :h
               (mjr_util_strip-kwarg kw-arg :strip-list (list :density :xdat :ydat :type)))))

;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------
  
;   Various mathematical objects and graphical presentations   ||   Various Graph Primitives
;   -----------------------------------------------------------||--------------------------------------------------------
;   Math object     Function Types     Graphical Presentation  ||   
;   1-manifold      R    ->R           C2g                     ||   C2 - Curve in 2D (points, segments, bars)               
;                   R    ->RxR         C2p                     ||   C3 - Curve in 3D (points, segments, bars)               
;                   R    ->RxRxR       C3p                     ||   S3 - Surface in 3D (points, segments, bars, polygons)   
;   2-manifold      RxR  ->R           S3g, I2, G2             ||   V2 - Vector field in 2D                                 
;                   RxR  ->RxRxR       S3p                     ||   V3 - Vector field in 3D                                 
;   Vector Field    RxR  ->RxR         V2                      ||   S2 - Slope field in 2D                                  
;                   RxRxR->RxRxR       V3                      ||   S2 - Slope field in 3D                                  
;   Slope Field     RxR  ->R+\infty    S2                      ||   I2 - Pixel image in 2D                                  
;                   RxRxR->R+\infty    S3                      ||   I3 - Voxel image in 3D                                  
;   Scalar Field    RxR  ->R           S3, I2, G2              ||   G2 - Glyph field in 2D                                  
;                   RxRxR->R           I3, G3                  ||   G3 - Glyph field in 3D                                  
;
; Various function types along with implementation information:
;
;                   +--------+---+---+-----+-------+
;                   | in\out | R | C | RxR | RxRxR |       I    Implemented
;                   +--------+---+---+-----+-------+       2    Require 2D vector field plots
;                   |   R    | I | I |  I  |   I   |       3    Require 3D scalar field plots
;                   +--------+---+---+-----+-------+       4    Require 3D vector field plots
;                   |   C    | I | 2 |  2  |   5   |       5    Parametric surface in 3D (should be implemented)
;                   +--------+---+---+-----+-------+       6    Wacky object with no good representation
;                   |  RxR   | I | 2 |  2  |   5   |  
;                   +--------+---+---+-----+-------+  
;                   | RxRxR  | 3 | 6 |  6  |   4   |  
;                   +--------+---+---+-----+-------+  


