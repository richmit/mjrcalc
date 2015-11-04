;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      pre-vdraw.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2015 by Mitch Richling.  All rights reserved.
;; @brief     Vector drawing tools with backend interface to mjrDRsrv.@EOL
;; @Keywords  lisp interactive vdraw mjrDRsrv
;; @Std       Common Lisp
;;
;;            Still experimental -- and likely on the path to deprecation. :)
;;
;;            The idea is to provide "live" vector drawing capabilities on "interactive" X11 displays via an external drawing
;;            server.  mjrDRsrv is the external drawing server, and it comes in various flavors with the PlotUtils version being the
;;            most popular. Note that mjrDRsrv behaves similarly to the 'plot' command that is part of GNU PlotUtils; however, the
;;            'plot' command is quite unforgiving.  Some examples of the kinds of things we might want to draw include:
;;
;;               * Moving bodes in a restricted 3-body simulation
;;               * The game of life
;;               * Swarm simulations
;;               * The exploration point for 2D optimization and root finding problems
;;               * The grid in a dynamic ODE solver
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_VDRAW
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Vector drawing tools with backend interface to mjrDRsrv.;")
  (:EXPORT #:mjr_vdraw_send-command #:mjr_vdraw_disconnect 

           #:mjr_vdraw_quit #:mjr_vdraw_clear-canvas #:mjr_vdraw_draw-circle #:mjr_vdraw_draw-ellipse #:mjr_vdraw_draw-line
           #:mjr_vdraw_draw-box #:mjr_vdraw_draw-point #:mjr_vdraw_draw-line-to #:mjr_vdraw_set-fill-color #:mjr_vdraw_set-bg-color
           #:mjr_vdraw_set-line-color #:mjr_vdraw_set-line-width #:mjr_vdraw_set-coords #:mjr_vdraw_set-point #:mjr_vdraw_set-echo
           #:mjr_vdraw_set-fill

           ;; NOT EXPORTED
           ;; *#:mjr_vdraw_stream-echo* *#:mjr_vdraw_cmd-or-fifo*
           ))

(in-package :MJR_VDRAW)

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_vdraw_stream* nil)

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_vdraw_stream-echo* nil
  "Duplicate all mjrDRsrv commands to standard out for debug")

;;----------------------------------------------------------------------------------------------------------------------------------
(defvar *mjr_vdraw_cmd-or-fifo*
  (if (user-homedir-pathname)   ;;(probe-file "/home/richmit/")
      (namestring (merge-pathnames (user-homedir-pathname) ".lispy-mjrDRsrv-fifo")))
  "How to communicate with mjrDRsrv.  If set to a string containing 'fifo', then this value is assumed to be a
FIFO connected to a running mjrDRsrv process.  See the mjrDRsrvGO.sh script for one way to make sure that a mjrDRsrv
process is always running and listening to the FIFO. Otherwise, it is assumed that this value is the name of the
mjrDRsrv command.  In this case, a mjrDRsrv process is spawned and then used for drawing.")

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vdraw_send-command (command)
  "Send a command to mjrDRsrv.
If an active stream is not open, then create one."
  (if (not *mjr_vdraw_cmd-or-fifo*)
      (error "mjr_vdraw_send-command: No mjrDRsrv command or FIFO configured (See: *MJR_VDRAW_CMD-OR-FIFO*)!"))
  (if (not *mjr_vdraw_stream*) ;; If no existing stream, then create one
      (progn 
        (if (search "fifo" *mjr_vdraw_cmd-or-fifo*)
            (if (member :WIN32 *features*)
                (error "mjr_vdraw_send-command: FIFO mode not supported on native windows -- use POPEN mode or use under cygwin instead...")
                (progn
                  (print "If the reader prompt hasn't returned, then start-up a listening mjrDRsrv!!")
                  (setq *mjr_vdraw_stream* (open *mjr_vdraw_cmd-or-fifo* :direction :output :if-exists  :overwrite))))
            (progn
              #+CLISP (setq *mjr_vdraw_stream* (ext:make-pipe-output-stream *mjr_vdraw_cmd-or-fifo*))
              #+GCL   (setq *mjr_vdraw_stream* (open (concatenate 'string "| " *mjr_vdraw_cmd-or-fifo*) :direction :output))
              #+CMU   (setq *mjr_vdraw_stream* (ext:process-input    (ext:run-program    *mjr_vdraw_cmd-or-fifo* nil :input :stream :output nil :wait nil)))
              #+SBCL  (setq *mjr_vdraw_stream* (sb-ext:process-input (sb-ext:run-program *mjr_vdraw_cmd-or-fifo* nil :input :stream :output nil :wait nil :search t)))
              #+SCL   (setq *mjr_vdraw_stream* (ext:process-input    (ext:run-program    *mjr_vdraw_cmd-or-fifo* nil :input :stream :output nil :wait nil)))
              #-(or CLISP CMU SBCL GCL SCL)          (error "mjr_vdraw_send-command: No support for popen on this LISP!!")
              ))))
  (if (not *mjr_vdraw_stream*)
      (error "mjr_vdraw_send-command: Could not communicate with mjrDRsrv")
      (progn (format *mjr_vdraw_stream* "~a ~%" command)
             (if *mjr_vdraw_stream-echo*
                 (format 't "~a ~%" command))
             (force-output *mjr_vdraw_stream*))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vdraw_disconnect ()
  (if *mjr_vdraw_stream*
      (progn
        (ignore-errors (close *mjr_vdraw_stream*))
        (setq *mjr_vdraw_stream* nil))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_vdraw_quit            ()              "Quit"              (mjr_vdraw_send-command (format nil "q")))
(defun mjr_vdraw_clear-canvas    ()              "Clear Window"      (mjr_vdraw_send-command (format nil "cw")))
(defun mjr_vdraw_draw-circle     (x y r)         "Draw Circle"       (mjr_vdraw_send-command (format nil "dc ~f ~f ~f" x y r)))
(defun mjr_vdraw_draw-ellipse    (xc yc rr yr a) "Draw Ellipse"      (mjr_vdraw_send-command (format nil "de ~f ~f ~f ~f ~f" xc yc rr yr a)))
(defun mjr_vdraw_draw-line       (x1 y1 x2 y2)   "Draw Line"         (mjr_vdraw_send-command (format nil "dl ~f ~f ~f ~f" x1 y1 x2 y2)))
(defun mjr_vdraw_draw-box        (x1 y1 x2 y2)   "Draw Box"          (mjr_vdraw_send-command (format nil "db ~f ~f ~f ~f" x1 y1 x2 y2)))
(defun mjr_vdraw_draw-point      (x y)           "Draw Point"        (mjr_vdraw_send-command (format nil "dp ~f ~f" x y)))
(defun mjr_vdraw_draw-line-to    (x1 x2)         "draw Line To"      (mjr_vdraw_send-command (format nil "lt ~f ~f" x1 x2)))
(defun mjr_vdraw_set-bg-color    (r g b)         "BackGround color"  (mjr_vdraw_send-command (format nil "bg ~d ~d ~d" r g b)))
(defun mjr_vdraw_set-fill-color  (r g b)         "Fill Color"        (mjr_vdraw_send-command (format nil "fc ~d ~d ~d" r g b)))
(defun mjr_vdraw_set-line-color  (r g b)         "Line Color"        (mjr_vdraw_send-command (format nil "lc ~d ~d ~d" r g b)))
(defun mjr_vdraw_set-line-width  (w)             "Line Width"        (mjr_vdraw_send-command (format nil "lw ~f" w)))
(defun mjr_vdraw_set-coords      (x1 y1 x2 y2)   "User Coordinates"  (mjr_vdraw_send-command (format nil "uc ~f ~f ~f ~f" x1 y1 x2 y2)))
(defun mjr_vdraw_set-point       (x1 x2)         "Move To"           (mjr_vdraw_send-command (format nil "mt ~f ~f" x1 x2)))
(defun mjr_vdraw_set-echo        (bool)          "Echo STDOUT"       (mjr_vdraw_send-command (format nil (if bool "e1" "e0"))))
(defun mjr_vdraw_set-fill        (bool)          "Fill"              (mjr_vdraw_send-command (format nil (if bool "f1" "f0"))))
