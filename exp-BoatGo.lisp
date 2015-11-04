;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-BoatGo.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
;; @brief     Drug smuggler boat path@EOL
;; @Keywords  mjrcalc example pursuit problem boat drug smuggler lisp 
;; @Std       Common Lisp
;;
;;            A boat starts out at $(1,0)$.  A light house is located at $(0,0)$.  The light house operator points the light at the
;;            boat at all times.  The boat operator keeps the boat moving in a direction $90$ degrees to the light at a constant
;;            speed.  What is the path of the boat?
;;
;;            NOTE: This is a ODE, and could be easily solved with the ODE package; however, we directly implement an Euler leapfrog
;;            method just for fun.
;;
;;            How to get a nice PNG of the output:
;;
;;             1) In the REPL:
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set term pdfcairo")
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set output \"foo.pdf\"")
;;                 (load "exp-BoatGo.lisp")
;;                 (mjr_plot::mjr_plot_drv-gnup-send-command "set output")
;;             2) In shell:
;;                 convert -density 300 -resize 1024x768 foo.pdf exBoatGo-ART.png
;;                 rm foo.pdf
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(let ((lightDir 0)
      (boatLoc  #C(1 0)))
  (mjr_plot_func-r1-c1
   (lambda (tim)
     (and tim)
     (let ((md       0.1)
           (boatDir  (+ lightDir (/ pi 4))))
       (incf boatLoc (complex (* md (cos boatDir)) (* md (sin boatDir))))
       (setf lightDir (phase boatLoc))
       boatLoc))
   :title "Smuggler Boat Path"
   :udat '(:start 0 :end 1 :len 10000)))
