;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-Newton2.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Modified Newton's method fractals colored with the arg of the 20'th iterate.@EOL
;; @Std       Common Lisp
;;
;;            Note that this example uses an RGB image while most of the others use TrueColor ones.
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(let* ((n   11)
       (xmax (expt 2 n))
       (ymax (expt 2 n))
       (smax  20)
       (img1 (mjr_img_make xmax ymax :color-space :cs-rgb)))
  (declare (fixnum xmax ymax smax))
  (loop with left   short-float =  -1.2
        with top    short-float =   1.2
        with xside  short-float =   2.4
        with yside  short-float =   2.4
        with xscale short-float = (/ xside xmax)
        with yscale short-float = (/ yside ymax)
        for y fixnum from 0 upto (1- ymax)
        for cy short-float = (- top (* y yscale) )
        do (print y)
        do (loop for x fixnum from 0 upto (1- xmax)
                 for cx short-float = (+ (* x xscale) left)
                 do (loop for   z = (complex cx cy) then (if (mjr_chk_!=0 (abs z)) (- z (* 1         (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; STD
                          ;;for z = (complex cx cy) then (if (mjr_chk_!=0 (abs z)) (- z (* -.5       (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A = -.5
                          ;;for z = (complex cx cy) then (if (mjr_chk_!=0 (abs z)) (- z (* 2.0       (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A =  2.0
                          ;;for z = (complex cx cy) then (if (mjr_chk_!=0 (abs z)) (- z (* #C(-.5 2) (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A = -.5+2i
                          for cnt fixnum from 1 upto smax
                          finally (mjr_img_set-px-color img1 x y (mjr_colorizer_ut-rgb-from-gradient (/ (abs (+ pi (phase z))) (* 2 pi)) "RYGCBMR" )))))
  (mjr_img_tga-write "exp-Newton2-OUT.tga" img1 :color-space :cs-rgb))
