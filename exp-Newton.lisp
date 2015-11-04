;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-Newton.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Draw Newton fractal and output to a TGA file.@EOL
;; @Keywords  tga newton fractal
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(let* ((n    11)
       (xmax (expt 2 n))
       (ymax (expt 2 n))
       (smax  256)
       (sfac1 15)
       (sfac2 300)
       (img1 (mjr_img_make xmax ymax))
       (img2 (mjr_img_make xmax ymax)))
  (declare (fixnum xmax ymax smax sfac1 sfac2))
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
                 do (loop for z = (complex cx cy) then (if (mjr_chk_!=0 (abs z)) (- z (/ (- (* z z z) 1.0) (* z z 3.0))) 0.0)
                          for cnt fixnum from 1
                          maximize (abs z) into zmax
                          until (or (>= cnt smax) (mjr_eps_= 0 (- (* z z z) 1) .0001))
                          finally (let ((cm1 (mod (* cnt sfac1) 256))
                                        (cb  (cond ((mjr_eps_= z #C( 1.0  0.0000000) .0001) #(1 0 0))
                                                   ((mjr_eps_= z #C(-0.5  0.8660254) .0001) #(0 1 0))
                                                   ((mjr_eps_= z #C(-0.5 -0.8660254) .0001) #(0 0 1))
                                                   ('t                                      #(0 0 0))))
                                        (cm2 (mod (truncate (* sfac2 zmax)) 256)))
                                    (mjr_img_set-px-color img1 x y (mjr_vec_- (mjr_vec_* 255 cb) (mjr_vec_* cm1 cb)))
                                    (mjr_img_set-px-color img2 x y (mjr_vec_- (mjr_vec_* 255 cb) (mjr_vec_* cm2 cb)))))))
  (mjr_img_tga-write "exp-Newton-OUT-1.tga" img1)
  (mjr_img_tga-write "exp-Newton-OUT-2.tga" img2))
