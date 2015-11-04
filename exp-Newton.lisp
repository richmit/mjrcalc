;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-Newton.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Draw Newton fractal and output to a TGA file.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
