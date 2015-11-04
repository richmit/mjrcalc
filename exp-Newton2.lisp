;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-Newton2.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Modified Newton's method fractals colored with the arg of the 20'th iterate.@EOL
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
;; @filedetails
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(time
 (let* ((n   11)
        (xmax (expt 2 n))
        (ymax (expt 2 n))
        (dquad (mjr_fsamp_dq-func-r123-r123 (lambda (x y)
                                              (loop with smax = 20 
                                                    ;;for z = (complex x y) then (if (mjr_chk_!=0 (abs z)) (- z (* 1         (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; STD
                                                    ;;for z = (complex x y) then (if (mjr_chk_!=0 (abs z)) (- z (* -.5       (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A = -.5
                                                    ;;for z = (complex x y) then (if (mjr_chk_!=0 (abs z)) (- z (* 2.0       (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A =  2.0
                                                      for z = (complex x y) then (if (mjr_chk_!=0 (abs z)) (- z (* #C(-.5 2) (/ (- (* z z z) 1.0) (* z z 3.0)))) 0.0) ;; A = -.5+2i
                                                    for cnt fixnum from 1 upto smax
                                                    finally (return (/ (abs (+ pi (phase z))) (* 2 pi)))))
                                            :xdat (list :start -1.2 :end 1.2 :len xmax)
                                            :ydat (list :start -1.2 :end 1.2 :len ymax))))
   (mjr_dquad_colorize dquad :data "f" :color-method "RYGCBMR" :ano-nam "c")
   (mjr_tga_from-dquad "exp-Newton2-OUT-3.tga" dquad :data "c")))
