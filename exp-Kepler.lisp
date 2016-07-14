;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-Kepler.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Draw Newton-like fractal and output to a TGA file.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;
;;  Kepler's equation is
;;  
;;      $$M = E - e\cdot\sin(E)$$
;;  
;;  We fix $M=1$, the mean anomaly, and $e=0.083$, the eccentricity.  Newton's method can then be used to solve for $E$, the eccentric anomaly.
;;  While $E$ is always a real number in celestial mechanics applications, we consider her the case of a complex valued $E$ and draw Newton-type
;;  fractals.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (speed 3) (safety 0) ( debug 0) (compilation-speed 0)))

(defun drawNewtonFract (x0 y0 x1 y1 out-file max-z max-d max-c sfac xmax ymax col-method fdf)
  (let ((dquad (mjr_fsamp_dq-func-r123-color (lambda (x y)
                                               (loop with ctr    complex     = (complex (/ (+ x0 x1) 2) (/ (+ y0 y1) 2))
                                                     for (tv bv) (complex complex) = '(#C(1.0 0.0) #C(1.0 0.0)) then (funcall fdf z)
                                                     for z complex = (complex x y) then (if (mjr_chk_!=0 (abs bv)) (- z (/ tv bv)) 0)
                                                     for zd short-float = (abs (- ctr z))
                                                     for cnt fixnum from 1
                                                     for fv = (cond ((<= max-d zd)        (vector   0   0 255))
                                                                    ((<= max-z (abs z))   (vector   0   0 150))
                                                                    ((< max-c cnt)        (vector   0   0 100))
                                                                    ((mjr_eps_=0 tv 1e-3) (let ((cm1 (case col-method
                                                                                                       (:col-itr  (mod (* sfac cnt) 255))
                                                                                                       (:col-maxd (truncate (mod (* sfac zd)  255) 1)))))
                                                                                            (vector (- 255 cm1) cm1 0))))
                                                     do (if fv (return fv))))
                                             :ano-typ :ano-typ-truvec
                                             :xdat (list :start x0 :end x1 :len xmax)
                                             :ydat (list :start y0 :end y1 :len ymax))))
    (mjr_tga_from-dquad out-file dquad :data "c")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (drawNewtonFract -66.5  0.2               ;; x0 y0       | -0.5 2.7 | -66.5 0.2 |
                       -59.5  8.2               ;; x1 y1       |  0.4 3.6 | -59.5 8.2 |
                       "exp-Kepler-OUT-3.tga"   ;; out-file
                       90.0                     ;; max-z
                       95.0                     ;; max-d       | 50.0     | 95.0     |
                       40                       ;; max-c
                       130                      ;; sfac
                       30                       ;; sfac
                       (expt 2 9)               ;; xmax
                       (expt 2 9)               ;; ymax
                       :col-itr                 ;; col-method
                       (lambda (z)              ;; func and diff
                         (declare ((complex (short-float)) z))
                         (list (- z           (* #C(0.08 0.0) (sin z)) #C(1.0 0.0))
                               (- #C(1.0 0.0) (* #C(0.08 0.0) (cos z)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (drawNewtonFract -66.5  0.2               ;; x0 y0 
                       -59.5  8.2               ;; x1 y1 
                       "exp-Kepler-OUT-4.tga"   ;; out-file
                       90.0                     ;; max-z
                       95.0                     ;; max-d 
                       40                       ;; max-c
                       130                      ;; sfac
                       (expt 2 11)              ;; xmax
                       (expt 2 11)              ;; ymax
                       :col-maxd                ;; col-method
                       (lambda (z)              ;; func and diff
                         (declare ((complex (short-float)) z))
                         (list (- z           (* #C(0.08 0.0) (sin z)) #C(1.0 0.0))
                               (- #C(1.0 0.0) (* #C(0.08 0.0) (cos z)))))))
