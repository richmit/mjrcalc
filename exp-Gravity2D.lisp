;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-Gravity2D.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Demonstrate the danger of assuming everything is a sphere in gravity problems.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  We compare two scenarios:
;;  
;;  Three Iridium spheres.  One (m1) of radius 1000m located at (0, 2000), a second (m2) of the same radius located at (0, -2000), and a third (mp) of radius
;;  10 who's location varies from (1000*2^(1/3)+10, 0) to (10000, 0).  We compute the magnitude of the gravitational force on the small sphere assuming the
;;  large spheres stay fixed.
;;  
;;  Two Iridium spheres.  One (m0) with radius 1000*2^(1/3) (twice the mass of each big sphere in the previous scenario), and a second sphere (mp) of radius
;;  10 who's location varies from (1000*2^(1/3)+10, 0) to (10000,0).  We compute the magnitude of the gravitational force on the small sphere assuming the
;;  large sphere stays fixed.
;;  
;;  The total mass in both systems is the same.  The larger sphere in the second scenario is located at the center of mass for the two large spheres in the
;;  first scenario.  One might be tempted to combine the two large spheres into a single sphere located at the center of mass when faced with this problem;
;;  however, this experiment shows the very different forces involved.
;;  
;;  Note: I used my vec_ package, but this is a 2D problem that could have been done with complex numbers...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flet ((mi (r) (* (expt r 3) pi 4/3 22560))) ;; Compute the mass of an Iridium sphere of given radius
  (let* ((p   (vector 0 0))
         (b0  (vector 0 0))
         (b1  (vector 0 2000))
         (b2  (vector 0 -2000))
         (G   6.67428L-11)
         (r1  1000)
         (m1  (mi r1))
         (r2  1000)
         (m2  (mi r2))
         (r0  (* r1 (expt 2 1/3)))
         (m0  (mi r0))
         (rp  10)
         (mp  (mi rp))
         (dat (mjr_dquad_make-from-axis "x" (list :start (+ r0 rp) :end (* 10 (+ r0 rp)) :len 100))))
    (mjr_dquad_add-data-from-map dat
                                 (lambda (x) 
                                   (setf (aref p 0) x)
                                   (mjr_vec_norm-two
                                    (mjr_vec_+ (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b1) (* G m1 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b1)))
                                               (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b2) (* G m2 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b2))))))
                                 :axes 't
                                 :ano-nam "dfv"
                                 :ano-typ :ano-typ-real)
    (mjr_dquad_add-data-from-map dat
                                 (lambda (x)
                                   (setf (aref p 0) x)
                                   (mjr_vec_norm-two (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b0) (* G m0 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b0)))))
                                 :axes 't
                                 :ano-nam "sfv"
                                 :ano-typ :ano-typ-real)
    (mjr_gnupl_dquad dat :xlab "distance" :ylab "force" :main "Difference between two gravity sources and a single one")))
