;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-sierpinski.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Draw a colorful Sierpinski gasket-like thingy, and output the drawing as an SVG file
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2010,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;   convert -background black -flatten exp-sierpinski-OUT.svg  exp-sierpinski-ART.png
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (dest "exp-sierpinski-OUT.svg" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (labels ((dump-tri (color v0 v1 v2) (format dest
                                              "<polygon points='~a,~a ~a,~a ~a,~a' fill='~a' stroke='black' stroke-width='0' />~%"
                                              (truncate (aref v0 0)) (truncate (aref v0 1))
                                              (truncate (aref v1 0)) (truncate (aref v1 1))
                                              (truncate (aref v2 0)) (truncate (aref v2 1))
                                              color)))
    (format dest "<svg width='1182' height='1024'>~%")
    (loop with tris = '((#(1 1024) #(1182 1024) #(591 1)))
          for i from 1 upto 8
          for r = 255 then (max 0 (- r 32))
          for g = (- 255 (truncate r 4))
          for b = (- 255 r)
          do (loop with new-tris = nil
                   for (v0 v1 v2) in tris
                   for v01 = (mjr_vec_/ (mjr_vec_+ v0 v1) 2) ;; (map 'vector (lambda (x) (/ x 2)) (map 'vector #'+ v0 v1) 2)
                   for v02 = (mjr_vec_/ (mjr_vec_+ v0 v2) 2)
                   for v12 = (mjr_vec_/ (mjr_vec_+ v1 v2) 2)
                   do (push (list v0  v02 v01) new-tris)
                   do (push (list v02 v2  v12) new-tris)
                   do (push (list v01 v12 v1 ) new-tris)
                   do (dump-tri (format nil "#~2,'0x~2,'0x~2,'0x" r g b) v01 v02 v12)
                   finally (setf tris new-tris)))
    (format dest "</svg>~%")))
