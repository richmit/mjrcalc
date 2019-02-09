;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-svgCircles.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Generate some SVGs with mathematical art using thousands of circles.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2011,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(let* ((n       12000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-frog.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 11 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 10 pi k) n)) 2))))
        for y = (* (sin (/ (* 17 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 12 pi k) n)) 2))))
        for s = (+ 1/80 (* 1/10 (expt (sin (/ (* (* 56 ) pi k) n)) 2)))
        for r = (* 255 (/ (+ 0.5 (expt (sin (/ (* (* 56 ) pi k) n)) 4)) 2))
        for g = (* 255 (/ (+ 0.5 (expt (cos (/ (* (* 42 ) pi k) n)) 2)) 2))
        for b = (* 255 (/ (+ 0.5 (expt (sin (/ (* (* 56 ) pi k) n)) 4)) 2))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.6 :stroke (list r g b) :stroke-width 0.3))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       15000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-gateway.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 50 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.04 :stroke (list r g b) :stroke-width 1.6))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       10000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-flower.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 6 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (* 1/10 (expt (sin (/ (* (* 50 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.04 :stroke (list r g b) :stroke-width 1.6))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       10000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-skinnyfrog.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 8 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 25 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.05 :stroke (list r g b) :stroke-width 2.0))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       20000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-victorianbulb.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 18 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 50 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.04 :stroke (list r g b) :stroke-width 1.6))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       10000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-craneknot.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 28 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 50 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.04 :stroke (list r g b) :stroke-width 3.6))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       20000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-flight.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 48 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 50 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.04 :stroke (list r g b) :stroke-width 1.6))
  (mjr_svg_finish svgf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((n       40000)
       (svgf    (mjr_svg_create "exp-svgCircles-OUT-turbulence.svg" :width 1000 :height 1000 :background "white" :xmin -1.2 :xmax 1.2 :ymin -1.2 :ymax 1.2)))
  (loop for k from 0 upto n
        for x = (* (cos (/ (* 48 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 26 pi k) n)) 2))))
        for y = (* (sin (/ (* 2 pi k) n)) (- 1 (* 3/4 (expt (cos (/ (* 24 pi k) n)) 2))))
        for s = (+ 1/80 (*  1/10 (expt (sin (/ (* (* 150 ) pi k) n)) 4)))
        for g = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        for r = (* 255 (/ (+ 0.0 (expt (cos (/ (* (* 50 ) pi k) n)) 6)) 1))
        for b = (* 255 (/ (+ 0.0 (expt (sin (/ (* (* 50 ) pi k) n)) 2)) 1.5))
        do (mjr_svg_circle svgf x y s :stroke-opacity 0.01 :stroke (list r g b) :stroke-width 1.6))
  (mjr_svg_finish svgf))
