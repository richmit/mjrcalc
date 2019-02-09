;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-svgDemo.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Demo of the SVG package.@EOF
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((svgf (mjr_svg_create "exp-svgDemo-OUT.svg" :width 1920 :height 1080 :background "pink")))
  (mjr_svg_rect svgf 0 0 1920 1080 :stroke "none" :fill "white")
  (mjr_svg_line svgf 5 10 300 500  :stroke-width 4)
  (mjr_svg_line svgf 5 10 300 400  :stroke-width 10)
  (mjr_svg_line svgf 5 10 400 400  :stroke-width 10 :stroke-linecap "round")
  (mjr_svg_line svgf 5 10 600 500  :stroke-width 10 :stroke-opacity 0.5)
  (mjr_svg_polyline svgf '((200 100) (300 200) (400 100) (500 200) (600 50)) :stroke-width 14 :fill "yellow" :stroke-linecap "round" :stroke-linejoin "round")
  (mjr_svg_polygon svgf '((700 100) (800 150) (900 300) (700 400)) :stroke-width 5  :fill "cyan")
  (mjr_svg_text svgf 800 800 "hello, world!" :font-size 50 :font-family "monospace")
  (mjr_svg_rect svgf 1000 200 100 150 )  
  (mjr_svg_rect svgf 1400 200 100 150 :stroke-width 15 :stroke-linejoin "bevel")  
  (mjr_svg_circle svgf 500 500 100 :stroke-width 5 :fill '(200 200 0))
  (mjr_svg_circle svgf 800 500 100 :stroke-width 5 :fill "red" :fill-opacity 0.5)
  (mjr_svg_circle svgf 900 500 100 :stroke-width 15 :fill "red" :stroke-opacity 0.5)
  (mjr_svg_ellipse svgf 1000 500 100 50 :stroke-width 1 :fill "red" :fill-opacity 0.5)
  (mjr_svg_finish svgf))
