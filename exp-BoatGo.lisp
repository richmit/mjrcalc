;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-BoatGo.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Drug smuggler boat path@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2011,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;  A boat starts out at $(1,0)$.  A light house is located at $(0,0)$.  The light house operator points the light at the boat at all times.  The boat
;;  operator keeps the boat moving in a direction $90$ degrees to the light at a constant speed.  What is the path of the boat?
;;
;;  NOTE: This is a ODE, and could be easily solved with the ODE package; however, we directly implement an Euler leapfrog method just for fun.
;;
;;  How to get a nice PNG of the output:
;;
;;   1) In the REPL:
;;       (mjr_gnupl_send-command "set term pdfcairo")
;;       (mjr_gnupl_send-command "set output \"foo.pdf\"")
;;       (load "exp-BoatGo.lisp")
;;       (mjr_gnupl_send-command "set output")
;;   2) In shell:
;;       convert -density 300 -resize 1024x768 foo.pdf exBoatGo-ART.png
;;       rm foo.pdf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((lightDir 0)
       (boatLoc  #C(1 0))
       (curve    (mjr_fsamp_dq-func-r123-r123 (lambda (tim)
                                                (declare (ignore tim))
                                                (let ((md       0.1)
                                                      (boatDir  (+ lightDir (/ pi 4))))
                                                  (incf boatLoc (complex (* md (cos boatDir)) (* md (sin boatDir))))
                                                  (setf lightDir (phase boatLoc))
                                                  (vector (realpart boatLoc) (imagpart boatLoc))))
                                              :xdat '(:start 0 :end 1 :len 10000))))
  (mjr_gnupl::mjr_gnupl_dquad curve :title "Smuggler Boat Path")
  (mjr_vtk_from-dquad "exp-BoatGo-OUT.vtk" curve))
