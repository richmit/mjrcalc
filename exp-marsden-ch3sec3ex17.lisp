;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-marsden-ch3sec3ex17.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @date      2015-09-25
;; @version   VERSION
;; @brief     Solving a vector calculus problem.@EOL
;; @keywords  
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;  An example function used for several problems in Marsden's vector calculus text.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((fstr  "(x^2+3*y^2)*exp(1-(x^2+y^2))")
      (xlim  '(-2 2))
      (ylim  '(-2 2))
      (ngrd  100))
  ;;(mjr_mxp_tree-to-code (mjr_mxp_infix-to-tree "(x^2+3*y^2)*exp(1-(x^2+y^2))"))
  ;;(* (+ (EXPT X 2) (* 3 (EXPT Y 2))) (EXP (- 1 (+ (EXPT X 2) (EXPT Y 2)))))
  ;;(mjr_mxp_tree-to-code (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "(x^2+3*y^2)*exp(1-(x^2+y^2))") "x")))
  ;;(+ (* -2 X (+ (* 3 (EXPT Y 2)) (EXPT X 2)) (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))) (* 2 X (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))))
  ;;(mjr_mxp_tree-to-code (mjr_cas_canonize (mjr_cas_diff (mjr_mxp_infix-to-tree "(x^2+3*y^2)*exp(1-(x^2+y^2))") "y")))
  ;;(+ (* -2 Y (+ (* 3 (EXPT Y 2)) (EXPT X 2)) (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))) (* 6 Y (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))))
  (flet ((f  (x y) (* (+ (EXPT X 2) (* 3 (EXPT Y 2))) (EXP (- 1 (+ (EXPT X 2) (EXPT Y 2))))))
         (dx (x y) (+ (* -2 X (+ (* 3 (EXPT Y 2)) (EXPT X 2)) (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))) (* 2 X (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2))))))))
         (dy (x y) (+ (* -2 Y (+ (* 3 (EXPT Y 2)) (EXPT X 2)) (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))) (* 6 Y (EXP (+ 1 (* -1 (+ (EXPT X 2) (EXPT Y 2)))))))))
    (let ((dq (mjr_fsamp_dq-func-r123-r123 (list #'f #'dx #'dy)
                                           :func-lab '("f" "Dx" "Dy")
                                           :xdat (list :start (first xlim) :end (second xlim) :len ngrd)
                                           :ydat (list :start (first ylim) :end (second ylim) :len ngrd))))
      (mjr_vtk_from-dquad "exp-marsden-ch3sec3ex17-OUT.vtk" dq :data '(0 1 2) )
      ;;(mjr_gnupl_dquad dq :data '(0))
      )))
