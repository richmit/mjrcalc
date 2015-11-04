;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODEalgCmp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Solve a simple ODE, and plot the results. @EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2010,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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

(time (let ((do-int nil) ;; MJR_ODE_SLV-IVP-ERK-INTERVAL or MJR_ODE_SLV-IVP-ERK-MESH
            (slvarg (list :x-delta-max .5
                          :y-err-abs-max 1d-6
                          ;;:y-delta-abs-max 1
                          ;;:y-delta-rel-max 0.2
                          ;;:algorithm #'mjr_ode_erk-step-runge-kutta-4
                          ;;:algorithm #'mjr_ode_erk-step-heun-euler-1-2
                          ;;:algorithm #'mjr_ode_erk-step-bogackia-shampine-3-2
                          ;;:algorithm #'mjr_ode_erk-step-merson-4-5
                          ;;:algorithm #'mjr_ode_erk-step-zonneveld-4-3
                          ;;:algorithm #'mjr_ode_erk-step-fehlberg-4-5
                          ;;:algorithm #'mjr_ode_erk-step-dormand-prince-5-4
                          ;;:algorithm #'mjr_ode_erk-step-cash-karp-5-4
                          ;;:algorithm #'mjr_ode_erk-step-verner-6-5
                          :algorithm #'mjr_ode_erk-step-fehlberg-7-8
                          ;;:suppress-warnings 't
                          ;;:show-progress 't
                          ))
            (start  3.0)
            (end    6.0)
            (ivy    48.0))
        (flet ((f (x y) (or y) (mjr_poly_eval #(7 -168 1630 -8080 21075 -26000 10000) x))
               (s (x)   (mjr_poly_eval #(1 -28 326 -2020 7025 -13000 10000 0) x)))
          (let* ((num-sol (if do-int
                              (apply #'mjr_ode_slv-ivp-erk-interval #'f ivy start end :return-all-steps 't         slvarg)
                              (apply #'mjr_ode_slv-ivp-erk-mesh     #'f ivy (list :start start :end end :step 0.1) slvarg)))
                 (data    (mjr_dquad_make-from-axis "x" (mjr_arr_get-col num-sol 0))))
            (mjr_dquad_add-data-from-map data #'s                          :ano-nam "Symbolic"  :ano-typ :ano-typ-real :axes 't)
            (mjr_dquad_add-data          data  (mjr_arr_get-col num-sol 1) :ano-nam "Numerical" :ano-typ :ano-typ-real)
            (mjr_gnupl_dquad data :main "Numerical vs Symbolic Solution" :ylim '(-2 50) :type (list :l :p))))))
