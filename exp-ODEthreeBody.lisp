;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-ODEthreeBody.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     A famous 3-body problem.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2010,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      @EOL@EOL
;; @warning   @EOL@EOL
;; @bug       @EOL@EOL
;; @filedetails
;;
;;  This particular example is documented in two good texts:
;;    Butcher (2008); Numerical Methods for Ordinary Differential Equations; p29-30
;;    Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p129-130
;;  Set ACTION to select option:
;;    * (setf action :threeLobe-fehlberg)
;;    * (setf action :threeLobe-euler)
;;    * (setf action :threeLobe-rk4)
;;    * (setf action :fiveLobe)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (let ((action (if (boundp 'action) (symbol-value 'action) :fiveLobe)))
        (flet ((pvf (time y) 
                 (declare (ignore time))
                 (let* ((x1  (aref y 0))
                        (x2  (aref y 1))
                        (v1  (aref y 2))
                        (v2  (aref y 3))
                        (mu  (/ 1 81.45d0))
                        (s1  (+ x1 mu -1))
                        (s2  (- 1 mu))
                        (s3  (+ x1 mu))
                        (x22 (expt x2 2))
                        (s12 (expt s1 2))
                        (s32 (expt s3 2))
                        (bf1 (expt (+ x22 s12) 3/2))
                        (bf2 (expt (+ x22 s32) 3/2)))
                   (declare (type double-float x1 x2 v1 v2 mu s1 s2 s3 x22 s12 s32 bf1 bf2))
                   (vector v1
                           v2
                           (- (* 2 v2) (- x1) (/ (* mu s1) bf1) (/ (* s2 s3) bf2))
                           (- x2 (* 2 v1) (/ (* mu x2) bf1) (/ (* s2 x2) bf2))))))
          (let ((sol (cond ((eq action :fiveLobe)           (mjr_ode_slv-ivp-erk-interval #'pvf
                                                                                          #(0.87978d0 0d0 0d0 -0.3797d0)
                                                                                          0d0 19.14045706162071d0
                                                                                          :x-delta-min   1d-15
                                                                                          :y-err-abs-max 1d-10
                                                                                          :x-delta-max (/ 17.06521656015796d0 1000)
                                                                                          :algorithm #'mjr_ode_erk-step-fehlberg-7-8
                                                                                          :return-all-steps 't))
                           ((eq action :threeLobe-fehlberg) (mjr_ode_slv-ivp-erk-interval #'pvf
                                                                                          #(0.994d0 0d0 0d0 -2.0015851063790825224d0)
                                                                                          0d0 17.06521656015796d0
                                                                                          :x-delta-min   1d-15
                                                                                          :y-err-abs-max 1d-10
                                                                                          :x-delta-max (/ 17.06521656015796d0 1000)
                                                                                          :algorithm #'mjr_ode_erk-step-fehlberg-7-8
                                                                                          :return-all-steps 't))
                           ((eq action :threeLobe-rk4)      (mjr_ode_slv-ivp-erk-interval #'pvf
                                                                                          #(0.994d0 0d0 0d0 -2.0015851063790825224d0)
                                                                                          0d0 17.06521656015796d0
                                                                                          :x-delta-max (/ 17.06521656015796d0 6000)
                                                                                          :algorithm #'mjr_ode_erk-step-runge-kutta-4
                                                                                          :return-all-steps 't))
                           ((eq action :threeLobe-euler)    (mjr_ode_slv-ivp-erk-interval #'pvf
                                                                                          #(0.994d0 0d0 0d0 -2.0015851063790825224d0)
                                                                                          0d0 17.06521656015796d0
                                                                                          :x-delta-max (/ 17.06521656015796d0 24000)
                                                                                          :algorithm #'mjr_ode_erk-step-euler-1
                                                                                          :return-all-steps 't)))))
            ;; Add a single data element with the path points.
            (mjr_dquad_add-data-from-map sol #'vector :data '("y_0" "y_1") :ano-nam "path"     :ano-typ :ano-typ-rvec)
            (mjr_dquad_add-data-from-map sol #'vector :data '("y_2" "y_3") :ano-nam "velocity" :ano-typ :ano-typ-rvec)
            ;; Dump it to a VTK file
            (mjr_vtk_from-dsimp (concatenate 'string "exp-ODEthreeBody-OUT-" (symbol-name action) ".vtk")  
                                (mjr_dsimp_make-from-dquad sol 0 "path" :domain-data-names "time" :data "velocity")
                                :simplices 1)))))
