;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-pursuit.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Drug smuggler boat path@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2011,2015,2017 Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;  Interesting things for the next step....
;;   * Model cat & mouse in terms of vehicle dynamics -- i.e. turning radius, acceleration capabilities, etc....
;;   * Model cat & mouse in terms of external forces like gravity and/or drag (space craft in a solar system or aircraft/submarines near earth).
;;   * Obstacles & collisions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pursuit (steps mouse-step-function cat-step-function mouse-initial-loc cat-initial-loc)
  "Execute a pursuit scheme, and return a dquad with a time axis, and two vector data components (one for the cat and one for the mouse)

The problem is discredited into STEPS points on time [0, 1].  The mouse-step-function and cat-step-function consume system state, and produce
next mouse and cat location respectively.  These functions take cat-position, mouse-position, time, step, and time-delta.  Locations are vectors
in 1, 2, or 3 space."
  (let* ((steps-1     (1- steps))
         (step-delta  (/ 1.0 steps-1))
         (curves (mjr_dquad_make-from-axis '((:ano-nam . "time") (ano-typ . :ano-typ-real)) (list :start 0.0 :end 1.0 :len steps))))
    (let ((m-locs (make-array steps))
          (c-locs (make-array steps)))
      (loop for step from 0 upto steps-1
            for time = (* step step-delta)
            for c-loc = cat-initial-loc    then (funcall cat-step-function    c-loc m-loc time step step-delta)
            for m-loc = mouse-initial-loc  then (funcall mouse-step-function  c-loc m-loc time step step-delta)
            do (setf (aref m-locs step) m-loc)
            do (setf (aref c-locs step) c-loc))
      (mjr_dquad_add-data curves c-locs :ano-nam "cat"   :ano-typ :ano-typ-rvec)
      (mjr_dquad_add-data curves m-locs :ano-nam "mouse" :ano-typ :ano-typ-rvec))
    curves))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Both cat and mouse go strait up
(let ((c-vel  1.50)
      (m-vel  1.00)
      (c-0    #(0 0 0))
      (m-0    #(1 0 0)))
  (let ((curves (pursuit
                 10
                 (lambda (c m tim step time-step) (declare (ignore c m step time-step)) (mjr_vec_+ m-0 (vector 0 (* m-vel tim) 0)))
                 (lambda (c m tim step time-step) (declare (ignore c m step time-step)) (mjr_vec_+ c-0 (vector 0 (* c-vel tim) 0)))
                 m-0
                 c-0)))
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-c.vtk" (mjr_dsimp_make-from-dquad curves "time" "cat")   :simplices 1)
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-m.vtk" (mjr_dsimp_make-from-dquad curves "time" "mouse") :simplices 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pierre Bouguer's Pirate Ship Problem (1732):
;;   Pursuer (cat)   Initial Location:  $(0, 0, 0)$
;;   Pursuer (cat)   Location:          Pure Pursuit (aims toward mouse at each step)
;;   Pursued (mouse) Initial Location:  $(1, 0, 0)$
;;   Pursued (mouse) Location:          $m(t)=(1, t\cdot V_m)$
(let ((c-vel  1.50)
      (m-vel  1.00)
      (c-0    #(0 0 0))
      (m-0    #(1 0 0)))
  (let ((curves (pursuit
                 1000
                 (lambda (c m tim step time-step)
                   (declare (ignore c m step time-step))
                   (mjr_vec_+ m-0 (vector 0 (* m-vel tim) 0)))
                 (lambda (c m tim step time-step)
                   (declare (ignore tim step))
                   (let* ((dirv (mjr_vec_- m c))
                          (dird (mjr_vec_norm-two dirv)))
                     (if (mjr_chk_!=0 dird)
                         (mjr_vec_+ c (mjr_vec_* dirv (* c-vel time-step)))
                         m)))
                 m-0
                 c-0)))
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-c.vtk" (mjr_dsimp_make-from-dquad curves "time" "cat")   :simplices 1)
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-m.vtk" (mjr_dsimp_make-from-dquad curves "time" "mouse") :simplices 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arthur Stafford Hathaway (1920) -- dog 'n duck
;;   Pursuer (cat)   Initial Location:  $(0, 0, 0)$
;;   Pursuer (cat)   Location:          
;;   Pursued (mouse) Initial Location:  $(1, 0, 0)$
;;   Pursued (mouse) Location:          $m(t)=(cos(t\cdot V_m), sin(t\cdot V_m))  -- i.e. duck swim in a circle
(let ((c-vel  (* 3 pi))
      (m-vel  (* 4 pi))
      (c-0    #(0 0 0))
      (m-0    #(1 0 0)))
  (let ((curves (pursuit
                 1000
                 (lambda (c m tim step time-step)
                   (declare (ignore c m step time-step))
                   (vector (cos (* m-vel tim)) (sin (* m-vel tim)) 0))
                 (lambda (c m tim step time-step)
                   (declare (ignore tim step))
                   (let* ((dirv (mjr_vec_- m c))
                          (dird (mjr_vec_norm-two dirv)))
                     (if (mjr_chk_!=0 dird)
                         (mjr_vec_+ c (mjr_vec_* dirv (* c-vel time-step)))
                         m)))
                 m-0
                 c-0)))
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-c.vtk" (mjr_dsimp_make-from-dquad curves "time" "cat")   :simplices 1)
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-m.vtk" (mjr_dsimp_make-from-dquad curves "time" "mouse") :simplices 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chase a target on a helical course
(let ((c-vel  (* 3 pi))
      (m-vel  (* 15 pi))
      (c-0    #(1 1 5))
      (m-0    #(1 0 0)))
  (let ((curves (pursuit
                 1000
                 (lambda (c m tim step time-step)
                   (declare (ignore c m step time-step))
                   (vector (cos (* m-vel tim)) (sin (* m-vel tim)) tim))
                 (lambda (c m tim step time-step)
                   (declare (ignore tim step))
                   (let* ((dirv (mjr_vec_- m c))
                          (dird (mjr_vec_norm-two dirv)))
                     (if (mjr_chk_!=0 dird)
                         (mjr_vec_+ c (mjr_vec_* dirv (* c-vel time-step)))
                         m)))
                 m-0
                 c-0)))
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-c.vtk" (mjr_dsimp_make-from-dquad curves "time" "cat")   :simplices 1)
    (mjr_vtk_from-dsimp "exp-pursuit-OUT-m.vtk" (mjr_dsimp_make-from-dquad curves "time" "mouse") :simplices 1)))
