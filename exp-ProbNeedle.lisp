;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ProbNeedle.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2005 by Mitch Richling.  All rights reserved.
;; @brief     Buffon's Needle Problem.@EOL
;; @Std       Common Lisp
;;
;;            Drop a 1 unit long needle onto a plane with lines 1 unit apart.  Compute the probability that the needle hits some
;;            line.
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop with num-trials = (expt 10 6)                         ;; Number of trials to perform
      with num-hit = 0                                      ;; Number of needle hits (updated during simulation)
      for cur-trial from 1 upto num-trials                  ;; Primary loop control (cur-trial has current trial count)
      for a = (random (/ pi 2))                             ;; Compute a random angle between 0 and 90 degrees
      for p = (- 1 (sin a))                                 ;; Compute probability a needle at this angle misses
      for r = (random 1.0d0)                                ;; Compute a random number between 0 and 1
      do (if (> r p) (incf num-hit))                        ;; Did the needle hit?  If so, increment num-hit
      when (zerop (mod cur-trial (truncate num-trials 20))) ;; If we are on a 5% mark, then print
      do (let ((a-prob (/ num-hit cur-trial)))              ;; Compute approximate probability
           (format 't "~20,10f ~20,10f~%"                   ;; Print partial results:
                   a-prob (abs (- a-prob (/ 2 pi))))))      ;;   Approximate probability & Error from theoretical value
