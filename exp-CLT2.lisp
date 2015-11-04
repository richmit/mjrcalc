;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-CLT2.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1995,1996,2013 by Mitch Richling.  All rights reserved.
;; @brief     Probability distributions for sums of a non-normal empirically defined probability distributions.@EOL
;; @Keywords  theoretical probability distributions sums non-normal empirical epdf
;; @Std       Common Lisp
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defun exCLT2-draw-pdf-seq (epdf num-frames pow2 fps)
  "Draw NUM-FRAMES no faster than FPS frames per second of n*epdf
N=1,2,...(NUM-FRAMES) when pow2 is NIL, and N=2^k for k=0,1,...,(1- NUM-FRAMES) when pow2 is non-NIL"
  (let ((epdf (map 'vector (lambda (x) (float x 1.0d0)) epdf)))
    (loop for tim1 = (get-internal-run-time)
          for i from 0 upto (1- num-frames)
          for cepdf = epdf then (if pow2 (mjr_probe_epdf-+ cepdf cepdf) (mjr_probe_epdf-+ epdf cepdf))
          for m = (mjr_probe_epdf-e cepdf)
          for v = (mjr_probe_epdf-v cepdf)
          for minx = (- m (* 5 (sqrt v)))
          for maxx = (+ m (* 5 (sqrt v)))
          for gx = (mjr_vec_make-seq :start minx :end maxx :len 100)
          for gy = (mjr_vec_make-from-func (lambda (x) (mjr_prob_normal-pdf x m v)) :points gx)
          for px = (mjr_vec_make-seq :start 0 :end (1- (length cepdf)))
          do (mjr_plot_data :main (if pow2 (expt 2 i) (1+ i))
                            :xdat (list px gx)
                            :ydat (list cepdf gy)
                            :xlim (list minx maxx)
                            :ylim (list 0 (* 2.1 (reduce #'max gy))))
          (loop for tim2 = (get-internal-run-time)
                until (> (- tim2 tim1) (truncate internal-time-units-per-second fps))))))

;(exCLT2-draw-pdf-seq #(0 1/6 1/6 1/6 1/6 1/6 1/6 0)  30 nil 5)       ;; a 6-sided die
;(exCLT2-draw-pdf-seq #(0 1/4 1/4 1/4 1/4 0)          30 nil 5)       ;; a 4-sided die

;(exCLT2-draw-pdf-seq #(0/14 6/14 2/14 6/14 0/14)     60 nil 5)       ;; no middle zero -- SLOW convergence
;(exCLT2-draw-pdf-seq #(0/14 7/14 0/14 7/14 0/14)     60 nil 5)       ;; middle zero    -- FAST convergence

;(exCLT2-draw-pdf-seq #(49/100 2/100 49/100)         130 nil 25)       ;; V with low middle
;(exCLT2-draw-pdf-seq #(50/100 0/100 50/100)         130 nil 25)       ;; V with zero middle

;(exCLT2-draw-pdf-seq #(1/100 98/100 1/100)          150 nil 125)      ;; tent with low sides
;(exCLT2-draw-pdf-seq #(5/100 90/100 5/100)           60 nil 125)      ;; tent with high sides
;(exCLT2-draw-pdf-seq #(1/100 98/100 1/100)           11 't  5)        ;; tent with low sides geometric

;(exCLT2-draw-pdf-seq #(1/100 99/100)                 15 't  5)        ;; lean-to with low side
;(exCLT2-draw-pdf-seq #(5/100 95/100)                 13 't  5)        ;; lean-to with mid side
;(exCLT2-draw-pdf-seq #(10/100 90/100)                10 't  5)        ;; lean-to with high side
