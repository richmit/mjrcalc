;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-ODElorenzEOC.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1998,2004,2010,2012 by Mitch Richling.  All rights reserved.
;; @brief     The "Butterfly" from "The Essence of Chaos".@EOL
;; @Keywords  butterfly essence chaos
;; @Std       Common Lisp
;;
;;            The equations:
;;              $$\frac{\mathrm{d}x}{\mathrm{d}t} = -s\cdot x+s\cdot y  $$ 
;;              $$\frac{\mathrm{d}y}{\mathrm{d}t} = -x\cdot z+r\cdot x-y$$
;;              $$\frac{\mathrm{d}z}{\mathrm{d}t} = x\cdot y-b\cdot z   $$
;;
;;            We solve them using Euler's method, and plot $x$ vs. $z$.
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(let ((b 8/3)
      (s  10)
      (r  28)
      (dt 0.009)
      (st 10000)
      (x  #(1 1 1)))
  (mjr_plot_func-r1-c1 (lambda (m)
                         (and m)
                         (setq x (map 'vector #'+ x 
                                      (map 'vector (lambda (darg) (* darg dt))
                                           (list (- (* s (aref x 1))          (* s (aref x 0))) 
                                                 (- (* r (aref x 0))          (* (aref x 0) (aref x 2)) (aref x 1))
                                                 (- (* (aref x 0) (aref x 1)) (* b (aref x 2)))))))
                         (complex (aref x 0) (aref x 2)))
                       :udat (list :start 0 :end 1 :len st)))

;;----------------------------------------------------------------------------------------------------------------------------------
(format 't "Press [enter] to continue...~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
(let ((b 8/3)
      (s  10)
      (r  28)
      (dt 0.005)
      (st 10000)
      (x  #(1 1 1)))
  (mjr_plot_func-r1-r3 (lambda (m)
                         (and m)
                         (setq x (mjr_vec_+ 
                                  x 
                                  (mjr_vec_* 
                                   dt
                                   (make-array 3 
                                               :initial-contents (list (- (* s (aref x 1))          (* s (aref x 0))) 
                                                                       (- (* r (aref x 0))          (* (aref x 0) (aref x 2)) (aref x 1))
                                                                       (- (* (aref x 0) (aref x 1)) (* b (aref x 2)))))))))
                       :udat (list :start 0 :end 1 :len st)))

;;----------------------------------------------------------------------------------------------------------------------------------
(let* ((b 8/3)
       (s  10)
       (r  28)
       (dt 0.009)
       (st 10000)
       (x  #(1 1 1)))
  (flet ((nxt-point ()
           (setq x (map 'vector #'+ x 
                        (map 'vector (lambda (darg) (* darg dt))
                             (list (- (* s (aref x 1))          (* s (aref x 0))) 
                                   (- (* r (aref x 0))          (* (aref x 0) (aref x 2)) (aref x 1))
                                   (- (* (aref x 0) (aref x 1)) (* b (aref x 2)))))))))
    (with-open-file (stream "exp-ODElorenzEOC-OUT.tab" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for n from 1 upto st do (progn (nxt-point)
                                           (format stream "~f ~f ~f~%" (aref x 0) (aref x 1) (aref x 2)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(print "Note: The data for the graph may be found in the file 'exODElorenzEOC.tab'...")
