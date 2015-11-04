;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      exp-Gravity.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Demonstrate the danger of assuming everything is a sphere in gravity problems.@EOL
;; @Keywords  iridium gravity sphere example
;; @Std       Common Lisp
;;
;;            We compare two scenarios:
;;            
;;            Three Iridium spheres.  One (m1) of radius 1000m located at (0, 2000), a second (m2) of the same radius located at
;;            (0, -2000), and a third (mp) of radius 10 who's location varies from (1000*2^(1/3)+10, 0) to (10000, 0).  We
;;            compute the magnitude of the gravitational force on the small sphere assuming the large spheres stay fixed.
;;            
;;            Two Iridium spheres.  One (m0) with radius 1000*2^(1/3) (twice the mass of each big sphere in the previous
;;            scenario), and a second sphere (mp) of radius 10 who's location varies from (1000*2^(1/3)+10, 0) to (10000,0).  We
;;            compute the magnitude of the gravitational force on the small sphere assuming the large sphere stays fixed.
;;            
;;            The total mass in both systems is the same.  The larger sphere in the second scenario is located at the center of
;;            mass for the two large spheres in the first scenario.  One might be tempted to combine the two large spheres into
;;            a single sphere located at the center of mass when faced with this problem; however, this experiment shows the
;;            very different forces involved.
;;            
;;            Note: I used my vec_ package, but this is a 2D problem that could have been done with complex numbers...
;;            

;;----------------------------------------------------------------------------------------------------------------------------------

(flet ((mi (r) (* (expt r 3) pi 4/3 22560))) ;; Compute the mass of an Iridium sphere of given radius
  (let* ((p   (vector 0 0))
         (b0  (vector 0 0))
         (b1  (vector 0 2000))
         (b2  (vector 0 -2000))
         (G   6.67428L-11)
         (r1  1000)
         (m1  (mi r1))
         (r2  1000)
         (m2  (mi r2))
         (r0  (* r1 (expt 2 1/3)))
         (m0  (mi r0))
         (rp  10)
         (mp  (mi rp))
         (xv  (mjr_vec_make-seq :start (+ r0 rp) :end (* 10 (+ r0 rp)) :len 100))
         (dfv (mjr_vec_ewuo xv (lambda (x) 
                                 (setf (aref p 0) x)
                                 (mjr_vec_norm-two
                                  (mjr_vec_+ (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b1) (* G m1 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b1)))
                                             (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b2) (* G m2 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b2))))))))
         (sfv (mjr_vec_ewuo xv (lambda (x)
                                 (setf (aref p 0) x)
                                 (mjr_vec_norm-two (mjr_vec_/ (mjr_vec_* (mjr_vec_- p b0) (* G m0 mp)) (mjr_vec_norm-two-squared (mjr_vec_- p b0))))))))
                                        ; Draw the graph
    (mjr_plot_data :xdat xv :ydat (list dfv sfv) 
    :xlab "distance" :ylab "force"  :main "Difference between two gravity sources and a single one")
    ))
