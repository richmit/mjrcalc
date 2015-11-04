;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-MultiPlot.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
;; @brief     Demonstrate various ways of drawing some common graph types.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            Notes here
;;            
;;            We can plot data in three different using the calculator program:
;;
;;             1) Interactively with GNUPlot
;;             2) Offline with an interactive, VTK aware visualization tool (VisIt & ParaView are my favorites)
;;             2) Offline with an non-interactive, POV-Ray aware rendering tool
;;
;;                     |---------------------------+---------+--------+----------+---------|
;;                     | Metric                    | GNUPlot | VisIt  | ParaView | POV-Ray |
;;                     |---------------------------+---------+--------+----------+---------|
;;                     | Quality of vector output  | best    | good   | N/A      | N/A     |
;;                     | Quality of raster output  | good    | best   | good     | best    |
;;                     | Easy to do simple graphs  | best    | better | good     | poor    |
;;                     | Easy to do complex graphs | good    | best   | good     | better  |
;;                     | Analysis of data          | poor    | best   | good     | N/A     |
;;                     | Transformation of data    | poor    | best   | good     | good    |
;;                     | Update graph on new data  | best    | good   | poor     | N/A     |
;;                     | Tweak graphical appearance| poor    | better | good     | best    |
;;                     | Interaction quality       | poor    | best   | great    | N/A     |
;;                     | Combining graphs          | good    | best   | good     | better  |
;;                     |---------------------------+---------+--------+----------+---------|
;;
;;           When to use each tool:
;;            GNUPlot -- quick graphs of curves and/or surfaces
;;            VTK     -- volume data, vector fields, slope fields
;;                       Complex groupings of graphs in the same spacial domain
;;                       When sophisticated exploration of data is required
;;                       CAD objects
;;            POV-Ray -- When the results must be visually spectacular

;;----------------------------------------------------------------------------------------------------------------------------------
;; A set of points
(let* ((n-pts 400)
       (t-dat (mjr_vec_make-seq :start (* -5 pi) :end (* 5 pi) :len n-pts))
       (x-dat (map 'vector (lambda (tim) (* 7 (cos (* 1/2 tim)))) t-dat))
       (y-dat (map 'vector (lambda (tim) (* 7 (cos (* 1/5 tim)) (sin tim))) t-dat))
       (z-dat (map 'vector (lambda (tim) (* 2 (sin (* 1/5 tim)) (cos (* 1/3 tim)))) t-dat)))
  (mjr_plot_data :xdat x-dat :ydat y-dat :zdat z-dat :type :l)
  (format 't "Dumping VTK...~%")
  (mjr_vtk_from-gndata "exp-MultiPlot-OUT-crv.vtk" :xdat x-dat :ydat y-dat :zdat z-dat :scalar-array z-dat :dims (list n-pts 1 1) :poly 't)
  (format 't "Dumping Povray...~%")
  (mjr_pov_make-from-gndata "exp-MultiPlot-OUT-crv.pov" :xdat x-dat :ydat y-dat :zdat z-dat :dims (list n-pts 1 1))
  )

(format 't "Press [enter] to continue...~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
;; A set of points
(let* ((n-pts 100)
       (t-dat (mjr_vec_make-seq :start -10 :end 10 :len n-pts))
       (x-dat (map 'vector (lambda (tim) (* 7 (sin tim))) t-dat))
       (y-dat (map 'vector (lambda (tim) (* 7 (cos tim))) t-dat))
       (z-dat (mjr_vec_make-seq :start -2 :end 2 :len n-pts)))
  (mjr_plot_data :xdat x-dat :ydat y-dat :zdat z-dat :type :p)
  (format 't "Dumping VTK...~%")
  (mjr_vtk_from-gndata "exp-MultiPlot-OUT-spiral.vtk" :xdat x-dat :ydat y-dat :zdat z-dat :scalar-array z-dat :dims '(1 1 1) :poly 't)
  (format 't "Dumping Povray...~%")
  (mjr_pov_make-from-gndata "exp-MultiPlot-OUT-spiral.pov" :xdat x-dat :ydat y-dat :zdat z-dat :dims '(1 1 1))
  )

(format 't "Press [enter] to continue...~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
;; A high density surface mesh
(let* ((n-pts 100)
       (x-dat (mjr_vec_make-seq :start -7 :end 7 :len n-pts))
       (y-dat (mjr_vec_make-seq :start -7 :end 7 :len n-pts))
       (z-dat (mjr_mat_make-from-func (lambda (i j) (let ((d (sqrt (+ (* i i) (* j j))))) (* (/ 15 (+ (* d d) 8)) (cos (* 1/5 d d))))) :cpoints y-dat :rpoints x-dat)))
  (mjr_plot_data :xdat x-dat :ydat y-dat :zdat z-dat :span -1 :type :f)
  (format 't "Dumping VTK...~%")
  (mjr_vtk_from-gndata "exp-MultiPlot-OUT-hds.vtk" :xdat x-dat :ydat y-dat :zdat z-dat :scalar-array z-dat :dims (list n-pts n-pts 1) :poly 't)
  (format 't "Dumping Povray...~%")
  (mjr_pov_make-from-gndata "exp-MultiPlot-OUT-hds.pov" :xdat x-dat :ydat y-dat :zdat z-dat :dims (list n-pts n-pts 1) :draw-points nil :draw-surface-grid nil :surface-smooth 't)
  )

(format 't "Press [enter] to continue...~%")
(read-char)

;;----------------------------------------------------------------------------------------------------------------------------------
;; A low density surface mesh
(let* ((n-pts 20)
       (x-dat (mjr_vec_make-seq :start -7 :end 7 :len n-pts))
       (y-dat (mjr_vec_make-seq :start -7 :end 7 :len n-pts))
       (z-dat (mjr_mat_make-from-func (lambda (i j) (+ (sin i) (cos j))) :rpoints x-dat :cpoints y-dat)))
  (mjr_plot_data :xdat x-dat :ydat y-dat :zdat z-dat :span -1 :type :f)
  (format 't "Dumping VTK...~%")
  (mjr_vtk_from-gndata "exp-MultiPlot-OUT-lds.vtk" :xdat x-dat :ydat y-dat :zdat z-dat :scalar-array z-dat :dims (list n-pts n-pts 1) :poly 't)
  (format 't "Dumping Povray...~%")
  (mjr_pov_make-from-gndata "exp-MultiPlot-OUT-lds.pov" :xdat x-dat :ydat y-dat :zdat z-dat :dims (list n-pts n-pts 1))
  )
