;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two POIs
(labels ((f (x y) (+ 1 (* 0 x y))))
  (let* ((bqt (mjr_rptree_new 8 #(-8 -8) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :domain-POI (list #(-7 7) #(7 7)) :min-depth 0 :domain-POI-near 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single POI (on vertex)
(labels ((f (x y) (/ (+ 1/5 (mjr_numu_hypot (- x 7) (- y 7))))))
  (let* ((bqt (mjr_rptree_new 8 #(-8 -8) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :domain-POI (list #(7 7)) :min-depth 0 :domain-POI-near 2 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single POI (off vertex)
(labels ((f (x y) (/ (+ 1/5 (mjr_numu_hypot (- x 7.1) (- y 6.9))))))
  (let* ((bqt (mjr_rptree_new 8 #(-8 -8) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :domain-POI (list #(7.1 6.9)) :min-depth 0 :domain-POI-near 2 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Single singularity (off vertex)
(labels ((f (x y) (/ (mjr_numu_hypot (- x 7.1) (- y 7)))))
  (let* ((bqt (mjr_rptree_new 8 #(-8 -8) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :domain-POI (list #(7 7)) :min-depth 0 :domain-POI-near 2 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single singularity (on vertex)
(labels ((f (x y) (/ (mjr_numu_hypot (- x 7) (- y 7)))))
  (let ((bqt (mjr_rptree_new 10 #(-8 -8) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :domain-POI (list #(7 7)) :min-depth 1 :max-depth 8 :domain-POI-near 2 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Axis crossing
(labels ((f (v) (- (mjr_vec_norm-two v) 4)))
  (let ((bqt (mjr_rptree_new 7 #(-8.1 -8.1) #(8 8))))
    (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :arg-mode :arg-vector :min-depth 1 :refine-on-axis '(x y z) :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_FSAMP_RP-FUNC-R2-R1-ADAPTIVE")
    (mjr_rptree_balance bqt #'f :arg-mode :arg-vector :func-check #'realp :depth 1 :show-progress 't)
    (mjr_rptree_print-meta bqt :title "After MJR_RPTREE_BALANCE")
    (mjr_ply3d_from-dsimp "bq.ply" (mjr_dsimp_make-from-rptree bqt 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Half sphere shifted down 1 
(labels ((f (x y) (max -1 (min 1 (/ (* (+ (- (+ (- (- (* (* 2 (EXPT X 2)) Y) (* 2 (EXPT X 2))) (* 3 X)) (EXPT Y 3)) (* 33 Y)) 32) (+ (+ (EXPT (- X 2) 2) (EXPT Y 2)) 3)) 20000)))))
  (let ((bqt (mjr_rptree_new 8 #(-16 -7) #(16 7))))
    (format 't "Sample~%")
    (time (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :slice '(z 0) :min-depth 1 :show-progress 't))
    (mjr_rptree_print-meta bqt)
    (format 't "Balance~%")
    (time (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 3 :show-progress 't))
    (mjr_rptree_print-meta bqt)
    (format 't "make DSIMP~%")
    (let ((dso (time (mjr_dsimp_make-from-rptree bqt 8))))
      (format 't "write DSIMP~%")
      (time (mjr_ply3d_from-dsimp "bq.ply" dso)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLassical s-curve
(labels ((f (x y) (- (sqrt (max 0 (- 4 (* x x) (* y y)))) 1)))
  (let ((bqt (mjr_rptree_new 9 #(-3 -3) #(3 3))))
    (format 't "Sample~%")
    (time (mjr_fsamp_rp-func-r2-r1-adaptive bqt #'f :slice '(z -0.9999) :min-depth 3 :max-depth 8 :do-singularity-all nil :do-singularity-some 8 :show-progress 't))
    (mjr_rptree_print-meta bqt)
    (format 't "Balance~%")
    (time (mjr_rptree_balance bqt #'f :arg-mode :arg-number :func-check #'realp :depth 3 :show-progress 't))
    (mjr_rptree_print-meta bqt)
    (format 't "make DSIMP~%")
    (let ((dso (time (mjr_dsimp_make-from-rptree bqt 8))))
      (format 't "write DSIMP~%")
      (time (mjr_ply3d_from-dsimp "bq.ply" dso)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Display level of every coordiante for a 2D quadtree
;; (let ((bqt (mjr_rptree_new 5 #(0 0) #(1 1))))
;;   (format 't "~3a ~{~1a ~}~%" " " (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                         collect (truncate i 10)))
;;   (format 't "~3a ~{~1a ~}~%" " " (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                         collect (- i (* 10 (truncate i 10)))))
;;   (loop for k from 0
;;         for r in (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                        collect (loop for j from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                      for l = (mjr_rptree_quad-get-level bqt (vector i j))
;;                                      collect (if l (format nil "~,2d" l) "-")))
;;         do (format 't "~3a ~{~1a ~}~%" k r)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Display level of every coordiante for a 1D quadtree
;; (let ((bqt (mjr_rptree_new 6 0 1)))
;;   (format 't "~{~1a ~}~%" (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                 collect (truncate i 10)))
;;   (format 't "~{~1a ~}~%" (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                 collect (- i (* 10 (truncate i 10)))))
;;   (format 't "~{~1a ~}~%" (loop for i from 0 upto (ash 1 (mjr_rptree_struct-mesh-power bqt))
;;                                 for l = (mjr_rptree_quad-get-level bqt i)
;;                                 collect (if l (format nil "~,2d" l) "-"))))
