;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-geom.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1994,1995,1997,1998,2004,2011,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for use-geom.lisp.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GEOM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_GEOM :MJR_COMBC :MJR_EPS :MJR_VEC :MJR_PRNG))

(in-package :MJR_GEOM-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_geom_simplex-area 
  (assert-equal 1 (mjr_geom_simplex-area #(0 0) #(0 1)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 0) #(1 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 1) #(0 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(1 0) #(0 0)))

  (assert-equal 1 (mjr_geom_simplex-area #(0 0 0) #(1 0 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 0 0) #(0 1 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 0 0) #(0 0 1)))
  (assert-equal 1 (mjr_geom_simplex-area #(1 0 0) #(0 0 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 1 0) #(0 0 0)))
  (assert-equal 1 (mjr_geom_simplex-area #(0 0 1) #(0 0 0)))

  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0)   #(0 1)   #(1 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0)   #(1 0)   #(1 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 1)   #(0 0)   #(1 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 0)   #(0 0)   #(1 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 1)   #(1 1)   #(0 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 0)   #(1 1)   #(0 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0)   #(1 1)   #(0 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0)   #(1 1)   #(1 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 1)   #(0 0)   #(0 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 1)   #(0 0)   #(1 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 1)   #(0 1)   #(0 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(1 1)   #(1 0)   #(0 0)))

  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(1 0 0) #(1 1 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(1 0 0) #(1 0 1)))

  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(0 1 0) #(1 1 0)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(0 1 0) #(0 1 1)))

  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(0 0 1) #(0 1 1)))
  (assert-equal 1/2 (mjr_geom_simplex-area #(0 0 0) #(0 0 1) #(1 0 1)))
  ;; Randomized tests
  (dotimes (i 200)
    (let* ((p21  (mjr_prng_vector 2 #'mjr_prng_int-cc -10 10))
           (p22  (mjr_prng_vector 2 #'mjr_prng_int-cc -10 10))
           (p23  (mjr_prng_vector 2 #'mjr_prng_int-cc -10 10))
           (p31  (mjr_prng_vector 3 #'mjr_prng_int-cc -10 10))
           (p32  (mjr_prng_vector 3 #'mjr_prng_int-cc -10 10))
           (p33  (mjr_prng_vector 3 #'mjr_prng_int-cc -10 10))
           (at1  (mjr_geom_simplex-area p21 p22))
           (at2  (mjr_geom_simplex-area p21 p22 p23))
           (at3  (mjr_geom_simplex-area p31 p32))
           (at4  (mjr_geom_simplex-area p31 p32 p33)))
      ;; Area should be invariant under permutation
      (assert-equalp at1 (mjr_geom_simplex-area p22 p21))
      (assert-equalp at2 (mjr_geom_simplex-area p21 p23 p22))
      (assert-equalp at2 (mjr_geom_simplex-area p23 p21 p22))
      (assert-equalp at2 (mjr_geom_simplex-area p23 p22 p21))
      (assert-equalp at2 (mjr_geom_simplex-area p22 p21 p23))
      (assert-equalp at2 (mjr_geom_simplex-area p22 p23 p21))
      (assert-equalp at3 (mjr_geom_simplex-area p32 p31))
      (assert-equalp at4 (mjr_geom_simplex-area p31 p33 p32))
      (assert-equalp at4 (mjr_geom_simplex-area p33 p31 p32))
      (assert-equalp at4 (mjr_geom_simplex-area p33 p32 p31))
      (assert-equalp at4 (mjr_geom_simplex-area p32 p31 p33))
      (assert-equalp at4 (mjr_geom_simplex-area p32 p33 p31))
      ;; Area should be invariant under shift
      (dotimes (i 10)
        (let* ((o    (mjr_prng_int-cc -10 10))
               (op21  (mjr_vec_+ o p21))
               (op22  (mjr_vec_+ o p22))
               (op23  (mjr_vec_+ o p23))
               (op31  (mjr_vec_+ o p31))
               (op32  (mjr_vec_+ o p32))
               (op33  (mjr_vec_+ o p33)))
          (assert-equalp at1 (mjr_geom_simplex-area op22 op21))
          (assert-equalp at2 (mjr_geom_simplex-area op21 op22 op23))
          (assert-equalp at2 (mjr_geom_simplex-area op21 op23 op22))
          (assert-equalp at2 (mjr_geom_simplex-area op23 op21 op22))
          (assert-equalp at2 (mjr_geom_simplex-area op23 op22 op21))
          (assert-equalp at2 (mjr_geom_simplex-area op22 op21 op23))
          (assert-equalp at2 (mjr_geom_simplex-area op22 op23 op21))
          (assert-equalp at3 (mjr_geom_simplex-area op32 op31))
          (assert-equalp at4 (mjr_geom_simplex-area op31 op32 op33))
          (assert-equalp at4 (mjr_geom_simplex-area op31 op33 op32))
          (assert-equalp at4 (mjr_geom_simplex-area op33 op31 op32))
          (assert-equalp at4 (mjr_geom_simplex-area op33 op32 op31))
          (assert-equalp at4 (mjr_geom_simplex-area op32 op31 op33))
          (assert-equalp at4 (mjr_geom_simplex-area op32 op33 op31))))
          ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_geom_simplex-degeneratep
  (assert-true  (mjr_geom_simplex-degeneratep nil #(0)     #(0)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1)     #(1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(0 0)   #(0 0)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1)   #(1 1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(0 0 0) #(0 0 0)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1 1) #(1 1 1)))

  (assert-true  (mjr_geom_simplex-degeneratep nil #(2)     #(2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1)     #(1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(2 2)   #(2 2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1)   #(1 1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(2 2 2) #(2 2 2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1 1) #(1 1 1)))

  (assert-false (mjr_geom_simplex-degeneratep nil #(1)     #(0)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(0)     #(1)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(1 1)   #(0 0)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(0 0)   #(1 1)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(1 1 1) #(0 0 0)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(0 0 0) #(1 1 1)))

  (assert-false (mjr_geom_simplex-degeneratep nil #(1)     #(2)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(2)     #(1)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(1 1)   #(2 2)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(2 2)   #(1 1)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(1 1 1) #(2 2 2)))
  (assert-false (mjr_geom_simplex-degeneratep nil #(2 2 2) #(1 1 1)))

  (assert-true  (mjr_geom_simplex-degeneratep nil #(1)     #(1)     #(2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(2)     #(2)     #(1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1)   #(1 1)   #(2 2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(2 2)   #(2 2)   #(1 1)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(1 1 1) #(1 1 1) #(2 2 2)))
  (assert-true  (mjr_geom_simplex-degeneratep nil #(2 2 2) #(2 2 2) #(1 1 1)))
  )

;; ;;----------------------------------------------------------------------------------------------------------------------------------
;; (XXXXX mjr_geom_area-2d-right-triangle-with-sides-parallel-to-axis-in-r2
;;   ;; Test all triangles with a vertex in the integer lattice [-5,5]x[-5,5]
;;   (loop for x from -5 upto 5
;;         do (loop for y from -5 upto 5
;;                  for tri-lst = (loop for xo in '(1 -1 -1  1)
;;                                      for yo in '(1 -1  1 -1)
;;                                      collect (list (vector x y) (vector (+ x xo) y) (vector x        (+ y yo)))
;;                                      collect (list (vector x y) (vector (+ x xo) y) (vector (+ x xo) (+ y yo))))  
;;                  do (loop for tri in tri-lst
;;                           do (loop with plst = (mjr_combc_gen-all-permutations-gry tri :collect-value #'copy-seq)
;;                                    for p in plst
;;                                    do (assert-equal 1/2 (apply #'mjr_geom_area-2d-right-triangle-with-sides-parallel-to-axis-in-r2 (concatenate 'list p)))))))
;;   ;; Randomly located triangles with area 1/2 and real coordinates
;;   (loop for x = (mjr_prng_float-co -100 100)
;;         for y = (mjr_prng_float-co -100 100)
;;         for i from 0 upto 20
;;         for tri-lst = (loop for xo in '(1 -1 -1  1)
;;                             for yo in '(1 -1  1 -1)
;;                             collect (list (vector x y) (vector (+ x xo) y) (vector x        (+ y yo)))
;;                             collect (list (vector x y) (vector (+ x xo) y) (vector (+ x xo) (+ y yo))))
;;         do (loop for tri in tri-lst
;;                  do (loop with plst = (mjr_combc_gen-all-permutations-gry tri :collect-value #'copy-seq)
;;                           for p in plst
;;                           do (assert-equality #'mjr_eps_= 1/2 (apply #'mjr_geom_area-2d-right-triangle-with-sides-parallel-to-axis-in-r2 (concatenate 'list p))))))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
