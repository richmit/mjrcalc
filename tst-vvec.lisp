;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-vec.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2008,2012 by Mitch Richling.  All rights reserved.
;; @brief     tests for :MJR_VVEC.@EOL
;; @Keywords  lisp interactive chebyshev uniform interval partition virtual vector
;; @Std       Common Lisp
;;
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_VVEC-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_VVEC))

(in-package :MJR_VVEC-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_normalize-vvt-aseq
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4 :end 2 :step -1 :len 3                    )))  ;; nothing is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2 :end 4 :step 1  :len 3                    )))  ;; nothing is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4 :end 2 :step -1                           )))  ;; LEN is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2 :end 4 :step 1                            )))  ;; LEN is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2 :end 4          :len 3                    )))  ;; STEP is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4 :end 2          :len 3                    )))  ;; STEP is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 2 :step -1 :len 3                    )))  ;; START is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 4 :step 1  :len 3                    )))  ;; START is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4        :step -1 :len 3                    )))  ;; END is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2        :step 1  :len 3                    )))  ;; END is computed
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 5 :step  1 :len 4 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2                 :len 4                    )))  ;; Special case: STEP=1
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 1 :step  1 :len 2 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list                          :len 2                    )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2 :end 4                                    )))  ;; Special case: STEP=1
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4 :end 2                                    )))  ;; Special case: STEP=-1
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 3 :step  3 :len 2 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 3          :len 2                    )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 3 :step  1 :len 4 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 3                                    )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 1 :step  1 :len 2 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list                 :step 1  :len 2                    )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 3 :step  1 :len 4 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 3 :step 1                            )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 0 :end 3 :step  1 :len 4 :map-fun nil)         (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list          :end 3                                    )))  ;; Special case: START=0
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 4 :end 2 :step -1 :len 3 :map-fun #'identity)  (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 4 :end 2 :step -1 :len 3 :map-fun #'identity)))  ;; Have a :map-fun
  (assert-equalp (list :vvec-type :VVT-ASEQ :start 2 :end 4 :step  1 :len 3 :map-fun #'identity)  (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2 :end 4 :step 1  :len 3 :map-fun #'identity)))  ;; Have a :map-fun
  ;; Error Cases                                                                                                                  
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2                                              )))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 2        :step 1                               )))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list                 :step 1                               )))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list                                                       )))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 0 :end 1          :len 1                       )))  ;; len should be 2
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 1 :end 2 :step 1  :len 1                       )))  ;; len should be 2
   (assert-error 'warning                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :start 1 :end 2 :step 1  :len 3                     )))  ;; len should be 2
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :vvec-type :vvt-points :start 4 :end 2 :step -1 :len 3)))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :vvec-type :vvt-fun    :start 4 :end 2 :step -1 :len 3)))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :vvec-type :vvt-cheb   :start 4 :end 2 :step -1 :len 3)))
   (assert-error 'error                                                                     (mjr_vvec::mjr_vvec_normalize-vvt-aseq (list :vvec-type :vvt-mitch1 :start 4 :end 2 :step -1 :len 3)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_normalize-vvt-points
  (assert-equalp '(:vvec-type :vvt-points :points (2)       :start 0 :end 0 :len 1 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(2))))
  (assert-equalp '(:vvec-type :vvt-points :points (2 3 4)   :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(2 3 4))))
  (assert-equalp '(:vvec-type :vvt-points :points #(2 3 4)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(2 3 4))))
  (assert-equalp '(:vvec-type :vvt-points :points #(4 9 16) :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (2 3 4)   :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(2 3 4)  :len 3)))
  (assert-equalp '(:vvec-type :vvt-points :points #(2 3 4)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(2 3 4)  :len 3)))
  (assert-equalp '(:vvec-type :vvt-points :points #(4 9 16) :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(4 9 16) :len 3)))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(4 9 16) :len 3)))
  (assert-equalp '(:vvec-type :vvt-points :points (2 3 4)   :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(2 3 4)  :len 3.0)))
  (assert-equalp '(:vvec-type :vvt-points :points #(2 3 4)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(2 3 4)  :len 3.0)))
  (assert-equalp '(:vvec-type :vvt-points :points #(4 9 16) :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points #(4 9 16) :len 3.0)))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :points '(4 9 16) :len 3.0)))
  ;; Subsets
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 1 :len 2 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 2   :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 3.1 :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 2.9 :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 1 :end 2 :len 2 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :start 1 :end 2         :len 2   :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 1 :end 2 :len 2 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list          :end 2         :len 2   :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 2 :len 3 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list          :end 2                  :points '(4 9 16))))
  (assert-equalp '(:vvec-type :vvt-points :points (4 9 16)  :start 0 :end 1 :len 2 :map-fun nil)   (mjr_vvec::mjr_vvec_normalize-vvt-points (list          :end 1                  :points '(4 9 16))))
  ;; Error Cases
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 0   :points '())))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                                  :points '())))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                                  :points #())))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 4   :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list                         :len 0   :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :start -1                        :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :start 3                         :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list             :end -1              :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list             :end 3               :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :start -1                :len 2  :points '(4 9 16))))
  (assert-error 'error                                                   (mjr_vvec::mjr_vvec_normalize-vvt-points (list :end -1                  :len 2  :points '(4 9 16))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_map-filter-reduce
  ;; Various combinations of args
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0  :end 10 :step 1  :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list           :end 10 :step 1  :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0          :step 1  :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0  :end 10          :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0  :end 10 :step 1         )))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list                   :step 1  :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list           :end 10          :len 11)))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list           :end 10 :step 1         )))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0                   :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list :start 0          :step 1         )))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list :start 0  :end 10                 )))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list                            :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list                   :step 1         )))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list   (list           :end 10                 )))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list :start 0                          )))
  ;; (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list                                   ))) ;; TODO: FIX
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0  :end 10 :step 1  :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list           :end 10 :step 1  :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0          :step 1  :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0  :end 10          :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0  :end 10 :step 1         )))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list                   :step 1  :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list           :end 10          :len 11)))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list           :end 10 :step 1         )))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0                   :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'vector (list :start 0          :step 1         )))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list :start 0  :end 10                 )))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list                            :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'vector (list                   :step 1         )))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector (list           :end 10                 )))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'vector (list :start 0                          )))
  ;; (assert-error 'error                      (mjr_vvec_map-filter-reduce 'vector (list                                   ))) ;; TODO: FIX
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0  :end 10 :step 1  :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list           :end 10 :step 1  :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0          :step 1  :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0  :end 10          :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0  :end 10 :step 1         )))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list                   :step 1  :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list           :end 10          :len 11)))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list           :end 10 :step 1         )))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0                   :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce nil     (list :start 0          :step 1         )))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list :start 0  :end 10                 )))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list                            :len 11)))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce nil     (list                   :step 1         )))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil     (list           :end 10                 )))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce nil     (list :start 0                          )))
  ;; (assert-error 'error                      (mjr_vvec_map-filter-reduce nil     (list                                   ))) ;; TODO: FIX
  ;; Inconsistent
  (assert-error 'warning                    (mjr_vvec_map-filter-reduce 'list   (list :start 0  :end 10 :step 1  :len 12)))
  (assert-error 'warning                    (mjr_vvec_map-filter-reduce 'vector (list :start 0  :end 10 :step 1  :len 12)))
  (assert-error 'warning                    (mjr_vvec_map-filter-reduce nil     (list :start 0  :end 10 :step 1  :len 12)))
  ;; Points
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list     (list                           :len 11 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector   (list                           :len 11 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil       (list                           :len 11 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'list     (list                                   :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9 10)  (mjr_vvec_map-filter-reduce 'vector   (list                                   :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-equalp nil                        (mjr_vvec_map-filter-reduce nil       (list                                   :points '(0 1 2 3 4 5 6 7 8 9 10))))
  ;; Inconsistent                                                          (list 
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list                             :len 12 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'vector (list                             :len 12 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  (assert-error 'error                      (mjr_vvec_map-filter-reduce nil     (list                             :len 12 :points '(0 1 2 3 4 5 6 7 8 9 10))))
  ;; Can't have :step
  (assert-error 'error                      (mjr_vvec_map-filter-reduce 'list   (list                    :step 1  :len 11 :points '(0 1 2 3 4 5 6 7 8 9 10))))
)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_vvec2fi
  ;; Note: This function dosen't need test cases -- tested by mjr_vvec_map-filter-reduce
  1
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_map-sum

  (assert-equalp 29 (mjr_vvec_map-sum (list :start 4 :end 2 :step -1 :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 2 :end 4 :step 1  :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 2 :end 4 :len 3   :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 4 :end 2 :len 3   :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 2 :end 4          :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 4 :end 2          :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :points '(2 3 4)         :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :points #(2 3 4)         :map-fun (lambda (x) (* x x)))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :points #(4 9 16))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :points '(4 9 16))))
  (assert-equalp 29 (mjr_vvec_map-sum (list :start 2 :end 4 :step 1) :point-fun (lambda (x i) (declare (ignore i)) (* x x))))

  (assert-equalp 25 (mjr_vvec_map-sum (list :start 1 :end 9 :step 2)))
  (assert-equalp 25 (mjr_vvec_map-sum (list :start 1 :end 10 :map-fun  (lambda (i) (if (oddp i) i 0)))))
  (assert-equalp 25 (mjr_vvec_map-sum (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (and (oddp v) v))))
  (assert-equalp 25 (mjr_vvec_map-sum (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (if (oddp v) v 0))))
  (assert-equalp 25 (mjr_vvec_map-sum (list :start 1 :end 10) :filter-fun (lambda (v fv i) (declare (ignore fv i)) (oddp v))))

  (assert-equalp 4  (mjr_vvec_map-sum (list :points '(4))))
  (assert-equalp 4  (mjr_vvec_map-sum (list :start 4 :end 4)))
)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_map-prod
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 4 :end 2 :step -1 :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 2 :end 4 :step 1  :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 2 :end 4 :len 3   :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 4 :end 2 :len 3   :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 2 :end 4          :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 4 :end 2          :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :points '(2 3 4)         :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :points #(2 3 4)         :map-fun (lambda (x) (* x x)))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :points #(4 9 16))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :points '(4 9 16))))
  (assert-equalp 576 (mjr_vvec_map-prod (list :start 2 :end 4 :step 1) :point-fun (lambda (x i) (declare (ignore i)) (* x x))))

  (assert-equalp 945 (mjr_vvec_map-prod (list :start 1 :end 9 :step 2)))
  (assert-equalp 945 (mjr_vvec_map-prod (list :start 1 :end 10 :map-fun  (lambda (i) (if (oddp i) i 1)))))
  (assert-equalp 945 (mjr_vvec_map-prod (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (and (oddp v) v))))
  (assert-equalp 945 (mjr_vvec_map-prod (list :start 1 :end 10) :point-fun (lambda (v i) (declare (ignore i)) (if (oddp v) v 1))))
  (assert-equalp 945 (mjr_vvec_map-prod (list :start 1 :end 10) :filter-fun (lambda (v fv i) (declare (ignore fv i)) (oddp v))))
  
  (assert-equalp 4   (mjr_vvec_map-prod (list :points '(4))))
  (assert-equalp 4   (mjr_vvec_map-prod (list :start 4 :end 4)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_map-maxi

  (assert-equalp '(0 100)   (multiple-value-list (mjr_vvec_map-maxi (list :map-fun (lambda (x) (* x x))      :start -10 :end 10 :len 21))))
  (assert-equalp '(10 0)    (multiple-value-list (mjr_vvec_map-maxi (list :map-fun (lambda (x) (- (* x x)))  :start -10 :end 10 :len 21))))

  (assert-equalp '(2 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(3 2 4 2)))))

  (assert-equalp '(2 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(3  2 4 1)))))

  (assert-equalp '(3 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(1  2  3  4)))))
  (assert-equalp '(0 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(4  3  2  1)))))

  (assert-equalp '(0 -1)    (multiple-value-list (mjr_vvec_map-maxi (list :points '(-1 -2 -3 -4)))))
  (assert-equalp '(3 -1)    (multiple-value-list (mjr_vvec_map-maxi (list :points '(-4 -3 -2 -1)))))

  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(2  2  2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(2  2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points '(2)))))

  (assert-equalp '(3 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(1  2  3  4)))))
  (assert-equalp '(0 4)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(4  3  2  1)))))

  (assert-equalp '(0 -1)    (multiple-value-list (mjr_vvec_map-maxi (list :points #(-1 -2 -3 -4)))))
  (assert-equalp '(3 -1)    (multiple-value-list (mjr_vvec_map-maxi (list :points #(-4 -3 -2 -1)))))

  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(2  2  2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(2  2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(2  2)))))
  (assert-equalp '(0 2)     (multiple-value-list (mjr_vvec_map-maxi (list :points #(2)))))
  
  ;; Error as empty lists don't have a maximum -- ;; TODO: Think about returning nil instead.
  (assert-error 'error                      (mjr_vvec_map-maxi (list :points '())))
  (assert-error 'error                      (mjr_vvec_map-maxi (list :points nil)))
  (assert-error 'error                      (mjr_vvec_map-maxi (list :points #())))
 
 )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_map-mini
  (assert-equalp '(10 0)     (multiple-value-list (mjr_vvec_map-mini (list :map-fun (lambda (x) (* x x))      :start -10 :end 10 :len 21))))
  (assert-equalp '(0 -100)   (multiple-value-list (mjr_vvec_map-mini (list :map-fun (lambda (x) (- (* x x)))  :start -10 :end 10 :len 21))))

  (assert-equalp '(2 1)      (multiple-value-list (mjr_vvec_map-mini (list :points '(3  2  1  4)))))

  (assert-equalp '(0 1)      (multiple-value-list (mjr_vvec_map-mini (list :points '(1  2  3  4)))))
  (assert-equalp '(3 1)      (multiple-value-list (mjr_vvec_map-mini (list :points '(4  3  2  1)))))

  (assert-equalp '(3 -4)     (multiple-value-list (mjr_vvec_map-mini (list :points '(-1 -2 -3 -4)))))
  (assert-equalp '(0 -4)     (multiple-value-list (mjr_vvec_map-mini (list :points '(-4 -3 -2 -1)))))

  ;; Make sure it gets the FIRST instance
  (assert-equalp '(0 2)      (multiple-value-list (mjr_vvec_map-mini (list :points '(2  2  2  2)))))
  (assert-equalp '(0 2)      (multiple-value-list (mjr_vvec_map-mini (list :points '(2  2  2)))))
  (assert-equalp '(0 2)      (multiple-value-list (mjr_vvec_map-mini (list :points '(2  2)))))
  (assert-equalp '(0 2)      (multiple-value-list (mjr_vvec_map-mini (list :points '(2)))))

  ;; Error as empty lists don't have a minimum -- ;; TODO: Think about returning nil instead.
  (assert-error 'error                      (multiple-value-list (mjr_vvec_map-mini (list :points '()))))
  (assert-error 'error                      (mjr_vvec_map-mini (list :points nil)))
  (assert-error 'error                      (multiple-value-list (mjr_vvec_map-mini (list :points #()))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_help
  ;; Note: This function dosen't need test cases..
  1
  )

;; TODO: (defun mjr_vvec_gen-1sim
;; TODO: (defun mjr_vvec_convert-rep
;; TODO: (defun mjr_vvec_to-vec-maybe
;; TODO: (defun mjr_vvec_normalize-all-rep
;; TODO: (defun mjr_vvec_normalize-vvt-mitch1
;; TODO: (defun mjr_vvec_normalize-vvt-cheb
;; TODO: (defun mjr_vvec_normalize-vvt-rfun
;; TODO: (defun mjr_vvec_normalize-vvt-nfun
;; TODO: (defun mjr_vvec_normalize-all

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_vvec_gen-0sim
  ;; -------------------------------------------------------------------------------------------------------------------------------
  ;; vvt-rfun
  ;; A000045: Fibonacci numbers: F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1
  (assert-equalp #(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393
                   196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169)
                 (mjr_vvec_gen-0sim 'vector (list :start '(0 1) :len 39 :rfun (lambda (a b) (+ a b)))))
  ;; A000032: Lucas numbers (beginning at 2): L(n) = L(n-1) + L(n-2)
  (assert-equalp #(2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349 15127 24476 39603 64079 103682 167761
                   271443 439204 710647 1149851 1860498 3010349 4870847 7881196 12752043 20633239 33385282)
                 (mjr_vvec_gen-0sim 'vector (list :start '(2 1) :len 37 :rfun (lambda (a b) (+ a b)))))
  ;; A000129: Pell numbers: a(0) = 0, a(1) = 1; for n > 1, a(n) = 2*a(n-1) + a(n-2)
  (assert-equalp #(0 1 2 5 12 29 70 169 408 985 2378 5741 13860 33461 80782 195025 470832 1136689 2744210 6625109 15994428 38613965
                   93222358 225058681 543339720 1311738121 3166815962 7645370045 18457556052 44560482149 107578520350 259717522849)
                 (mjr_vvec_gen-0sim 'vector (list :start '(0 1) :len 32 :rfun (lambda (a b) (+ (* 2 b) a)))))
  ;; A000931: Padovan sequence: a(n) = a(n-2) + a(n-3) with a(0)=1, a(1)=a(2)=0
  (assert-equalp #(1 0 0 1 0 1 1 1 2 2 3 4 5 7 9 12 16 21 28 37 49 65 86 114 151 200 265 351 465 616 816 1081 1432 1897 2513 3329
                   4410 5842 7739 10252 13581 17991 23833 31572 41824 55405 73396 97229 128801 170625)
                 (mjr_vvec_gen-0sim 'vector (list :start '(1 0 0) :len 50 :rfun (lambda (a b c) (declare (ignore c)) (+ a b)))))
  ;; A001608: Perrin sequence: a(n) = a(n-2) + a(n-3).
  (assert-equalp #(3 0 2 3 2 5 5 7 10 12 17 22 29 39 51 68 90 119 158 209 277 367 486 644 853 1130 1497 1983 2627 3480 4610 6107
                   8090 10717 14197 18807 24914 33004 43721 57918 76725 101639 134643 178364 236282 313007)
                 (mjr_vvec_gen-0sim 'vector (list :start '(3 0 2) :len 46 :rfun (lambda (a b c) (declare (ignore c)) (+ a b)))))
  ;; -------------------------------------------------------------------------------------------------------------------------------
  ;; vvt-rep
  (loop for n from 1 upto 20
        for c = (random 100)
        for v = (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-rep :start c :len n))
        do (assert-true (every (lambda (a) (= a c)) v))
        do (assert-equalp n (length v)))
  (assert-error 'error (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-rep :start 1 :len 0         )))
  (assert-error 'error (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-rep :start 1 :len -1        )))
  (assert-error 'error (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-rep :start 1                )))
  (assert-error 'error (mjr_vvec_gen-0sim 'vector (list :vvec-type :vvt-rep          :len 1         )))
  ;; -------------------------------------------------------------------------------------------------------------------------------
  ;; :vvec-rep == :vvr-vec
  (assert-equalp #(3 0 2 3 2 5 5 7 10 12)        (mjr_vvec_gen-0sim 'vector #(3 0 2 3 2 5 5 7 10 12)))
  (assert-equalp #(3 0 2 3 2)                    (mjr_vvec_gen-0sim 'vector #(3 0 2 3 2)))
  (assert-equalp #(3)                            (mjr_vvec_gen-0sim 'vector #(3)))
  (assert-error 'error                           (mjr_vvec_gen-0sim 'vector #()))
  (assert-equalp '(3 0 2 3 2 5 5 7 10 12)        (mjr_vvec_gen-0sim 'list   #(3 0 2 3 2 5 5 7 10 12)))
  (assert-equalp '(3 0 2 3 2)                    (mjr_vvec_gen-0sim 'list   #(3 0 2 3 2)))
  (assert-equalp '(3)                            (mjr_vvec_gen-0sim 'list   #(3)))
  (assert-equalp '(3)                            (mjr_vvec_gen-0sim 'list   #(3)))
  ;; -------------------------------------------------------------------------------------------------------------------------------
  ;; :vvec-rep == :vvr-list
  (assert-equalp #(3 0 2 3 2 5 5 7 10 12)        (mjr_vvec_gen-0sim 'vector '(3 0 2 3 2 5 5 7 10 12)))
  (assert-equalp #(3 0 2 3 2)                    (mjr_vvec_gen-0sim 'vector '(3 0 2 3 2)))
  (assert-equalp #(3)                            (mjr_vvec_gen-0sim 'vector '(3)))
  (assert-error 'error                           (mjr_vvec_gen-0sim 'vector '()))
  (assert-equalp '(3 0 2 3 2 5 5 7 10 12)        (mjr_vvec_gen-0sim 'list   '(3 0 2 3 2 5 5 7 10 12)))
  (assert-equalp '(3 0 2 3 2)                    (mjr_vvec_gen-0sim 'list   '(3 0 2 3 2)))
  (assert-equalp '(3)                            (mjr_vvec_gen-0sim 'list   '(3)))
  (assert-error 'error                           (mjr_vvec_gen-0sim 'list   '()))
  ;; -------------------------------------------------------------------------------------------------------------------------------
  ;; :vvec-rep == :vvr-int
  (assert-equalp #(0 1 2 3 4 5 6 7 8 9)          (mjr_vvec_gen-0sim 'vector  10))
  (assert-equalp #(0 1 2 3)                      (mjr_vvec_gen-0sim 'vector   4))
  (assert-equalp #(0)                            (mjr_vvec_gen-0sim 'vector   1))
  (assert-error 'error                           (mjr_vvec_gen-0sim 'vector   0))
  (assert-equalp #(1)                            (mjr_vvec_gen-0sim 'vector  -1))
  (assert-equalp #(1 2 3 4)                      (mjr_vvec_gen-0sim 'vector  -4))
  (assert-equalp #(1 2 3 4 5 6 7 8 9 10)         (mjr_vvec_gen-0sim 'vector -10))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests
 ;; mjr_vvec_gen-0sim
 ;; mjr_vvec_normalize-vvt-aseq
 ;; mjr_vvec_normalize-vvt-points
 ;; mjr_vvec_map-filter-reduce
 ;; mjr_vvec_map-sum
 ;; mjr_vvec_map-prod
 ;; mjr_vvec_map-maxi
 ;; mjr_vvec_map-mini
 )


