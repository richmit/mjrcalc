;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-combc.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2011,2013,2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_COMBC.@EOL
;; @Keywords  unit tests
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_COMBC-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_COMBC :MJR_PRNG))

(in-package :MJR_COMBC-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_combc_gen-all-ipartitions-revlex
  ;; Test values computed via Maxima integer_partitions funciton.
  (assert-equalp (mjr_combc_gen-all-ipartitions-revlex 3  :collect-value #'copy-seq)  '(#(1 1 1) #(2 1) #(3)))
  (assert-equalp (mjr_combc_gen-all-ipartitions-revlex 4  :collect-value #'copy-seq)  '(#(1 1 1 1) #(2 1 1) #(2 2) #(3 1) #(4)))
  (assert-equalp (mjr_combc_gen-all-ipartitions-revlex 10 :collect-value #'copy-seq)  '(#(1 1 1 1 1 1 1 1 1 1) #(2 1 1 1 1 1 1 1 1)
                                                                                        #(2 2 1 1 1 1 1 1) #(2 2 2 1 1 1 1)
                                                                                        #(2 2 2 2 1 1) #(2 2 2 2 2)
                                                                                        #(3 1 1 1 1 1 1 1) #(3 2 1 1 1 1 1)
                                                                                        #(3 2 2 1 1 1) #(3 2 2 2 1)
                                                                                        #(3 3 1 1 1 1) #(3 3 2 1 1) #(3 3 2 2)
                                                                                        #(3 3 3 1) #(4 1 1 1 1 1 1)
                                                                                        #(4 2 1 1 1 1) #(4 2 2 1 1) #(4 2 2 2)
                                                                                        #(4 3 1 1 1) #(4 3 2 1) #(4 3 3)
                                                                                        #(4 4 1 1) #(4 4 2) #(5 1 1 1 1 1)
                                                                                        #(5 2 1 1 1) #(5 2 2 1) #(5 3 1 1)
                                                                                        #(5 3 2) #(5 4 1) #(5 5) #(6 1 1 1 1)
                                                                                        #(6 2 1 1) #(6 2 2) #(6 3 1) #(6 4)
                                                                                        #(7 1 1 1) #(7 2 1) #(7 3) #(8 1 1) #(8 2)
                                                                                        #(9 1) #(10)))
  (loop for i from 1 upto 50
        do (assert-true (every (lambda (x) (= x i))
                               (mjr_combc_gen-all-ipartitions-revlex i :collect-value (lambda (x) (reduce #'+ x))))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_combc_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
