;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-combc.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-combc.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_COMBC-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_COMBC :MJR_COMBE :MJR_PRNG))

(in-package :MJR_COMBC-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combc_gen-all-partitions-revlex
  ;; Test values computed via Maxima integer_partitions funciton.
  (assert-equalp (mjr_combc_gen-all-partitions-revlex 3  :collect-value #'copy-seq)  '(#(1 1 1) #(2 1) #(3)))
  (assert-equalp (mjr_combc_gen-all-partitions-revlex 4  :collect-value #'copy-seq)  '(#(1 1 1 1) #(2 1 1) #(2 2) #(3 1) #(4)))
  (assert-equalp (mjr_combc_gen-all-partitions-revlex 10 :collect-value #'copy-seq)  '(#(1 1 1 1 1 1 1 1 1 1) #(2 1 1 1 1 1 1 1 1)
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
  ;; Make sure we have teh correct number of elements
  (loop for i from 1 upto 50
        for parts = (mjr_combc_gen-all-partitions-revlex i :collect-value #'copy-seq)
        do (assert-true (every (lambda (x) (= x i)) (map 'list (lambda (x) (reduce #'+ x)) parts)))
        do (assert-equal (mjr_combe_partitions i) (length parts))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combc_gen-all-k-partitions-colex
  ;; Test from Knuth V4
  (assert-equalp (mjr_combc_gen-all-k-partitions-colex 11 3 :collect-value #'copy-seq) '(#(4 4 3) #(5 3 3) #(5 4 2) #(6 3 2) #(7 2 2)
                                                                                         #(5 5 1) #(6 4 1) #(7 3 1) #(8 2 1) #(9 1 1)))
  ;; Hand picked examplel (Checked with maxima's integer_partitions functino)
  (assert-equalp (mjr_combc_gen-all-k-partitions-colex 11 2 :collect-value #'copy-seq) '(#(6 5) #(7 4) #(8 3) #(9 2) #(10 1)))
  (assert-equalp (mjr_combc_gen-all-k-partitions-colex 6  5 :collect-value #'copy-seq) '(#(2 1 1 1 1)))
  
  ;; Trivial cases
  (loop for i from 1 upto 50        
        do (assert-equalp (mjr_combc_gen-all-k-partitions-colex i i :collect-value #'copy-seq)
                          (list (concatenate 'vector (loop for j from 1 upto i
                                                           collect  1)))))
  (loop for i from 1 upto 50        
        do (assert-equalp (mjr_combc_gen-all-k-partitions-colex i 1 :collect-value #'copy-seq) (list (vector i))))
  
  ;; Make sure we have teh correct number of elements
  (loop for i from 1 upto 40
        do (loop for j from 1 upto i
                 for parts = (mjr_combc_gen-all-k-partitions-colex i j :collect-value #'copy-seq)
                 do (assert-true (every (lambda (x) (= x i)) (map 'list (lambda (x) (reduce #'+ x)) parts)))
                 do (assert-equal (mjr_combe_k-partitions i j) (length parts))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_combc_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests 
 )
