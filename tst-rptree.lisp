;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-rptree.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       use-rptree.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2006,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_RPTREE-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_RPTREE :MJR_EPS :MJR_PRNG :MJR_NUMU))

(in-package :MJR_RPTREE-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test mjr_rptree_quad-get-children
  (let ((an-rptree-2d (mjr_rptree_new 5 #(-5 -5) #(5 5)))
        (an-rptree-1d (mjr_rptree_new 5 -5 5)))
    (assert-equalp '(16)                                                                                  (mjr_rptree_quad-get-children an-rptree-1d 16 0))
    (assert-equalp '(8 24)                                                                                (mjr_rptree_quad-get-children an-rptree-1d 16 1))
    (assert-equalp '(4 12 20 28)                                                                          (mjr_rptree_quad-get-children an-rptree-1d 16 2))
    (assert-equalp '(2 6 10 14 18 22 26 30)                                                               (mjr_rptree_quad-get-children an-rptree-1d 16 3))
    (assert-equalp '(1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31)                                          (mjr_rptree_quad-get-children an-rptree-1d 16 4))
    (assert-equalp (values '(#(16 16)) 0)                                                                 (mjr_rptree_quad-get-children an-rptree-2d #(16 16) 0))
    (assert-equalp (values '(#(8 8) #(8 24) #(24 8) #(24 24)) 1)                                          (mjr_rptree_quad-get-children an-rptree-2d #(16 16) 1))
    (assert-equalp '(#(4 4) #(4 12) #(4 20) #(4 28) #(12 4) #(12 12) #(12 20) #(12 28) #(20 4) 
                     #(20 12) #(20 20) #(20 28) #(28 4) #(28 12) #(28 20) #(28 28))                       (mjr_rptree_quad-get-children an-rptree-2d #(16 16) 2))
    (assert-equalp '(#(2 2) #(2 6) #(2 10) #(2 14) #(2 18) #(2 22) #(2 26) #(2 30) #(6 2) #(6 6) 
                     #(6 10) #(6 14) #(6 18) #(6 22) #(6 26) #(6 30) #(10 2) #(10 6) #(10 10) #(10 14)
                     #(10 18) #(10 22) #(10 26) #(10 30) #(14 2) #(14 6) #(14 10) #(14 14) #(14 18)
                     #(14 22) #(14 26) #(14 30) #(18 2) #(18 6) #(18 10) #(18 14) #(18 18) #(18 22)
                     #(18 26) #(18 30) #(22 2) #(22 6) #(22 10) #(22 14) #(22 18) #(22 22) #(22 26)
                     #(22 30) #(26 2) #(26 6) #(26 10) #(26 14) #(26 18) #(26 22) #(26 26) #(26 30)
                     #(30 2) #(30 6) #(30 10) #(30 14) #(30 18) #(30 22) #(30 26) #(30 30))               (mjr_rptree_quad-get-children an-rptree-2d #(16 16) 3))
    (assert-equalp '(#(1 1) #(1 3) #(1 5) #(1 7) #(1 9) #(1 11) #(1 13) #(1 15) #(1 17) #(1 19) #(1 21)
                     #(1 23) #(1 25) #(1 27) #(1 29) #(1 31) #(3 1) #(3 3) #(3 5) #(3 7) #(3 9) 
                     #(3 11) #(3 13) #(3 15) #(3 17) #(3 19) #(3 21) #(3 23) #(3 25) #(3 27) #(3 29)
                     #(3 31) #(5 1) #(5 3) #(5 5) #(5 7) #(5 9) #(5 11) #(5 13) #(5 15) #(5 17) 
                     #(5 19) #(5 21) #(5 23) #(5 25) #(5 27) #(5 29) #(5 31) #(7 1) #(7 3) #(7 5) 
                     #(7 7) #(7 9) #(7 11) #(7 13) #(7 15) #(7 17) #(7 19) #(7 21) #(7 23) #(7 25) 
                     #(7 27) #(7 29) #(7 31) #(9 1) #(9 3) #(9 5) #(9 7) #(9 9) #(9 11) #(9 13) 
                     #(9 15) #(9 17) #(9 19) #(9 21) #(9 23) #(9 25) #(9 27) #(9 29) #(9 31) #(11 1)
                     #(11 3) #(11 5) #(11 7) #(11 9) #(11 11) #(11 13) #(11 15) #(11 17) #(11 19) 
                     #(11 21) #(11 23) #(11 25) #(11 27) #(11 29) #(11 31) #(13 1) #(13 3) #(13 5)
                     #(13 7) #(13 9) #(13 11) #(13 13) #(13 15) #(13 17) #(13 19) #(13 21) #(13 23)
                     #(13 25) #(13 27) #(13 29) #(13 31) #(15 1) #(15 3) #(15 5) #(15 7) #(15 9) 
                     #(15 11) #(15 13) #(15 15) #(15 17) #(15 19) #(15 21) #(15 23) #(15 25) #(15 27)
                     #(15 29) #(15 31) #(17 1) #(17 3) #(17 5) #(17 7) #(17 9) #(17 11) #(17 13) 
                     #(17 15) #(17 17) #(17 19) #(17 21) #(17 23) #(17 25) #(17 27) #(17 29) #(17 31)
                     #(19 1) #(19 3) #(19 5) #(19 7) #(19 9) #(19 11) #(19 13) #(19 15) #(19 17) 
                     #(19 19) #(19 21) #(19 23) #(19 25) #(19 27) #(19 29) #(19 31) #(21 1) #(21 3)
                     #(21 5) #(21 7) #(21 9) #(21 11) #(21 13) #(21 15) #(21 17) #(21 19) #(21 21)
                     #(21 23) #(21 25) #(21 27) #(21 29) #(21 31) #(23 1) #(23 3) #(23 5) #(23 7) 
                     #(23 9) #(23 11) #(23 13) #(23 15) #(23 17) #(23 19) #(23 21) #(23 23) #(23 25)
                     #(23 27) #(23 29) #(23 31) #(25 1) #(25 3) #(25 5) #(25 7) #(25 9) #(25 11) 
                     #(25 13) #(25 15) #(25 17) #(25 19) #(25 21) #(25 23) #(25 25) #(25 27) #(25 29)
                     #(25 31) #(27 1) #(27 3) #(27 5) #(27 7) #(27 9) #(27 11) #(27 13) #(27 15) 
                     #(27 17) #(27 19) #(27 21) #(27 23) #(27 25) #(27 27) #(27 29) #(27 31) #(29 1)
                     #(29 3) #(29 5) #(29 7) #(29 9) #(29 11) #(29 13) #(29 15) #(29 17) #(29 19) 
                     #(29 21) #(29 23) #(29 25) #(29 27) #(29 29) #(29 31) #(31 1) #(31 3) #(31 5)
                     #(31 7) #(31 9) #(31 11) #(31 13) #(31 15) #(31 17) #(31 19) #(31 21) #(31 23)
                     #(31 25) #(31 27) #(31 29) #(31 31))                                                 (mjr_rptree_quad-get-children an-rptree-2d #(16 16) 4))
    (assert-equalp '(24)                                                                                  (mjr_rptree_quad-get-children an-rptree-1d 24 0))
    (assert-equalp '(20 28)                                                                               (mjr_rptree_quad-get-children an-rptree-1d 24 1))
    (assert-equalp '(18 22 26 30)                                                                         (mjr_rptree_quad-get-children an-rptree-1d 24 2))
    (assert-equalp '(17 19 21 23 25 27 29 31)                                                             (mjr_rptree_quad-get-children an-rptree-1d 24 3))
    (assert-equalp 'NIL                                                                                   (mjr_rptree_quad-get-children an-rptree-1d 24 4))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rptree_quad-get-neighbors
  (let (;(an-rptree-2d (mjr_rptree_new 5 #(-5 -5) #(5 5)))
        (an-rptree-1d (mjr_rptree_new 5 -5 5)))
    (assert-equalp (values '(8 40)  0) (mjr_rptree_quad-get-neighbors an-rptree-1d 24 0))
    (assert-equalp (values '(12 36) 1) (mjr_rptree_quad-get-neighbors an-rptree-1d 24 1))
    (assert-equalp (values '(14 34) 2) (mjr_rptree_quad-get-neighbors an-rptree-1d 24 2))
    (assert-equalp (values '(15 33) 3) (mjr_rptree_quad-get-neighbors an-rptree-1d 24 3))
    (assert-equalp 'NIL                (mjr_rptree_quad-get-neighbors an-rptree-1d 24 4))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_rptree_quad-get-boundary-points
  (let* ((qbt   (mjr_rptree_new 9 #(-8 -8) #(8 8)))
         (qcrd  #(256 256))
         (pnts  (mjr_rptree_quad-get-boundary-points qbt qcrd 3))
         (spnts (sort pnts (mjr_rptree_make-2d-boundary-< qbt qcrd))))
    (assert-equalp '(#(0 512) #(0 448) #(0 384) #(0 320) #(0 256) #(0 192) #(0 128) #(0 64) #(0 0)
                     #(64 0) #(128 0) #(192 0) #(256 0) #(320 0) #(384 0) #(448 0) #(512 0) #(512 64)
                     #(512 128) #(512 192) #(512 256) #(512 320) #(512 384) #(512 448) #(512 512)
                     #(448 512) #(384 512) #(320 512) #(256 512) #(192 512) #(128 512) #(64 512))         spnts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
