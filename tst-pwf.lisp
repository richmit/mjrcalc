;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-pwf.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit tests.@EOL
;; @std       Common Lisp
;; @see       tst-pwf.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2003,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_PWF-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PWF))

(in-package :MJR_PWF-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_search-interval-mesh
  (assert-equalp -1    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 1))
  (assert-equalp  0    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 2))
  (assert-equalp  0    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 3))
  (assert-equalp  1    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 4))
  (assert-equalp  1    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 5))
  (assert-equalp  1    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 6))
  (assert-equalp -2    (mjr_pwf_search-interval-mesh :interval-type-left-closed #(2 4 6) 7))
                       
  (assert-equalp -1    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 1))
  (assert-equalp  0    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 2))
  (assert-equalp  0    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 3))
  (assert-equalp  0    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 4))
  (assert-equalp  1    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 5))
  (assert-equalp  1    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 6))
  (assert-equalp -2    (mjr_pwf_search-interval-mesh :interval-type-left-open   #(2 4 6) 7))
  
  (assert-error 'error (mjr_pwf_search-interval-mesh "interval-type-left-open"  #(2 4 6) 7))
  (assert-error 'error (mjr_pwf_search-interval-mesh :interval-type-left-silly  #(2 4 6) 7))

  (assert-error 'error (mjr_pwf_search-interval-mesh :interval-type-left-silly  #(2 4 6) #C(1 2)))
  (assert-error 'error (mjr_pwf_search-interval-mesh :interval-type-left-silly  #(2 4 6) :foo))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_eval-linear

  (assert-error 'error (mjr_pwf_eval-linear   7 #(2 4 6) #(3 5)))

  (assert-equalp    2 (mjr_pwf_eval-linear   1 #(2 4 6) #(3 5 7)))
  (assert-equalp    3 (mjr_pwf_eval-linear   2 #(2 4 6) #(3 5 7)))
  (assert-equalp    4 (mjr_pwf_eval-linear   3 #(2 4 6) #(3 5 7)))
  (assert-equalp    5 (mjr_pwf_eval-linear   4 #(2 4 6) #(3 5 7)))
  (assert-equalp    6 (mjr_pwf_eval-linear   5 #(2 4 6) #(3 5 7)))
  (assert-equalp    7 (mjr_pwf_eval-linear   6 #(2 4 6) #(3 5 7)))
  (assert-equalp    8 (mjr_pwf_eval-linear   7 #(2 4 6) #(3 5 7)))

  (assert-equalp    2 (mjr_pwf_eval-linear   1 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    3 (mjr_pwf_eval-linear   2 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    4 (mjr_pwf_eval-linear   3 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    5 (mjr_pwf_eval-linear   4 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    6 (mjr_pwf_eval-linear   5 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    7 (mjr_pwf_eval-linear   6 #(2 4 6) #(3 5 7) :nil-outside-interval nil))
  (assert-equalp    8 (mjr_pwf_eval-linear   7 #(2 4 6) #(3 5 7) :nil-outside-interval nil))

  (assert-equalp  nil (mjr_pwf_eval-linear   1 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp    3 (mjr_pwf_eval-linear   2 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp    4 (mjr_pwf_eval-linear   3 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp    5 (mjr_pwf_eval-linear   4 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp    6 (mjr_pwf_eval-linear   5 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp    7 (mjr_pwf_eval-linear   6 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  (assert-equalp  nil (mjr_pwf_eval-linear   7 #(2 4 6) #(3 5 7) :nil-outside-interval 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_eval-step

  (assert-error 'error (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5)))
  (assert-error 'error (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 6 7)))
  
  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7)))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7)))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :left-tail-value 0))

  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :right-tail-value 8))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :left-tail-value 0 :right-tail-value 8))

  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0))

  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :right-tail-value 8))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-closed :left-tail-value 0 :right-tail-value 8))
  
  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    3 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    5 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    3 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    5 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))
  (assert-equalp    7 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0))

  (assert-equalp    3 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :right-tail-value 8))

  (assert-equalp    0 (mjr_pwf_eval-step   1 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   2 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   3 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    3 (mjr_pwf_eval-step   4 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   5 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    5 (mjr_pwf_eval-step   6 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   7 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    7 (mjr_pwf_eval-step   8 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))
  (assert-equalp    8 (mjr_pwf_eval-step   9 #(2 4 6 8) #(3 5 7) :interval-type :interval-type-left-open :left-tail-value 0 :right-tail-value 8))

  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_eval-funcs

  (assert-error 'error (mjr_pwf_eval-funcs   1 #(2 4 6) '((lambda (x) (x)))))
  (assert-error 'error (mjr_pwf_eval-funcs   1 #(2 4 6) '((lambda (x) (x)) (lambda (x) (x)) (lambda (x) x))))

  (assert-equalp    2 (mjr_pwf_eval-funcs 1 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    1 (mjr_pwf_eval-funcs 2 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    0 (mjr_pwf_eval-funcs 3 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    1 (mjr_pwf_eval-funcs 4 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    0 (mjr_pwf_eval-funcs 5 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp   -1 (mjr_pwf_eval-funcs 6 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    0 (mjr_pwf_eval-funcs 7 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    1 (mjr_pwf_eval-funcs 8 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))
  (assert-equalp    2 (mjr_pwf_eval-funcs 9 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1)))

  (assert-equalp    2 (mjr_pwf_eval-funcs 1 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    1 (mjr_pwf_eval-funcs 2 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    0 (mjr_pwf_eval-funcs 3 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    1 (mjr_pwf_eval-funcs 4 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    0 (mjr_pwf_eval-funcs 5 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp   -1 (mjr_pwf_eval-funcs 6 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    0 (mjr_pwf_eval-funcs 7 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    1 (mjr_pwf_eval-funcs 8 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))
  (assert-equalp    2 (mjr_pwf_eval-funcs 9 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed))

  (assert-equalp    2 (mjr_pwf_eval-funcs 1 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    1 (mjr_pwf_eval-funcs 2 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    0 (mjr_pwf_eval-funcs 3 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp   -1 (mjr_pwf_eval-funcs 4 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    0 (mjr_pwf_eval-funcs 5 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    1 (mjr_pwf_eval-funcs 6 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    0 (mjr_pwf_eval-funcs 7 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    1 (mjr_pwf_eval-funcs 8 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))
  (assert-equalp    2 (mjr_pwf_eval-funcs 9 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open))

  (assert-equalp  nil (mjr_pwf_eval-funcs 1 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 2 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 3 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 4 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 5 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp   -1 (mjr_pwf_eval-funcs 6 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 7 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 8 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))
  (assert-equalp  nil (mjr_pwf_eval-funcs 9 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-closed :nil-outside-interval 't))

  (assert-equalp  nil (mjr_pwf_eval-funcs 1 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 2 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 3 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp   -1 (mjr_pwf_eval-funcs 4 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 5 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 6 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    0 (mjr_pwf_eval-funcs 7 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp    1 (mjr_pwf_eval-funcs 8 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  (assert-equalp  nil (mjr_pwf_eval-funcs 9 #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)) :fixed-domain '(-1 1) :interval-type :interval-type-left-open   :nil-outside-interval 't))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_func-linear
  (loop with f = (mjr_pwf_func-linear #(2 4 6) #(3 5 7))
        for x from 1 upto 9
        do (assert-equalp  (funcall f x) (mjr_pwf_eval-linear x #(2 4 6) #(3 5 7))))
  )
                                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_func-step
  (loop with f = (mjr_pwf_func-step #(2 4 6 8) #(3 5 7))
        for x from 1 upto 9
        do (assert-equalp (funcall f x) (mjr_pwf_eval-step x #(2 4 6 8) #(3 5 7))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pwf_func-funcs
  (loop with f = (mjr_pwf_func-funcs #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)))
        for x from 1 upto 9
        do (assert-equalp  (funcall f x) (mjr_pwf_eval-funcs x #(2 4 6 8) (list (lambda (x) (- x)) (lambda (x) (abs x)) (lambda (x) x)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )

