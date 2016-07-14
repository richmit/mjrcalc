;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-pov.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-pov.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2012,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Add tests!!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_POV-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_POV :MJR_UTIL))

(in-package :MJR_POV-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pov_make-from-dsimp

  ;; ;; R1->R1 coordinate curve plot: y=x^2
  ;; (let ((p (probe-file "tst-pov-OUT-g11.pov"))) (if p (delete-file p)))
  ;; (mjr_pov_make-from-func-r12-r13 "tst-pov-OUT-g11.pov"
  ;;                                 (lambda (u) (* u u))
  ;;                                 :udat (list :start 0 :end  10 :len 11)
  ;;                                 :arg-mode :arg-number)
  ;; (assert-equal (mjr_util_read-file "tst-pov-OUT-g11.pov")
  ;;               (mjr_util_read-file "tst-pov-REG-g11.pov"))

  ;; ;; R2->R1 coordinate surface plot: x^2+y^2
  ;; (let ((p (probe-file "tst-pov-OUT-g21.pov"))) (if p (delete-file p)))
  ;; (mjr_pov_make-from-func-r12-r13 "tst-pov-OUT-g21.pov"
  ;;                                 (lambda (u v) (+ (* u u) (* v v)))
  ;;                                 :udat (list :start -6 :end 6 :len 13)
  ;;                                 :vdat (list :start -5 :end 5 :len 11)
  ;;                                 :arg-mode :arg-number)
  ;; (assert-equal (mjr_util_read-file "tst-pov-OUT-g21.pov")
  ;;               (mjr_util_read-file "tst-pov-REG-g21.pov"))

  ;; ;; R1->R3 parametric curve: u -> (u, u^2, u^3) -- the twisted cubic
  ;; (let ((p (probe-file "tst-pov-OUT-p13.pov"))) (if p (delete-file p)))
  ;; (mjr_pov_make-from-func-r12-r13 "tst-pov-OUT-p13.pov"
  ;;                                 (lambda (u) (vector u (* u u) (* u u u)))
  ;;                                 :udat (list :start -1 :end  1 :len 11)
  ;;                                 :arg-mode :arg-number)
  ;; (assert-equal (mjr_util_read-file "tst-pov-OUT-p13.pov")
  ;;               (mjr_util_read-file "tst-pov-REG-p13.pov"))

  ;; ;; R2-R2 parametric surface: (u,v) -> (u^2, v^2, u+v)
  ;; (let ((p (probe-file "tst-pov-OUT-p23.pov"))) (if p (delete-file p)))
  ;; (mjr_pov_make-from-func-r12-r13 "tst-pov-OUT-p23.pov"
  ;;                                 (lambda (u v) (vector (* u u) (* v v) (+ u v)))
  ;;                                 :udat (list :start  0 :end  1 :len 6)
  ;;                                 :vdat (list :start -1 :end 1   :len 11)
  ;;                                 :arg-mode :arg-number)
  ;; (assert-equal (mjr_util_read-file "tst-pov-OUT-p23.pov")
  ;;               (mjr_util_read-file "tst-pov-REG-p23.pov"))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pov_code-curve
  ;; XREF: this function is tested heavily by mjr_pov_make-from-func-r12-r13
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_pov_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
