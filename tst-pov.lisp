;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-pov.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
;; @brief     Interactive tests for use-pov.lisp.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

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
