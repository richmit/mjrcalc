;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-geo.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_GEO.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GEO-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_GEO :MJR_A :MJR_CMP))

(in-package :MJR_GEO-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_geo_geod-distance
  (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b (abs (* 0.01 (/ (+ a b) 2))))))
                   4164074.239
                   (mjr_geo_geod-distance (mjr_a_dms2d 42 15)  (mjr_a_dms2d 71 7) (mjr_a_dms2d 45  31) (mjr_a_dms2d 123 41)))

  (assert-equality (lambda (a b) (and a b (mjr_cmp_= a b (abs (* 0.01 (/ (+ a b) 2))))))
                   969954.114
                   (mjr_geo_geod-distance (mjr_a_dms2d 50 03 58.76) (mjr_a_dms2d 5 42 53.10) (mjr_a_dms2d 58 38 38.48) (mjr_a_dms2d 3 04 12.34)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
