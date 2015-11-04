;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-chem.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_CHEM.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CHEM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CHEM))

(in-package :MJR_CHEM-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_chem_find-element-property
  ;; Search with vaious key types

  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property "Ti"          "melingpoint"                 ))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property "Ti"          "melingpoint" :ek-symbol      ))
  (assert-equalp '("Titanium" . 1935)         (mjr_chem_find-element-property "Ti"          "melingpoint" :ek-name        ))
  (assert-equalp '(22 . 1935)                 (mjr_chem_find-element-property "Ti"          "melingpoint" :ek-atomicnumber))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property :ti           "melingpoint"                 ))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property :ti           "melingpoint" :ek-symbol      ))
  (assert-equalp '("Titanium" . 1935)         (mjr_chem_find-element-property :ti           "melingpoint" :ek-name        ))
  (assert-equalp '(22 . 1935)                 (mjr_chem_find-element-property :ti           "melingpoint" :ek-atomicnumber))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property "Titanium"    "melingpoint"                 ))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property "Titanium"    "melingpoint" :ek-symbol      ))
  (assert-equalp '("Titanium" . 1935)         (mjr_chem_find-element-property "Titanium"    "melingpoint" :ek-name        ))
  (assert-equalp '(22 . 1935)                 (mjr_chem_find-element-property "Titanium"    "melingpoint" :ek-atomicnumber))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property :titanium     "melingpoint"                 ))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property :titanium     "melingpoint" :ek-symbol      ))
  (assert-equalp '("Titanium" . 1935)         (mjr_chem_find-element-property :titanium     "melingpoint" :ek-name        ))
  (assert-equalp '(22 . 1935)                 (mjr_chem_find-element-property :titanium     "melingpoint" :ek-atomicnumber))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property 22            "melingpoint"                 ))
  (assert-equalp '("Ti" . 1935)               (mjr_chem_find-element-property 22            "melingpoint" :ek-symbol      ))
  (assert-equalp '("Titanium" . 1935)         (mjr_chem_find-element-property 22            "melingpoint" :ek-name        ))
  (assert-equalp '(22 . 1935)                 (mjr_chem_find-element-property 22            "melingpoint" :ek-atomicnumber))

  ;; Should get a list when we start with a list
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '("Ti")       "melingpoint"                 ))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '("Ti")       "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Titanium" . 1935))       (mjr_chem_find-element-property '("Ti")       "melingpoint" :ek-name        ))
  (assert-equalp '((22 . 1935))               (mjr_chem_find-element-property '("Ti")       "melingpoint" :ek-atomicnumber))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(:ti)        "melingpoint"                 ))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(:ti)        "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Titanium" . 1935))       (mjr_chem_find-element-property '(:ti)        "melingpoint" :ek-name        ))
  (assert-equalp '((22 . 1935))               (mjr_chem_find-element-property '(:ti)        "melingpoint" :ek-atomicnumber))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '("Titanium") "melingpoint"                 ))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '("Titanium") "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Titanium" . 1935))       (mjr_chem_find-element-property '("Titanium") "melingpoint" :ek-name        ))
  (assert-equalp '((22 . 1935))               (mjr_chem_find-element-property '("Titanium") "melingpoint" :ek-atomicnumber))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(:titanium)  "melingpoint"                 ))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(:titanium)  "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Titanium" . 1935))       (mjr_chem_find-element-property '(:titanium)  "melingpoint" :ek-name        ))
  (assert-equalp '((22 . 1935))               (mjr_chem_find-element-property '(:titanium)  "melingpoint" :ek-atomicnumber))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(22)         "melingpoint"                 ))
  (assert-equalp '(("Ti" . 1935))             (mjr_chem_find-element-property '(22)         "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Titanium" . 1935))       (mjr_chem_find-element-property '(22)         "melingpoint" :ek-name        ))
  (assert-equalp '((22 . 1935))               (mjr_chem_find-element-property '(22)         "melingpoint" :ek-atomicnumber))

  ;; Look for two elemetns
  (assert-equalp '((22 . 1935) (26 . 1808))       (mjr_chem_find-element-property '("Ti" "Fe") "melingpoint" :ek-atomicnumber))
  (assert-equalp '(("Ti" . 1935) ("Fe" . 1808))   (mjr_chem_find-element-property '("Ti" "Fe") "melingpoint" :ek-symbol      ))
  (assert-equalp '(("Ti" . 1935) ("Fe" . 1808))   (mjr_chem_find-element-property '("Ti" "Fe") "melingpoint"                 ))

  ;; NIL should get all elements with the property defined
  (assert-equalp 103                          (length (mjr_chem_find-element-property nil "melingpoint" :ek-atomicnumber)))
  (assert-equalp 118                          (length (mjr_chem_find-element-property nil "atomicnumber" :ek-atomicnumber)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_chem_find-element-key
  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol '("melingpoint" . 3695)))
  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol '("symbol" . "W")))
  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol '("melingpoint" . 3695) '("symbol" . "W")))

  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol (cons "melingpoint"  (lambda (x) (=             x  3695)))))
  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol (cons "symbol"       (lambda (x) (string-equal  x "W")))))

  (assert-equalp '("W") (mjr_chem_find-element-key :ek-symbol '("melingpoint" . 3695) (cons "symbol" (lambda (x) (string-equal  x "W")))))

  (assert-equalp '("Lu" "Tm")                 (mjr_chem_find-element-key :ek-symbol (cons "melingpoint" (lambda (x) (> x 1808))) '("family" . "Rare_Earth")))

  (assert-equalp '(("C" . 5100) ("W" . 5825)) (mjr_chem_find-element-property (mjr_chem_find-element-key :ek-symbol (cons "melingpoint" (lambda (x) (> x 3500)))) "boilingpoint"))
  (assert-equalp '("C" "W")                   (mjr_chem_find-element-key :ek-symbol (cons "melingpoint" (lambda (x) (> x 3500)))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
