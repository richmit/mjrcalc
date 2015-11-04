;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-chem.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Tests for :MJR_CHEM.@EOL
;; @std       Common Lisp
;; @see       use-chem.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CHEM-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CHEM))

(in-package :MJR_CHEM-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
