;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-util.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,2004,2010,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :MJR_UTIL..@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_UTIL-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_UTIL))

(in-package :MJR_UTIL-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_non-empty-seqp
  (assert-equalp nil (mjr_util_non-empty-seqp nil))
  (assert-equalp nil (mjr_util_non-empty-seqp #()))
  (assert-equalp 1   (mjr_util_non-empty-seqp #(1)))
  (assert-equalp 1   (mjr_util_non-empty-seqp '(1)))
  (assert-equalp 3   (mjr_util_non-empty-seqp #(1 2 3)))
  (assert-equalp 3   (mjr_util_non-empty-seqp '(1 2 3)))
  (assert-equalp nil (mjr_util_non-empty-seqp 1))
  (assert-equalp nil (mjr_util_non-empty-seqp 't))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_max-print-width
  (assert-equalp 2 (mjr_util_max-print-width '(1 2 3 4 5 6 7 8 9 10)))
  (assert-equalp 2 (mjr_util_max-print-width #(1 2 3 4 5 6 7 8 9 10)))
  (assert-equalp 2 (mjr_util_max-print-width '(1 2 3 4 5 6 7 8 9 10) "~a"))
  (assert-equalp 2 (mjr_util_max-print-width '(1 2 3 4 5 6 7 8 9 10) "~d"))
  (assert-equalp 5 (mjr_util_max-print-width '(1 2 3 4 5 6 7 8 9 10) "~5d"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_strip-kwarg
  (assert-equalp '(:FOO 1 :BAR 2 :FOOBAR 3)     (mjr_util_strip-kwarg '(:FOO 1 :BAR 2 :FOOBAR 3)))
  (assert-equalp '(:FOO 1 :FOOBAR 3)            (mjr_util_strip-kwarg '(:FOO 1 :BAR 2 :FOOBAR 3) :STRIP-LIST '(:BAR)))
  (assert-equalp '(:BAR 2)                      (mjr_util_strip-kwarg '(:FOO 1 :BAR 2 :FOOBAR 3) :KEEP-LIST  '(:BAR)))
  (assert-equalp nil                            (mjr_util_strip-kwarg nil))
  ;; Errors
  (assert-error 'error                          (mjr_util_strip-kwarg 't))
  (assert-error 'error                          (mjr_util_strip-kwarg '(:FOO 1 :BAR 2 :FOOBAR 3 4)))
  (assert-error 'error                          (mjr_util_strip-kwarg '(:FOO 1 4 :BAR 2 :FOOBAR 3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_split-seq-if
  (assert-equalp '((1 1 1) (3) (4))  (mjr_util_split-seq-if (list 1 1 1 7 3 9 4) (lambda (x) (> x 4))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_split-seq-on-elt
  (assert-equalp '((1 1 1) (3) (4))  (mjr_util_split-seq-on-elt (list 1 1 1 2 3 2 4) 2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_super-concatenate
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list 1 2 3 4 5 6 7 8 9 0)                 #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2 3 4 5 6 7 8 9 0))              #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list #(1 2 3 4 5 6 7 8 9 0))              #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2 3 4 5) '(6 7 8 9 0))           #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list #(1 2 3 4 5) #(6 7 8 9 0))           #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2 3 4 5) #(6 7 8 9 0))           #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list #(1 2 3 4 5) '(6 7 8 9 0))           #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2) #(3 4) #(5 6) '(7 8) 9 0)     #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2) #(3 4) #(5 6) '(7 8) 9 #(0))  #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2) #(3 4) #(5 6) '(7 8) #(9 0))  #'<))
  (assert-equalp '(0 1 2 3 4 5 6 7 8 9)  (sort (mjr_util_super-concatenate  'list '(1 2) 3 4 #(5 6) '(7 8) #(9 0))     #'<))
  (assert-equalp nil                           (mjr_util_super-concatenate  'list ))
  (assert-equalp nil                           (mjr_util_super-concatenate  'list nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_util_get-kwarg-vals
  (assert-equalp '(1 2)             (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:bar 2 :foo 1))))
  (assert-equalp '(1 2)             (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foo 1 :bar 2))))
  (assert-equalp '(nil nil)         (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :barfoo 2))))
  (assert-equalp '(nil nil)         (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '())))
  (assert-equalp '()                (multiple-value-list (mjr_util_get-kwarg-vals '()          '(:foo 1 :bar 2))))
  (assert-equalp '(nil 2)           (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:bar 2))))
  (assert-equalp '(1 2)             (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :bar 2 :foo 1))))
  (assert-equalp '(nil 2)           (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :bar 2))))

  (assert-error 'error              (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :barfoo 2)     't)))
  (assert-error 'error              (multiple-value-list (mjr_util_get-kwarg-vals '()          '(:foo 1 :bar 2)           't)))
  (assert-error 'error              (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :bar 2 :foo 1) 't)))
  (assert-error 'error              (multiple-value-list (mjr_util_get-kwarg-vals '(:foo :bar) '(:foobar 1 :bar 2)        't)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests
 )
