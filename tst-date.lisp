;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-date.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Tests for use-date.lisp@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_DATE-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_DATE))

(in-package :MJR_DATE-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_month-name
  (assert-equalp "January"  (mjr_date_month-name 1))
  (assert-equalp "April"    (mjr_date_month-name 4))
  (assert-equalp "May"      (mjr_date_month-name 5))
  (assert-equalp "October"  (mjr_date_month-name 10))
  (assert-equalp "December" (mjr_date_month-name 12))
  ;; Test with jan index set to zero
  (assert-equalp "January"  (mjr_date_month-name 0  :january-index 0))
  (assert-equalp "April"    (mjr_date_month-name 3  :january-index 0))
  (assert-equalp "May"      (mjr_date_month-name 4  :january-index 0))
  (assert-equalp "October"  (mjr_date_month-name 9  :january-index 0))
  (assert-equalp "December" (mjr_date_month-name 11 :january-index 0))
  ;; Make sure the jan index default works.
  (loop for i from 1 upto 12
        do (assert-equalp (mjr_date_month-name i) (mjr_date_month-name i :january-index 1)))
  ;; string2int
  (assert-equalp 1          (mjr_date_month-name "January"))
  (assert-equalp 4          (mjr_date_month-name "April"))
  (assert-equalp 5          (mjr_date_month-name "May"))
  (assert-equalp 10         (mjr_date_month-name "October"))
  (assert-equalp 12         (mjr_date_month-name "December"))
  ;; Test with jan index set to zero
  (assert-equalp 0  (mjr_date_month-name "January"  :january-index 0))
  (assert-equalp 3  (mjr_date_month-name "April"    :january-index 0))
  (assert-equalp 4  (mjr_date_month-name "May"      :january-index 0))
  (assert-equalp 9  (mjr_date_month-name "October"  :january-index 0))
  (assert-equalp 11 (mjr_date_month-name "December" :january-index 0))
  ;; Errors
  (assert-error 'error      (mjr_date_month-name 13))
  (assert-error 'error      (mjr_date_month-name 0))
  (assert-error 'error      (mjr_date_month-name 't))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_day-name
  (assert-equalp "Monday"     (mjr_date_day-name 1))
  (assert-equalp "Wednesday"  (mjr_date_day-name 3))
  (assert-equalp "Friday"     (mjr_date_day-name 5))
  (assert-equalp "Sunday"     (mjr_date_day-name 7))
  ;; Test with mon index set to zero
  (assert-equalp "Monday"     (mjr_date_day-name 0 :monday-index 0))
  (assert-equalp "Wednesday"  (mjr_date_day-name 2 :monday-index 0))
  (assert-equalp "Friday"     (mjr_date_day-name 4 :monday-index 0))
  (assert-equalp "Sunday"     (mjr_date_day-name 6 :monday-index 0))
  ;; Make sure the jan index default works.
  (loop for i from 1 upto 7
        do (assert-equalp (mjr_date_day-name i) (mjr_date_day-name i :monday-index 1)))
  ;; string2int
  (assert-equalp 1             (mjr_date_day-name "Monday"))
  (assert-equalp 3             (mjr_date_day-name "Wednesday"))
  (assert-equalp 5             (mjr_date_day-name "Friday"))
  (assert-equalp 7             (mjr_date_day-name "Sunday"))
  ;; Test with mon index set to zero
  (assert-equalp 0             (mjr_date_day-name "Monday"    :monday-index 0))
  (assert-equalp 2             (mjr_date_day-name "Wednesday" :monday-index 0))
  (assert-equalp 4             (mjr_date_day-name "Friday"    :monday-index 0))
  (assert-equalp 6             (mjr_date_day-name "Sunday"    :monday-index 0))
  ;; Errors
  (assert-error 'error        (mjr_date_day-name 0))
  (assert-error 'error        (mjr_date_day-name 8))
  (assert-error 'error        (mjr_date_day-name 't))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_jdate2alternate
  (assert-equalp (values 3586294861/86400 41508.04237268519d0)   (mjr_date_jdate2alternate 210946338061/86400))
  (assert-equalp (values 3586294861/86400 41508.04237268519d0)   (mjr_date_jdate2alternate 210946338061/86400 :mjd))
  (assert-equalp (values 2441508 2441508.0d0)                    (mjr_date_jdate2alternate 210946338061/86400 :jdn))
  (assert-equalp (values 2441509 2441509.0d0)                    (mjr_date_jdate2alternate 210946338061/86400 :cjd))
  (assert-equalp (values 3586338061/86400 41508.54237268519d0)   (mjr_date_jdate2alternate 210946338061/86400 :rjd))
  (assert-equalp (values 130294861/86400 1508.0423726851852d0)   (mjr_date_jdate2alternate 210946338061/86400 :tjd))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_convert
  ;; Start of update (LISP time)
  (assert-equalp '(1900 1 1 0 0 0 0)       (mjr_date_convert :dt-string :dt-list   "1900-01-01 00:00:00Z"))
  (assert-equalp 0                         (mjr_date_convert :dt-string :dt-lisp   "1900-01-01 00:00:00Z"))
  (assert-equalp -2208988800               (mjr_date_convert :dt-string :dt-posix  "1900-01-01 00:00:00Z"))
  (assert-equalp 4830041/2                 (mjr_date_convert :dt-string :dt-julian "1900-01-01 00:00:00Z"))
  (assert-equalp -2208988800               (mjr_date_convert :dt-julian :dt-posix  4830041/2 0))
  (assert-equalp 0                         (mjr_date_convert :dt-julian :dt-lisp   4830041/2 0))
  (assert-equalp '(1900 1 1 0 0 0 0)       (mjr_date_convert :dt-julian :dt-list   4830041/2 0))
  (assert-equalp "1900-01-01 00:00:00Z"    (mjr_date_convert :dt-julian :dt-string 4830041/2 0))
  ;; Start of pdate (POSIX/UNIX/C time)
  (assert-equalp '(1970 1 1 0 0 0 0)       (mjr_date_convert :dt-string :dt-list   "1970-01-01 00:00:00Z"))
  (assert-equalp 2208988800                (mjr_date_convert :dt-string :dt-lisp   "1970-01-01 00:00:00Z"))
  (assert-equalp 0                         (mjr_date_convert :dt-string :dt-posix  "1970-01-01 00:00:00Z"))
  (assert-equalp 4881175/2                 (mjr_date_convert :dt-string :dt-julian "1970-01-01 00:00:00Z"))
  (assert-equalp 0                         (mjr_date_convert :dt-julian :dt-posix  4881175/2 0))
  (assert-equalp 2208988800                (mjr_date_convert :dt-julian :dt-lisp   4881175/2 0))
  (assert-equalp '(1970 1 1 0 0 0 0)       (mjr_date_convert :dt-julian :dt-list   4881175/2 0))
  (assert-equalp "1970-01-01 00:00:00Z"    (mjr_date_convert :dt-julian :dt-string 4881175/2 0))
  ;; MJR TODO NOTE mjr_date_convert: Make sure inverse conversions consistent
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_add-delta
  (assert-equalp "1972-07-12 01:01:01Z" (mjr_date_add-delta "1972-07-10 01:01:01Z" 2 :return-zone 0))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_date_delta
  (assert-equalp 2 (mjr_date_delta "1972-07-10 01:01:01Z" "1972-07-12 01:01:01Z"))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
