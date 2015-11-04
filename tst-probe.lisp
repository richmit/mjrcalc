;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      tst-probe.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
;; @brief     Test package for use-probe.lisp.@EOL
;; @Keywords
;; @Std       Common Lisp
;;

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_PROBE-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_PROBE :MJR_PRNG :MJR_EPS))

(in-package :MJR_PROBE-TESTS)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_probe_epdf-int*-naive (epdf pos-int)
  "Compute product of the EPDF and POS-INT"
  (cond ((not (integerp pos-int)) (error "mjr_probe_epdf-int*: pos-int must be an integer"))
        ((>= 0 pos-int)           (error "mjr_probe_epdf-int*: pos-int must be positive")))
  (let ((epdf   (if (vectorp epdf) epdf (concatenate 'vector epdf)))
        (newpdf (copy-seq epdf)))
    (dotimes (i (1- pos-int) newpdf)
      (setf newpdf (mjr_probe_epdf-+ newpdf epdf)))))

(defvar ecdf1 #(1))
(defvar ecdf2 #(1/10 3/10 1/10 1/5 1/5 1/10))
(defvar ecdf3 #(1/45 2/45 1/15 4/45 1/9 2/15 7/45 8/45 1/5))
(defvar ecdf4 #(1/52 3/208 5/208 7/208 9/208 1/208 5/208 7/208 3/208 7/208 3/104 1/26 3/208
                9/208 1/104 1/208 1/26 0 0 1/52 1/208 3/208 3/208 0 5/208 1/104 0 3/104 1/208
                0 1/52 9/208 1/104 9/208 3/208 5/208 7/208 7/208 3/104 1/26 5/208 0 1/208
                1/26 1/52 1/104 1/208 1/52 1/104 1/26))

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2ecwt
  (assert-equalp #(1    3    6    11)    (mjr_probe_ewt2ecwt  #(1    2    3    5)))         ;; FREQ
  (assert-equalp #(1/11 3/11 6/11 11/11) (mjr_probe_ewt2ecwt  #(1/11 2/11 3/11 5/11)))      ;; PDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2epdf
  (assert-equalp #(1/11 2/11 3/11 5/11)  (mjr_probe_ewt2epdf  #(1    2    3    5)))         ;; FREQ
  (assert-equalp #(1/11 2/11 3/11 5/11)  (mjr_probe_ewt2epdf  #(1/11 2/11 3/11 5/11)))      ;; PDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2efreq
  (assert-equalp #(1    2    3    5)     (mjr_probe_ewt2efreq  #(1    2    3    5)))         ;; FREQ
  (assert-equalp #(1    2    3    5)     (mjr_probe_ewt2efreq  #(1/2  2/2  3/2  5/2)))       ;; WT
  (assert-equalp #(1    2    3    5)     (mjr_probe_ewt2efreq  #(2    4    6    10)))        ;; FREQ (not minimal)
  (assert-equalp #(1    2    3    5)     (mjr_probe_ewt2efreq  #(1/11 2/11 3/11 5/11)))      ;; PDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ecwt2ecfreq
  (assert-equalp #(1    3    6    11)    (mjr_probe_ecwt2ecfreq  #(1    3    6    11)))      ;; CFREQ
  (assert-equalp #(1    3    6    11)    (mjr_probe_ecwt2ecfreq  #(1/2  3/2  6/2  11/2)))    ;; CWT
  (assert-equalp #(1    3    6    11)    (mjr_probe_ecwt2ecfreq  #(2    6    12   22)))      ;; CFREQ (not minimal)
  (assert-equalp #(1    3    6    11)    (mjr_probe_ecwt2ecfreq  #(1/11 3/11 6/11 11/11)))   ;; CDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ecwt2ecdf
  (assert-equalp #(1/11 3/11 6/11 11/11) (mjr_probe_ecwt2ecdf #(1    3    6    11)))        ;; CFREQ
  (assert-equalp #(1/11 3/11 6/11 11/11) (mjr_probe_ecwt2ecdf #(1/11 3/11 6/11 11/11)))     ;; CDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ecwt2ewt
  (assert-equalp #(1    2    3    5)     (mjr_probe_ecwt2ewt  #(1    3    6    11)))        ;; CFREQ
  (assert-equalp #(1/11 2/11 3/11 5/11)  (mjr_probe_ecwt2ewt  #(1/11 3/11 6/11 11/11)))     ;; CDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_data2efreq
  (assert-equalp #(1    2    3    5)    (mjr_probe_data2efreq  #(0 1 1 2 2 2 3 3 3 3 3)))                         ;; DATA
  (assert-equalp #(1    2    3    5)    (mjr_probe_data2efreq  #(0 1 1 2 2 2 3 3 3 3 3 0 1 1 2 2 2 3 3 3 3 3)))   ;; DATA
  (assert-equalp #(1    2    3    5)    (mjr_probe_data2efreq  #(0 1 2 2 3 3 1 2 2 3 3 3 0 1 1 2 3 3 3 2 3 3)))   ;; DATA
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2data
  (assert-equalp #(0 1 1 2 2 2 3 3 3 3 3) (mjr_probe_ewt2data  #(1    2    3    5)))         ;; FREQ
  (assert-equalp #(0 1 1 2 2 2 3 3 3 3 3) (mjr_probe_ewt2data  #(2    4    6    10)))        ;; FREQ (not minimal)
  (assert-equalp #(0 1 1 2 2 2 3 3 3 3 3) (mjr_probe_ewt2data  #(1/2  2/2  3/2  5/2)))       ;; WT
  (assert-equalp #(0 1 1 2 2 2 3 3 3 3 3) (mjr_probe_ewt2data  #(1/11 2/11 3/11 5/11)))      ;; PDF
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_epdf-+
  (assert-equalp #(0 1 1 1 1)                                                   (mjr_probe_epdf-+  #(0 1 1 1 1)))                                                     ;; 1 4-sided die
  (assert-equalp #(0 0 1 2 3 4 3 2 1)                                           (mjr_probe_epdf-+  #(0 1 1 1 1) #(0 1 1 1 1)))                                        ;; 2 4-sided die
  (assert-equalp #(0 0 0 1 3 6 10 12 12 10 6 3 1)                               (mjr_probe_epdf-+  #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1)))                           ;; 3 4-sided die
  (assert-equalp #(0 0 0 0 1 4 10 20 31 40 44 40 31 20 10 4 1)                  (mjr_probe_epdf-+  #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1)))              ;; 4 4-sided die
  (assert-equalp #(0 0 0 0 0 1 5 15 35 65 101 135 155 155 135 101 65 35 15 5 1) (mjr_probe_epdf-+  #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1) #(0 1 1 1 1))) ;; 5 4-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_epdf-e
  (assert-equalp 5/2   (mjr_probe_epdf-e       #(0 1/4 1/4 1/4 1/4)))                 ;; 1 4-sided die
  (assert-equalp 7/2   (mjr_probe_epdf-e       #(0 1/6 1/6 1/6 1/6 1/6 1/6)))         ;; 1 6-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_epdf-v
  (assert-equalp 5/4   (mjr_probe_epdf-v       #(0 1/4 1/4 1/4 1/4)))                 ;; 1 4-sided die
  (assert-equalp 35/12 (mjr_probe_epdf-v       #(0 1/6 1/6 1/6 1/6 1/6 1/6)))         ;; 1 6-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_epdf-moment
  (assert-equalp 5/2   (mjr_probe_epdf-moment  #(0 1/4 1/4 1/4 1/4)           1))         ;; 1 4-sided die
  (assert-equalp 7/2   (mjr_probe_epdf-moment  #(0 1/6 1/6 1/6 1/6 1/6 1/6)   1))         ;; 1 6-sided die
  (assert-equalp 15/2  (mjr_probe_epdf-moment  #(0 1/4 1/4 1/4 1/4)           2))         ;; 1 4-sided die
  (assert-equalp 91/6  (mjr_probe_epdf-moment  #(0 1/6 1/6 1/6 1/6 1/6 1/6)   2))         ;; 1 6-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2pdf
  (loop for ewt in '(#(1    2    3    5)         ;; FREQ
                     #(2    4    6    10)        ;; FREQ (not minimal)
                     #(1/2  2/2  3/2  5/2)       ;; WT
                     #(1/11 2/11 3/11 5/11))     ;; PDF
        for epdf = (mjr_probe_ewt2epdf ewt)
        for f    = (mjr_probe_ewt2pdf ewt)
        do (loop for p1 across epdf
                 for i from 0
                 do (assert-equalp p1 (funcall f i))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2cdf
  (loop for ewt in '(#(1    3    6    11)      ;; CFREQ
                     #(1/2  3/2  6/2  11/2)    ;; CWT
                     #(2    6    12   22)      ;; CFREQ (not minimal)
                     #(1/11 3/11 6/11 11/11))  ;; CDF
        for ecdf = (mjr_probe_ecwt2ecdf ewt)
        for f    = (mjr_probe_ecwt2cdf ewt)
        do (loop for p1 across ecdf
                 for i from 0
                 do (assert-equalp p1 (funcall f i))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_data2prng
  (loop for data = (mjr_prng_vector 10 #'mjr_prng_int-cc 0 5)
        for maxd = (reduce #'max data)
        for mind = (reduce #'min data)
        for prng = (mjr_probe_data2prng data)
        for i from 1 upto 100
        do (loop for j from 1 upto 100
                 for r = (funcall prng)
                 do (assert-true (<= r maxd))
                 do (assert-true (>= r mind))))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ecwt2prng
  ;; ;; This is how we check this function, but every now and then we get unlucky. :)  It is a PRNG afterall...
  ;; (dotimes (k 10)
  ;;   (let* ((freq (mjr_prng_vector 5 #'mjr_prng_int-cc 0 10))
  ;;          (prng (mjr_probe_ewt2prng freq))
  ;;          (data (loop for i from 0 upto 1000000
  ;;                      collect (funcall prng)))
  ;;          (pdf  (mjr_probe_ewt2epdf (mjr_probe_data2efreq data))))
  ;;     (assert-equality (mjr_eps_make-fixed= .1) pdf (mjr_probe_ewt2epdf freq))))
  1
  )

;; (let* ((freq #(1 1 0 0 0 2 1 1 3 1 0 0))
;;                 (prng (mjr_probe_ewt2prng freq))
;;                 (data (loop for i from 0 upto 100000
;;                             collect (funcall prng)))
;;                 (pdf1  (mjr_probe_ewt2epdf (mjr_probe_data2efreq data)))
;;                 (pdf2  (mjr_probe_ewt2epdf freq)))
;;            (mjr_vec_print pdf1 "~10,3f")
;;            (mjr_vec_print pdf2 "~10,3f")
;;            (mjr_vec_print (mjr_vec_- pdf1 pdf2) "~10,3f")
;;            nil)

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ecwt2prng
  (loop for i from 1 upto 10
        for epdf = (mjr_probe_ewt2epdf (mjr_prng_vector (mjr_prng_int-cc 1 15) #'mjr_prng_int-cc 1 10))
        do (loop for j from 1 upto 10
                 for n = (mjr_prng_int-cc 1 15)
                 for p1 = (apply #'mjr_probe_epdf-+ (loop for k from 1 upto n
                                                          collect epdf))
                 for p2 = (mjr_probe_epdf-int* epdf n)
                 do (assert-equalp p1 p2)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_epdf-int*
  (loop for i from 1 upto 10
        do (assert-equalp (MJR_PROBE_EPDF-INT*-naive ecdf1 i) (MJR_PROBE_EPDF-INT* ecdf1 i)))
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_pdf2epdf
  (assert-equalp #(0 1/6 1/6 1/6 1/6 1/6 1/6)  (mjr_probe_pdf2epdf 1 6 (lambda (x) (declare (ignore x)) 1/6))) ;; 1 6-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_cdf2ecdf
  (assert-equalp #(0 1/6 1/3 1/2 2/3 5/6 1)    (mjr_probe_pdf2epdf 1 6 (lambda (x) (/ x 6)))) ;; 1 6-sided die
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_ewt2prng
  ;; The test cases for mjr_probe_ecwt2prng make heavy use of this function, so we don't need many tests here.
  1
  )
  
;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_probe_help
  ;; Note: This function dosen't need test cases..
  1
  )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests 
 )
