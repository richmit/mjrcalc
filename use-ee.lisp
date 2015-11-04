;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-ee.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Electronics Engineering.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_EE
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_NLEQM
        :MJR_OPTM)
  (:DOCUMENTATION "Brief: Electronics Engineering.;")
  (:EXPORT #:mjr_ee_get-r-series
           #:mjr_ee_get-std-r
           #:mjr_ee_minimize
           #:mjr_ee_solve
           ))

(in-package :MJR_EE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ee_get-r-series (ser)
  (let ((s2tol  (pairlis '("E6" "E12" "E24" "E48" "E96" "E192" "E192" "E192")
                         '(20   10     5    2     1     1/2    1/4    1/10)))
        (s2val  (pairlis (list "E6" "E12" "E24" "E48" "E96" "E192")
                         (list (list 100 150 220 330 470 680)
                               (list 100 120 150 180 220 270 330 390 
                                     470 560 680 820)
                               (list 100 110 120 130 150 160 180 200 
                                     220 240 270 300 330 360 390 430 
                                     470 510 560 620 680 750 820 910)
                               (list 100 105 110 115 121 127 133 140 
                                     147 154 162 169 178 187 196 205 
                                     215 226 237 249 261 274 287 301 
                                     316 332 348 365 383 402 422 442 
                                     464 487 511 536 562 590 619 649 
                                     681 715 750 787 825 866 909 953)
                               (list 100 102 105 107 110 113 115 118 
                                     121 124 127 130 133 137 140 143 
                                     147 150 154 158 162 165 169 174 
                                     178 182 187 191 196 200 205 210 
                                     215 221 226 232 237 243 249 255 
                                     261 267 274 280 287 294 301 309 
                                     316 324 332 340 348 357 365 374 
                                     383 392 402 412 422 432 442 453 
                                     464 475 487 499 511 523 536 549 
                                     562 576 590 604 619 634 649 665 
                                     681 698 715 732 750 768 787 806 
                                     825 845 866 887 909 931 953 976)
                               (list 100 101 102 104 105 106 107 109 
                                     110 111 113 114 115 117 118 120 
                                     121 123 124 126 127 129 130 132 
                                     133 135 137 138 140 142 143 145 
                                     147 149 150 152 154 156 158 160 
                                     162 164 165 167 169 172 174 176 
                                     178 180 182 184 187 189 191 193 
                                     196 198 200 203 205 208 210 213 
                                     215 218 221 223 226 229 232 234 
                                     237 240 243 246 249 252 255 258 
                                     261 264 267 271 274 277 280 284 
                                     287 291 294 298 301 305 309 312 
                                     316 320 324 328 332 336 340 344 
                                     348 352 357 361 365 370 374 379 
                                     383 388 392 397 402 407 412 417 
                                     422 427 432 437 442 448 453 459 
                                     464 470 475 481 487 493 499 505 
                                     511 517 523 530 536 542 549 556 
                                     562 569 576 583 590 597 604 612 
                                     619 626 634 642 649 657 665 673 
                                     681 690 698 706 715 723 732 741 
                                     750 759 768 777 787 796 806 816 
                                     825 835 845 856 866 876 887 898 
                                     909 919 931 942 953 965 976 988))))
        (cap10  (list 1/1000000000 3/400000000 7/125000000 47/100000000 9/2500000 47/100000000 11/5000 11/10000000000 41/5000000000
                      31/500000000 51/100000000 39/10000000 17/25000000 33/10000 3/2500000000 91/10000000000 17/250000000 7/12500000
                      43/10000000 1/1000000 47/10000 13/10000000000 1/100000000 3/40000000 31/50000000 47/10000000 3/2000000 17/2500
                      3/2000000000 11/1000000000 41/500000000 17/25000000 51/10000000 11/5000000 1/100 1/625000000 3/250000000
                      91/1000000000 3/4000000 7/1250000 33/10000000 9/5000000000 13/1000000000 1/10000000 41/50000000 31/5000000
                      47/10000000 1/500000000 3/200000000 11/100000000 91/100000000 17/2500000 17/2500000 11/5000000000 1/62500000
                      3/25000000 1/1000000 3/400000 1/100000 3/1250000000 9/500000000 13/100000000 11/10000000 41/5000000 3/200000
                      27/10000000000 1/50000000 3/20000000 3/2500000 91/10000000 11/500000 3/1000000000 11/500000000 1/6250000
                      13/10000000 1/100000000 33/1000000 33/10000000000 3/125000000 9/50000000 3/2000000 3/200000000 47/1000000
                      9/2500000000 27/1000000000 1/5000000 1/625000 11/500000000 17/250000 39/10000000000 3/100000000 11/50000000
                      9/5000000 33/1000000000 1/10000 43/10000000000 33/1000000000 3/12500000 1/500000 47/1000000000 3/20000
                      47/10000000000 9/250000000 27/100000000 11/5000000 17/250000000 11/50000 51/10000000000 39/1000000000 3/10000000
                      3/1250000 1/10000000 33/100000 7/1250000000 43/1000000000 9/25000000 27/10000000 3/20000000 47/100000
                      31/5000000000 47/1000000000 39/100000000 3/1000000 11/50000000 17/25000 17/2500000000 51/1000000000 43/100000000
                      33/10000000 33/100000000 1/1000))
        (ser    (or (and (stringp ser) (string-upcase ser)) (and (symbolp ser) (symbol-name ser)))))
    (declare (ignore s2tol)) ;; Keep compiler from complaining.
    (if (string= ser "CAP10")
        cap10
      (loop
        for m = 1/1000 then (* m 10)
        for i from 1 upto 10
        append (mapcar (lambda (x) (* m x)) (cdr (assoc ser s2val :test #'string=)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ee_get-std-r (r &key (series :E12) (condition :near))
  ":near  :below  :above"
  (let* ((the-series  (mjr_ee_get-r-series series))
         (series-max  (reduce #'max the-series))
         (mult        (loop for i = (/ 1 100000) then (* 10 i) do (if (<= r (* series-max i)) (return i))))
         (the-seriesm (mapcar (lambda (x) (* x mult)) the-series))
         (the-min-err (loop for rv in the-seriesm
                            when (or (eq condition :near) (and (eq condition :below) (< rv r)) (and (eq condition :above) (> rv r)))
                            minimize (mjr_numu_absdif r rv)))
         (the-r-vals  (loop for rv in the-seriesm
                            when (or (eq condition :near) (and (eq condition :below) (< rv r)) (and (eq condition :above) (> rv r)))
                            when (= the-min-err (mjr_numu_absdif r rv))
                            collect rv)))
    (mapcar #'float the-r-vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ee_minimize (func var-vals &key arg-mode (show-progress nil))
  "Find values for which the FUNC is minimal

VAR-VALS is a list of sequences and/or symbols indicating a set of standard component values for the function arguments.

The list of values (as vectors) minimum value are returned."
  (mjr_optm_minimize-comb-search func
                                (mapcar (lambda (x) (if (symbolp x) (mjr_ee_get-r-series x) x)) var-vals)
                                :arg-mode arg-mode 
                                :show-progress show-progress))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ee_solve (func var-vals &key (target 0) arg-mode (show-progress nil))
  "Find values for which the FUNC is closest to :TARGET

VAR-VALS is a list of sequences and/or symbols indicating a set of standard component values for the function arguments.

The list of values (as vectors) minimizing distance and minimum error value are returned.

Example (Find r1, r2, & c such that a 555 will have a frequency of 1001Hz):
       (mjr_ee_solve (lambda (v) (let ((r1 (aref v 0))
                                       (r2 (aref v 1))
                                       (c  (aref v 2)))
                                   (/ 144 (* 100 c (+ r1 (* 2 r2))))))
                     (list :e12 :e12 :cap10)
                     :target 1001)"
  (mjr_nleqm_root-comb-search func
                              (mapcar (lambda (x) (if (symbolp x) (mjr_ee_get-r-series x) x)) var-vals)
                              :target target
                              :arg-mode arg-mode 
                              :show-progress show-progress))

