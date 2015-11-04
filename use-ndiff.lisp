 ;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-ndiff.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Numerical differentiation.@EOL
;; @std       Common Lisp
;; @see       tst-ndiff.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,2010,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      Flexible argument control (See: mjr_util_fun-adapt-eval-v) -- especially for multivariate functions.@EOL@EOL
;; @todo      Create mndiff for functions of multiple vars.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_NDIFF
  (:USE :COMMON-LISP
        :MJR_POLY
        :MJR_INTRP)
  (:DOCUMENTATION "Brief: Numerical differentiation.;")
  (:EXPORT #:mjr_ndif_help
           #:mjr_ndiff_central #:mjr_ndiff_backward #:mjr_ndiff_forward
           #:mjr_ndiff_complex
           #:mjr_ndiff_lagrange
           ))

(in-package :MJR_NDIFF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndif_help ()
  "Help for MJR_NDIF:  Numerical differentiation.

Design Goals
  * Speed (These routines will be used inside tight inner loops, and thus must be fast)
  * Ease of use.  Good defaults.

The 'Magic Numbers' were computed with Maxima:
  linel:500;
  display2d:false;
  fdif(order, low, high) := (h^order)*create_list(sublis([y=(x+0*h)], diff(prod((if (m=j) then 1 else (y-(x+m*h))/((x+j*h)-(x+m*h))), m, low, high), y, order)), j, low, high);
  CENTER RULES
  for ptsHalf: 1 step 1 thru 7 do for ord: 1 step 1 thru 2*ptsHalf do display(fdif(ord, -ptsHalf, ptsHalf));
  RIGHT RULES
  for pts: 1 step 1 thru 10 do for ord: 1 step 1 thru pts do display(fdif(ord, 0, pts));
  LEFT RULES
  for pts: 1 step 1 thru 10 do for ord: 1 step 1 thru pts do display(fdif(ord, -pts, 0));"
  (documentation 'mjr_ndif_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndiff_central (f x &key (order 1) (h (sqrt single-float-epsilon)) (points nil))
  "Estimate the value of the ORDER'th derivative of F at X using a POINTS-point rule.

Notes:
 * No error checking is performed -- speed is essential for this function
 * ORDER may be a list, and the return will be a list of derivative estimates.  POINTS is never a list.
 * POINTS must be an odd integer with 3 =< points <= 15
 * ORDER, or each element of ORDER if ORDER is a list, must be strictly less than POINTS
 * A reasonable value for H is (SQRT MACHINE-EPS) --- usually.
 * For well behaved functions, the error is roughly O(H^(POINTS/2)) -- i.e. error gets better as POINTS gets bigger.
 * In all cases, F will be evaluated no more than POINTS times.
 * If POINTS is omitted or NIL, then the smallest value of POINTS for the given ORDER will be used.
 * The stencil is uniformly spaced: {...,-2h, -h, 0, h, 2h,...}.
 * The weights correspond to the Lagrange interpolation weights, and thus the approximation is equal to the derivative of the Lagrange interpolating
   polynomial on the stencil evaluated at X."
  (let* ((weights #(nil ;0
                    nil ;1
                    nil ;2
                    #(#(-1/2  0 1/2)
                      #( 1   -2 1))
                    nil ;4
                    #(#( 1/12 -2/3   0    2/3 -1/12)
                      #(-1/12  4/3  -5/2  4/3 -1/12)
                      #(-1/2   1     0   -1    1/2)
                      #( 1    -4     6   -4    1))
                    nil ;6
                    #(#(-1/60  3/20 -3/4   0     3/4 -3/20 1/60)
                      #( 1/90 -3/20  3/2 -49/18  3/2 -3/20 1/90)
                      #( 1/8  -1    13/8   0   -13/8  1   -1/8)
                      #(-1/6   2   -13/2  28/3 -13/2  2   -1/6)
                      #(-1/2   2    -5/2   0     5/2 -2    1/2)
                      #( 1    -6    15   -20    15   -6    1))
                    nil ;8
                    #(#( 1/280 -4/105    1/5    -4/5    0      4/5   -1/5    4/105 -1/280)
                      #(-1/560  8/315   -1/5     8/5 -205/72   8/5   -1/5    8/315 -1/560)
                      #(-7/240  3/10  -169/120  61/30   0    -61/30 169/120 -3/10   7/240)
                      #( 7/240 -2/5    169/60 -122/15  91/8 -122/15 169/60  -2/5    7/240)
                      #( 1/6   -3/2     13/3   -29/6    0     29/6  -13/3    3/2   -1/6)
                      #(-1/4    3      -13      29    -75/2   29    -13      3     -1/4)
                      #(-1/2    3       -7       7      0     -7      7     -3      1/2)
                      #( 1     -8       28     -56     70    -56     28     -8      1))
                    nil ;10
                    #(#( -1/1260     5/504     -5/84        5/21      -5/6       0          5/6     -5/21      5/84     -5/504     1/1260)
                      #(  1/3150    -5/1008     5/126      -5/21       5/3   -5269/1800     5/3     -5/21      5/126    -5/1008    1/3150)
                      #( 41/6048 -1261/15120  541/1120  -4369/2520  1669/720     0      -1669/720 4369/2520 -541/1120 1261/15120 -41/6048)
                      #(-41/7560  1261/15120 -541/840    4369/1260 -1669/180  1529/120  -1669/180 4369/1260 -541/840  1261/15120 -41/7560)
                      #(-13/288     19/36     -87/32       13/2     -323/48      0        323/48   -13/2      87/32    -19/36     13/288)
                      #( 13/240    -19/24      87/16      -39/2      323/8   -1023/20     323/8    -39/2      87/16    -19/24     13/240)
                      #(  5/24     -13/6       69/8       -17         63/4       0        -63/4     17       -69/8      13/6      -5/24)
                      #( -1/3       13/3      -23          68       -126       154       -126       68       -23        13/3      -1/3)
                      #( -1/2        4        -27/2        24        -21         0         21      -24        27/2      -4         1/2)
                      #(  1        -10         45        -120        210      -252        210     -120        45       -10         1))
                    nil ;12
                    #(#(1/5544 -1/385 1/56 -5/63 15/56 -6/7 0 6/7 -15/56 5/63 -1/56 1/385 -1/5544)
                      #(-1/16632 2/1925 -1/112 10/189 -15/56 12/7 -5369/1800 12/7 -15/56 10/189 -1/112 2/1925 -1/16632)
                      #(-479/302400 19/840 -643/4200 4969/7560 -4469/2240 1769/700 0 -1769/700 4469/2240 -4969/7560
                        643/4200 -19/840 479/302400)
                      #(479/453600 -19/1050 643/4200 -4969/5670 4469/1120 -1769/175 37037/2700 -1769/175 4469/1120
                        -4969/5670 643/4200 -19/1050 479/453600)
                      #(139/12096 -121/756 3125/3024 -3011/756 33853/4032 -1039/126 0 1039/126 -33853/4032 3011/756
                        -3125/3024 121/756 -139/12096)
                      #(-139/12096 121/630 -3125/2016 3011/378 -33853/1344 1039/21 -44473/720 1039/21 -33853/1344
                        3011/378 -3125/2016 121/630 -139/12096)
                      #(-31/480 41/48 -601/120 755/48 -885/32 971/40 0 -971/40 885/32 -755/48 601/120 -41/48 31/480)
                      #(31/360 -41/30 601/60 -755/18 885/8 -971/5 7007/30 -971/5 885/8 -755/18 601/60 -41/30 31/360)
                      #(1/4 -3 15 -41 261/4 -54 0 54 -261/4 41 -15 3 -1/4)
                      #(-5/12 6 -75/2 410/3 -1305/4 540 -637 540 -1305/4 410/3 -75/2 6 -5/12)
                      #(-1/2 5 -22 55 -165/2 66 0 -66 165/2 -55 22 -5 1/2)
                      #(1 -12 66 -220 495 -792 924 -792 495 -220 66 -12 1))
                    nil ;14
                    #(#(-1/24024 7/10296 -7/1320 7/264 -7/72 7/24 -7/8 0 7/8 -7/24 7/72 -7/264 7/1320 -7/10296 1/24024)
                      #(1/84084 -7/30888 7/3300 -7/528 7/108 -7/24 7/4 -266681/88200 7/4 -7/24 7/108 -7/528 7/3300 -7/30888
                        1/84084)
                      #(59/158400 -20137/3326400 2077/44352 -31957/138600 247081/302400 -222581/100800 90281/33600 0
                        -90281/33600 222581/100800 -247081/302400 31957/138600 -2077/44352 20137/3326400 -59/158400)
                      #(-59/277200 20137/4989600 -2077/55440 31957/138600 -247081/226800 222581/50400 -90281/8400 54613/3780
                        -90281/8400 222581/50400 -247081/226800 31957/138600 -2077/55440 20137/4989600 -59/277200)
                      #(-37/12960 2767/60480 -6271/18144 73811/45360 -157477/30240 1819681/181440 -286397/30240 0 286397/30240
                        -1819681/181440 157477/30240 -73811/45360 6271/18144 -2767/60480 37/12960)
                      #(37/15120 -2767/60480 6271/15120 -73811/30240 157477/15120 -1819681/60480 286397/5040 -353639/5040
                        286397/5040 -1819681/60480 157477/15120 -73811/30240 6271/15120 -2767/60480 37/15120)
                      #(311/17280 -101/360 6995/3456 -2363/270 135073/5760 -40987/1080 184297/5760 0 -184297/5760 40987/1080
                        -135073/5760 2363/270 -6995/3456 101/360 -311/17280)
                      #(-311/15120 101/270 -1399/432 2363/135 -135073/2160 40987/270 -184297/720 19162/63 -184297/720
                        40987/270 -135073/2160 2363/135 -1399/432 101/270 -311/15120)
                      #(-7/80 13/10 -139/16 166/5 -6283/80 1153/10 -7323/80 0 7323/80 -1153/10 6283/80
                        -166/5 139/16 -13/10 7/80)
                      #(1/8 -13/6 139/8 -83 6283/24 -1153/2 7323/8 -1066 7323/8 -1153/2 6283/24 -83 139/8 -13/6 1/8)
                      #(7/24 -4 575/24 -248/3 1441/8 -748/3 1529/8 0 -1529/8 748/3 -1441/8 248/3 -575/24 4 -7/24)
                      #(-1/2 8 -115/2 248 -1441/2 1496 -4587/2 2640 -4587/2 1496 -1441/2 248 -115/2 8 -1/2)
                      #(-1/2 6 -65/2 104 -429/2 286 -429/2 0 429/2 -286 429/2 -104 65/2 -6 1/2)
                      #(1 -14 91 -364 1001 -2002 3003 -3432 3003 -2002 1001 -364 91 -14 1)))))
    (if (listp order)
        (let ((points (or points (let ((max-order (reduce #'max order)))
                                   (if (oddp max-order) (+ 2 max-order) (+ 1 max-order)))))
              (fval   (loop for i from 1 upto (length (aref (aref weights points) 0))
                            for xi from (- x (* h (truncate points 2))) by h
                            collect (funcall f xi))))
          (loop for cur-order in order
                collect (if (= 0 cur-order)
                            (nth (truncate points 2) fval)
                            (/ (loop for wi across (aref (aref weights points) (1- cur-order))
                                     for fv in fval
                                     sum (* fv wi))
                               (expt h cur-order)))))
        (if (= order 0)
            (funcall f x)
            (let ((points (or points (if (oddp order) (+ 2 order) (+ 1 order)))))
              (/ (loop for i from 0
                       for xi from (- x (* h (truncate points 2))) by h
                       for wi across (aref (aref weights points) (1- order))
                       when (not (zerop wi))
                       sum (* (funcall f xi) wi))
                 (expt h order)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndiff_backward (f x &key (order 1) (h (sqrt single-float-epsilon)) (points nil))
  "Estimate the value of the ORDER'th derivative of F at X using a POINTS-point rule.

Notes:
 * No error checking is performed -- speed is essential for this function
 * ORDER may be a list, and the return will be a list of derivative estimates.  POINTS is never a list.
 * POINTS must be an integer such that 2 <= points <= 10.
 * ORDER, or each element of ORDER if ORDER is a list, must be strictly less than POINTS
 * Suggestion for h: A normally reasonable value is (sqrt machine-eps).
 * Error: For well behaved functions, the error is roughly O(h^(POINTS/2)) -- i.e. error gets better as POINTS gets bigger.
 * In all cases, F will be evaluated no more than ORDER times.
 * If POINTS is omitted or NIL, then the smallest value of POINTS for the given ORDER will be used.
 * The stencil is uniformly spaced: {...,-2h, -h, 0}.
 * The weights correspond to the Lagrange interpolation weights, and thus the approximation is equal to the derivative of the Lagrange interpolating
   polynomial on the stencil evaluated at X."
  (let* ((weights #(nil ;0
                    nil ;1
                    #(#(-1 1))
                    #(#(1/2 -2 3/2)
                      #(1 -2 1))
                    #(#(-1/3 3/2 -3 11/6)
                      #(-1 4 -5 2)
                      #(-1 3 -3 1))
                    #(#(1/4 -4/3 3 -4 25/12)
                      #(11/12 -14/3 19/2 -26/3 35/12)
                      #(3/2 -7 12 -9 5/2)
                      #(1 -4 6 -4 1))
                    #(#(-1/5 5/4 -10/3 5 -5 137/60)
                      #(-5/6 61/12 -13 107/6 -77/6 15/4)
                      #(-7/4 41/4 -49/2 59/2 -71/4 17/4)
                      #(-2 11 -24 26 -14 3)
                      #(-1 5 -10 10 -5 1))
                    #(#(1/6 -6/5 15/4 -20/3 15/2 -6 49/20)
                      #(137/180 -27/5 33/2 -254/9 117/4 -87/5 203/45)
                      #(15/8 -13 307/8 -62 461/8 -29 49/8)
                      #(17/6 -19 107/2 -242/3 137/2 -31 35/6)
                      #(5/2 -16 85/2 -60 95/2 -20 7/2)
                      #(1 -6 15 -20 15 -6 1))
                    #(#(-1/7 7/6 -21/5 35/4 -35/3 21/2 -7 363/140)
                      #(-7/10 1019/180 -201/10 41 -949/18 879/20 -223/10 469/90)
                      #(-29/15 1849/120 -268/5 2545/24 -389/3 3929/40 -638/15 967/120)
                      #(-7/2 82/3 -185/2 176 -1219/6 142 -111/2 28/3)
                      #(-25/6 95/3 -207/2 565/3 -1235/6 135 -295/6 23/3)
                      #(-3 22 -69 120 -125 78 -27 4)
                      #(-1 7 -21 35 -35 21 -7 1))
                    #(#(1/8 -8/7 14/3 -56/5 35/2 -56/3 14 -8 761/280)
                      #(363/560 -206/35 2143/90 -282/5 691/8 -4006/45 621/10 -962/35 29531/5040)
                      #(469/240 -527/30 561/8 -4891/30 1457/6 -2391/10 18353/120 -349/6 801/80)
                      #(967/240 -536/15 2803/20 -4772/15 10993/24 -2144/5 15289/60 -1316/15 1069/80)
                      #(35/6 -305/6 195 -2581/6 1790/3 -1065/2 895/3 -575/6 27/2)
                      #(23/4 -49 183 -391 1045/2 -447 239 -73 39/4)
                      #(7/2 -29 105 -217 280 -231 119 -35 9/2)
                      #(1 -8 28 -56 70 -56 28 -8 1))
                    #(#(-1/9 9/8 -36/7 14 -126/5 63/2 -28 18 -9 7129/2520)
                      #(-761/1260 3407/560 -967/35 6709/90 -265/2 6499/40 -6289/45 5869/70 -4609/140 6515/1008)
                      #(-29531/15120 5469/280 -12303/140 84307/360 -3273/8 19557/40 -72569/180 62511/280 -42417/560 4523/378)
                      #(-89/20 10579/240 -2939/15 10279/20 -5273/6 122249/120 -4013/5 24901/60 -7667/60 285/16)
                      #(-1069/144 3487/48 -3817/12 9823/12 -32773/24 36769/24 -13873/12 6787/12 -7807/48 3013/144)
                      #(-9 347/4 -373 939 -1525 3313/2 -1203 563 -154 75/4)
                      #(-91/12 287/4 -302 742 -2345/2 2471/2 -868 392 -413/4 145/12)
                      #(-4 37 -152 364 -560 574 -392 172 -44 5)
                      #(-1 9 -36 84 -126 126 -84 36 -9 1))
                    #(#(1/10 -10/9 45/8 -120/7 35 -252/5 105/2 -40 45/2 -10 7381/2520)
                      #(7129/12600 -263/42 3533/112 -2006/21 6961/36 -6877/25 6751/24 -13082/63 6121/56 -4861/126 177133/25200)
                      #(1303/672 -161353/7560 119601/1120 -22439/70 461789/720 -3591/4 71689/80 -400579/630 347769/1120 -79913/840 84095/6048)
                      #(4523/945 -197741/3780 435893/1680 -242639/315 273431/180 -62549/30 728587/360 -433739/315 264767/420 -663941/3780 341693/15120)
                      #(285/32 -6947/72 45449/96 -8321/6 129067/48 -43319/12 163313/48 -13349/6 92771/96 -6041/24 8591/288)
                      #(3013/240 -3229/24 10427/16 -3759/2 28603/8 -93773/20 34343/8 -5419/2 18047/16 -6709/24 7513/240)
                      #(105/8 -833/6 5299/8 -1877 13993/4 -4480 15967/4 -2443 7861/8 -469/2 605/24)
                      #(29/3 -302/3 472 -1312 2394 -2996 2604 -1552 607 -422/3 44/3)
                      #(9/2 -46 423/2 -576 1029 -1260 1071 -624 477/2 -54 11/2)
                      #(1 -10 45 -120 210 -252 210 -120 45 -10 1)))))
    (if (= order 0)
        (funcall f x)
        (let ((points (or points (1+ order))))
          (cond ((<= points order) (error "mjr_ndiff_backward: POINTS must be greater than ORDER!"))
                ((> 1 points)      (error "mjr_ndiff_backward: POINTS must be greater than 2!"))
                ((< 11 points)     (error "mjr_ndiff_backward: POINTS must be less than 16!")))
          (/ (loop for i from 0
                   for xi from (- x (* h (1- points))) by h
                   for wi across (aref (aref weights points) (1- order))
                   when (not (zerop wi))
                   sum (* (funcall f xi) wi))
             (expt h order))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndiff_forward (f x &key (order 1) (h (sqrt single-float-epsilon)) (points nil))
  "Estimate the value of the ORDER'th derivative of F at X using a POINTS-point rule.

Notes:
 * No error checking is performed -- speed is essential for this function
 * ORDER may be a list, and the return will be a list of derivative estimates.  POINTS is never a list.
 * POINTS must be an integer such that 2 <= points <= 10.
 * ORDER, or each element of ORDER if ORDER is a list, must be strictly less than POINTS
 * Suggestion for h: A normally reasonable value is (sqrt machine-eps).
 * Error: For well behaved functions, the error is roughly O(h^(POINTS/2)) -- i.e. error gets better as POINTS gets bigger.
 * In all cases, F will be evaluated no more than ORDER times.
 * If POINTS is omitted or NIL, then the smallest value of POINTS for the given ORDER will be used.
 * The stencil is uniformly spaced: {0, h, 2h,...}.
 * The weights correspond to the Lagrange interpolation weights, and thus the approximation is equal to the derivative of the Lagrange interpolating
   polynomial on the stencil evaluated at X."
  (let* ((weights #(nil ;0
                    nil ;1
                    #(#(-1 1))
                    #(#(-3/2 2 -1/2)
                      #(1 -2 1))
                    #(#(-11/6 3 -3/2 1/3)
                      #(2 -5 4 -1)
                      #(-1 3 -3 1))
                    #(#(-25/12 4 -3 4/3 -1/4)
                      #(35/12 -26/3 19/2 -14/3 11/12)
                      #(-5/2 9 -12 7 -3/2)
                      #(1 -4 6 -4 1))
                    #(#(-137/60 5 -5 10/3 -5/4 1/5)
                      #(15/4 -77/6 107/6 -13 61/12 -5/6)
                      #(-17/4 71/4 -59/2 49/2 -41/4 7/4)
                      #(3 -14 26 -24 11 -2)
                      #(-1 5 -10 10 -5 1))
                    #(#(-49/20 6 -15/2 20/3 -15/4 6/5 -1/6)
                      #(203/45 -87/5 117/4 -254/9 33/2 -27/5 137/180)
                      #(-49/8 29 -461/8 62 -307/8 13 -15/8)
                      #(35/6 -31 137/2 -242/3 107/2 -19 17/6)
                      #(-7/2 20 -95/2 60 -85/2 16 -5/2)
                      #(1 -6 15 -20 15 -6 1))
                    #(#(-363/140 7 -21/2 35/3 -35/4 21/5 -7/6 1/7)
                      #(469/90 -223/10 879/20 -949/18 41 -201/10 1019/180 -7/10)
                      #(-967/120 638/15 -3929/40 389/3 -2545/24 268/5 -1849/120 29/15)
                      #(28/3 -111/2 142 -1219/6 176 -185/2 82/3 -7/2)
                      #(-23/3 295/6 -135 1235/6 -565/3 207/2 -95/3 25/6)
                      #(4 -27 78 -125 120 -69 22 -3)
                      #(-1 7 -21 35 -35 21 -7 1))
                    #(#(-761/280 8 -14 56/3 -35/2 56/5 -14/3 8/7 -1/8)
                      #(29531/5040 -962/35 621/10 -4006/45 691/8 -282/5 2143/90 -206/35 363/560)
                      #(-801/80 349/6 -18353/120 2391/10 -1457/6 4891/30 -561/8 527/30 -469/240)
                      #(1069/80 -1316/15 15289/60 -2144/5 10993/24 -4772/15 2803/20 -536/15 967/240)
                      #(-27/2 575/6 -895/3 1065/2 -1790/3 2581/6 -195 305/6 -35/6)
                      #(39/4 -73 239 -447 1045/2 -391 183 -49 23/4)
                      #(-9/2 35 -119 231 -280 217 -105 29 -7/2)
                      #(1 -8 28 -56 70 -56 28 -8 1))
                    #(#(-7129/2520 9 -18 28 -63/2 126/5 -14 36/7 -9/8 1/9)
                      #(6515/1008 -4609/140 5869/70 -6289/45 6499/40 -265/2 6709/90 -967/35 3407/560 -761/1260)
                      #(-4523/378 42417/560 -62511/280 72569/180 -19557/40 3273/8 -84307/360 12303/140 -5469/280 29531/15120)
                      #(285/16 -7667/60 24901/60 -4013/5 122249/120 -5273/6 10279/20 -2939/15 10579/240 -89/20)
                      #(-3013/144 7807/48 -6787/12 13873/12 -36769/24 32773/24 -9823/12 3817/12 -3487/48 1069/144)
                      #(75/4 -154 563 -1203 3313/2 -1525 939 -373 347/4 -9)
                      #(-145/12 413/4 -392 868 -2471/2 2345/2 -742 302 -287/4 91/12)
                      #(5 -44 172 -392 574 -560 364 -152 37 -4)
                      #(-1 9 -36 84 -126 126 -84 36 -9 1))
                    #(#(-7381/2520 10 -45/2 40 -105/2 252/5 -35 120/7 -45/8 10/9 -1/10)
                      #(177133/25200 -4861/126 6121/56 -13082/63 6751/24 -6877/25 6961/36 -2006/21 3533/112 -263/42 7129/12600)
                      #(-84095/6048 79913/840 -347769/1120 400579/630 -71689/80 3591/4 -461789/720 22439/70 -119601/1120 161353/7560 -1303/672)
                      #(341693/15120 -663941/3780 264767/420 -433739/315 728587/360 -62549/30 273431/180 -242639/315 435893/1680 -197741/3780 4523/945)
                      #(-8591/288 6041/24 -92771/96 13349/6 -163313/48 43319/12 -129067/48 8321/6 -45449/96 6947/72 -285/32)
                      #(7513/240 -6709/24 18047/16 -5419/2 34343/8 -93773/20 28603/8 -3759/2 10427/16 -3229/24 3013/240)
                      #(-605/24 469/2 -7861/8 2443 -15967/4 4480 -13993/4 1877 -5299/8 833/6 -105/8)
                      #(44/3 -422/3 607 -1552 2604 -2996 2394 -1312 472 -302/3 29/3)
                      #(-11/2 54 -477/2 624 -1071 1260 -1029 576 -423/2 46 -9/2)
                      #(1 -10 45 -120 210 -252 210 -120 45 -10 1)))))
    (if (= order 0)
        (funcall f x)
        (let ((points (or points (1+ order))))
          (cond ((<= points order) (error "mjr_ndiff_backward: POINTS must be greater than ORDER!"))
                ((> 1 points)      (error "mjr_ndiff_backward: POINTS must be greater than 2!"))
                ((< 11 points)     (error "mjr_ndiff_backward: POINTS must be less than 16!")))
          (/ (loop for i from 0
                   for xi from x by h
                   for wi across (aref (aref weights points) (1- order))
                   when (not (zerop wi))
                   sum (* (funcall f xi) wi))
             (expt h order))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndiff_complex (f x &key (h (sqrt single-float-epsilon)))
  "Compute an approximation to the value of the derivative of F at X using complex numbers.

Reference:
   William Squire & George Trapp (1998); Using Complex Variables To Estimate Derivatives Of Real Functions; SIAM Review; Vol. 40; pp 110-112"
  (/ (imagpart (funcall f (+ x (complex 0 h)))) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_ndiff_lagrange (f x stencil &optional (order 1))
  "Approximate the value of the ORDER'th derivative of F at X.

Algorithm:  Compute the value of the ORDER'th derivative at X of the Lagrange interpolating polynomial on the stencil."
  (let* ((x-data (map 'vector (lambda (s) (+ x s)) stencil))
         (y-data (map 'vector f x-data)))
    (car (last (multiple-value-list (mjr_poly_eval-poly-and-first-n-derivatives (mjr_intrp_poly x-data y-data) x order))))))
