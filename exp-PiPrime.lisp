;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-PiPrime.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Find primes who's quotient approximates pi.  We want the smallest primes for any given accuracy.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1994,1997,1998,2004,2008,2010,2012,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @filedetails
;;
;;  Output on a relatively slow system running SBCL:
;;               2                    5     .6415926535897931
;;               2                    7     .3584073464102069
;;               5                   17     .2584073464102068
;;               7                   23     .14412163212449247
;;              13                   41     .012253500256360628
;;              53                  167     .009350742636621945
;;              67                  211     .007661077753490453
;;              71                  223     .0007475831672580924
;;             197                  619     .000539326105638338
;;             241                  757     .0005138154155193142
;;             311                  977     .00011355391133660575
;;             353                 1109     .00005040590029192771
;;            1427                 4483     .0000369423073824926
;;            1667                 5237     .000020967926925852254
;;            1723                 5413     .000020811297032352627
;;            3023                 9497     .00001144287196330751
;;            4591                14423     .000011298765136391609
;;            5113                16063     .000007189946291230598
;;            5749                18061     .0000028118781911778967
;;            9817                30841     .0000015361404703817527
;;           14563                45751     .0000009485839562728415
;;           15241                47881     .0000008945188660902659
;;           19309                60661     .0000006498609619320916
;;           43717               137341     .00000013809238952333658
;;           51853               162901     .00000007456832840091465
;;           56599               177811     .000000045946548343778204
;;           67447               211891     .00000000434903313362156
;;          199403               626443     .0000000004826130606261358
;;          265381               833719     .000000000008715250743307479
;;        12141887             38144863     .000000000006801670338063559
;;        12871487             40436969     .000000000001823874384854207
;;        14397343             45230587     .0000000000008637535131583718
;;        29723689             93379723     .0000000000004121147867408581
;;  Evaluation took:
;;      24.519 seconds of real time
;;      24.497276 seconds of total run time (24.432286 user, 0.064990 system)
;;      [ Run times consist of 0.666 seconds GC time, and 23.832 seconds non-GC time. ]
;;      99.91% CPU
;;      58,680,416,487 processor cycles
;;      1,281,338,528 bytes consed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (null mjr_prime::*mjr_prime_small-list*)
    (mjr_prime_init-small-prime-list))

(time (loop with bg = 100000
            for i from 0 upto (1- mjr_prime::*mjr_prime_small-count*)
            for ip = (mjr_prime_nth-small-prime i)
            for jb = (mjr_prime_small-prime-index (mjr_prime_prev (floor   (* pi ip))))
            for je = (mjr_prime_small-prime-index (mjr_prime_next (ceiling (* pi ip))))
            until (or (null jb) (null je))
            do (loop for j  from jb upto je
                     for jp  = (mjr_prime_nth-small-prime j)
                     for adf = (- pi (/ jp ip))
                     for df  = (abs adf)
                     do (if (> bg df)
                            (format 't "~20d ~20d     ~-30f~%" ip jp (setf bg df))))))

