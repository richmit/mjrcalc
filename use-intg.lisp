;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-intg.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Numerical integration of univariate real functions.@EOL
;; @std       Common Lisp
;; @see       tst-intg.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997,1998,2004,2008,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Integrate data instead of a function.@EOL@EOL
;; @todo      Arc length of f:R->R^n on an interval [a,b] via various base integration rules.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_INTG
  (:USE :COMMON-LISP
        :MJR_NUMU
        :MJR_VVEC
        :MJR_UTIL
        :MJR_PRNG
        :MJR_EPS
        :MJR_MXP)
  (:DOCUMENTATION "Brief: Numerical integration of univariate real functions.;")
  (:EXPORT #:mjr_intg_help
           #:mjr_intg_simple-rect-left          #:mjr_intg_simple-rect-right             ;; fixed-order     simple      Riemann rules
           #:mjr_intg_simple-rect-mid           #:mjr_intg_simple-milne                  ;; fixed-order     simple      Open NC rules
           #:mjr_intg_simple-trap               #:mjr_intg_simple-simpson                ;; fixed-order     simple      Closed NC rules
           #:mjr_intg_simple-simpson-3/8        #:mjr_intg_simple-boole                  ;; fixed-order     simple      Closed NC rules
           #:mjr_intg_simple-newton-cotes                                                ;; variable-order  simple      All NC rules
           #:mjr_intg_simple-gauss-legendre                                              ;; variable-order  simple      Gauss Legendre
           #:mjr_intg_simple-monte-carlo                                                 ;; variable-order  simple      Monte Carlo
           #:mjr_intg_simple-gauss-kronrod                                               ;; variable-order  simple      embedded GK
           #:mjr_intg_composite-trap            #:mjr_intg_composite-simpson             ;; variable-order  Composite   NC rules
           #:mjr_intg_glb-adp-composite-romberg #:mjr_intg_glb-adp-composite-trapezoidal ;; Global Adaptive Composite   Closed NC rules
           #:mjr_intg_loc-adp-dnc-trapezoidal   #:mjr_intg_loc-adp-dnc-simpson           ;; Local Adaptive  Div&Conq    Closed NC rules
           ;; Experimental
           #:mjr_intg_gbl-adp-factory-order                                              ;; Global Adaptive Order       various (adapter)
           #:mjr_intg_divide-and-conquer                                                 ;; Local Adaptive  Div&Conq    various (adapter)
           ;; Not exported (backend use)
           ;;#:mjr_intg_glb-adp-composite-trap-or-romberg
           ))

(in-package :MJR_INTG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_help ()
  "Help for MJR_INTG:

Single variable definite integrals computed over an interval

The functions:

   mjr_intg_simple-newton-cotes .............. Good for smooth functions well approximated by a polynomial of degree < order of method
   mjr_intg_simple-gauss-legendre ............ Fastest for smooth functions well approximated by a polynomial.
   mjr_intg_glb-adp-composite-romberg ........ Good choice for well behaved functions -- $C^\infty$
   mjr_intg_glb-adp-composite-trapezoidal .... Good choice for moderately ill-behaved functions.  For example if the derivative fails to exist at a few
                                               points, but the variation is generally consistent across interval of integration.
   mjr_intg_loc-adp-dnc-trapezoidal .......... Good choice for functions with a widely changing second derivative or strong variation that break
                                               mjr_intg_glb-adp-composite-trapezoidal.  Also good when a particular mesh must be refined.
   mjr_intg_loc-adp-dnc-simpson .............. Like mjr_intg_loc-adp-dnc-trapezoidal, but uses simpson's rule.
   mjr_intg_composite-trap ................... Fixed step size composite trapezoidal rule.
   mjr_intg_composite-simpson ................ Fixed step size composite simpson rule.

This is a pathological example

   CL> (mjr_intrp_poly #(0 1/4 1/2 3/4 1) #(0 1/2 1/2 0 1))
   #(64/3 -32 32/3 1 0)

   CL> (float (mjr_poly_integrate #(64/3 -32 32/3 1 0) 0 1))
   0.32222223

   CL> (float (mjr_intg_glb-adp-composite-trapezoidal  (lambda (x) (mjr_poly_eval #(64/3 -32 32/3 1 0) x)) :start 0 :end 1 :good-err-stop 1))
   0.5

   CL> (float (mjr_intg_glb-adp-composite-trapezoidal  (lambda (x) (mjr_poly_eval #(64/3 -32 32/3 1 0) x)) :start 0 :end 1 :good-err-stop 2))
   0.3222256

   CL> (float (mjr_intg_loc-adp-dnc-trapezoidal (lambda (x) (mjr_poly_eval #(64/3 -32 32/3 1 0) x)) :start 0 :end 1 :len 2))
   0.5

   CL> (float (mjr_intg_loc-adp-dnc-trapezoidal (lambda (x) (mjr_poly_eval #(64/3 -32 32/3 1 0) x)) :start 0 :end 1 :len 4))
   0.3222203"
  (documentation 'mjr_intg_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-monte-carlo (fun &key start end (order 10000) show-progress)
  "Compute the definite integral of FUN on the given interval using the Monte Carlo method."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (let ((wid  (- end start))
          (fsum 0)
          (spi  (if (> order 30) (truncate order 30) 1)))
      (loop for ect from 1 upto order
            for x = (mjr_prng_float-cc start end)
            finally (return (* wid fsum (/ order)))
            do (incf fsum (funcall fun x))
            do (if (and show-progress
                        (zerop (mod ect spi)))
                   (format 't "~7d :: ~15f ~%" ect (* wid fsum (/ ect))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_composite-trap (fun &key start end step len)
  "Compute the definite integral of FUN using the composite Trapezoidal Rule.
   START, END, STEP, LEN are processed by MJR_VVEC_NORMALIZE-KW-VVT-ASEQ"
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (multiple-value-bind (start end step len)
        (mjr_util_get-kwarg-vals '(:start :end :step :len)
                                 (mjr_vvec_normalize-kw-vvt-aseq (mjr_util_strip-nil-val-kwarg (list :start start :end end :step step :len len))))
      (cond ((< len 2)     (error "mjr_intg_trap: :LEN must be greater than 1!"))
            ((> start end) (error "mjr_intg_trap: :START must be numerically less than :END!")))
      (* 1/2 (* step (+ (funcall fun start)
                        (funcall fun end)
                        (* 2 (mjr_numu_sum :seq-fun fun :start (+ start step) :end (- end step) :step step :len (- len 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_composite-simpson (fun &key start end step len)
  "Compute the definite integral of FUN using the composite Simpson Rule.
   START, END, STEP, LEN are processed by MJR_VVEC_NORMALIZE-ASEQ"
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (multiple-value-bind (start end step len)
        (mjr_util_get-kwarg-vals '(:start :end :step :len)
                                 (mjr_vvec_normalize-kw-vvt-aseq (mjr_util_strip-nil-val-kwarg (list :start start :end end :step step :len len))))
      (cond ((< len 2)     (error "mjr_intg_simpson: :LEN must be greater than 5!"))
            ((evenp len)   (error "mjr_intg_simpson: :LEN must be odd!"))
            ((> start end) (error "mjr_intg_simpson: :START must be numerically less than :END!")))
      (* 1/3 (* step (+ (funcall fun start)
                        (funcall fun end)
                        (loop for x = (+ start step) then (+ x step)
                              for i from 1 upto (- len 2)
                              sum (* (if (oddp i) 4 2) (funcall fun x)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_loc-adp-dnc-simpson (fun &key points start end step len seq-next min-width (the-err 1e-5) max-evals show-progress)
  "Compute the definite integral of FUN between A and B using an adaptive Simpson's rule.

Each sub-interval of the given partition is recursively bisected until the error estimate on each interval is below THE-ERR, the number of function
evaluations grows beyond MAX-EVALS, or the intervals become smaller than MIN-WIDTH.

If only :START and :END are given of the partition, then :LEN will be set to 2.
If provided, :POINTS must be a vector or a non-NIL list.

If the error goal is satisfied, then the return is the integral approximation and the number of function evaluations. If any of the limits are violated on any
part of the interval, then the return will be nil, the integral approximation, the number of function evaluations required, the total length of the
sub-intervals upon which no limits were violated during the computation, and the number of intervals that had a violated limit.

References:
  Kythe & Schaferkotter (2005); Handbook of Computational Methods for Integration; pp 89-94"
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (flet ((simpson (a b fa fm fb) (* (/ (- b a) 6) (+ fa (* 4 fm) fb))))
      (let ((ect   0)
            (len   (if (and start end (not (or points step len seq-next)))
                       2
                       len)))
        (let ((ilist (loop for x0 = nil then x1
                           for x1 in (mjr_vvec_to-list (mjr_util_strip-nil-val-kwarg
                                                        (list :points points :start start :end end :step step :len len :map-fun seq-next)))
                           for f0 = nil then f1
                           for f1 = (funcall fun x1)
                           do (incf ect)
                           when x0
                           collect (let* ((m (/ (+ x0 x1) 2))
                                          (fm (funcall fun m)))
                                     (incf ect)
                                     (list x0 m x1 f0 fm f1 (simpson x0 f1 f0 fm f1) (/ (* the-err (- x1 x0)) (- end start))))))
              (badt  0)
              (cerw  0)
              (civ   0))
          (loop for (a m b fa fm fb vo eps) = (pop ilist)
                for iwid = (and a (- b a))
                while iwid
                finally (return (if (zerop badt)
                                    (values civ ect)
                                    (values nil civ ect cerw badt)))
                do (if (and min-width (< iwid min-width))
                       (progn (if show-progress (format 't "~4d :: ~15f TLW ~15f ~%" ect civ iwid))
                              (incf badt)
                              (incf civ vo))
                       (if (and max-evals (> ect max-evals))
                           (progn (if show-progress (format 't "~4d :: ~15f TLF ~15f~%" ect civ iwid))
                                  (incf badt)
                                  (incf civ vo))
                           (let* ((ml   (/ (+ a m) 2))
                                  (mr   (/ (+ m b) 2))
                                  (fml  (funcall fun ml))
                                  (fmr  (funcall fun mr))
                                  (eps2 (/ eps 2))
                                  (vl   (simpson a m fa fml fm))
                                  (vr   (simpson m b fm fmr fb))
                                  (err  (abs (- vo vl vr))))
                             (incf ect 2)
                             (if (< err eps2)
                                 (progn (if show-progress (format 't "~4d :: ~15f TER ~15f ~%" ect civ iwid))
                                        (incf cerw iwid)
                                        (incf civ (+ vl vr)))
                                 (progn (if show-progress (format 't "~4d :: ~15f CER ~15f ~15f ~%" ect civ iwid err))
                                        (setq ilist (append ilist (list (list a ml m fa fml fm vl eps2)
                                                                        (list m mr b fm fmr fb vr eps2)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_glb-adp-composite-trap-or-romberg (fun &key start end max-evals min-width the-err good-err-stop show-progress do-romberg)
  "Back end function to do adaptive trapezoidal or adaptive Romberg. See: MJR_INTG_TRAP-ADP and/or MJR_INTG_ROMBERG-ADP"
  (cond ((not (numberp start))                      (error "mjr_intg_glb-adp-composite-trap-or-romberg: Lower limit (START) must be a number!"))
        ((complexp start)                           (error "mjr_intg_glb-adp-composite-trap-or-romberg: Lower limit (START) must be a real number!"))
        ((not (numberp end))                        (error "mjr_intg_glb-adp-composite-trap-or-romberg: Upper limit (END) must be a number!"))
        ((complexp end)                             (error "mjr_intg_glb-adp-composite-trap-or-romberg: Upper limit (END) must be a real number!"))
        ((> start end)                              (error "mjr_intg_glb-adp-composite-trap-or-romberg: Lower limit (START) must be numerically less than upper limit (END)!"))
        ((and max-evals (not (integerp max-evals))) (error "mjr_intg_glb-adp-composite-trap-or-romberg: MAX-EVALS must be an integer!"))
        ((and max-evals (< max-evals 3))            (error "mjr_intg_glb-adp-composite-trap-or-romberg: MAX-EVALS must be greater than 2!"))
        ((not (integerp good-err-stop))             (error "mjr_intg_glb-adp-composite-trap-or-romberg: GOOD-ERR-STOP must be an integer!"))
        ((< good-err-stop 1)                        (error "mjr_intg_glb-adp-composite-trap-or-romberg: GOOD-ERR-STOP must be 1 or greater!"))
        ((not (numberp the-err))                    (error "mjr_intg_glb-adp-composite-trap-or-romberg: THE-ERR must be a number!"))
        ((complexp the-err)                         (error "mjr_intg_glb-adp-composite-trap-or-romberg: THE-ERR must be a real number!"))
        ((<= the-err 0)                             (error "mjr_intg_glb-adp-composite-trap-or-romberg: THE-ERR must be a positive number!")))
  (loop with good-c  = 0
        with lim-del = (- end start)
        for n from 2
        for np = 1 then (* np 2)
        for ect = 2 then (+ ect np)
        for hn = (/ lim-del 2 np)
        for old-trap = (/ (* lim-del (+ (funcall fun start) (funcall fun end))) 2) then new-trap
        for new-trap = (+ (/ old-trap 2)
                          (* hn (loop for j from 1 upto np
                                      sum (funcall fun (+ start (* hn (- (* 2 j) 1)))))))
        for old-romb = old-trap then new-romb
        for rom-list = (list old-trap) then (if do-romberg
                                                (loop for m = 1 then (* 4 m)
                                                      for j from 2 upto n
                                                      for or in (append (list nil) rom-list)
                                                      for r = old-trap then (+ r (/ (- r or) (- m 1)))
                                                      collect r))
        for new-romb = (car (last rom-list))
        for new-int = (if do-romberg new-romb new-trap)
        for old-int = (if do-romberg old-romb old-trap)
        ;;do (format 't "~a~%" rom-list)
        do (if show-progress (format 't "~4d :: ~15f ~%" ect new-int))
        do (if (< (mjr_numu_absdif old-int new-int) the-err) (incf good-c) (setq good-c 0))
        do (if (and (< 4 ect) (>= good-c good-err-stop)) (return-from mjr_intg_glb-adp-composite-trap-or-romberg (values     new-int ect)))
        do (if (and min-width (> min-width hn))          (return-from mjr_intg_glb-adp-composite-trap-or-romberg (values nil new-int ect)))
        do (if (and max-evals (> ect max-evals))         (return-from mjr_intg_glb-adp-composite-trap-or-romberg (values nil new-int ect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_glb-adp-composite-romberg (fun &key start end max-evals min-width (the-err 1e-5) (good-err-stop 2) show-progress)
  "Compute the definite integral of FUN between A and B using an adaptive Romberg scheme.

The number of steps starts at 2 and is doubled (roughly) until it grows beyond MAX-N or we see GOOD-ERR-STOP consecutive iterations with an integral
approximation that changes by less than THE-ERR.  If the error goal is satisfied, then the return is the integral approximation and the number of function
evaluations. If MAX-N is violated, then nil, the integral approximation, and the number of function evaluations returned.  min-width sets the smallest
intervals that will be used, and if it is violated then nil, the integral approximation, and the number of function evaluations are returned."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (mjr_intg_glb-adp-composite-trap-or-romberg fun
                                                :start start :end end
                                                :max-evals max-evals :min-width min-width
                                                :the-err the-err :good-err-stop good-err-stop
                                                :show-progress show-progress
                                                :do-romberg 't)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_glb-adp-composite-trapezoidal (fun &key start end max-evals min-width (the-err 1e-5) (good-err-stop 2) show-progress)
  "Compute the definite integral of FUN between A and B using adaptive the trapezoidal rule.

The number of steps starts at 2 and is doubled (roughly) until it grows beyond MAX-N or we see GOOD-ERR-STOP consecutive iterations with an integral
approximation that changes by less than THE-ERR.  If the error goal is satisfied, then the return is the integral approximation and the number of function
evaluations. If MAX-N is violated, then nil, the integral approximation, and the number of function evaluations returned.  min-width sets the smallest
intervals that will be used, and if it is violated then nil, the integral approximation, and the number of function evaluations are returned."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (mjr_intg_glb-adp-composite-trap-or-romberg fun
                                                :start start :end end
                                                :max-evals max-evals :min-width min-width
                                                :the-err the-err :good-err-stop good-err-stop
                                                :show-progress show-progress
                                                :do-romberg nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_divide-and-conquer (fun simple-integration-func vvec &rest int-args)
  "Compute the definite integral of FUN on the given numeric sequence using the given interval integration rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (mjr_vvec_map-sum vvec :pair-fun (lambda (x0 x1 fx0 fx1 i)
                                       (declare (ignore fx0 fx1 i))
                                       (apply simple-integration-func fun :start x0 :end x1 int-args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_loc-adp-dnc-trapezoidal (fun vvec &key min-width max-evals (good-err-stop 2) (the-err 1e-5) suppress-warnings show-progress)
  "Compute the definite integral of FUN between A and B using a doubly adaptive trapezoidal rule.

The initial intervals specified by the virtual vector passed in vvec are recursively bisected until intervals are obtained that are the result
of :GOOD-ERR-STOP consecutive iterations with an integral approximation that changes by less than :THE-ERR scaled for the interval's size.  If the error goal
is satisfied, then the return is the integral approximation and the number of times the function was evaluated. If :MAX-EVALS is violated, the return is NIL
and the number of times the function was evaluated. If an interval with an unsatisfactory integral approximation is less than :MIN-WIDTH, it will not be
subdivided -- the unsatisfactory approximation will be used and a warning will be printed unless :SUPPRESS-WARNINGS is non-NIL."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (cond ((and max-evals (not (integerp max-evals))) (error "mjr_intg_loc-adp-dnc-trapezoidal: MAX-EVALS must be an integer!"))
          ((and max-evals (< max-evals 3))            (error "mjr_intg_loc-adp-dnc-trapezoidal: MAX-EVALS must be greater than 2!"))
          ((not (integerp good-err-stop))             (error "mjr_intg_loc-adp-dnc-trapezoidal: GOOD-ERR-STOP must be an integer!"))
          ((< good-err-stop 1)                        (error "mjr_intg_loc-adp-dnc-trapezoidal: GOOD-ERR-STOP must be 1 or greater!"))
          ((and min-width (not (numberp min-width)))  (error "mjr_intg_loc-adp-dnc-trapezoidal: MIN-WIDTH must be a number!"))
          ((and min-width (complexp min-width))       (error "mjr_intg_loc-adp-dnc-trapezoidal: MIN-WIDTH must be a real number!"))
          ((and min-width (<= min-width 0))           (error "mjr_intg_loc-adp-dnc-trapezoidal: MIN-WIDTH must be a positive number!"))
          ((not (numberp the-err))                    (error "mjr_intg_loc-adp-dnc-trapezoidal: THE-ERR must be a number!"))
          ((complexp the-err)                         (error "mjr_intg_loc-adp-dnc-trapezoidal: THE-ERR must be a real number!"))
          ((<= the-err 0)                             (error "mjr_intg_loc-adp-dnc-trapezoidal: THE-ERR must be a positive number!")))
    (let* ((points (mjr_vvec_to-vec vvec))
           (len    (length points)))
      (cond ((< len 2)                     (error "mjr_intg_loc-adp-dnc-trapezoidal: :LEN must be greater than 1!")))
      (let* ((intervals (mjr_vvec_map-filter-reduce 'list points
                                                    :point-fun (lambda (x i) (declare (ignore i)) (funcall fun x))
                                                    :pair-fun  (lambda (x0 x1 fx0 fx1 i) (declare (ignore i)) (list x0 x1 fx0 fx1 0))))
             (int-width (- (aref points (1- len)) (aref points 0))))
        ;; Refine intervals till we get a good value.
        (loop with intv = 0
              for ect from (1+ len) ;; func evaluation count
              for (cur-x0 cur-x1 cur-fx0 cur-fx1 cur-gct) = (pop intervals)
              for cur-sum = (+ cur-fx1 cur-fx0)
              for cur-wid = (- cur-x1 cur-x0)
              for cur-itg = (/ (* cur-sum cur-wid) 2)
              for new-x   = (/ (+ cur-x0 cur-x1) 2)
              for new-mfx = (funcall fun new-x)
              for new-itg = (/ (* (+ cur-sum (* 2 new-mfx)) cur-wid) 4)
              do (if show-progress (format 't "~4d :: [~15f,~15f] :: ~15f :: ~15f ~%" ect cur-x0 cur-x1 cur-itg new-itg))
              do (if (< (mjr_numu_absdif cur-itg new-itg) (/ (* the-err cur-wid) int-width))
                     (if (>= (1+ cur-gct) good-err-stop)
                         (incf intv new-itg)
                         (push (list cur-x0 cur-x1 cur-fx0 cur-fx1 (1+ cur-gct)) intervals))
                     (if (and min-width (> min-width (- new-x cur-x0)))
                         (progn (incf intv new-itg)
                                (or suppress-warnings (warn "mjr_intg_loc-adp-dnc-trapezoidal: Violated MIN-WIDTH constraint!")))
                         (progn (push (list new-x  cur-x1 new-mfx cur-fx1 0) intervals)
                                (push (list cur-x0 new-x  cur-fx0 new-mfx 0) intervals))))
              do (if (null intervals)                  (return-from mjr_intg_loc-adp-dnc-trapezoidal (values intv ect)))
              do (if (and max-evals (> ect max-evals)) (return-from mjr_intg_loc-adp-dnc-trapezoidal (values nil  ect))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-trap (fun &key start end)
  "Compute the definite integral of FUN between A and B using the Trapezoidal rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (* (- end start)
       (/ (+ (funcall fun start) (funcall fun end)) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-rect-left (fun &key start end)
  "Compute the definite integral of FUN between A and B using the Left Rectangle rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (* (- end start) (funcall fun start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-rect-right (fun &key start end)
  "Compute the definite integral of FUN between A and B using the Right Rectangle rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (* (- end start) (funcall fun end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-rect-mid (fun &key start end)
  "Compute the definite integral of FUN between A and B using the Midpoint Rectangle rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
  (* (- end start) (funcall fun (/ (+ start end) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-simpson (fun &key start end)
  "Compute the definite integral of FUN between A and B using Simpson's rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
  (* (/ (- end start) 6) (+ (funcall fun start)
                            (* 4 (funcall fun (/ (+ end start) 2)))
                            (funcall fun end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-simpson-3/8 (fun &key start end)
  "Compute the definite integral of FUN between A and B using Simpson's 3/8 rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (* (/ (- end start) 8) (+ (funcall fun start)
                              (* 3 (funcall fun (/ (+ end (* 2 start)) 3)))
                              (* 3 (funcall fun (/ (+ (* 2 end) start) 3)))
                              (funcall fun end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-boole (fun &key start end)
  "Compute the definite integral of FUN between A and B using Boole's rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x"))
        (h   (/ (- end start) 4)))
    (* (/ (- end start) 90) (+ (* 7  (funcall fun start))
                               (* 32 (funcall fun (+ start h)))
                               (* 12 (funcall fun (+ start (* 2 h))))
                               (* 32 (funcall fun (+ start (* 3 h))))
                               (* 7  (funcall fun end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-milne (fun &key start end)
  "Compute the definite integral of FUN between A and B using Milne's rule."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x"))
        (h   (/ (- end start) 4)))
    (* (/ (- end start) 3) (+ (* 2 (funcall fun (+ start h)))
                              (- (funcall fun (+ start (* 2 h))))
                              (* 2 (funcall fun (+ start (* 3 h))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-newton-cotes (fun &key start end (order 3) (closed 't))
  "Newton Cotes Integration (both open and closed)

When closed is non-NIL, the order must be in [1,20].  When closed is NIL, the order must be in [2,20].

        |-------+-------------+-----------------------------+-------------|
        | order | open/closed | LISP function               | Name        |
        |-------+-------------+-----------------------------+-------------|
        |     1 | closed      | mjr_intg_simple-trap        | Trapezoidal |
        |     2 | closed      | mjr_intg_simple-simpson     | Simpson     |
        |     3 | closed      | mjr_intg_simple-simpson-3/8 | Simpson 3/8 |
        |     4 | closed      | mjr_intg_simple-boole       | Boole       |
        |-------+-------------+-----------------------------+-------------|
        |     2 | open        | mjr_intg_simple-mid         | Midpoint    |
        |     4 | open        | mjr_intg_simple-milne       | Milne       |
        |-------+-------------+-----------------------------+-------------|"

;;  Accuracy generally improves as order increases, but only up to a point.  For example, we can integrate 1/x from 1 to 2 with various orders to illustrate
;;  the point:
;; ----------------------------------------------------------------------------------------------------
;;    (loop for o from 1 upto 20
;;          for ic = (if (> o 0) (mjr_intg_simple-newton-cotes (lambda (x) (/ (1+ x))) :start 0d0 :end 1d0 :order o :closed 't))
;;          for io = (if (> o 1) (mjr_intg_simple-newton-cotes (lambda (x) (/ (1+ x))) :start 0d0 :end 1d0 :order o :closed nil))
;;          do (if io
;;                 (format 't "~5d : ~15,10f ~15,10f ~15,10f ~15,10f~%" o ic (abs (- ic (log 2.0d0))) io  (abs (- io (log 2.0d0))))
;;                 (format 't "~5d : ~15,10f ~15,10f ~15a ~15a~%"       o ic (abs (- ic (log 2.0d0))) "-" "-")))
;; ----------------------------------------------------------------------------------------------------
;;      1 :    0.7500000000    0.0568528194               -               -
;;      2 :    0.6944444444    0.0012972639    0.6666666667    0.0264805139
;;      3 :    0.6937500000    0.0006028194    0.6750000000    0.0181471806
;;      4 :    0.6931746032    0.0000274226    0.6920634921    0.0010836885
;;      5 :    0.6931630291    0.0000158485    0.6923776455    0.0007695351
;;      6 :    0.6931480623    0.0000008817    0.6930952381    0.0000519425
;;      7 :    0.6931477333    0.0000005528    0.6931097433    0.0000374373
;;      8 :    0.6931472145    0.0000000340    0.6931445039    0.0000026766
;;      9 :    0.6931472028    0.0000000222    0.6931452353    0.0000019452
;;     10 :    0.6931471820    0.0000000015    0.6931470368    0.0000001438
;;     11 :    0.6931471815    0.0000000010    0.6931470755    0.0000001051
;;     12 :    0.6931471806    0.0000000001    0.6931471726    0.0000000079
;;     13 :    0.6931471806    0.0000000000    0.6931471747    0.0000000058
;;     14 :    0.6931471806    0.0000000000    0.6931471801    0.0000000004
;;     15 :    0.6931471806    0.0000000000    0.6931471802    0.0000000003
;;     16 :    0.6931471806    0.0000000000    0.6931471805    0.0000000000
;;     17 :    0.6931471806    0.0000000000    0.6931471805    0.0000000000
;;     18 :    0.6931471806    0.0000000000    0.6931471806    0.0000000000
;;     19 :    0.6931471806    0.0000000000    0.6931471806    0.0000000000
;;     20 :    0.6931471806    0.0000000000    0.6931471806    0.0000000000
;; ----------------------------------------------------------------------------------------------------
;;  The closed weights have the following formula:
;;      $$\\frac{-1^{n-r}}{r!(n-r)!}\\int_0^n\\left(\\prod_{k=0}^{r-1}(t-k)\\right)\\left(\\prod_{k=r+1}^n(t-k)\\right) dx$$
;; ----------------------------------------------------------------------------------------------------
;;  The closed weights were computed like so:
;;     (loop for n from 1 upto 20
;;           collect (loop for r from 0 upto n
;;                         collect (* (if (evenp (- n r)) 1 -1)
;;                                    (/ (* (mjr_combe_! r)
;;                                          (mjr_combe_! (- n r))))
;;                                    (mjr_poly_intg (mjr_poly_make-from-roots (loop for i from 0 upto n
;;                                                                                   when (not (= i r))
;;                                                                                   collect i))
;;                                                   0 n))))
;; ----------------------------------------------------------------------------------------------------
;;  The open weights have the following formula:
;;      $$\\frac{-1^{n-r}}{(r-1)!(1+n-r)!}\\int_0^n\\left(\\prod_{k=0}^{r-1}(t-k)\\right)\\left(\\prod_{k=r+1}^n(t-k)\\right) dx$$
;; ----------------------------------------------------------------------------------------------------
;;  The closed weights were computed like so:
;;     (loop for n from 3 upto 20
;;           collect (loop for r from 1 upto (1- n)
;;                         collect (* (if (evenp (1+ (- n r))) 1 -1)
;;                                    (/ (* (mjr_combe_! (1- r))
;;                                          (mjr_combe_! (- n (1+ r)))))
;;                                    (mjr_poly_intg (mjr_poly_make-from-roots (loop for i from 1 upto (1- n)
;;                                                                                   when (not (= i r))
;;                                                                                   collect i))
;;                                                   0 n))))
;; ----------------------------------------------------------------------------------------------------
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
    (if closed
        (let ((wts #(#(1/2 1/2)
                     #(1/3 4/3 1/3)
                     #(3/8 9/8 9/8 3/8)
                     #(14/45 64/45 8/15 64/45 14/45)
                     #(95/288 125/96 125/144 125/144 125/96 95/288)
                     #(41/140 54/35 27/140 68/35 27/140 54/35 41/140)
                     #(5257/17280 25039/17280 343/640 20923/17280 20923/17280 343/640 25039/17280 5257/17280)
                     #(3956/14175 23552/14175 -3712/14175 41984/14175 -3632/2835 41984/14175 -3712/14175 23552/14175 3956/14175)
                     #(25713/89600 141669/89600 243/2240 10881/5600 26001/44800 26001/44800 10881/5600 243/2240 141669/89600 25713/89600)
                     #(80335/299376 132875/74844 -80875/99792 28375/6237 -24125/5544 89035/12474 -24125/5544 28375/6237 -80875/99792 132875/74844
                       80335/299376)
                     #(4777223/17418240 49450643/29030400 -35608243/87091200 6166523/1935360 -17591827/14515200 28404871/14515200 28404871/14515200
                       -17591827/14515200 6166523/1935360 -35608243/87091200 49450643/29030400 4777223/17418240)
                     #(1364651/5255250 150048/79625 -1264644/875875 3572512/525525 -3432753/350350 14586048/875875 -2090408/125125 14586048/875875
                       -3432753/350350 3572512/525525 -1264644/875875 150048/79625 1364651/5255250)
                     #(106364763817/402361344000 731649485593/402361344000 -22582626859/22353408000 144926245243/28740096000 -78862978129/16094453760
                       298542743759/44706816000 -46704658663/33530112000 -46704658663/33530112000 298542743759/44706816000 -78862978129/16094453760
                       144926245243/28740096000 -22582626859/22353408000 731649485593/402361344000 106364763817/402361344000)
                     #(631693279/2501928000 311056753/156370500 -5395044599/2501928000 765940609/78185250 -46375653541/2501928000 5525678207/156370500
                       -39205297537/833976000 712193069/13030875 -39205297537/833976000 5525678207/156370500 -46375653541/2501928000 765940609/78185250
                       -5395044599/2501928000 311056753/156370500 631693279/2501928000)
                     #(25221445/98402304 442589775/229605376 -388226775/229605376 1746295975/229605376 -372104925/32800768 4103141115/229605376
                       -10001664025/688816128 1698012675/229605376 1698012675/229605376 -10001664025/688816128 4103141115/229605376 -372104925/32800768
                       1746295975/229605376 -388226775/229605376 442589775/229605376 25221445/98402304)
                     #(120348894184/488462349375 1021012852736/488462349375 -31952201728/10854718875 1331538968576/97692469875 -280654342912/8881133625
                       3713412349952/54273594375 -54452275263488/488462349375 14990200029184/97692469875 -606473420576/3618239625
                       14990200029184/97692469875 -54452275263488/488462349375 3713412349952/54273594375 -280654342912/8881133625
                       1331538968576/97692469875 -31952201728/10854718875 1021012852736/488462349375 120348894184/488462349375)
                     #(85455477715379/342372925440000 170070083613041/83691159552000 -2303599607287621/941525544960000 294905863842803/26900729856000
                       -37533141482453/1743565824000 2688251482636513/67251824640000 -9269566080827161/188305108992000 4755096326451617/104613949440000
                       -6391636155891919/376610217984000 -6391636155891919/376610217984000 4755096326451617/104613949440000
                       -9269566080827161/188305108992000 2688251482636513/67251824640000 -37533141482453/1743565824000 294905863842803/26900729856000
                       -2303599607287621/941525544960000 170070083613041/83691159552000 85455477715379/342372925440000)
                     #(611197056507/2534852320000 55461906657/25348523200 -1927646624637/506970464000 1455921591759/79214135000
                       -1588823650719/31685654000 2777157949977/22632610000 -2128376034297/9053044000 5981920958997/15842827000
                       -628691926671297/1267426160000 34599550825551/63371308000 -628691926671297/1267426160000 5981920958997/15842827000
                       -2128376034297/9053044000 2777157949977/22632610000 -1588823650719/31685654000 1455921591759/79214135000
                       -1927646624637/506970464000 55461906657/25348523200 611197056507/2534852320000)
                     #(1311546499957236437/5377993912811520000 18351023301032567/8604790260498432 -10034170825244579/3064383995904000
                       495333631700360617/32593902501888000 -5459170901406129527/149388719800320000 1309851809386170571/16598746644480000
                       -76111777777911661/609749876736000 13908243317345626267/89633231880192000 -2609922351726730283/19918495973376000
                       143909204406256715953/2688996956405760000 143909204406256715953/2688996956405760000 -2609922351726730283/19918495973376000
                       13908243317345626267/89633231880192000 -76111777777911661/609749876736000 1309851809386170571/16598746644480000
                       -5459170901406129527/149388719800320000 495333631700360617/32593902501888000 -10034170825244579/3064383995904000
                       18351023301032567/8604790260498432 1311546499957236437/5377993912811520000)
                     #(1145302367137/4842604238472 3355823042500/1470076286679 -97339548544375/20581068013506 82748714972500/3430178002251
                       -2069649611963125/27441424018008 101305879622128/490025428893 -1557905611303750/3430178002251 2869553648930000/3430178002251
                       -2511881305088125/1960101715572 17040565224805000/10290534006753 -1684005984173647/935503091523 17040565224805000/10290534006753
                       -2511881305088125/1960101715572 2869553648930000/3430178002251 -1557905611303750/3430178002251 101305879622128/490025428893
                       -2069649611963125/27441424018008 82748714972500/3430178002251 -97339548544375/20581068013506 3355823042500/1470076286679
                       1145302367137/4842604238472)))
              (h   (/ (- end start) order)))
          (* h (loop for n from 0 upto order
                     for w across (aref wts (1- order))
                     for x = start then (+ x h)
                     sum (* w (funcall fun x)))))
        (let ((wts #(#(2)
                     #(3/2 3/2)
                     #(8/3 -4/3 8/3)
                     #(55/24 5/24 5/24 55/24)
                     #(33/10 -21/5 39/5 -21/5 33/10)
                     #(4277/1440 -1057/480 1967/720 1967/720 -1057/480 4277/1440)
                     #(736/189 -848/105 1952/105 -19672/945 1952/105 -848/105 736/189)
                     #(16083/4480 -25227/4480 44703/4480 -15399/4480 -15399/4480 44703/4480 -25227/4480 16083/4480)
                     #(20225/4536 -4175/324 41675/1134 -137675/2268 169555/2268 -137675/2268 41675/1134 -4175/324 20225/4536)
                     #(4325321/1036800 -72635189/7257600 4310317/181440 -11746361/453600 48901919/3628800 48901919/3628800
                       -11746361/453600 4310317/181440 -72635189/7257600 4325321/1036800)
                     #(9626/1925 -35771/1925 123058/1925 -266298/1925 427956/1925 -494042/1925 427956/1925 -266298/1925 123058/1925
                       -35771/1925 9626/1925)
                     #(4527766399/958003200 -4881098833/319334400 43831288241/958003200 -1651951561/21288960 13602071249/159667200
                       -1158778153/31933440 -1158778153/31933440 13602071249/159667200 -1651951561/21288960 43831288241/958003200
                       -4881098833/319334400 4527766399/958003200)
                     #(2303435659/416988000 -1746642583/69498000 7067957029/69498000 -11311677097/41698800 1670445427/3088800
                       -9326809303/11583000 32009894483/34749000 -9326809303/11583000 1670445427/3088800 -11311677097/41698800
                       7067957029/69498000 -1746642583/69498000 2303435659/416988000)
                     #(905730205/172204032 -3689759795/172204032 2226544555/28700672 -2147796905/12300288 47259022115/172204032
                       -15560694095/57401344 1684042445/14350336 1684042445/14350336 -15560694095/57401344 47259022115/172204032
                       -2147796905/12300288 2226544555/28700672 -3689759795/172204032 905730205/172204032)
                     #(11555275136/1915538625 -62273397568/1915538625 290404217984/1915538625 -919494024608/1915538625
                       2192567376256/1915538625 -3992965568192/1915538625 1897702417792/638512875 -2131717882256/638512875
                       1897702417792/638512875 -3992965568192/1915538625 2192567376256/1915538625 -919494024608/1915538625
                       290404217984/1915538625 -62273397568/1915538625 11555275136/1915538625)
                     #(362555126427073/62768369664000 -1782924415592401/62768369664000 7581522218007113/62768369664000
                       -7030469481873619/20922789888000 6061162410557827/8966909952000 -60117144155671973/62768369664000
                       56621644374387437/62768369664000 -7822950144565727/20922789888000 -7822950144565727/20922789888000
                       56621644374387437/62768369664000 -60117144155671973/62768369664000 6061162410557827/8966909952000
                       -7030469481873619/20922789888000 7581522218007113/62768369664000 -1782924415592401/62768369664000
                       362555126427073/62768369664000)
                     #(62209540161/9529520000 -193893576633/4764760000 186372621/866320 -750890558259/952952000 208563085359/95295200
                       -22447758358269/4764760000 9590151541221/1191190000 -2105424409923/190590400 11691873278343/952952000
                       -2105424409923/190590400 9590151541221/1191190000 -22447758358269/4764760000 208563085359/95295200
                       -750890558259/952952000 186372621/866320 -193893576633/4764760000 62209540161/9529520000)
                     #(57424625956493833/9146248151040000 -51499668595907713/1422749712384000 2831206484232759701/16005934264320000
                       -373766725994602289/640237370572800 337230604409417/237124952064 -266340535976610163/103934638080000
                       986233830545113243/291016986624000 -5408953361592551857/1778437140480000 1586799402762999283/1280474741145600
                       1586799402762999283/1280474741145600 -5408953361592551857/1778437140480000 986233830545113243/291016986624000
                       -266340535976610163/103934638080000 337230604409417/237124952064 -373766725994602289/640237370572800
                       2831206484232759701/16005934264320000 -51499668595907713/1422749712384000 57424625956493833/9146248151040000)
                     #(4373703751565/623668727682 -20649923434495/415779151788 61065477359735/207889575894
                       -253893379558405/207889575894 402631871310710/103944787947 -142866589182035/14849255421
                       284667128261570/14849255421 -6459981265186655/207889575894 4301173329207145/103944787947
                       -28374376947229255/623668727682 4301173329207145/103944787947 -6459981265186655/207889575894
                       284667128261570/14849255421 -142866589182035/14849255421 402631871310710/103944787947
                       -253893379558405/207889575894 61065477359735/207889575894 -20649923434495/415779151788
                       4373703751565/623668727682)))
              (h   (/ (- end start) order)))
          (* h (loop for n from 1 upto (1- order)
                     for w across (aref wts (- order 2))
                     for x = (+ start h) then (+ x h)
                     sum (* w (funcall fun x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-gauss-legendre (fun &key start end (order 10))
  "Compute the definite integral using Gaussian quadrature (1<ORDER<21)."
  (let ((fun (mjr_mxp_string-or-func-to-lambda fun "x")))
  (let ((abscissa #(nil
                    nil
                    (-0.577350269189625764509148780501957455647601751270126876018602326483977672302933345693715395585749525225208713805135567676656648364999650826270551837364791216176031077300768527355991606700361558307755005104114422301107628883557418222973945990409015710553455953862673016662179126619796489216d0
                     0.577350269189625764509148780501957455647601751270126876018602326483977672302933345693715395585749525225208713805135567676656648364999650826270551837364791216176031077300768527355991606700361558307755005104114422301107628883557418222973945990409015710553455953862673016662179126619796489216d0)
                    (0.0d0
                     -0.77459666924148337703585307995647992216658434105831816531751475322269661838739580670385747537173470358326044137218992940263790808783272992313597834922424070221375095820269871625678390624577785851316928340561250183863468253197296369109292571026318805252353452810172926009011556212639457618d0
                     0.77459666924148337703585307995647992216658434105831816531751475322269661838739580670385747537173470358326044137218992940263790808783272992313597834922424070221375095820269871625678390624577785851316928340561250183863468253197296369109292571026318805252353452810172926009011556212639457618d0)
                    (-0.339981043584856264802665759103244687200575869770914352592953976821020030463237034477875280435554811548960239520746493213584500324171249199277636368433832822153861118235283631110415834062152112412502382193286424003476708675262956094341082153414679167140544266850815175616973289892495319553d0
                     0.339981043584856264802665759103244687200575869770914352592953976821020030463237034477875280435554811548960239520746493213584500324171249199277636368433832822153861118235283631110415834062152112412502382193286424003476708675262956094341082153414679167140544266850815175616973289892495319553d0
                     -0.861136311594052575223946488892809505095725379629717637615721920906529471495048865704162339884479305210576920931978176324963743839115791976408493845861885576287293132744136994429012259846971026190645868156474521936211491606609767805318718058026853914122347178087019863937224741695107377055d0
                     0.861136311594052575223946488892809505095725379629717637615721920906529471495048865704162339884479305210576920931978176324963743839115791976408493845861885576287293132744136994429012259846971026190645868156474521936211491606609767805318718058026853914122347178087019863937224741695107377055d0)
                    (0.0d0
                     -0.538469310105683091036314420700208804967286606905559956202231627059471185367755291035803667250570931571367057232104349551081691215874404642068348607562748153397812382858336931784613238752679616679650205379956362987867171636166076758485220009741807924140625605757101960272001927052309375033d0
                     0.538469310105683091036314420700208804967286606905559956202231627059471185367755291035803667250570931571367057232104349551081691215874404642068348607562748153397812382858336931784613238752679616679650205379956362987867171636166076758485220009741807924140625605757101960272001927052309375033d0
                     -0.906179845938663992797626878299392965125651910762530862873762286543770794916686846941142989553542261911583624816705116093202066008434972191537486957012541865906170054027301208653060409120782156294270419378670729821731536876900237602953790773893552884739789555764810391679786814060095349890d0
                     0.906179845938663992797626878299392965125651910762530862873762286543770794916686846941142989553542261911583624816705116093202066008434972191537486957012541865906170054027301208653060409120782156294270419378670729821731536876900237602953790773893552884739789555764810391679786814060095349890d0)
                    (0.661209386466264513661399595019905347006448564395170070814526705852183496607143100944286403746461456429888371639275146679557346772225380438172319801009336742391853886430007901629944262514588490245571882197038630322362011735232135702218793618906974301231555871064213101639896769013566165126115051499783d0
                     -0.661209386466264513661399595019905347006448564395170070814526705852183496607143100944286403746461456429888371639275146679557346772225380438172319801009336742391853886430007901629944262514588490245571882197038630322362011735232135702218793618906974301231555871064213101639896769013566165126115051499783d0
                     -0.238619186083196908630501721680711935418610630140021350181395164574274934275639842249224427257349131609072223097010687202955453035077205135262887217518998298513986621681263622903057829877085944097699929861758573946921613621659222233462641640013936777894532787145324672151888999339900094540815051499783d0
                     0.238619186083196908630501721680711935418610630140021350181395164574274934275639842249224427257349131609072223097010687202955453035077205135262887217518998298513986621681263622903057829877085944097699929861758573946921613621659222233462641640013936777894532787145324672151888999339900094540615051499783d0
                     -0.932469514203152027812301554493994609134765737712289824872549616526613500844200196276288739921925985047863679726572834106587971379511638404192178618075021016921157845203893084631037296117463252461261976049743797407422632089671621172178385230505104744277222209386367655366917903888025232677115051499783d0
                     0.932469514203152027812301554493994609134765737712289824872549616526613500844200196276288739921925985047863679726572834106587971379511638404192178618075021016921157845203893084631037296117463252461261976049743797407422632089671621172178385230505104744277222209386367655366917903888025232677115051499783d0)
                    (0.0d0
                     0.405845151377397166906606412076961463347382014099370126387043251794663813226125655328312689727746587765286758666048018678014238977408789960245829345943115240370586485013602819294679864699749418886916976554265450535738460310065859847627071045099488348002459926711388547267949016204332142257415051499783d0
                     -0.405845151377397166906606412076961463347382014099370126387043251794663813226125655328312689727746587765286758666048018678014238977408789960245829345943115240370586485013602819294679864699749418886916976554265450535738460310065859847627071045099488348002459926711388547267949016204332142257415051499783d0
                     -0.741531185599394439863864773280788407074147647141390260119955351967429874672180513792826832366863247059692518093112014243600054398229835347170385715274049833296074760797610715069876902693284456195815124609596217181595028716982161914070972011887539155583460195541497146710346290127809457209715051499783d0
                     0.741531185599394439863864773280788407074147647141390260119955351967429874672180513792826832366863247059692518093112014243600054398229835347170385715274049833296074760797610715069876902693284456195815124609596217181595028716982161914070972011887539155583460195541497146710346290127809457209315051499783d0
                     -0.94910791234275852452618968404785126240077093767061778354876910391306333035484014080573077002792572414430073966699521619419562581135355311827778991585981008501390100017988824773230504010481514885111290494043742057945997910849844239795226108144013882318870495006827477432277606366971303987341505149978320d0
                     0.94910791234275852452618968404785126240077093767061778354876910391306333035484014080573077002792572414430073966699521619419562581135355311827778991585981008501390100017988824773230504010481514885111290494043742057945997910849844239795226108144013882318870495006827477432277606366971303987341505149978320d0)
                    (-0.183434642495649804939476142360183980666757812912973782317188473699204474221542114116068223711123353745267658764286766608919601252387686568378856999516066356810447555161713850196638581076420553237088265474949281231496124776461936356277064571645661315940513405298505817196917430606444528963815051499783d0
                     0.183434642495649804939476142360183980666757812912973782317188473699204474221542114116068223711123353745267658764286766608919601252387686568378856999516066356810447555161713850196638581076420553237088265474949281231496124776461936356277064571645661315940513405298505817196917430606444528963815051499783d0
                     -0.525532409916328985817739049189246349041964243120392857750857099272454820768561272523961400193631982061909682924825260850710879376663877993980539530366825363111901827303240236006071747000612790147958757675624128889533661964352833082562426347054018422460368881753793853965850211387695359887915051499783d0
                     0.525532409916328985817739049189246349041964243120392857750857099272454820768561272523961400193631982061909682924825260850710879376663877993980539530366825363111901827303240236006071747000612790147958757675624128889533661964352833082562426347054018422460368881753793853965850211387695359887915051499783d0
                     -0.796666477413626739591553936475830436837171731615964832070170295039217305676473092147151927295725939019197453453097309265365649491701085960277256207462168967615393501629034232564558263420530154585606009572734260355741576126514042885195734193371080372278313611362813726763065141331999333800215051499783d0
                     0.796666477413626739591553936475830436837171731615964832070170295039217305676473092147151927295725939019197453453097309265365649491701085960277256207462168967615393501629034232564558263420530154585606009572734260355741576126514042885195734193371080372278313611362813726763065141331999333800215051499783d0
                     -0.96028985649753623168356086856947299042823523430145203827163977737242489774341928443943895926331226831042439281729417621023895815521712854793736422049096997004339826183266373468087812635533469278673596634808705975425476039293185338665681328688426134748962892320876399889524097724893873242561505149978320d0
                     0.96028985649753623168356086856947299042823523430145203827163977737242489774341928443943895926331226831042439281729417621023895815521712854793736422049096997004339826183266373468087812635533469278673596634808705975425476039293185338665681328688426134748962892320876399889524097724893873242561505149978320d0)
                    (0.0d0
                     -0.836031107326635794299429788069734876544106718124675996104371979639455006881590118893946197025857540256375810391056186876792170039985281349361196379534838829807268362865585871428630769092182750327917949337801790339028293128779263817006144234628841636676825929526852272549143759269877561638615051499783d0
                     0.836031107326635794299429788069734876544106718124675996104371979639455006881590118893946197025857540256375810391056186876792170039985281349361196379534838829807268362865585871428630769092182750327917949337801790339028293128779263817006144234628841636676825929526852272549143759269877561638615051499783d0
                     -0.968160239507626089835576202903672870049404800491925329550023311849080374396600753061873749226894111602487591123317815990652281196960250934108000611145715735257732059403074293910520074222179958144883241218047916016566855721762825317860506425581684503058984360543305378197872694642571982147915051499783d0
                     0.968160239507626089835576202903672870049404800491925329550023311849080374396600753061873749226894111602487591123317815990652281196960250934108000611145715735257732059403074293910520074222179958144883241218047916016566855721762825317860506425581684503058984360543305378197872694642571982147915051499783d0
                     -0.324253423403808929038538014643336608571956260736973088827047476842186579535124249193098601698497567207777825717350737391171804557523843239457286500570533380502549159913263023505363039892493128636190932894017334518781329619368723169492697363765187071546927093522355027447511765458528669807515051499783d0
                     0.324253423403808929038538014643336608571956260736973088827047476842186579535124249193098601698497567207777825717350737391171804557523843239457286500570533380502549159913263023505363039892493128636190932894017334518781329619368723169492697363765187071546927093522355027447511765458528669807515051499783d0
                     -0.613371432700590397308702039341474184785720604940564692872812942281267346491001198583240013903568584578233489596859768561939711752851974687245834604037155999620233482831298746351692646681288853297828062018202759053137127401722978736792193480338153401517695411359740276390469781469727328691715051499783d0
                     0.613371432700590397308702039341474184785720604940564692872812942281267346491001198583240013903568584578233489596859768561939711752851974687245834604037155999620233482831298746351692646681288853297828062018202759053137127401722978736792193480338153401517695411359740276390469781469727328691715051499783d0)
                    (-0.148874338981631210884826001129719984617564859420691695707989253515903617355668521371177629799463691230031160805255338826102890181864376540231676196996809091305073782772037105907094247585942274324983717717424734621691485290294292900319346665908243383809435507599683357023000500383728063435d0
                     0.148874338981631210884826001129719984617564859420691695707989253515903617355668521371177629799463691230031160805255338826102890181864376540231676196996809091305073782772037105907094247585942274324983717717424734621691485290294292900319346665908243383809435507599683357023000500383728063435d0
                     -0.433395394129247190799265943165784162200071837656246496502701513143766989077703501225102757950117721223682935040998937947274224757723249205126774103282208620095231927093346203201132832038769158406341114980112982314148878744320432476641442157678880770848387945248811854979703928792696425422d0
                     0.433395394129247190799265943165784162200071837656246496502701513143766989077703501225102757950117721223682935040998937947274224757723249205126774103282208620095231927093346203201132832038769158406341114980112982314148878744320432476641442157678880770848387945248811854979703928792696425422d0
                     -0.679409568299024406234327365114873575769294711834809467664817188952558575395074924615078573570480379499833902047399315060836740842576630090768274171820292354319785284697740971836914371201355296283773315310867912693254495485472934132472721168027426848661712101171203022718105101071880444416d0
                     0.679409568299024406234327365114873575769294711834809467664817188952558575395074924615078573570480379499833902047399315060836740842576630090768274171820292354319785284697740971836914371201355296283773315310867912693254495485472934132472721168027426848661712101171203022718105101071880444416d0
                     -0.865063366688984510732096688423493048527543014965330452521959731845374755138055561356790728946045770694404631086411765168678300161493453563739272939689095001157134968989305161207243576048090097972592331792379553573929059587977695683242770223694276591148364371481692378170157259728913932231d0
                     0.865063366688984510732096688423493048527543014965330452521959731845374755138055561356790728946045770694404631086411765168678300161493453563739272939689095001157134968989305161207243576048090097972592331792379553573929059587977695683242770223694276591148364371481692378170157259728913932231d0
                     -0.973906528517171720077964012084452053428269946692382119231212066696595203234636159625723564956268556258233042518774211215022168601434477779920540958725994243670441369576488125879914663314351075873711987787521056706745243536871368303386090938831164665358170712568697066873725922944928438379d0
                     0.973906528517171720077964012084452053428269946692382119231212066696595203234636159625723564956268556258233042518774211215022168601434477779920540958725994243670441369576488125879914663314351075873711987787521056706745243536871368303386090938831164665358170712568697066873725922944928438379d0)
                    (0.0d0
                     -0.269543155952344972331531985400861524679621862439052281623925631880057066223694735703821592244267301324437218670784420131703261031386159869445874008997209814711085966422911070381054841761125359843362910496130436491969434886922045442038145834882949205959927254713680948821359002280879954443d0
                     0.269543155952344972331531985400861524679621862439052281623925631880057066223694735703821592244267301324437218670784420131703261031386159869445874008997209814711085966422911070381054841761125359843362910496130436491969434886922045442038145834882949205959927254713680948821359002280879954443d0
                     -0.519096129206811815925725669458609554480227115119928489020922611486695926451072892825598780104549055281481215816090930712917361798754863975442799357423474217239587193205507468310495416288439100472666429554298168995269710820706918265652351038999613979036703931150707065849469728644598483166d0
                     0.519096129206811815925725669458609554480227115119928489020922611486695926451072892825598780104549055281481215816090930712917361798754863975442799357423474217239587193205507468310495416288439100472666429554298168995269710820706918265652351038999613979036703931150707065849469728644598483166d0
                     -0.730152005574049324093416252031153458049643062026130311978378339687013245058519229595423410971283700061986544502194758426628956134627091547790548664119143683413655247204817042580914616539990379564392003921975793793977449019859563636451545786548452408936258244702250735195988884904955403826d0
                     0.730152005574049324093416252031153458049643062026130311978378339687013245058519229595423410971283700061986544502194758426628956134627091547790548664119143683413655247204817042580914616539990379564392003921975793793977449019859563636451545786548452408936258244702250735195988884904955403826d0
                     -0.887062599768095299075157769303927266631675751225314384967411055537611313857372667442290595126597374158255564695132794001307669559450979818581018089623059990416546759673322838354589458072859986661987212368584044690409619203511802608761335141934582280347537507219942127949058314505651165627d0
                     0.887062599768095299075157769303927266631675751225314384967411055537611313857372667442290595126597374158255564695132794001307669559450979818581018089623059990416546759673322838354589458072859986661987212368584044690409619203511802608761335141934582280347537507219942127949058314505651165627d0
                     -0.97822865814605699280393800112285739077142240891978441542580106598366379938088998820031939816734476989934793299892787316159661817350259814121745466962601538022664674175913583425370673723273279697824062414990545460156218144587586872077516009539839808019464994612006789931275526902407373741d0
                     0.97822865814605699280393800112285739077142240891978441542580106598366379938088998820031939816734476989934793299892787316159661817350259814121745466962601538022664674175913583425370673723273279697824062414990545460156218144587586872077516009539839808019464994612006789931275526902407373741d0)
                    (-0.12523340851146891547244136946385312998339691630544427321292175474846205624138968874286829846949135959410459879132051097315159969664463407959720578930281363427149751877364610797786290401085851749803458163536009061915338533985792224380950454509734206424773968688379951776094896413752291920d0
                     0.12523340851146891547244136946385312998339691630544427321292175474846205624138968874286829846949135959410459879132051097315159969664463407959720578930281363427149751877364610797786290401085851749803458163536009061915338533985792224380950454509734206424773968688379951776094896413752291920d0
                     -0.367831498998180193752691536643717561256360141335409621311799879504089929516787873878734428500546577234633126395977145215135152179327439353241991637742753828713203896641622743037182844709631889345478848418226114612275269796093716296005046396231978742367666804603302524255853636261789436667d0
                     0.367831498998180193752691536643717561256360141335409621311799879504089929516787873878734428500546577234633126395977145215135152179327439353241991637742753828713203896641622743037182844709631889345478848418226114612275269796093716296005046396231978742367666804603302524255853636261789436667d0
                     -0.587317954286617447296702418940534280369098514048052481510270879667340699375895262435710764988748201909601559992928892677231069591088671751424991898437041519657996549315217924868346993422457465422705569591078717943491541436351391916742855459687794049113975692317744768973884912086543556314d0
                     0.587317954286617447296702418940534280369098514048052481510270879667340699375895262435710764988748201909601559992928892677231069591088671751424991898437041519657996549315217924868346993422457465422705569591078717943491541436351391916742855459687794049113975692317744768973884912086543556314d0
                     -0.769902674194304687036893833212818075984925750018931637664419064249116543108471224016424999223421910617617540454221856207040162852653547594919420351587547115144351846268965701433678578699607070682628221024887602161567892357590625431095153841089934179754923070702138246759697562146447713416d0
                     0.769902674194304687036893833212818075984925750018931637664419064249116543108471224016424999223421910617617540454221856207040162852653547594919420351587547115144351846268965701433678578699607070682628221024887602161567892357590625431095153841089934179754923070702138246759697562146447713416d0
                     -0.904117256370474856678465866119096192537596709213297546554075760681234795729235790486969427823733267811860382896410422348899719819542996010635249012582682919983473544486142061408991002470096825762582216934464486987461675807578423980743809206406595454017167918085020519670289496391235944849d0
                     0.904117256370474856678465866119096192537596709213297546554075760681234795729235790486969427823733267811860382896410422348899719819542996010635249012582682919983473544486142061408991002470096825762582216934464486987461675807578423980743809206406595454017167918085020519670289496391235944849d0
                     -0.98156063424671925069054909014928082296015519981373151046268212180779324431825398222525726789045223578555649237284127318524545703044707716708276967488752886112565550184482662910041202137201539996961235882788466302337187351583920530374414763938317041938954347092061854318067356922598837056d0
                     0.98156063424671925069054909014928082296015519981373151046268212180779324431825398222525726789045223578555649237284127318524545703044707716708276967488752886112565550184482662910041202137201539996961235882788466302337187351583920530374414763938317041938954347092061854318067356922598837056d0)
                    (0.0d0
                     -0.230458315955134794065528121097988835211542375883531163469261497837162083688581950812802944764680129323851280249316341249564089037515340500150378476582875632094060958377370363510785662604029567533315343557218243772138957860186497429704186124682681908579373550719712362293706019278019013404d0
                     0.230458315955134794065528121097988835211542375883531163469261497837162083688581950812802944764680129323851280249316341249564089037515340500150378476582875632094060958377370363510785662604029567533315343557218243772138957860186497429704186124682681908579373550719712362293706019278019013404d0
                     -0.448492751036446852877912852127639867801921667441757878958282947458244092066217143645038208554733010096052635557135968576263976716632951607211102025554678503759960780940766795259845271561685846563097781712113788027057373143985195362212842424207197126759428860733945225739416778929130239321d0
                     0.448492751036446852877912852127639867801921667441757878958282947458244092066217143645038208554733010096052635557135968576263976716632951607211102025554678503759960780940766795259845271561685846563097781712113788027057373143985195362212842424207197126759428860733945225739416778929130239321d0
                     -0.642349339440340220643984606995515650071697398261576857389142407918643553144993574914388300114191555122246770264547193812404734954265351007638053667092958130495680907147627991756483150448118177492859264424724153388575072424600399064574606010215889502833922310209255150038832165563668242499d0
                     0.642349339440340220643984606995515650071697398261576857389142407918643553144993574914388300114191555122246770264547193812404734954265351007638053667092958130495680907147627991756483150448118177492859264424724153388575072424600399064574606010215889502833922310209255150038832165563668242499d0
                     -0.801578090733309912794206489582859890305615724790500029897384713859222116226640122030029874185317060252361603376094785075486852487026517211691384815161571326399065747961902539586565576117152492158815961731203548116605143403845729488947950355774107487081487002809786642849867021932300492097d0
                     0.801578090733309912794206489582859890305615724790500029897384713859222116226640122030029874185317060252361603376094785075486852487026517211691384815161571326399065747961902539586565576117152492158815961731203548116605143403845729488947950355774107487081487002809786642849867021932300492097d0
                     -0.91759839922297796520654783650071951239047479011168329589528534565965920858960991428348539034058292445105178102177510723069758495547370559751550558291199507157208120911214434502259399276696212009910336252552620928878159412151195841989888000995810961009790029201464987391771805691990718461d0
                     0.91759839922297796520654783650071951239047479011168329589528534565965920858960991428348539034058292445105178102177510723069758495547370559751550558291199507157208120911214434502259399276696212009910336252552620928878159412151195841989888000995810961009790029201464987391771805691990718461d0
                     -0.984183054718588149472829448807109611064990561925874908694007320428595237875626841860569287261418588401316395767351975831718083600922990060887656449483169693709019104432369061431450624010623981289045340066348962388901681355122477529480878162264260165464579716185038981121495082557795946992d0
                     0.984183054718588149472829448807109611064990561925874908694007320428595237875626841860569287261418588401316395767351975831718083600922990060887656449483169693709019104432369061431450624010623981289045340066348962388901681355122477529480878162264260165464579716185038981121495082557795946992d0)
                    (-0.108054948707343662066244650219834747611951605474237557040821061308013529011730007130100688176689367237450202642446647463809923263225819142756721819731504097528061372738422650694879443087753215088445556391329819060204836416480024319739665907101250616170281442501463564322177354100132889276d0
                     0.108054948707343662066244650219834747611951605474237557040821061308013529011730007130100688176689367237450202642446647463809923263225819142756721819731504097528061372738422650694879443087753215088445556391329819060204836416480024319739665907101250616170281442501463564322177354100132889276d0
                     -0.319112368927889760435671824168475466834261203533843956596650187257333440512792783164933705421346413180279315182609039449614564057871001771650886322223962456080121209931285421723488082877164586378479374239121304478425121768114783511643536777896294999744846055821475967652564484135180159485d0
                     0.319112368927889760435671824168475466834261203533843956596650187257333440512792783164933705421346413180279315182609039449614564057871001771650886322223962456080121209931285421723488082877164586378479374239121304478425121768114783511643536777896294999744846055821475967652564484135180159485d0
                     -0.515248636358154091965290718551188662308885282569306036951504769092784951832055660452072020350772892392290793290509013869527403557134004759391826056530572110116376520732003425808230385320417840203436173906624491224801618641571038235567674745455397963743862763549078606489291245148197372128d0
                     0.515248636358154091965290718551188662308885282569306036951504769092784951832055660452072020350772892392290793290509013869527403557134004759391826056530572110116376520732003425808230385320417840203436173906624491224801618641571038235567674745455397963743862763549078606489291245148197372128d0
                     -0.687292904811685470148019803019334137538401212747170675619266488628184896183133256947373070505211838410660363021679005472962743271541850101068212468817273890829526628854435899128393386081069593714595904926885388784713769175169784875289055161406787799647571765065314798269480402634235125407d0
                     0.687292904811685470148019803019334137538401212747170675619266488628184896183133256947373070505211838410660363021679005472962743271541850101068212468817273890829526628854435899128393386081069593714595904926885388784713769175169784875289055161406787799647571765065314798269480402634235125407d0
                     -0.827201315069764993189794742650394961039701101475081181560709054241479830810028873570426390137889545399124140627398653533327566122673781617958264510699079368086693175647780145678598550782511472915830426696849656086721489336979443959282673643228642517214320892425110662404429503712773749011d0
                     0.827201315069764993189794742650394961039701101475081181560709054241479830810028873570426390137889545399124140627398653533327566122673781617958264510699079368086693175647780145678598550782511472915830426696849656086721489336979443959282673643228642517214320892425110662404429503712773749011d0
                     -0.928434883663573517336391139377874264477039210409837618717962447482131093544359853111413905683657517636355126155988260360700857801078653925801898454004406504941578880981795311611477191308252353458596605653673043686690855550898698329741248613224574938848389094543645740470554948434817872100d0
                     0.928434883663573517336391139377874264477039210409837618717962447482131093544359853111413905683657517636355126155988260360700857801078653925801898454004406504941578880981795311611477191308252353458596605653673043686690855550898698329741248613224574938848389094543645740470554948434817872100d0
                     -0.986283808696812338841597266704052801676091407239225881644070811777749554132491637910646239665151752760261256294135857868985260306744797449411972703247108982071700729556750481802616879705559894475396929426197069500447181272675429908986256542893367646391480247767729174500296582776736074173d0
                     0.986283808696812338841597266704052801676091407239225881644070811777749554132491637910646239665151752760261256294135857868985260306744797449411972703247108982071700729556750481802616879705559894475396929426197069500447181272675429908986256542893367646391480247767729174500296582776736074173d0)
                    (0.0d0
                     -0.201194093997434522300628303394596207812836454462637679615949724609948239003020187601836258067521059089679022573865094211894279283025488572786246829676268952047232310529610626112465135761441799741803512103540824774964819456117931525058045796956522701284997876907383257784780634036379574947d0
                     0.201194093997434522300628303394596207812836454462637679615949724609948239003020187601836258067521059089679022573865094211894279283025488572786246829676268952047232310529610626112465135761441799741803512103540824774964819456117931525058045796956522701284997876907383257784780634036379574947d0
                     -0.394151347077563369897207370981045468362752776158698255031165343951608957786961417975497114161659762025893521696356480024758478126035823395715149345555300752188691439260658374215424847957974984236026113280979797965149913749006546819968564769360993598261631794270178322666904875353325418311d0
                     0.394151347077563369897207370981045468362752776158698255031165343951608957786961417975497114161659762025893521696356480024758478126035823395715149345555300752188691439260658374215424847957974984236026113280979797965149913749006546819968564769360993598261631794270178322666904875353325418311d0
                     -0.570972172608538847537226737253910641238386396282749604853265417054195379869758579483414628569826144779126464970262570403511550191277644376134045080451646378107636454965688669489244636592039009340155675255353148254715457212663016223408296537135309486202433337095590793601383870191959080341d0
                     0.570972172608538847537226737253910641238386396282749604853265417054195379869758579483414628569826144779126464970262570403511550191277644376134045080451646378107636454965688669489244636592039009340155675255353148254715457212663016223408296537135309486202433337095590793601383870191959080341d0
                     -0.72441773136017004741618605461393800963089929458410256355142342070412378167792521899610109760313432626923598549381925112038656420089731543571352817517060844095108302046001626297456208587636256942340716576088693523805022510967483283007959936537779041346686430165514920416950579616308634374d0
                     0.72441773136017004741618605461393800963089929458410256355142342070412378167792521899610109760313432626923598549381925112038656420089731543571352817517060844095108302046001626297456208587636256942340716576088693523805022510967483283007959936537779041346686430165514920416950579616308634374d0
                     -0.848206583410427216200648320774216851366256174736992634095727558760675075174145485197607719750821480850903738357133399177465586306711247802474115523337852878393170575214139894191014720013698702122900968746862382080956083135926124502807359720250831534576527289787096448963279046353202620600d0
                     0.848206583410427216200648320774216851366256174736992634095727558760675075174145485197607719750821480850903738357133399177465586306711247802474115523337852878393170575214139894191014720013698702122900968746862382080956083135926124502807359720250831534576527289787096448963279046353202620600d0
                     -0.937273392400705904307758947710209471243996273515304457901363076350202973797045527950547586174268086597468240446031568449200951335283439053694924559043052786175746581001188374918360116273162506619052335979984445928662550828058087774487772344475212283780253684252108572228026381301697830140d0
                     0.937273392400705904307758947710209471243996273515304457901363076350202973797045527950547586174268086597468240446031568449200951335283439053694924559043052786175746581001188374918360116273162506619052335979984445928662550828058087774487772344475212283780253684252108572228026381301697830140d0
                     -0.987992518020485428489565718586612581146972817123761489999997515587388437369019424712722050368319144976675168439900792501939582367069205780699275856792078596934070279127563012049733722807922933019892231200697993716178408450076710211341576822105065369152246283329685836223823968572851964705d0
                     0.987992518020485428489565718586612581146972817123761489999997515587388437369019424712722050368319144976675168439900792501939582367069205780699275856792078596934070279127563012049733722807922933019892231200697993716178408450076710211341576822105065369152246283329685836223823968572851964705d0)
                    (-0.095012509837637440185319335424958063130353055689065456697219817225125298244592132984758692975783352099655391242316312448307477322448756550755282537668331759004263943067522680862196829830639838583409406235445273885367337095224271687515391202189168029043598678311955706723538935184424572487d0
                     0.095012509837637440185319335424958063130353055689065456697219817225125298244592132984758692975783352099655391242316312448307477322448756550755282537668331759004263943067522680862196829830639838583409406235445273885367337095224271687515391202189168029043598678311955706723538935184424572487d0
                     -0.281603550779258913230460501460496106486069490770599800548834733955925179499130770441440229152040159284337367075667679943958608231731859592427781740737461652997267317253218182999323504712804139056838901612216710295650017782508839689124831507619987030673289319607769930008078327816038826852d0
                     0.281603550779258913230460501460496106486069490770599800548834733955925179499130770441440229152040159284337367075667679943958608231731859592427781740737461652997267317253218182999323504712804139056838901612216710295650017782508839689124831507619987030673289319607769930008078327816038826852d0
                     -0.458016777657227386342419442983577573540031613035523490901154750947759174290293607735435527935988093250889048880252410981937838726387574837437245680248141865615320954226737392097436317324094222204031261233053265312320550120442111110407476217618631666705702234642200753367438387066992816317d0
                     0.458016777657227386342419442983577573540031613035523490901154750947759174290293607735435527935988093250889048880252410981937838726387574837437245680248141865615320954226737392097436317324094222204031261233053265312320550120442111110407476217618631666705702234642200753367438387066992816317d0
                     -0.617876244402643748446671764048791018991882217765657794103797355541733317754811424456911030427958503112200056927562415107693692572784801040259587690327324717951779891436251146410287663884856270141407139522242799658052450289707423746241939384051849138442880667652732358538107817206001610679d0
                     0.617876244402643748446671764048791018991882217765657794103797355541733317754811424456911030427958503112200056927562415107693692572784801040259587690327324717951779891436251146410287663884856270141407139522242799658052450289707423746241939384051849138442880667652732358538107817206001610679d0
                     -0.755404408355003033895101194847442268353813656457503009781757176922296861031271677720622056919249443421653922625775767897977695175560629164439783379772236988524324204690679886634995082921719758892531043716808436951136968926137627113210371753896434818023445773729725803533854770400353079352d0
                     0.755404408355003033895101194847442268353813656457503009781757176922296861031271677720622056919249443421653922625775767897977695175560629164439783379772236988524324204690679886634995082921719758892531043716808436951136968926137627113210371753896434818023445773729725803533854770400353079352d0
                     -0.865631202387831743880467897712393132387335384847526708103511425567760397712490558257132494364772354203821428331341464301386002990866175024061842106069569135788506000044642568396185726655607946093020656455010215324186946875986673906662683677019924493315721108336550609714189255266432784710d0
                     0.865631202387831743880467897712393132387335384847526708103511425567760397712490558257132494364772354203821428331341464301386002990866175024061842106069569135788506000044642568396185726655607946093020656455010215324186946875986673906662683677019924493315721108336550609714189255266432784710d0
                     -0.944575023073232576077988415534608345091139272591072600925553652066609788902682304219565728738158318949328931100907318886410952680610249479819600771779911178859167648841949072781417014484322049432347858125788197212092768569983767713535900969047797698658140281925051278387269799896663005836d0
                     0.944575023073232576077988415534608345091139272591072600925553652066609788902682304219565728738158318949328931100907318886410952680610249479819600771779911178859167648841949072781417014484322049432347858125788197212092768569983767713535900969047797698658140281925051278387269799896663005836d0
                     -0.989400934991649932596154173450332627426274071657645130051223904731324137215825396938536431906798181013513435859897866508253023707879179735930382232441399969509571107808772790530719908063571954612679838095993881138043500973565299223064246463993858934792537582800905112705690921922390353317d0
                     0.989400934991649932596154173450332627426274071657645130051223904731324137215825396938536431906798181013513435859897866508253023707879179735930382232441399969509571107808772790530719908063571954612679838095993881138043500973565299223064246463993858934792537582800905112705690921922390353317d0)
                    (0.0d0
                     -0.178484181495847855850677493654065557475419332691525643562951814270697550479300576170288922209635004579580413665869573869461919197910600427022801272640186923306163522149808438849536785228576635930762685591079682912198393756197446815638692161039746503374702411284192083324239196592812271001d0
                     0.178484181495847855850677493654065557475419332691525643562951814270697550479300576170288922209635004579580413665869573869461919197910600427022801272640186923306163522149808438849536785228576635930762685591079682912198393756197446815638692161039746503374702411284192083324239196592812271001d0
                     -0.351231763453876315297185517095346005040539751575675023319161019547794261646749572897583144603509104881038158847738240791225263866486505741231996601180922570996839640315202757391135554810463095286351885637508263924364169101716530583264510119020233418649028702879769284687070622862630881649d0
                     0.351231763453876315297185517095346005040539751575675023319161019547794261646749572897583144603509104881038158847738240791225263866486505741231996601180922570996839640315202757391135554810463095286351885637508263924364169101716530583264510119020233418649028702879769284687070622862630881649d0
                     -0.512690537086476967886246568629551874582923722411172905912731498964284924942785480281980412380517601458432505292516519296636013965232584796734319348139140480582772456459981468753489312365348142837608188998824341849522603598797508763544625759991504992804175990986275432731526260819843595933d0
                     0.512690537086476967886246568629551874582923722411172905912731498964284924942785480281980412380517601458432505292516519296636013965232584796734319348139140480582772456459981468753489312365348142837608188998824341849522603598797508763544625759991504992804175990986275432731526260819843595933d0
                     -0.657671159216690765850302216643002335147805891475973243805231695507324297574697581752991187305510129783375919836507998836939225086271254706464732560709357033025290162882066462072060399507083918421909339401850078210286258433668480359740850045717947210635970724773085747211386025836004073334d0
                     0.657671159216690765850302216643002335147805891475973243805231695507324297574697581752991187305510129783375919836507998836939225086271254706464732560709357033025290162882066462072060399507083918421909339401850078210286258433668480359740850045717947210635970724773085747211386025836004073334d0
                     -0.781514003896801406925230055520476050223972472740568512513314535512577616159513076173380500866961618653909355652359975581640558772645206984434355133010670558244317291995305532224685381142053480329403966175696984008686469073611018150518361762087740739780571119774301808069888748650656056315d0
                     0.781514003896801406925230055520476050223972472740568512513314535512577616159513076173380500866961618653909355652359975581640558772645206984434355133010670558244317291995305532224685381142053480329403966175696984008686469073611018150518361762087740739780571119774301808069888748650656056315d0
                     -0.880239153726985902122955694488155692623416817934427900351910159324027130504411211518307193868559189571061418102152607314411726325021879968942229769880358249670849037155658938176686890037627035892696245024109117180906980912855671474101534567475113675537213583314705739293547120589569001135d0
                     0.880239153726985902122955694488155692623416817934427900351910159324027130504411211518307193868559189571061418102152607314411726325021879968942229769880358249670849037155658938176686890037627035892696245024109117180906980912855671474101534567475113675537213583314705739293547120589569001135d0
                     -0.950675521768767761222716957895803021443385046559108707669969212412115366272198594750036902751468357735265683111473673539734255853685907960013656615410196161849110809023515606178705676664652972276583583373496328746959288291404308255573464061077083241825100730794258196450707602099239988614d0
                     0.950675521768767761222716957895803021443385046559108707669969212412115366272198594750036902751468357735265683111473673539734255853685907960013656615410196161849110809023515606178705676664652972276583583373496328746959288291404308255573464061077083241825100730794258196450707602099239988614d0
                     -0.990575475314417335675434019940665276507789850459564302783908786742343857470622033711358440649741978435977995595275714793833581782609500730471684065281696860818499621895853010556863239864242514817576866500310487052033356461301153269232678929514127699829947103139692599433634502554555126634d0
                     0.990575475314417335675434019940665276507789850459564302783908786742343857470622033711358440649741978435977995595275714793833581782609500730471684065281696860818499621895853010556863239864242514817576866500310487052033356461301153269232678929514127699829947103139692599433634502554555126634d0)
                    (-0.084775013041735301242261852935783811733317386906089920043364517625499790653280001637804550727439612443782927567279873122733177486959155574270786935450334780482724738681386008349294315396082737095005684913959303046598791551791018149432338541432224787022080161621016750344793221356315947878d0
                     0.084775013041735301242261852935783811733317386906089920043364517625499790653280001637804550727439612443782927567279873122733177486959155574270786935450334780482724738681386008349294315396082737095005684913959303046598791551791018149432338541432224787022080161621016750344793221356315947878d0
                     -0.251886225691505509588972854877911230162861765659640458020271031736784493555724221807077542109386247196463142449443265125060054853278524195280486160790949428423392109273698454961180174967446881906373048748798230141094167544475165341511137657870896648026074279710239992958390071803375932720d0
                     0.251886225691505509588972854877911230162861765659640458020271031736784493555724221807077542109386247196463142449443265125060054853278524195280486160790949428423392109273698454961180174967446881906373048748798230141094167544475165341511137657870896648026074279710239992958390071803375932720d0
                     -0.411751161462842646035931793833051637078989682120025511281148867790389673100425409774346850393651028926184321427325552367959619358487887936777717438386426048471820926589252866152703630580906218991671465034038946945728512297641259268637035959117824002106756357795199833248519537869237921513d0
                     0.411751161462842646035931793833051637078989682120025511281148867790389673100425409774346850393651028926184321427325552367959619358487887936777717438386426048471820926589252866152703630580906218991671465034038946945728512297641259268637035959117824002106756357795199833248519537869237921513d0
                     -0.559770831073947534607871548525329136927626485770709416639986944062184528206520519659082048360044368606430594998151770073020532672134502131227900738013811143661891947799725248891222884088927639156653030051155500040770915382366250638807753358873584327374217071158783381241074884173398505027d0
                     0.559770831073947534607871548525329136927626485770709416639986944062184528206520519659082048360044368606430594998151770073020532672134502131227900738013811143661891947799725248891222884088927639156653030051155500040770915382366250638807753358873584327374217071158783381241074884173398505027d0
                     -0.691687043060353207874891081288848389452270572817507758902162656837197368903629251133810808145132630251271543426328214601660222240437526351135808108135511702321547741402713256092487214200237615709047096463830128320817241028217560549669869337853177044465704049195938571891307624758747062808d0
                     0.691687043060353207874891081288848389452270572817507758902162656837197368903629251133810808145132630251271543426328214601660222240437526351135808108135511702321547741402713256092487214200237615709047096463830128320817241028217560549669869337853177044465704049195938571891307624758747062808d0
                     -0.803704958972523115682417455014590797103298921611922481750428064195388105428944287228286965442411759646715295500068022783654819533815545793751072673320920019800470250879210285662990694915425369672609850797634686398211244004456254685211473310999118638354218120127459059037079559331439226834d0
                     0.803704958972523115682417455014590797103298921611922481750428064195388105428944287228286965442411759646715295500068022783654819533815545793751072673320920019800470250879210285662990694915425369672609850797634686398211244004456254685211473310999118638354218120127459059037079559331439226834d0
                     -0.892602466497555739206060591127145515407895271352298214187466314907240582436784935830667587080603156394607122351544312623925390088751855574924264310967590585482956353194878729853790406359477959976169501143515101685995178441927299664146289849937732590753378904090508131934860817524148391602d0
                     0.892602466497555739206060591127145515407895271352298214187466314907240582436784935830667587080603156394607122351544312623925390088751855574924264310967590585482956353194878729853790406359477959976169501143515101685995178441927299664146289849937732590753378904090508131934860817524148391602d0
                     -0.955823949571397755181195892929776309972844134811306478845387629690898071204773223957882392959633924320855948246605688778890604311900251104868916993898240406646825367543014450970156708974515757573689502919327972937643448670607725941325447225957746824810157687017775066124990872729409212631d0
                     0.955823949571397755181195892929776309972844134811306478845387629690898071204773223957882392959633924320855948246605688778890604311900251104868916993898240406646825367543014450970156708974515757573689502919327972937643448670607725941325447225957746824810157687017775066124990872729409212631d0
                     -0.991565168420930946730016004706150770252578936845439692919675630239857093591779609751763469612601227152373107217977935587388130174059435925041562103209647600402564288257740924633692122333935158591771256791767762325700598557887622660969341943637260960816106366990540907222476258849576516079d0
                     0.991565168420930946730016004706150770252578936845439692919675630239857093591779609751763469612601227152373107217977935587388130174059435925041562103209647600402564288257740924633692122333935158591771256791767762325700598557887622660969341943637260960816106366990540907222476258849576516079d0)
                    (0.0d0
                     -0.160358645640225375868096115740743549504873500470875378874643451606320608680391073656739573499480912931848342018347879647591114145080971860790170509415741628782763672331771223036636724542070723676190815527498393142089735295570556537297261068089813980311299623954223032810613098624991419446d0
                     0.160358645640225375868096115740743549504873500470875378874643451606320608680391073656739573499480912931848342018347879647591114145080971860790170509415741628782763672331771223036636724542070723676190815527498393142089735295570556537297261068089813980311299623954223032810613098624991419446d0
                     -0.316564099963629831990117328849844917892285219132887245155728986797892885820576397996838892086480351073153508173025043084674042830940113652782872105153149007473343650000530964750897223767534051304178254970050112831012082953458820740149121864125196517400798337999512375441152245786710394877d0
                     0.316564099963629831990117328849844917892285219132887245155728986797892885820576397996838892086480351073153508173025043084674042830940113652782872105153149007473343650000530964750897223767534051304178254970050112831012082953458820740149121864125196517400798337999512375441152245786710394877d0
                     -0.464570741375960945717267148104102367976285714624136596984308877575684121113425406686449545720731944871091802995971430830893017793541345435885644663480565784277503515280284249556138537787055519377972248117618796869708936359805100026528994736081585309010992577108176875731596778439948184780d0
                     0.464570741375960945717267148104102367976285714624136596984308877575684121113425406686449545720731944871091802995971430830893017793541345435885644663480565784277503515280284249556138537787055519377972248117618796869708936359805100026528994736081585309010992577108176875731596778439948184780d0
                     -0.600545304661681023469638164946239279868322082732292567586586725668384908696870840424755795331454001435338204209040522183822845652668211228499539389160963948310298111378202516403643609261123189783557775419329170063033637998999273833807997583717733170256681985902256758747400495906424880731d0
                     0.600545304661681023469638164946239279868322082732292567586586725668384908696870840424755795331454001435338204209040522183822845652668211228499539389160963948310298111378202516403643609261123189783557775419329170063033637998999273833807997583717733170256681985902256758747400495906424880731d0
                     -0.720966177335229378617095860823781629657141832908666774938590471386614559147986052843433382671326574586514636876369589370593542668923144343735981669393576911159855383273594624324059255165696663563319328967685691134614308841050201080197519568778521049635306013282267299051082319486510481005d0
                     0.720966177335229378617095860823781629657141832908666774938590471386614559147986052843433382671326574586514636876369589370593542668923144343735981669393576911159855383273594624324059255165696663563319328967685691134614308841050201080197519568778521049635306013282267299051082319486510481005d0
                     -0.822714656537142824978922486712713901774538486206830041369863888605500342903685832026866223109260599647022561898459468510993677188742140860925313512863850876685704596894395851615618719662833019777486468813406364967779858658807257252074167126951516637951379215115990845275112221035383703727d0
                     0.822714656537142824978922486712713901774538486206830041369863888605500342903685832026866223109260599647022561898459468510993677188742140860925313512863850876685704596894395851615618719662833019777486468813406364967779858658807257252074167126951516637951379215115990845275112221035383703727d0
                     -0.903155903614817901642660928532312487809393934057355817695032424761635804590104218957520228369297045630078079901827830031217747515247810091984878712557903515583365581223187173522762863441164721599174548534935195938995081875586563558245261880693299394277880230844479855448328948548329413650d0
                     0.903155903614817901642660928532312487809393934057355817695032424761635804590104218957520228369297045630078079901827830031217747515247810091984878712557903515583365581223187173522762863441164721599174548534935195938995081875586563558245261880693299394277880230844479855448328948548329413650d0
                     -0.960208152134830030852778840687651526615091503274138105917816334432200749937488513220743561491541317725436819252316474216797883382431899161302962769710771600404838906157200156674825990955845357007495109947234885744092389927614169739572628944310724960504346921791861200353792002500163142136d0
                     0.960208152134830030852778840687651526615091503274138105917816334432200749937488513220743561491541317725436819252316474216797883382431899161302962769710771600404838906157200156674825990955845357007495109947234885744092389927614169739572628944310724960504346921791861200353792002500163142136d0
                     -0.992406843843584403189017670253260493589316401403210786679679439091231902958522883803674920873418801311979495833823608836414522698023847813354932780187443427599816683327888327164869517636136825379268304217880537765999844908545510240067369477976712027835727690140973426729682685648378701116d0
                     0.992406843843584403189017670253260493589316401403210786679679439091231902958522883803674920873418801311979495833823608836414522698023847813354932780187443427599816683327888327164869517636136825379268304217880537765999844908545510240067369477976712027835727690140973426729682685648378701116d0)
                    (-0.076526521133497333754640409398838211004796266813497500804795244384256342048336978241545114181556215606998505646364132704440537824571262884711188717276813384948071303463923222057331496896797657862400212690830988044264498119520490564867746815505079719399606097866630922035474016311889739746d0
                     0.076526521133497333754640409398838211004796266813497500804795244384256342048336978241545114181556215606998505646364132704440537824571262884711188717276813384948071303463923222057331496896797657862400212690830988044264498119520490564867746815505079719399606097866630922035474016311889739746d0
                     -0.227785851141645078080496195368574624743088937682927472314635739207171341863555827794952125190968708031773731315604302174299040876412812134852730947310351023101208877088910152689972786931295164727916623022208353223748577969803367857598723466309541401280105306768559162415814834436119575284d0
                     0.227785851141645078080496195368574624743088937682927472314635739207171341863555827794952125190968708031773731315604302174299040876412812134852730947310351023101208877088910152689972786931295164727916623022208353223748577969803367857598723466309541401280105306768559162415814834436119575284d0
                     -0.373706088715419560672548177024927237395746321705682711827948613515645764373059527895895683634533378944767722088528150153059350106844427395450066389359807687663211626916069403215673674827020366318684546043368197191105425738040844587161737194741396453537245738746513867035322772368524287066d0
                     0.373706088715419560672548177024927237395746321705682711827948613515645764373059527895895683634533378944767722088528150153059350106844427395450066389359807687663211626916069403215673674827020366318684546043368197191105425738040844587161737194741396453537245738746513867035322772368524287066d0
                     -0.510867001950827098004364050955250998425491329202426833472348619894734970390765728144031683050867779198329430688435262356565254022569114791269561738645060600383413353967296481546690995656151410109159001095754486392613186674667821174130784174407633800670942504541967577455615149803552040221d0
                     0.510867001950827098004364050955250998425491329202426833472348619894734970390765728144031683050867779198329430688435262356565254022569114791269561738645060600383413353967296481546690995656151410109159001095754486392613186674667821174130784174407633800670942504541967577455615149803552040221d0
                     -0.636053680726515025452836696226285936743389116799368463939446622546541262585430132558703195495761306582117109377725957362041081029709203240544699473944365053733190632158648879415701247475658216196020537951785503171732759595020097054287758642886663571296440583828750001803882040699369696031d0
                     0.636053680726515025452836696226285936743389116799368463939446622546541262585430132558703195495761306582117109377725957362041081029709203240544699473944365053733190632158648879415701247475658216196020537951785503171732759595020097054287758642886663571296440583828750001803882040699369696031d0
                     -0.746331906460150792614305070355641590310730679569176444139545906068535355038155064681104113620647520612384900651676561496311430972802890678937547240026921418389722006896328897905885020954874301332144248535149195712091118631390960963340541594809222588197860152935725445491005566277916765008d0
                     0.746331906460150792614305070355641590310730679569176444139545906068535355038155064681104113620647520612384900651676561496311430972802890678937547240026921418389722006896328897905885020954874301332144248535149195712091118631390960963340541594809222588197860152935725445491005566277916765008d0
                     -0.839116971822218823394529061701520685329629365065637373252492725532861093999324809919229340565957649220604220353069140945574426767033956115678455692787165130361475772038242246158437465676561640575484139265543282015038941960903871114795697446460081673324984393030410536559428139922026322240d0
                     0.839116971822218823394529061701520685329629365065637373252492725532861093999324809919229340565957649220604220353069140945574426767033956115678455692787165130361475772038242246158437465676561640575484139265543282015038941960903871114795697446460081673324984393030410536559428139922026322240d0
                     -0.912234428251325905867752441203298113049184797423691774795882219158070891208719078936444726192921387378760391754646026411736863382938836481213773107276416053929086173188798454383669151773002511312516050754361445299882342788913934003615577526165254268112129314312916225002670932579846455984d0
                     0.912234428251325905867752441203298113049184797423691774795882219158070891208719078936444726192921387378760391754646026411736863382938836481213773107276416053929086173188798454383669151773002511312516050754361445299882342788913934003615577526165254268112129314312916225002670932579846455984d0
                     -0.963971927277913791267666131197277221912060327806188856063537593892041580784383056980018125255964715631310434915964230528860410794595084191601289697385612544236272220403934492999313838319825291733578887730721635675941733791214619694779595704939156197670480224954889379517989045735661785126d0
                     0.963971927277913791267666131197277221912060327806188856063537593892041580784383056980018125255964715631310434915964230528860410794595084191601289697385612544236272220403934492999313838319825291733578887730721635675941733791214619694779595704939156197670480224954889379517989045735661785126d0
                     -0.993128599185094924786122388471320278222647130901655896148184131217984717627753780839449402496572209274728940347244190138014860387398777671238413679290367201847836339188347461444646792332145584899441212701351858459632865383975540792652920984266459774968029120927575983179284924813688613835d0
                     0.993128599185094924786122388471320278222647130901655896148184131217984717627753780839449402496572209274728940347244190138014860387398777671238413679290367201847836339188347461444646792332145584899441212701351858459632865383975540792652920984266459774968029120927575983179284924813688613835d0)
                    (0.0d0
                     -0.145561854160895090937030982338686330116326024437937757421488389985547280883319797690515978438208452995626608299795246000707504092115190182042811253447187002470372457933292883703507975536753881579331615614640015805815327091722080864819381527544892781651021779521265900540431012055553672891d0
                     0.145561854160895090937030982338686330116326024437937757421488389985547280883319797690515978438208452995626608299795246000707504092115190182042811253447187002470372457933292883703507975536753881579331615614640015805815327091722080864819381527544892781651021779521265900540431012055553672891d0
                     -0.288021316802401096600792516064600319909018263646033228754281524749244942941842767853339028360216826572323879161733636350902003271905567758843901143729507525384886646610205981232494093081574516844789525175329759819730152450699419072675955214319513023882462779791340455531680871158018268242d0
                     0.288021316802401096600792516064600319909018263646033228754281524749244942941842767853339028360216826572323879161733636350902003271905567758843901143729507525384886646610205981232494093081574516844789525175329759819730152450699419072675955214319513023882462779791340455531680871158018268242d0
                     -0.424342120207438783573668888543788052096445231839634584224258235896486883678801556999872092913827637630956631305098943913797924864474110473199333459250556819016923669984738104087027388383647670098779330223413543312611263011506436808731998015451414629272857713367495721775949562737664635455d0
                     0.424342120207438783573668888543788052096445231839634584224258235896486883678801556999872092913827637630956631305098943913797924864474110473199333459250556819016923669984738104087027388383647670098779330223413543312611263011506436808731998015451414629272857713367495721775949562737664635455d0
                     -0.551618835887219807059018796724313286622060224230679665004305839499604781711684037056980987899030959218004861818608512798653958183996611635847508449733226195548668792514669706808416160939986269551091585406125566015121756240605360065328999449946518423245248805143964991420431881542639461462d0
                     0.551618835887219807059018796724313286622060224230679665004305839499604781711684037056980987899030959218004861818608512798653958183996611635847508449733226195548668792514669706808416160939986269551091585406125566015121756240605360065328999449946518423245248805143964991420431881542639461462d0
                     -0.667138804197412319305966669990339162597029343311402842475467082347194193115696295299510488562791259986150901489919420307523048142546847091972774416411700745033156450750890500424354914235846981457212862489039711834932030096294622795090288424654825641465479311947016697312545137388466493863d0
                     0.667138804197412319305966669990339162597029343311402842475467082347194193115696295299510488562791259986150901489919420307523048142546847091972774416411700745033156450750890500424354914235846981457212862489039711834932030096294622795090288424654825641465479311947016697312545137388466493863d0
                     -0.768439963475677908615877851306228034820976705771369550868726665030018742539701179587071659093864387143939093120876814457832489968302418846471083976451034197764033032366086297439454869563714844696913538815112952826143789162745138811037333502601679752327161716316681569371225695902132486680d0
                     0.768439963475677908615877851306228034820976705771369550868726665030018742539701179587071659093864387143939093120876814457832489968302418846471083976451034197764033032366086297439454869563714844696913538815112952826143789162745138811037333502601679752327161716316681569371225695902132486680d0
                     -0.853363364583317283647250638587567670276105803179343970742991629394242906283835146972289637191696114496702335811831008545899296547885967209740184473890508780169736331389981006660212308403625164966581686898239284567243111672519717338551338816117053514318683081760607099724042650735429254649d0
                     0.853363364583317283647250638587567670276105803179343970742991629394242906283835146972289637191696114496702335811831008545899296547885967209740184473890508780169736331389981006660212308403625164966581686898239284567243111672519717338551338816117053514318683081760607099724042650735429254649d0
                     -0.92009933415040082879018713371496889415914760964822169717613548709098156306940782899173195137206826943420003549276990206325215077700794812180709974986401384973602471717035582221692160934121349568747137344755131559266143261438543501109994757892060825175628562357957559710695647125016597812d0
                     0.92009933415040082879018713371496889415914760964822169717613548709098156306940782899173195137206826943420003549276990206325215077700794812180709974986401384973602471717035582221692160934121349568747137344755131559266143261438543501109994757892060825175628562357957559710695647125016597812d0
                     -0.967226838566306294316622214907695161424693687329846849952971128085120960005080287971837401863924052764629398374960907073058803143907053376894445507462046704610216994197700602184785466271326970236321994512644084623803374718948797379529699595740416702843037100555157964607196471508355962787d0
                     0.967226838566306294316622214907695161424693687329846849952971128085120960005080287971837401863924052764629398374960907073058803143907053376894445507462046704610216994197700602184785466271326970236321994512644084623803374718948797379529699595740416702843037100555157964607196471508355962787d0
                     -0.993752170620389500260242035937940929193338454782329185580873854713096109022992806998128747135239077728837705337024145014399336388663210959135314833863825633328239322247892807655733780076508960540409377175440579665913176924277108971288729535808108159478232964863755577485528451979916104968d0
                     0.9937521706203895002602420359379409291933384547823291855808738547130961090229928069981287471352390777288377053370241450143993363886632109591353148338638256333282393222478928076557337800765089605404093771754405796659131769242771089712887295358081081594782329648637555774855284519799161049686)))
        (weights  #(nil
                    nil
                    (1.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d0
                     1.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d0)
                    (0.888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888d0
                     0.555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555d0
                     0.555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555d0)
                    (0.652145154862546142626936050778000592764651304166106459507470680481248132534089648278016232267741840490201896095236497845575557749674018219142975701678330375140713522955636080197366626048156401327353186073711970735316025600010778721158757861753204933745656092305798641208459046780812497408d0
                     0.652145154862546142626936050778000592764651304166106459507470680481248132534089648278016232267741840490201896095236497845575557749674018219142975701678330375140713522955636080197366626048156401327353186073711970735316025600010778721158757861753204933745656092305798641208459046780812497408d0
                     0.347854845137453857373063949221999407235348695833893540492529319518751867465910351721983767732258159509798103904763502154424442250325981780857024298321669624859286477044363919802633373951843598672646813926288029264683974399989221278841242138246795066254343907694201358791540953219187502570d0
                     0.347854845137453857373063949221999407235348695833893540492529319518751867465910351721983767732258159509798103904763502154424442250325981780857024298321669624859286477044363919802633373951843598672646813926288029264683974399989221278841242138246795066254343907694201358791540953219187502570d0)
                    (0.568888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888d0
                     0.478628670499366468041291514835638192912295553343141539972727667333838267152512456975562125061604110779446420947412229974292790167053187422023601976275538106998102019955970843343501735534169032469562210486353672159885926291364482848264050563713351360653192989328612756518538973258163438881d0
                     0.478628670499366468041291514835638192912295553343141539972727667333838267152512456975562125061604110779446420947412229974292790167053187422023601976275538106998102019955970843343501735534169032469562210486353672159885926291364482848264050563713351360653192989328612756518538973258163438881d0
                     0.236926885056189087514264040719917362643260002212414015582827888221717288403043098579993430493951444776109134608143325581262765388502368133531953579280017448557453535599584712212053820021386523085993345069201883395669629264191072707291504991842204194902362566226942799037016582297392116684d0
                     0.236926885056189087514264040719917362643260002212414015582827888221717288403043098579993430493951444776109134608143325581262765388502368133531953579280017448557453535599584712212053820021386523085993345069201883395669629264191072707291504991842204194902362566226942799037016582297392116684d0)
                    (0.360761573048138607569833513837716111661521892746745482289739240237140037837261718320962201988819347943117209140370798589879890278364321070776787211408581892211450272252575777112600073236882859163160289511180051740813685547074482472486101183259931449817216402425586777526768199930950310687315051499783d0
                     0.360761573048138607569833513837716111661521892746745482289739240237140037837261718320962201988819347943117209140370798589879890278364321070776787211408581892211450272252575777112600073236882859163160289511180051740813685547074482472486101183259931449817216402425586777526768199930950310687315051499783d0
                     0.467913934572691047389870343989550994811655605769210535311625319963914201620398127031110092584791982304766268789754797100928362554173502954593563559273386659336482592638255901803028127356350253624170461931825900099756987095900533474080074634376824431808173206369174103416261765346292788891715051499783d0
                     0.467913934572691047389870343989550994811655605769210535311625319963914201620398127031110092584791982304766268789754797100928362554173502954593563559273386659336482592638255901803028127356350253624170461931825900099756987095900533474080074634376824431808173206369174103416261765346292788891715051499783d0
                     0.171324492379170345040296142172732893526822501484043982398635439798945760542340154647927705426388669752116522069874404309191747167462175974629649229318031448452067135109168321084371799406766887212669248556994048159429327357024984053433824182363244118374610391205239119044219703570297749781215051499783d0
                     0.171324492379170345040296142172732893526822501484043982398635439798945760542340154647927705426388669752116522069874404309191747167462175974629649229318031448452067135109168321084371799406766887212669248556994048159429327357024984053433824182363244118374610391205239119044219703570297749781215051499783d0)
                    (0.417959183673469387755102040816326530612244897959183673469387755102040816326530612244897959183673469387755102040816326530612244897959183673469387755102040816326530612244897959183673469387755102040816326530612244897959183673469387755102040816326530612244897959183673469387755102040816326530d0
                     0.381830050505118944950369775488975133878365083533862734751083451030705546434129708348684659344044801450314671764585357334492895677638383756244318756637381699426351375030942512206904808219240596765715545816614021135044127667477389099828608650117997361185042096413231686710944989236244135904415051499783d0
                     0.381830050505118944950369775488975133878365083533862734751083451030705546434129708348684659344044801450314671764585357334492895677638383756244318756637381699426351375030942512206904808219240596765715545816614021135044127667477389099828608650117997361185042096413231686710944989236244135904415051499783d0
                     0.279705391489276667901467771423779582486925065226598764537014032693618810430562676813240942901197618766323375213372051519135636979563119944371352657812336854556359202533690919264319483350249334826790927964286230210815602062169280488966762830821416720073836555398966211997331706130630553785915051499783d0
                     0.279705391489276667901467771423779582486925065226598764537014032693618810430562676813240942901197618766323375213372051519135636979563119944371352657812336854556359202533690919264319483350249334826790927964286230210815602062169280488966762830821416720073836555398966211997331706130630553785915051499783d0
                     0.129484966168869693270611432679082018328587402259946663977208638724655234972042308715625418162920845089484402001634427881065344893818904462649634707999261037854024116312917588936938973736632517387085362953793626205160678433618636533653608110897320612618672368595965366597920974917635089702915051499783d0
                     0.129484966168869693270611432679082018328587402259946663977208638724655234972042308715625418162920845089484402001634427881065344893818904462649634707999261037854024116312917588936938973736632517387085362953793626205160678433618636533653608110897320612618672368595965366597920974917635089702915051499783d0)
                    (0.362683783378361982965150449277195612194146039894330540524823067566686734723906677324366042084828509550258769926296706552925821556989517384499557600786207684277835038286254630577100755337326971471489426832878043182277907784672296553554819960140248776750592897656099330902763273753782612750215051499783d0
                     0.362683783378361982965150449277195612194146039894330540524823067566686734723906677324366042084828509550258769926296706552925821556989517384499557600786207684277835038286254630577100755337326971471489426832878043182277907784672296553554819960140248776750592897656099330902763273753782612750215051499783d0
                     0.313706645877887287337962201986601313260328999002734937690263945074956271942173496961698076233928556049427574641077808616247246832265561605689062427646975899462250311877656255946328722202152043162646779472160382260129527689865250972318515799835315606241975173697256042395392373283878965791915051499783d0
                     0.313706645877887287337962201986601313260328999002734937690263945074956271942173496961698076233928556049427574641077808616247246832265561605689062427646975899462250311877656255946328722202152043162646779472160382260129527689865250972318515799835315606241975173697256042395392373283878965791915051499783d0
                     0.222381034453374470544355994426240884430130870051249564725909289293616814570449040853653142377197927842159266101212218123111437579852572241938182667453209057790861328953684040278939864887600438569720215748206325324719559022863157065131996558973354544060595281988067161677962118370430668823315051499783d0
                     0.222381034453374470544355994426240884430130870051249564725909289293616814570449040853653142377197927842159266101212218123111437579852572241938182667453209057790861328953684040278939864887600438569720215748206325324719559022863157065131996558973354544060595281988067161677962118370430668823315051499783d0
                     0.101228536290376259152531354309962190115394091051684957059003698064740178763470784860282739304045006558154389331413266707715494030892348767873197304113607358469053320882405073197630657572920546796143577946755249232873005502599295408994667681051081072946836646658577465034614371214200856686615051499783d0
                     0.101228536290376259152531354309962190115394091051684957059003698064740178763470784860282739304045006558154389331413266707715494030892348767873197304113607358469053320882405073197630657572920546796143577946755249232873005502599295408994667681051081072946836646658577465034614371214200856686615051499783d0)
                    (0.330239355001259763164525069286974048878810783572688334593096497858402620307382212144116906021667926429831191735953640715545477450239355001259763164525069286974048878810783572688334593096497858402620307382212144116906021667926429831191735953640715545477450239355001259763164525069286974048d0
                     0.180648160694857404058472031242912809514337821732040484498335906471357290544946269764594977303199704147607467960257793722679626846063012723179010080474557737481297396486827870555637043228886047714853923032902554110219821848121399005741349480006523487580823996820087127157666611178681698331215051499783d0
                     0.180648160694857404058472031242912809514337821732040484498335906471357290544946269764594977303199704147607467960257793722679626846063012723179010080474557737481297396486827870555637043228886047714853923032902554110219821848121399005741349480006523487580823996820087127157666611178681698331215051499783d0
                     0.08127438836157441197189215811052365067566172078241075071110767688068668630845206294557855470294257695779407331796303809459004879509395575952814137884475085376733397234956750732455812793813386830166739515724589680261123473969563167200333467476663659297529913527508431148499431108734619250721505149978320d0
                     0.08127438836157441197189215811052365067566172078241075071110767688068668630845206294557855470294257695779407331796303809459004879509395575952814137884475085376733397234956750732455812793813386830166739515724589680261123473969563167200333467476663659297529913527508431148499431108734619250721505149978320d0
                     0.312347077040002840068630406584443665598754861261904645554011165599143897324019316570121921888006353852295477318164697311639181809887527145960037090147840588557258909075764598405964135572237681654656152224542202496926638080274512773579379029213624522882074935779961400209707418114451390197315051499783d0
                     0.312347077040002840068630406584443665598754861261904645554011165599143897324019316570121921888006353852295477318164697311639181809887527145960037090147840588557258909075764598405964135572237681654656152224542202496926638080274512773579379029213624522882074935779961400209707418114451390197315051499783d0
                     0.260610696402935462318742869418632849771840204437299951939997002119610815668891244647646093095017401827387385535637650513318403823835826870702929868270316176707085282682448237369673396712493473127512375894203274531789294497945241633080068839192857623823076812447366531315259942263280984799815051499783d0
                     0.260610696402935462318742869418632849771840204437299951939997002119610815668891244647646093095017401827387385535637650513318403823835826870702929868270316176707085282682448237369673396712493473127512375894203274531789294497945241633080068839192857623823076812447366531315259942263280984799815051499783d0)
                    (0.29552422471475287017389299465133832942104671702685360135430802975599593821715232927035659579375421672271716440125255838681849078955200582600193634249418696660956271864888416804323130506153586740908305127066386528748390174687472659751595445077515891455654830832998639360593491238235667024d0
                     0.29552422471475287017389299465133832942104671702685360135430802975599593821715232927035659579375421672271716440125255838681849078955200582600193634249418696660956271864888416804323130506153586740908305127066386528748390174687472659751595445077515891455654830832998639360593491238235667024d0
                     0.269266719309996355091226921569469352859759938460883795800563276242153432319179276764226636709252760755595811450368698308692923469381145241556465884663442371165601443225996014172904452803034441129790297706714253753480628460839927657500691168674984281408628886853320804215041950888191639189d0
                     0.269266719309996355091226921569469352859759938460883795800563276242153432319179276764226636709252760755595811450368698308692923469381145241556465884663442371165601443225996014172904452803034441129790297706714253753480628460839927657500691168674984281408628886853320804215041950888191639189d0
                     0.219086362515982043995534934228163192458771870522677089880956543635199910652951281242683993177202192786591216872812887634766626908066947568830921184331665667710526991532207753677265282667102787824685101020883217332006427348325475625066841588534942071161341022729156547776892831330068870280d0
                     0.219086362515982043995534934228163192458771870522677089880956543635199910652951281242683993177202192786591216872812887634766626908066947568830921184331665667710526991532207753677265282667102787824685101020883217332006427348325475625066841588534942071161341022729156547776892831330068870280d0
                     0.149451349150580593145776339657697332402556639669427367835477268753238654726630010945947264634731951914005752561045436338234451706745497601471371601193710952879813482886511877095356643963933377393990920169020464908381561877915752257830034342778536175692764212879241228297015017259084289733d0
                     0.149451349150580593145776339657697332402556639669427367835477268753238654726630010945947264634731951914005752561045436338234451706745497601471371601193710952879813482886511877095356643963933377393990920169020464908381561877915752257830034342778536175692764212879241228297015017259084289733d0
                     0.06667134430868813759356880989333179285786483432015814512869488161341206408408710177678550968505887782109005471452041933148750712625440376213930498731699404163449536370640018701124231550439352624245062983271819871864748056604411786208647844923637855718071756920829502610511528815279442167d0
                     0.06667134430868813759356880989333179285786483432015814512869488161341206408408710177678550968505887782109005471452041933148750712625440376213930498731699404163449536370640018701124231550439352624245062983271819871864748056604411786208647844923637855718071756920829502610511528815279442167d0)
                    (0.272925086777900630714483528336342189156041969894783747597600411453225306039158853011666864480717294570108422922275736128549981363834177686991539805392619245433098246951060803874656688509502362316215130067943920757773571626385479199332013184827037640890454743268596082448896301710154524007d0
                     0.262804544510246662180688869890509195372764677603144556380055371485512803339545253552416997017026705959132335234890568593081565995938822048651666227712215340600756456342228884681424626511719758629111071587517561404112691682684627445261552370208659100020113361160717909799073251778614129202d0
                     0.262804544510246662180688869890509195372764677603144556380055371485512803339545253552416997017026705959132335234890568593081565995938822048651666227712215340600756456342228884681424626511719758629111071587517561404112691682684627445261552370208659100020113361160717909799073251778614129202d0
                     0.233193764591990479918523704843175139431798172316958509027319722121932213218880995954039948438261684416551109191678731811292015079506081013343360380137389781491210109000254593377071622573107342108144121879324690083723386126788957396680923342572305595646773575188806257404604346831361133288d0
                     0.233193764591990479918523704843175139431798172316958509027319722121932213218880995954039948438261684416551109191678731811292015079506081013343360380137389781491210109000254593377071622573107342108144121879324690083723386126788957396680923342572305595646773575188806257404604346831361133288d0
                     0.186290210927734251426097641431655891691284748040203411781506404173872348008785502469330758517979586131100964538966335431561817178298595244405987661318786946245612484190316818698108314855516852851128437880883715124059630869989659615104311827687335885317401259064477294820041034582112688968d0
                     0.186290210927734251426097641431655891691284748040203411781506404173872348008785502469330758517979586131100964538966335431561817178298595244405987661318786946245612484190316818698108314855516852851128437880883715124059630869989659615104311827687335885317401259064477294820041034582112688968d0
                     0.125580369464904624634694299223940100197615791395403500663934010817914575132471277634687641714054530397865058545084521935984106883937750607329088434736375676900592547462758934605515222646599223090998462329695124883718895987947059280952896273682394932770450907254052456339866567706347036988d0
                     0.125580369464904624634694299223940100197615791395403500663934010817914575132471277634687641714054530397865058545084521935984106883937750607329088434736375676900592547462758934605515222646599223090998462329695124883718895987947059280952896273682394932770450907254052456339866567706347036988d0
                     0.055668567116173666482753720442548578728515625696898148348384285674155407280737543883691222072318845810296321028241974163805504180401662242774127393398922632045279279528910366700551869158305642162510341288606948125498609519396956662334309593435785665800033525697648040411966648226978214171d0
                     0.055668567116173666482753720442548578728515625696898148348384285674155407280737543883691222072318845810296321028241974163805504180401662242774127393398922632045279279528910366700551869158305642162510341288606948125498609519396956662334309593435785665800033525697648040411966648226978214171d0)
                    (0.249147045813402785000562436042951210830460902569618831395351003116279427457288043031156800618042353064833476117877185833058511073603649688039642103770085429415027372210917282570196843016465919240216198207962552073243408577661378857966254032934783717074290411156565037184697232332501572093d0
                     0.249147045813402785000562436042951210830460902569618831395351003116279427457288043031156800618042353064833476117877185833058511073603649688039642103770085429415027372210917282570196843016465919240216198207962552073243408577661378857966254032934783717074290411156565037184697232332501572093d0
                     0.233492536538354808760849898924878056259409972199754874730523497821492000079411675280679026508563690466738756439708868833898542788408916096619750388473807535332481451794887503888121627928030424895983087822935772907916442310300187953065470731537580927084066998901889128195675313116519342326d0
                     0.233492536538354808760849898924878056259409972199754874730523497821492000079411675280679026508563690466738756439708868833898542788408916096619750388473807535332481451794887503888121627928030424895983087822935772907916442310300187953065470731537580927084066998901889128195675313116519342326d0
                     0.20316742672306592174906445580979837650651814727459014639859456579764563251047284379514439506460523243116042933686325996496137135190210132907910420189599423685656890245260738280276852445703846681240064758134063899875305215261728059344541572232792796333955754526142350078389928605285076759d0
                     0.20316742672306592174906445580979837650651814727459014639859456579764563251047284379514439506460523243116042933686325996496137135190210132907910420189599423685656890245260738280276852445703846681240064758134063899875305215261728059344541572232792796333955754526142350078389928605285076759d0
                     0.160078328543346226334652529543359071872011730490864177909899544157954225173291150681656552637057730527074877096812802627243763860882649044675031002434095112136798690206599792785600980463759139983872449388725863360590616677428638245529844487045839628388461094046672887477662582312492424738d0
                     0.160078328543346226334652529543359071872011730490864177909899544157954225173291150681656552637057730527074877096812802627243763860882649044675031002434095112136798690206599792785600980463759139983872449388725863360590616677428638245529844487045839628388461094046672887477662582312492424738d0
                     0.106939325995318430960254718193996224214570173470324880005126042102818993627497576540537318096316457413576359333141144161170330516963550844848008652326919600501143904476492048293555153585760791070524921807103379547042489571283093096780646759835851729890353613745182808901282281139658803725d0
                     0.106939325995318430960254718193996224214570173470324880005126042102818993627497576540537318096316457413576359333141144161170330516963550844848008652326919600501143904476492048293555153585760791070524921807103379547042489571283093096780646759835851729890353613745182808901282281139658803725d0
                     0.047175336386511827194615961485017060317029073994847089560505347003809721152038710670825907075414536096616101675596738579667480408239132996738463651099098085757979678858495989659756870548945257997002695191931793112453990710709421253212368266318016034223270336888266637456783305036418788718d0
                     0.047175336386511827194615961485017060317029073994847089560505347003809721152038710670825907075414536096616101675596738579667480408239132996738463651099098085757979678858495989659756870548945257997002695191931793112453990710709421253212368266318016034223270336888266637456783305036418788718d0)
                    (0.232551553230873910194589515268835948156627477306797986118665439344760024080703401382722062042741363420684100004779325458646137966817287496608175928855249534570213890893211572532251852931173610494289814969135648456327777007097686418365739045059724380403701083021762342441663120983800304479d0
                     0.226283180262897238412090186039776618434757737615557019864968548771575663120499971041826318666313929721126588910752683642655796807876342890249543815187202323744106602725693587514714051786116186939760864095084673373898671235167289838211499348544059116961374894892264041565589543760991944553d0
                     0.226283180262897238412090186039776618434757737615557019864968548771575663120499971041826318666313929721126588910752683642655796807876342890249543815187202323744106602725693587514714051786116186939760864095084673373898671235167289838211499348544059116961374894892264041565589543760991944553d0
                     0.207816047536888502312523219306052763386582609199503549219111119916538543565084432260595528046611926575982640705169072841678263124687769959985892906619671194948358727947568694243082445389738418444947274767734848060612050489829657323587730823937436972785130059481423084046788171142190066189d0
                     0.207816047536888502312523219306052763386582609199503549219111119916538543565084432260595528046611926575982640705169072841678263124687769959985892906619671194948358727947568694243082445389738418444947274767734848060612050489829657323587730823937436972785130059481423084046788171142190066189d0
                     0.178145980761945738280046691996097995512812650661016502986794440289145069498748723494716951222312701506967049720074394609783488324189387424191761741660338707839944532197692499676029069977702940122039837242995359345457807307332807335176130825369621911781149743287845111328296817971718997918d0
                     0.178145980761945738280046691996097995512812650661016502986794440289145069498748723494716951222312701506967049720074394609783488324189387424191761741660338707839944532197692499676029069977702940122039837242995359345457807307332807335176130825369621911781149743287845111328296817971718997918d0
                     0.13887351021978723846360177686887146762186271826329822764635501650577252153488953572518154285460562586240968427612598993158396182239240005614034918145022458198420438196433233579378463402365292665714819848919242851147334246468017544283626608211009382031211507981273098487693429772829581883d0
                     0.13887351021978723846360177686887146762186271826329822764635501650577252153488953572518154285460562586240968427612598993158396182239240005614034918145022458198420438196433233579378463402365292665714819848919242851147334246468017544283626608211009382031211507981273098487693429772829581883d0
                     0.092121499837728447914421775953797120923683999862236839088391547339593729727632926291897815755731972868846420618028111540528075582206133013257502545493112279952877826337858026009659544723971247627299217505286699804887879230664197596556209375762720295186001488262936418218706121020678618503d0
                     0.092121499837728447914421775953797120923683999862236839088391547339593729727632926291897815755731972868846420618028111540528075582206133013257502545493112279952877826337858026009659544723971247627299217505286699804887879230664197596556209375762720295186001488262936418218706121020678618503d0
                     0.040484004765315879520021592200986060041986545744988868135046607504994460512792710494420812433053161754325565767460084704447345355239322907870861845161826144245400983380249070496604327633231474961659700415138166675506360768777029254449294021746205692772378192751919188742853487995291744497d0
                     0.040484004765315879520021592200986060041986545744988868135046607504994460512792710494420812433053161754325565767460084704447345355239322907870861845161826144245400983380249070496604327633231474961659700415138166675506360768777029254449294021746205692772378192751919188742853487995291744497d0)
                    (0.21526385346315779019587644331626003527499755805412880021977639254361878735399460400102444141081957823725667233243677099294816597646493018903560190805098142804175780269156508228762641736544919294628120366203334537646052256431063441291265469834948726656273089751239371654942515513388778326d0
                     0.21526385346315779019587644331626003527499755805412880021977639254361878735399460400102444141081957823725667233243677099294816597646493018903560190805098142804175780269156508228762641736544919294628120366203334537646052256431063441291265469834948726656273089751239371654942515513388778326d0
                     0.205198463721295603965924065661218055710339061309419451716897290283367144825249720339431839991890895724369269442449428728453485613385064486591870230214031667141787332993474827839138111325684812825439676020905052976535424973123755325146919285189807239470704996472103177329225696533700546857d0
                     0.205198463721295603965924065661218055710339061309419451716897290283367144825249720339431839991890895724369269442449428728453485613385064486591870230214031667141787332993474827839138111325684812825439676020905052976535424973123755325146919285189807239470704996472103177329225696533700546857d0
                     0.185538397477937813741716590125157036248922602937331659020034925069098350263525444425552731146712222982561121505728918899077896497425216089508552524152836436072864040600272323796971413850753456093331227890449938852384485366393922617921879824760615027451493555701290988950306735641006783340d0
                     0.185538397477937813741716590125157036248922602937331659020034925069098350263525444425552731146712222982561121505728918899077896497425216089508552524152836436072864040600272323796971413850753456093331227890449938852384485366393922617921879824760615027451493555701290988950306735641006783340d0
                     0.157203167158193534569601938623842156605668037337323374969317043874768176369608298513958093362418076276853151999081188501885437492064657626748924291037264601987001022195647459107842322805610686116907713218466935160138377442838502265889923868443908468502286490512409657021586673314609200832d0
                     0.157203167158193534569601938623842156605668037337323374969317043874768176369608298513958093362418076276853151999081188501885437492064657626748924291037264601987001022195647459107842322805610686116907713218466935160138377442838502265889923868443908468502286490512409657021586673314609200832d0
                     0.121518570687903184689414809072476625956669345690074672291075392543159743892526492318819906270375007148915550653059256994281157431340886854809642125714454608028918541061542078620056467545629329602540610239636717985405900755004972904989241013019107235734182108332966386746482186753934196843d0
                     0.121518570687903184689414809072476625956669345690074672291075392543159743892526492318819906270375007148915550653059256994281157431340886854809642125714454608028918541061542078620056467545629329602540610239636717985405900755004972904989241013019107235734182108332966386746482186753934196843d0
                     0.080158087159760209805633277062854309583697785394594765201399065489571474457287169863536190819137755968622501590803884748795309138257260443437675511984474094779738772370053661057717852265395454912313554662497115946457665357652160093748935412771093753519883864927947562847351637873671292957d0
                     0.080158087159760209805633277062854309583697785394594765201399065489571474457287169863536190819137755968622501590803884748795309138257260443437675511984474094779738772370053661057717852265395454912313554662497115946457665357652160093748935412771093753519883864927947562847351637873671292957d0
                     0.035119460331751863031832876138191780619705609277127276581499890196416322837808270537676796998646463661421732476440551134558547851061984309867733408845957163947932488087444567290647414841477067503186014306010893702617623540676052379390445897465981008758718086540888510555621914760952620092d0
                     0.035119460331751863031832876138191780619705609277127276581499890196416322837808270537676796998646463661421732476440551134558547851061984309867733408845957163947932488087444567290647414841477067503186014306010893702617623540676052379390445897465981008758718086540888510555621914760952620092d0)
                    (0.202578241925561272880620199967519314838662158009477356796704116051435398754746074093393440712788032135351482670829990177309524628871948219267566586913906261225608544955864303183650502997822345141692461039780387099734419081738429057776377123696471015818335165654512973860293207612554931902d0
                     0.198431485327111576456118326443839324818692559957541993484737927929124797533434268133314999164817823207660208548893099176479147759161042113208661359998885753119270242641592723132704466789281604583409822872951738512417256972150773435642960523197328026860468402830148832276567031679630335628d0
                     0.198431485327111576456118326443839324818692559957541993484737927929124797533434268133314999164817823207660208548893099176479147759161042113208661359998885753119270242641592723132704466789281604583409822872951738512417256972150773435642960523197328026860468402830148832276567031679630335628d0
                     0.186161000015562211026800561866422824506226012277928402815495727310013255502699160618949768886099323605399777090013844353067270217388182221891365081752440239445463349845525498385498770455869834674382300553209446722504179210216696964972001434726003924068529180991028084022684134798052183713d0
                     0.186161000015562211026800561866422824506226012277928402815495727310013255502699160618949768886099323605399777090013844353067270217388182221891365081752440239445463349845525498385498770455869834674382300553209446722504179210216696964972001434726003924068529180991028084022684134798052183713d0
                     0.166269205816993933553200860481208811130900180098412907321865190563553563212278517710705174292415536214844615406571852227418171463017096775492517467183609820699122922385864963778999995473764431119924775697375085415328906873276139207316535701337313861942563869456339180192657606119593778869d0
                     0.166269205816993933553200860481208811130900180098412907321865190563553563212278517710705174292415536214844615406571852227418171463017096775492517467183609820699122922385864963778999995473764431119924775697375085415328906873276139207316535701337313861942563869456339180192657606119593778869d0
                     0.139570677926154314447804794511028322520850275315511243202391128631088444541907811680768257363571333638149088893276639904111091724549140962823334240644012637278479450575994119727336077130283564308863198401818146913364643015545810123737571855782729943585483298113249996123940740517204109653d0
                     0.139570677926154314447804794511028322520850275315511243202391128631088444541907811680768257363571333638149088893276639904111091724549140962823334240644012637278479450575994119727336077130283564308863198401818146913364643015545810123737571855782729943585483298113249996123940740517204109653d0
                     0.107159220467171935011869546685869303415543715758101980687022389121877994852315799725685857137608624044398087678375055581271810498840567823997470827861536162132383715090115548956500606448858109428368314657276212633795571025550997678837417685000585915368952941049509169130630333925429261260d0
                     0.107159220467171935011869546685869303415543715758101980687022389121877994852315799725685857137608624044398087678375055581271810498840567823997470827861536162132383715090115548956500606448858109428368314657276212633795571025550997678837417685000585915368952941049509169130630333925429261260d0
                     0.070366047488108124709267416450667338466708032754330719825907292914387055512874237044840452066693939219355489858595040538804614843277291079295655604153758247907096890237613378079766853518363742652238906597882622160277858183714862512415356376024732724609113642355723078407532199603716947543d0
                     0.070366047488108124709267416450667338466708032754330719825907292914387055512874237044840452066693939219355489858595040538804614843277291079295655604153758247907096890237613378079766853518363742652238906597882622160277858183714862512415356376024732724609113642355723078407532199603716947543d0
                     0.030753241996117268354628393577204417721748144833434074264228285504237189467117168039038770732399404002516991188859473130193131179330704913657212124948804008805379156745361616347367978684667540661966450699596554092444375178675505548189967862083070095655721082376745172915841349573527218529d0
                     0.030753241996117268354628393577204417721748144833434074264228285504237189467117168039038770732399404002516991188859473130193131179330704913657212124948804008805379156745361616347367978684667540661966450699596554092444375178675505548189967862083070095655721082376745172915841349573527218529d0)
                    (0.189450610455068496285396723208283105146908988395902975037513245200022890769133006300133977833533952282533805643209642329708365217540400731169038818943184441159581561276711845961617349817597427978336891209019602629426490614729343981816403794061883841216345087440356964646518869952610500940d0
                     0.189450610455068496285396723208283105146908988395902975037513245200022890769133006300133977833533952282533805643209642329708365217540400731169038818943184441159581561276711845961617349817597427978336891209019602629426490614729343981816403794061883841216345087440356964646518869952610500940d0
                     0.182603415044923588866763667969219939383556223654649282418495144379430464950111174960400425116985275314045024746816472035932341453176750367723128790193985504959328898036777933983526094546329357687103938118464303596748875141302288811161357351978863066074946500951991488131704863818070716480d0
                     0.182603415044923588866763667969219939383556223654649282418495144379430464950111174960400425116985275314045024746816472035932341453176750367723128790193985504959328898036777933983526094546329357687103938118464303596748875141302288811161357351978863066074946500951991488131704863818070716480d0
                     0.169156519395002538189312079030359962211639473416028281745082935680803664209930530932155322542078228047561666302917431130107024519401748567734525095856277629889126427742673505930431984799165331915253374075225714752944289644099596165744893049441308633212849006636356165964497026291062284003d0
                     0.169156519395002538189312079030359962211639473416028281745082935680803664209930530932155322542078228047561666302917431130107024519401748567734525095856277629889126427742673505930431984799165331915253374075225714752944289644099596165744893049441308633212849006636356165964497026291062284003d0
                     0.149595988816576732081501730547478548970491068207836466805421962187360404020417982451778638403059258074066697871098155201744160882394780436933273510870768389277556385847859811279100254785473938753417415963264645336667666655852496503102021131682735147573308936279429959084830995377934187260d0
                     0.149595988816576732081501730547478548970491068207836466805421962187360404020417982451778638403059258074066697871098155201744160882394780436933273510870768389277556385847859811279100254785473938753417415963264645336667666655852496503102021131682735147573308936279429959084830995377934187260d0
                     0.124628971255533872052476282192016420144886859222202679944750590429410963921465353597969190017655684360215049447227692316329874335616675995121980302753079395038117730273119473818553648417085338204981585755734162926525796558645613377565128381071046788453182664999381211102457315597086949018d0
                     0.124628971255533872052476282192016420144886859222202679944750590429410963921465353597969190017655684360215049447227692316329874335616675995121980302753079395038117730273119473818553648417085338204981585755734162926525796558645613377565128381071046788453182664999381211102457315597086949018d0
                     0.095158511682492784809925107602246226355263503183712658156822287229631759577681959447024957320804986489012589149620941483060348121053322847052172681839363105231396062664202588393793557586218307809275498456092642437860616138072614664550647420205174309346203838681681887393732372946374426442d0
                     0.095158511682492784809925107602246226355263503183712658156822287229631759577681959447024957320804986489012589149620941483060348121053322847052172681839363105231396062664202588393793557586218307809275498456092642437860616138072614664550647420205174309346203838681681887393732372946374426442d0
                     0.062253523938647892862843836994377694274986508352906857901303515819535738887043819061211709536851734532667740843701058748409510273393068858437504346927873553903025057832867123285444212522188507244473491492747187852852988178585221001436933295341385355557255705183145487562197335841444996462d0
                     0.062253523938647892862843836994377694274986508352906857901303515819535738887043819061211709536851734532667740843701058748409510273393068858437504346927873553903025057832867123285444212522188507244473491492747187852852988178585221001436933295341385355557255705183145487562197335841444996462d0
                     0.027152459411754094851780572456018103512267375566760797990610319073804113664216173249325779229030880899897425995408606754708375197423252195828376452615467980541867876325787717347532897525941790407157804929451740466973277068712825494622615576217602858565908259827656836114061220174549193587d0
                     0.027152459411754094851780572456018103512267375566760797990610319073804113664216173249325779229030880899897425995408606754708375197423252195828376452615467980541867876325787717347532897525941790407157804929451740466973277068712825494622615576217602858565908259827656836114061220174549193587d0)
                    (0.179446470356206525458265644261885621448780319897668523667668697955596754606280259404528445752504277600864981189385735243568298633187608111185110886678062293680815873732530317006970687776617717495755259606172246012221492335380753767442050324104832456918663676150710454353754536847107482930d0
                     0.176562705366992646325270990113197239150924418000748118043144406913209036207282680452334690684086655930100609211163104345627403466013027398098384050493409547031545866028558851804954711414069902334048534165747684193786885768790037650565710655688860120217673041730472383188639007907213062964d0
                     0.176562705366992646325270990113197239150924418000748118043144406913209036207282680452334690684086655930100609211163104345627403466013027398098384050493409547031545866028558851804954711414069902334048534165747684193786885768790037650565710655688860120217673041730472383188639007907213062964d0
                     0.168004102156450044509970663788323155021198128965074014269955851340323106500024586561203849738919771823620721348699564444445231804973463892913777830033827572263216458769266011720207735782925955182943467321363065421428618506873427254605617110277795675403801681454326726684860352437750758970d0
                     0.168004102156450044509970663788323155021198128965074014269955851340323106500024586561203849738919771823620721348699564444445231804973463892913777830033827572263216458769266011720207735782925955182943467321363065421428618506873427254605617110277795675403801681454326726684860352437750758970d0
                     0.15404576107681028808143159480195861194048305847101793438526471135144674185967645109161744935345090887642415341906524012571603631082988094781172580117840235693092677612162852253402855698997988465550091491955388678041988806049572712091522616485678727333824478994425059084845127488415666071d0
                     0.15404576107681028808143159480195861194048305847101793438526471135144674185967645109161744935345090887642415341906524012571603631082988094781172580117840235693092677612162852253402855698997988465550091491955388678041988806049572712091522616485678727333824478994425059084845127488415666071d0
                     0.135136368468525473286319981702350197372125853234489020377994610688867242989938544097509448313584322928296686702397398062711496265558389999439183570717574365965555974046595185225051014580127767162098093285546059269940332994201568919269462928721650298531439963678031546126608569171629643966d0
                     0.135136368468525473286319981702350197372125853234489020377994610688867242989938544097509448313584322928296686702397398062711496265558389999439183570717574365965555974046595185225051014580127767162098093285546059269940332994201568919269462928721650298531439963678031546126608569171629643966d0
                     0.111883847193403971094788385626355926735843424263077050018486482449095679643609835615221704527766309730393385623448500500908911809892676919717541693211493326369204052088970736171273474215780674243206005811718259283042870208728910687498517586131677180137963101946108354942289313999386744732d0
                     0.111883847193403971094788385626355926735843424263077050018486482449095679643609835615221704527766309730393385623448500500908911809892676919717541693211493326369204052088970736171273474215780674243206005811718259283042870208728910687498517586131677180137963101946108354942289313999386744732d0
                     0.085036148317179180883535370191062073850491389218505475766410360738783992787482647746987604293572999956487342241309297580100905162176538550486364837180266159464298525444769766251067126756125637218460741604511828958348211304306956123101387992817532295612203344917661311087778072905883521616d0
                     0.085036148317179180883535370191062073850491389218505475766410360738783992787482647746987604293572999956487342241309297580100905162176538550486364837180266159464298525444769766251067126756125637218460741604511828958348211304306956123101387992817532295612203344917661311087778072905883521616d0
                     0.05545952937398720112944016535824466051284625195322884699372407871025742600333463837327578874482190110229729948361993682235808278320637986463974588616253457520627261819462844867265815027136955762476437490079187876950259020965342135831574061830389232506280472228953427394195758808731629978d0
                     0.05545952937398720112944016535824466051284625195322884699372407871025742600333463837327578874482190110229729948361993682235808278320637986463974588616253457520627261819462844867265815027136955762476437490079187876950259020965342135831574061830389232506280472228953427394195758808731629978d0
                     0.024148302868547931960110026287565324691697315945025278311185148830218396705510486359585241467544990851947311375604090496347783080755838371300720887683460949928571792439317319117273886101311762831100238187681214317419856779259574002007311781149388603236537515964259586002538552181935029281d0
                     0.024148302868547931960110026287565324691697315945025278311185148830218396705510486359585241467544990851947311375604090496347783080755838371300720887683460949928571792439317319117273886101311762831100238187681214317419856779259574002007311781149388603236537515964259586002538552181935029281d0)
                    (0.169142382963143591840656470134986610334105819370343880269875191549059444191554642170920914039778153588223628785809602265199110957010540967972432878292121224556493406315903451243393777332638234021387730668200849040180302646442609051467510096189199791936113888712375938286368653086612797256d0
                     0.169142382963143591840656470134986610334105819370343880269875191549059444191554642170920914039778153588223628785809602265199110957010540967972432878292121224556493406315903451243393777332638234021387730668200849040180302646442609051467510096189199791936113888712375938286368653086612797256d0
                     0.164276483745832722986053776465927590412338953997352953244496970911648421845208007755363757747060489028464811350954504199364472637918528287111320757655767342414842170478964748657081550559093192874087258156570858740435182770077267835844839411836804114122357148621652253360239757820472778869d0
                     0.164276483745832722986053776465927590412338953997352953244496970911648421845208007755363757747060489028464811350954504199364472637918528287111320757655767342414842170478964748657081550559093192874087258156570858740435182770077267835844839411836804114122357148621652253360239757820472778869d0
                     0.154684675126265244925418003836374772193218396267354172666641914727782869230643730312346923852223681061082313863575915131446490450861944028586471078604756960848140452332460126473534448107923572859324597724838057005719079129399188757527079674300437072057296124385346588594892631355719462535d0
                     0.154684675126265244925418003836374772193218396267354172666641914727782869230643730312346923852223681061082313863575915131446490450861944028586471078604756960848140452332460126473534448107923572859324597724838057005719079129399188757527079674300437072057296124385346588594892631355719462535d0
                     0.140642914670650651204731303751947228095502410330972559883456195260465586927749897030662769026913321565560820021727577068554417025123171864430612900482402611894476663059108476332232761398248606167981139679368394824913551572213861077157061909792816681261249883678939170095126994734516863331d0
                     0.140642914670650651204731303751947228095502410330972559883456195260465586927749897030662769026913321565560820021727577068554417025123171864430612900482402611894476663059108476332232761398248606167981139679368394824913551572213861077157061909792816681261249883678939170095126994734516863331d0
                     0.122555206711478460184519126800201555228163897333439097167253513654057439599403681838197219925041014900716770845581016548679611501882713304014014340413792128839404551693805275200307294982662348727449076470114057118799302429633230719430091167505992776646109325581187751034387683473196251239d0
                     0.122555206711478460184519126800201555228163897333439097167253513654057439599403681838197219925041014900716770845581016548679611501882713304014014340413792128839404551693805275200307294982662348727449076470114057118799302429633230719430091167505992776646109325581187751034387683473196251239d0
                     0.100942044106287165562813984924834607062801138887678901610374536655650797380498523352450638711340451123591842370541413294311106159912579012457003242020539921513973690631675437417828762962981864985682842052373148870077502735380884116914634532056251929495961870167495710572600618288901617839d0
                     0.100942044106287165562813984924834607062801138887678901610374536655650797380498523352450638711340451123591842370541413294311106159912579012457003242020539921513973690631675437417828762962981864985682842052373148870077502735380884116914634532056251929495961870167495710572600618288901617839d0
                     0.076425730254889056529129677616636525605317906208358287449521379234410007291792979341765102678396155638675509437737125062471074010855455883051369471861674157940596194802194216405925805239119188363539765222904234487637584723588144297445426305643984968651769485081500103458097069277912711339d0
                     0.076425730254889056529129677616636525605317906208358287449521379234410007291792979341765102678396155638675509437737125062471074010855455883051369471861674157940596194802194216405925805239119188363539765222904234487637584723588144297445426305643984968651769485081500103458097069277912711339d0
                     0.049714548894969796453334946202638641680866246128910202246304307865572343295803416295041370949375536390157810912376107224618599108560667786291505796510844731290991172843802216163477040553053947516979116865321639879125159247509452471612280145730117148350631694594881205799756423504721874068d0
                     0.049714548894969796453334946202638641680866246128910202246304307865572343295803416295041370949375536390157810912376107224618599108560667786291505796510844731290991172843802216163477040553053947516979116865321639879125159247509452471612280145730117148350631694594881205799756423504721874068d0
                     0.021616013526483310313342710266452469387685231475589945462075990141353090237345121903251303069871196703526492411696739205355118147874398866085269534158100920701081697842086052106218558864279044483568473160308760033112334745755361672601076756944395517478510579176621278798530168457008166753d0
                     0.021616013526483310313342710266452469387685231475589945462075990141353090237345121903251303069871196703526492411696739205355118147874398866085269534158100920701081697842086052106218558864279044483568473160308760033112334745755361672601076756944395517478510579176621278798530168457008166753d0)
                    (0.161054449848783695979163625320916735039902558578516902128323152735771048455498072152540765716929046932632282286318499221374317886849820022227080130979756740034859676147755741579663442769041940356301119424930215257506270129261396733105884501412647412857748008511994978422760304538678184126d0
                     0.158968843393954347649956439465047201678780158195126095751174915324740978583385059742610476726006478565672695740730870929468194086947157277419388995200377948399895286324478693283409610368852907570404774075635261549560299746073965027839305649777050507145176726795163401178481408995966398442d0
                     0.158968843393954347649956439465047201678780158195126095751174915324740978583385059742610476726006478565672695740730870929468194086947157277419388995200377948399895286324478693283409610368852907570404774075635261549560299746073965027839305649777050507145176726795163401178481408995966398442d0
                     0.152766042065859666778855400897662998461008267236428623523101553074811043651063213654809847030787068402281257643851429418284269746731685258833997918956440006515856105536854328660680694581357567722532523459381765021714393394356580739371054915880699488073789666137102163493283906458863977347d0
                     0.152766042065859666778855400897662998461008267236428623523101553074811043651063213654809847030787068402281257643851429418284269746731685258833997918956440006515856105536854328660680694581357567722532523459381765021714393394356580739371054915880699488073789666137102163493283906458863977347d0
                     0.142606702173606611775746109441902972475668344824473860926571655906458248472903251014940071837250136908630601440658038331519582842282975249429591812236615842098454520988644587555455867107763620518418031550126467835226323029712582459448160743054427812948148557018959954491020106384893702199d0
                     0.142606702173606611775746109441902972475668344824473860926571655906458248472903251014940071837250136908630601440658038331519582842282975249429591812236615842098454520988644587555455867107763620518418031550126467835226323029712582459448160743054427812948148557018959954491020106384893702199d0
                     0.128753962539336227675515784856877117055839577093463034547104384622303604056631679931433002334152073215891645750356298056376863660258350843430798169900826391184262997331112328017160125681857781151873924715245211454108383910045194077341136144224174260428929264662261004193815294301079036128d0
                     0.128753962539336227675515784856877117055839577093463034547104384622303604056631679931433002334152073215891645750356298056376863660258350843430798169900826391184262997331112328017160125681857781151873924715245211454108383910045194077341136144224174260428929264662261004193815294301079036128d0
                     0.111566645547333994716023901681765997481331853839893775521461142876458976105646470601004284952592305405626155444210531474576319593284980829716076913199785896325520164044423036036524035783156214582878734361637253522403706008358916816736260896480514284874615131236321728010422030910186216232d0
                     0.111566645547333994716023901681765997481331853839893775521461142876458976105646470601004284952592305405626155444210531474576319593284980829716076913199785896325520164044423036036524035783156214582878734361637253522403706008358916816736260896480514284874615131236321728010422030910186216232d0
                     0.091490021622449999464462094123839652660911651296598784664989344902062221043797261605747715041888956274314663499304298180890418145844870950435995748481388511080218383429575241133959346314070050293784830630959614196654395839612140494631442870418895378200839993770647110830145840275823152703d0
                     0.091490021622449999464462094123839652660911651296598784664989344902062221043797261605747715041888956274314663499304298180890418145844870950435995748481388511080218383429575241133959346314070050293784830630959614196654395839612140494631442870418895378200839993770647110830145840275823152703d0
                     0.069044542737641226580708258006013044961848031687613131122900427696034974906439021517663095463334972637464287665389954252480169892566653682773778037169934650802804901585481301564601376973340721933757061495331934136163418037662174881320388330592329356730783559467785139182435714917495109553d0
                     0.069044542737641226580708258006013044961848031687613131122900427696034974906439021517663095463334972637464287665389954252480169892566653682773778037169934650802804901585481301564601376973340721933757061495331934136163418037662174881320388330592329356730783559467785139182435714917495109553d0
                     0.044814226765699600332838157401994211951754227467857602085854526767642991384279457644669035710255993420328032377586036034300033445831979678055220272528659938546448217555642582918040115323221829462400714844521688697621298532893481911020816242546411002880904111716316242706906518658956649761d0
                     0.044814226765699600332838157401994211951754227467857602085854526767642991384279457644669035710255993420328032377586036034300033445831979678055220272528659938546448217555642582918040115323221829462400714844521688697621298532893481911020816242546411002880904111716316242706906518658956649761d0
                     0.019461788229726477036312041464438435752906609069286640792680472461601437568105548210852088045267491703474519294753293711416989642826436218791612066836092445029109585129910030040337106481858336585798845154695695957794646436654265225738491956319174202287938984939445766702109026824150957238d0
                     0.019461788229726477036312041464438435752906609069286640792680472461601437568105548210852088045267491703474519294753293711416989642826436218791612066836092445029109585129910030040337106481858336585798845154695695957794646436654265225738491956319174202287938984939445766702109026824150957238d0)
                    (0.152753387130725850698084331955097593491948645112378597274701049817597453162737781535572487836503905935440018428137878260115027967960078300553003932184227735755401920312491510051849903818450728997920851172833809788484793023484039174578778760396031377475457602052123865230068273554271094426d0
                     0.152753387130725850698084331955097593491948645112378597274701049817597453162737781535572487836503905935440018428137878260115027967960078300553003932184227735755401920312491510051849903818450728997920851172833809788484793023484039174578778760396031377475457602052123865230068273554271094426d0
                     0.149172986472603746787828737001969436692679904081368316496211217809844422595586780693961326035210481051709138545673380068657567779217864943094172342466753204635647277422870956069480383323971410250075501973891170371717031000205331674471077478833194147431939631532006272768897466171161939763d0
                     0.149172986472603746787828737001969436692679904081368316496211217809844422595586780693961326035210481051709138545673380068657567779217864943094172342466753204635647277422870956069480383323971410250075501973891170371717031000205331674471077478833194147431939631532006272768897466171161939763d0
                     0.142096109318382051329298325067164933034515413392020303337367082983828087497934367616949224283200582601330685736662014018940243727274218273723625372448758856762127668826592271062697692499466959857504236884502900742224563126359698375676025656944698560801403975882427682280447932337130893411d0
                     0.142096109318382051329298325067164933034515413392020303337367082983828087497934367616949224283200582601330685736662014018940243727274218273723625372448758856762127668826592271062697692499466959857504236884502900742224563126359698375676025656944698560801403975882427682280447932337130893411d0
                     0.13168863844917662689849449974816313491611051114698352699643649370885435642948093314355797518397262924510598005463624701640031683060753870495293257768913220190202118166636287979514171352512880403052541092538802626478364321538954036878652219416510228825857535362417224838751045911977603176d0
                     0.13168863844917662689849449974816313491611051114698352699643649370885435642948093314355797518397262924510598005463624701640031683060753870495293257768913220190202118166636287979514171352512880403052541092538802626478364321538954036878652219416510228825857535362417224838751045911977603176d0
                     0.118194531961518417312377377711382287005041219548968775446889952020174748350511516305728687825819017446062675430923165913388681793742209912254230975711851338385629091978568844200464995148321588737885180243918848633075769827281685513849562051989598777429238311103226470864185967918860009569d0
                     0.118194531961518417312377377711382287005041219548968775446889952020174748350511516305728687825819017446062675430923165913388681793742209912254230975711851338385629091978568844200464995148321588737885180243918848633075769827281685513849562051989598777429238311103226470864185967918860009569d0
                     0.101930119817240435036750135480349876166691656023392556261971616196852322025394346475349315769479858213758590355254832659323440772197198294746811188579522696558158434548041676226476060912284238215014583007065700681795505931645853929825217000507721922492211238577140373972565745081878326731d0
                     0.101930119817240435036750135480349876166691656023392556261971616196852322025394346475349315769479858213758590355254832659323440772197198294746811188579522696558158434548041676226476060912284238215014583007065700681795505931645853929825217000507721922492211238577140373972565745081878326731d0
                     0.083276741576704748724758143222046206100177828583163290744882060785693082894079419471375190843790839349096116111932763770599149277088902782291022218769919318616070036439127513797968341309046000246993616694184414433015965077150726008540306730725188832193968046451485570593028416680886599608d0
                     0.083276741576704748724758143222046206100177828583163290744882060785693082894079419471375190843790839349096116111932763770599149277088902782291022218769919318616070036439127513797968341309046000246993616694184414433015965077150726008540306730725188832193968046451485570593028416680886599608d0
                     0.062672048334109063569506535187041606351601076578436364099584345437974811033665678644563766056832203512603253399592073261757210090635705748530564363537937205217456513288095664309619782478328250282204453624622925515079196351681494722393629750866821581906393878102425699106744028322251653137d0
                     0.062672048334109063569506535187041606351601076578436364099584345437974811033665678644563766056832203512603253399592073261757210090635705748530564363537937205217456513288095664309619782478328250282204453624622925515079196351681494722393629750866821581906393878102425699106744028322251653137d0
                     0.040601429800386941331039952274932109879090639989951536817606854561832296750987328295538920623044384976189825709675075022610038842058926324245527018956276304239421769562310276021319366391519402852430503328341466415607350969845090881858606217075650808824740263986913421364499833662733456389d0
                     0.040601429800386941331039952274932109879090639989951536817606854561832296750987328295538920623044384976189825709675075022610038842058926324245527018956276304239421769562310276021319366391519402852430503328341466415607350969845090881858606217075650808824740263986913421364499833662733456389d0
                     0.01761400713915211831186196235185281636214310554333673252434932667734841925962184781740310554214609766870371622751257000820832291921735671560811000965562113792806610595553840846498176059348261652944566214525073715421618147695653935002027415849599170318607169868807839543205187712021867641d0
                     0.01761400713915211831186196235185281636214310554333673252434932667734841925962184781740310554214609766870371622751257000820832291921735671560811000965562113792806610595553840846498176059348261652944566214525073715421618147695653935002027415849599170318607169868807839543205187712021867641d0)
                    (0.146081133649690427191985147683371188244809576941965444107322587515438592703399611929742191126466255721208419307318366640702329149070131539434993316081412009101913538456014278076792238339267066082812806734630580732431991046949112683089237643004668855199771436292058937344907305704016493538d0
                     0.144524403989970059063827166553752543609949962197099834752472988886097309343391869865269665864852023305015380718275105834047860791460924515529592249856299280891078317671539613959372225986842546970225318288699032995418865229028697018207152439798240435721769347714067042604011057117038415373d0
                     0.144524403989970059063827166553752543609949962197099834752472988886097309343391869865269665864852023305015380718275105834047860791460924515529592249856299280891078317671539613959372225986842546970225318288699032995418865229028697018207152439798240435721769347714067042604011057117038415373d0
                     0.139887394791073154722133423867583110892793160954919604373731326324243262465206156616321621602160099859990005560227105554135705153602875534756108634833096297865781397898730470034056776911270367457848435761186250452223350295204521994499435197752634390774640196769900065510465157192968127038d0
                     0.139887394791073154722133423867583110892793160954919604373731326324243262465206156616321621602160099859990005560227105554135705153602875534756108634833096297865781397898730470034056776911270367457848435761186250452223350295204521994499435197752634390774640196769900065510465157192968127038d0
                     0.132268938633337461781052574496775604329011540143156823218350323151782598565230399023690595615288782325743726007028420932982623566277795637300338029048548163549894324872530699213374599931065409053902262035132164297860681939382821192297976132476696737223245433707529672359741172830155927617d0
                     0.132268938633337461781052574496775604329011540143156823218350323151782598565230399023690595615288782325743726007028420932982623566277795637300338029048548163549894324872530699213374599931065409053902262035132164297860681939382821192297976132476696737223245433707529672359741172830155927617d0
                     0.1218314160537285341953671771257335983563376255615374502429838078427589012293509639419484540051080544153510699299080346511178145625220145574966063619992899189936923755660540749670166740162183031003939265301723207858813606789257737351769072934525632679164813433901192224373817229602239332d0
                     0.1218314160537285341953671771257335983563376255615374502429838078427589012293509639419484540051080544153510699299080346511178145625220145574966063619992899189936923755660540749670166740162183031003939265301723207858813606789257737351769072934525632679164813433901192224373817229602239332d0
                     0.108797299167148377663474578070105642033699595793856330552782087168973643766269388381176762430791971428043133669492949183341724922175644058982829690576621610455404437850284472174955659284335875907775406508047505215973843959632979669146006365903236081656238993347844238564574188235333159372d0
                     0.108797299167148377663474578070105642033699595793856330552782087168973643766269388381176762430791971428043133669492949183341724922175644058982829690576621610455404437850284472174955659284335875907775406508047505215973843959632979669146006365903236081656238993347844238564574188235333159372d0
                     0.093444423456033861553289741113932088483526647361592844866695271632577897773712387351868588566790756716297779979952375553032993669580068413123932560569416819852862476519066083037140168451489531004358355728533492263669621945039625592794105455632252658911290694714563173169533375216910207595d0
                     0.093444423456033861553289741113932088483526647361592844866695271632577897773712387351868588566790756716297779979952375553032993669580068413123932560569416819852862476519066083037140168451489531004358355728533492263669621945039625592794105455632252658911290694714563173169533375216910207595d0
                     0.076100113628379302017051653300183179226154352423966028521468485816453516586600749993136618812647532270150391233314310571031750850744106783263057282031735868541795225116087802590087089765553074371704933667730435864561038300661516943367816021525662233963700753777779335952598444131636465132d0
                     0.076100113628379302017051653300183179226154352423966028521468485816453516586600749993136618812647532270150391233314310571031750850744106783263057282031735868541795225116087802590087089765553074371704933667730435864561038300661516943367816021525662233963700753777779335952598444131636465132d0
                     0.057134425426857208283635826472447957491287596826256041442507323427049055399652008088286448567021646480091239958779562163978221194984571439299391411583066260280422202404695657213201250487292332559206566265071431921655623341154014984094132602296403088993882007899552223277317783664075643158d0
                     0.057134425426857208283635826472447957491287596826256041442507323427049055399652008088286448567021646480091239958779562163978221194984571439299391411583066260280422202404695657213201250487292332559206566265071431921655623341154014984094132602296403088993882007899552223277317783664075643158d0
                     0.036953789770852493799950668299329666188944308148729837427169181878785751157567408458523442876960329858352979839844448969904505764756822514093394813408145108722390242763170062441117506353453559234239398477708928433607179670043417515622580719639125695454585004050398456784045808156621795253d0
                     0.036953789770852493799950668299329666188944308148729837427169181878785751157567408458523442876960329858352979839844448969904505764756822514093394813408145108722390242763170062441117506353453559234239398477708928433607179670043417515622580719639125695454585004050398456784045808156621795253d0
                     0.016017228257774333324224616858471015265890422117902482548177910113558767361318862314906706095145675480360083449518503266075634949360110776437252308053074666295722230109833925331281929642845467298938993370403147402932439117452075013249268950020850981784280506482217100667877637618414704110d0
                     0.0160172282577743333242246168584710152658904221179024825481779101135587673613188623149067060951456754803600834495185032660756349493601107764372523080530746662957222301098339253312819296428454672989389933704031474029324391174520750132492689500208509817842805064822171006678776376184147041102))))
    (cond ((not (numberp start))  (error "mjr_intg_simple-gauss-legendre: Lower limit (START) must be a number!"))
          ((complexp start)       (error "mjr_intg_simple-gauss-legendre: Lower limit (START) must be a real number!"))
          ((not (numberp end))    (error "mjr_intg_simple-gauss-legendre: Upper limit (END) must be a number!"))
          ((complexp end)         (error "mjr_intg_simple-gauss-legendre: Upper limit (END) must be a real number!"))
          ((> start end)          (error "mjr_intg_simple-gauss-legendre: Lower limit (START) must be numerically less than upper limit (END)!"))
          ((not (integerp order)) (error "mjr_intg_simple-gauss-legendre: ORDER must be an integer!"))
          ((< order 2)            (error "mjr_intg_simple-gauss-legendre: ORDER must be greater than 2!"))
          ((> order 20)           (error "mjr_intg_simple-gauss-legendre: ORDER must be less than 21!")))
    (let ((wid (/ (- end start) 2))
          (mid (/ (+ end start) 2)))
      (* wid (loop for a in (aref abscissa order)
                   for w in (aref weights  order)
                   sum (* w (funcall fun (+ (* wid a) mid)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_simple-gauss-kronrod (fun &key start end (order 31))
  "Return two estimates (a high order one and a lower order one) for the integral of f on [start, end] using Gauss-Kronrod.

The order argument determines the order of the Kronrod rule and the embedded Gauss rule:
   * order=15 => 15-point Gauss-Kronrod +  7-point Gauss
   * order=21 => 21-point Gauss-Kronrod + 10-point Gauss
   * order=31 => 31-point Gauss-Kronrod + 15-point Gauss
   * order=41 => 41-point Gauss-Kronrod & 20-point Gauss
   * order=51 => 51-point Gauss-Kronrod & 25-point Gauss
   * order=61 => 61-point Gauss-Kronrod & 30-point Gauss

References:
  The quadrature weights and abscissas (computed to 80 decimal digits) are courtesy of L. W. Fullerton, Bell Labs, Nov. 1981."
  (cond ((not (member order '(15 21 31 41 51 61))) (error "mjr_intg_simple-gauss-kronrod: Order must be 15, 21, 31, 41, 51, or 61!"))
        ((not (numberp start))                     (error "mjr_intg_simple-gauss-kronrod: start must be a number!"))
        ((not (numberp end))                       (error "mjr_intg_simple-gauss-kronrod: end must be a number!"))
        ((< end start)                             (error "mjr_intg_simple-gauss-kronrod: start must be less than end!")))
  (let* ((fun    (mjr_mxp_string-or-func-to-lambda fun "x"))
         (oidx   (1- (truncate order 10)))
         (wg     (aref #(#(0.129484966168869693270611432679082d0 0.279705391489276667901467771423780d0 0.381830050505118944950369775488975d0 0.417959183673469387755102040816327d0)
                         #(0.066671344308688137593568809893332d0 0.149451349150580593145776339657697d0 0.219086362515982043995534934228163d0 0.269266719309996355091226921569469d0
                           0.295524224714752870173892994651338d0)
                         #(0.030753241996117268354628393577204d0 0.070366047488108124709267416450667d0 0.107159220467171935011869546685869d0 0.139570677926154314447804794511028d0
                           0.166269205816993933553200860481209d0 0.186161000015562211026800561866423d0 0.198431485327111576456118326443839d0 0.202578241925561272880620199967519d0)
                         #(0.017614007139152118311861962351853d0 0.040601429800386941331039952274932d0 0.062672048334109063569506535187042d0 0.083276741576704748724758143222046d0
                           0.101930119817240435036750135480350d0 0.118194531961518417312377377711382d0 0.131688638449176626898494499748163d0 0.142096109318382051329298325067165d0
                           0.149172986472603746787828737001969d0 0.152753387130725850698084331955098d0)
                         #(0.011393798501026287947902964113235d0 0.026354986615032137261901815295299d0 0.040939156701306312655623487711646d0 0.054904695975835191925936891540473d0
                           0.068038333812356917207187185656708d0 0.080140700335001018013234959669111d0 0.091028261982963649811497220702892d0 0.100535949067050644202206890392686d0
                           0.108519624474263653116093957050117d0 0.114858259145711648339325545869556d0 0.119455763535784772228178126512901d0 0.122242442990310041688959518945852d0
                           0.123176053726715451203902873079050d0)
                         #(0.007968192496166605615465883474674d0 0.018466468311090959142302131912047d0 0.028784707883323369349719179611292d0 0.038799192569627049596801936446348d0
                           0.048402672830594052902938140422808d0 0.057493156217619066481721689402056d0 0.065974229882180495128128515115962d0 0.073755974737705206268243850022191d0
                           0.080755895229420215354694938460530d0 0.086899787201082979802387530715126d0 0.092122522237786128717632707087619d0 0.096368737174644259639468626351810d0
                           0.099593420586795267062780282103569d0 0.101762389748405504596428952168554d0 0.102852652893558840341285636705415d0))
                       oidx))
         (xgk    (aref #(#(0.991455371120812639206854697526329d0 0.949107912342758524526189684047851d0 0.864864423359769072789712788640926d0 0.741531185599394439863864773280788d0
                           0.586087235467691130294144838258730d0 0.405845151377397166906606412076961d0 0.207784955007898467600689403773245d0 0.000000000000000000000000000000000d0)
                         #(0.995657163025808080735527280689003d0 0.973906528517171720077964012084452d0 0.930157491355708226001207180059508d0 0.865063366688984510732096688423493d0
                           0.780817726586416897063717578345042d0 0.679409568299024406234327365114874d0 0.562757134668604683339000099272694d0 0.433395394129247190799265943165784d0
                           0.294392862701460198131126603103866d0 0.148874338981631210884826001129720d0 0.000000000000000000000000000000000d0)
                         #(0.998002298693397060285172840152271d0 0.987992518020485428489565718586613d0 0.967739075679139134257347978784337d0 0.937273392400705904307758947710209d0
                           0.897264532344081900882509656454496d0 0.848206583410427216200648320774217d0 0.790418501442465932967649294817947d0 0.724417731360170047416186054613938d0
                           0.650996741297416970533735895313275d0 0.570972172608538847537226737253911d0 0.485081863640239680693655740232351d0 0.394151347077563369897207370981045d0
                           0.299180007153168812166780024266389d0 0.201194093997434522300628303394596d0 0.101142066918717499027074231447392d0 0.000000000000000000000000000000000d0)
                         #(0.998859031588277663838315576545863d0 0.993128599185094924786122388471320d0 0.981507877450250259193342994720217d0 0.963971927277913791267666131197277d0
                           0.940822633831754753519982722212443d0 0.912234428251325905867752441203298d0 0.878276811252281976077442995113078d0 0.839116971822218823394529061701521d0
                           0.795041428837551198350638833272788d0 0.746331906460150792614305070355642d0 0.693237656334751384805490711845932d0 0.636053680726515025452836696226286d0
                           0.575140446819710315342946036586425d0 0.510867001950827098004364050955251d0 0.443593175238725103199992213492640d0 0.373706088715419560672548177024927d0
                           0.301627868114913004320555356858592d0 0.227785851141645078080496195368575d0 0.152605465240922675505220241022678d0 0.076526521133497333754640409398838d0
                           0.000000000000000000000000000000000d0)
                         #(0.999262104992609834193457486540341d0 0.995556969790498097908784946893902d0 0.988035794534077247637331014577406d0 0.976663921459517511498315386479594d0
                           0.961614986425842512418130033660167d0 0.942974571228974339414011169658471d0 0.920747115281701561746346084546331d0 0.894991997878275368851042006782805d0
                           0.865847065293275595448996969588340d0 0.833442628760834001421021108693570d0 0.797873797998500059410410904994307d0 0.759259263037357630577282865204361d0
                           0.717766406813084388186654079773298d0 0.673566368473468364485120633247622d0 0.626810099010317412788122681624518d0 0.577662930241222967723689841612654d0
                           0.526325284334719182599623778158010d0 0.473002731445714960522182115009192d0 0.417885382193037748851814394594572d0 0.361172305809387837735821730127641d0
                           0.303089538931107830167478909980339d0 0.243866883720988432045190362797452d0 0.183718939421048892015969888759528d0 0.122864692610710396387359818808037d0
                           0.061544483005685078886546392366797d0 0.000000000000000000000000000000000d0)
                         #(0.999484410050490637571325895705811d0 0.996893484074649540271630050918695d0 0.991630996870404594858628366109486d0 0.983668123279747209970032581605663d0
                           0.973116322501126268374693868423707d0 0.960021864968307512216871025581798d0 0.944374444748559979415831324037439d0 0.926200047429274325879324277080474d0
                           0.905573307699907798546522558925958d0 0.882560535792052681543116462530226d0 0.857205233546061098958658510658944d0 0.829565762382768397442898119732502d0
                           0.799727835821839083013668942322683d0 0.767777432104826194917977340974503d0 0.733790062453226804726171131369528d0 0.697850494793315796932292388026640d0
                           0.660061064126626961370053668149271d0 0.620526182989242861140477556431189d0 0.579345235826361691756024932172540d0 0.536624148142019899264169793311073d0
                           0.492480467861778574993693061207709d0 0.447033769538089176780609900322854d0 0.400401254830394392535476211542661d0 0.352704725530878113471037207089374d0
                           0.304073202273625077372677107199257d0 0.254636926167889846439805129817805d0 0.204525116682309891438957671002025d0 0.153869913608583546963794672743256d0
                           0.102806937966737030147096751318001d0 0.051471842555317695833025213166723d0 0.000000000000000000000000000000000d0))
                       oidx))
         (wgk    (aref #(#(0.022935322010529224963732008058970d0 0.063092092629978553290700663189204d0 0.104790010322250183839876322541518d0 0.140653259715525918745189590510238d0
                           0.169004726639267902826583426598550d0 0.190350578064785409913256402421014d0 0.204432940075298892414161999234649d0 0.209482141084727828012999174891714d0)
                         #(0.011694638867371874278064396062192d0 0.032558162307964727478818972459390d0 0.054755896574351996031381300244580d0 0.075039674810919952767043140916190d0
                           0.093125454583697605535065465083366d0 0.109387158802297641899210590325805d0 0.123491976262065851077958109831074d0 0.134709217311473325928054001771707d0
                           0.142775938577060080797094273138717d0 0.147739104901338491374841515972068d0 0.149445554002916905664936468389821d0)
                         #(0.005377479872923348987792051430128d0 0.015007947329316122538374763075807d0 0.025460847326715320186874001019653d0 0.035346360791375846222037948478360d0
                           0.044589751324764876608227299373280d0 0.053481524690928087265343147239430d0 0.062009567800670640285139230960803d0 0.069854121318728258709520077099147d0
                           0.076849680757720378894432777482659d0 0.083080502823133021038289247286104d0 0.088564443056211770647275443693774d0 0.093126598170825321225486872747346d0
                           0.096642726983623678505179907627589d0 0.099173598721791959332393173484603d0 0.100769845523875595044946662617570d0 0.101330007014791549017374792767493d0)
                         #(0.003073583718520531501218293246031d0 0.008600269855642942198661787950102d0 0.014626169256971252983787960308868d0 0.020388373461266523598010231432755d0
                           0.025882133604951158834505067096153d0 0.031287306777032798958543119323801d0 0.036600169758200798030557240707211d0 0.041668873327973686263788305936895d0
                           0.046434821867497674720231880926108d0 0.050944573923728691932707670050345d0 0.055195105348285994744832372419777d0 0.059111400880639572374967220648594d0
                           0.062653237554781168025870122174255d0 0.065834597133618422111563556969398d0 0.068648672928521619345623411885368d0 0.071054423553444068305790361723210d0
                           0.073030690332786667495189417658913d0 0.074582875400499188986581418362488d0 0.075704497684556674659542775376617d0 0.076377867672080736705502835038061d0
                           0.076600711917999656445049901530102d0)
                         #(0.001987383892330315926507851882843d0 0.005561932135356713758040236901066d0 0.009473973386174151607207710523655d0 0.013236229195571674813656405846976d0
                           0.016847817709128298231516667536336d0 0.020435371145882835456568292235939d0 0.024009945606953216220092489164881d0 0.027475317587851737802948455517811d0
                           0.030792300167387488891109020215229d0 0.034002130274329337836748795229551d0 0.037116271483415543560330625367620d0 0.040083825504032382074839284467076d0
                           0.042872845020170049476895792439495d0 0.045502913049921788909870584752660d0 0.047982537138836713906392255756915d0 0.050277679080715671963325259433440d0
                           0.052362885806407475864366712137873d0 0.054251129888545490144543370459876d0 0.055950811220412317308240686382747d0 0.057437116361567832853582693939506d0
                           0.058689680022394207961974175856788d0 0.059720340324174059979099291932562d0 0.060539455376045862945360267517565d0 0.061128509717053048305859030416293d0
                           0.061471189871425316661544131965264d0 0.061580818067832935078759824240055d0)
                         #(0.001389013698677007624551591226760d0 0.003890461127099884051267201844516d0 0.006630703915931292173319826369750d0 0.009273279659517763428441146892024d0
                           0.011823015253496341742232898853251d0 0.014369729507045804812451432443580d0 0.016920889189053272627572289420322d0 0.019414141193942381173408951050128d0
                           0.021828035821609192297167485738339d0 0.024191162078080601365686370725232d0 0.026509954882333101610601709335075d0 0.028754048765041292843978785354334d0
                           0.030907257562387762472884252943092d0 0.032981447057483726031814191016854d0 0.034979338028060024137499670731468d0 0.036882364651821229223911065617136d0
                           0.038678945624727592950348651532281d0 0.040374538951535959111995279752468d0 0.041969810215164246147147541285970d0 0.043452539701356069316831728117073d0
                           0.044814800133162663192355551616723d0 0.046059238271006988116271735559374d0 0.047185546569299153945261478181099d0 0.048185861757087129140779492298305d0
                           0.049055434555029778887528165367238d0 0.049795683427074206357811569379942d0 0.050405921402782346840893085653585d0 0.050881795898749606492297473049805d0
                           0.051221547849258772170656282604944d0 0.051426128537459025933862879215781d0 0.051494729429451567558340433647099d0))
                       oidx))
         (centr  (/ (+ start end) 2))
         (hlgth  (/ (- end start) 2))
         (fc     (funcall fun centr))
         (resg   (if (oddp oidx)
                     0.0d0
                     (* fc (aref wg  (1- (length wg))))))
         (resk   (* fc (aref wgk (1- (length wgk))))))
    (loop for j from 1 upto (truncate order 2)
          for absc  = (* hlgth (aref xgk (1- j)))
          for fsum  = (+ (funcall fun (- centr absc))
                         (funcall fun (+ centr absc)))
          do (incf resk (* (aref wgk (1- j)) fsum))
          when (evenp j)
          do (incf resg (* (aref wg  (1- (/ j 2))) fsum)))
    (values (* resk hlgth)     ;; Gauss-Kronrod integral value
            (* resg hlgth))))  ;; Gauss integral value



;; (loop for ord in '(15 21 31 41 51 61)
;;       for (k g) = (multiple-value-list (gk (lambda (x) (sin (* 20 x))) :start 1 :end 2 :order ord))
;;       for e = (abs (- k g))
;;       do (format 't "~10d :: ~30,20f ~30,20f ~30,20f~%" ord k g e))
;;         15 ::         0.05375100448700819000         0.03329022396630121000         0.02046078052070697500
;;         21 ::         0.05375100604962934000         0.05376956145745758600         0.00001855540782824683
;;         31 ::         0.05375100608944126400         0.05375100600323407000         0.00000000008620719416
;;         41 ::         0.05375100610990241000         0.05375100617328299000         0.00000000006338057856
;;         51 ::         0.05375100612233012000         0.05375100607136562600         0.00000000005096449651
;;         61 ::         0.05375100613067555000         0.05375100617328321000         0.00000000004260766046

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_intg_gbl-adp-factory-order (fun &key start end order-min order-max order-step (err 0.0002) (imethod #'mjr_intg_simple-gauss-legendre) show-progress)
  "Compute the definite integral of FUN between A and B using the given integration method.

The order is stepped from order-min to order-max by order-step until the difference in two successive iterations is less than err.  Here are some possible
values for method:

   * #'mjr_intg_simple-newton-cotes   -- :order-min 3    :order-max 15     :order-step 1
   * #'mjr_intg_simple-gauss-legendre -- :order-min 2    :order-max 20     :order-step 1
   * #'mjr_intg_simple-monte-carlo    -- :order-min 1000 :order-max 100000 :order-step 1000
   * #'mjr_intg_simple-gauss-kronrod  -- :order-min 21   :order-max 61     :order-step 10

The values for order-min, order-max, and order-step will be set as above if all three are NIL."
  (if (not (or order-min order-max order-step))
      (cond
        ((equalp imethod #'mjr_intg_simple-gauss-legendre)
         (mjr_intg_gbl-adp-factory-order fun :start start :end end :err err :imethod imethod :show-progress show-progress :order-min 2    :order-max 20     :order-step 1))
        ((equalp imethod #'mjr_intg_simple-newton-cotes)
         (mjr_intg_gbl-adp-factory-order fun :start start :end end :err err :imethod imethod :show-progress show-progress :order-min 2    :order-max 15     :order-step 1))
        ((equalp imethod #'mjr_intg_simple-monte-carlo)
         (mjr_intg_gbl-adp-factory-order fun :start start :end end :err err :imethod imethod :show-progress show-progress :order-min 1000 :order-max 100000 :order-step 1000))
        ((equalp imethod #'mjr_intg_simple-gauss-kronrod)
         (mjr_intg_gbl-adp-factory-order fun :start start :end end :err err :imethod imethod :show-progress show-progress :order-min 21   :order-max 61     :order-step 10)))
      (loop for cur-ord from order-min upto order-max by order-step
            for prv-val = nil then cur-val
            for cur-val = (funcall imethod fun :start start :end end :order cur-ord)
            do (if show-progress (format 't "~10d : ~25,10f~%" cur-ord cur-val))
            when (and prv-val (mjr_eps_= prv-val cur-val err))
            do (return cur-val))))
