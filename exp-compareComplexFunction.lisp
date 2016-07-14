;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-compareComplexFunction.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Compare the value of a function in Maxima vs on in *MJRCALC*.@EOL
;; @std       Common Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;;
;;  Maxima has a ton of high quality functions built in, and is a handy source of regression data for *MJRCALC*.  In this example we compare the gamma
;;  function in Maxima to the Lanczos approximation of the gamma function implemented in :MJR_NUMU.
;;  
;;  Step 1a) Have our CAS evaluate it's gamma function on a grid, and write the result to some files:
;;  
;;            * exp-compareComplexFunction-OUT-TAG-x.out .... The real/imag points on the grid
;;            * exp-compareComplexFunction-OUT-TAG-r.out .... real part of the function value matrix
;;            * exp-compareComplexFunction-OUT-TAG-i.out .... imaginary part of the function value matrix
;;
;;            Here "TAG" is one of:
;;             * maxima
;;             * maple
;;
;;  Step 1b) Maxima code:
;;  
;;              x : makelist(x, x, -10.05, 10.05, .1)$
;;              a : genmatrix (lambda([i,j], gamma(float(x[i]+%i*x[j]))), length(x), length(x))$
;;              write_data(realpart(a), "exp-compareComplexFunction-OUT-maxima-r.out", comma);
;;              write_data(imagpart(a), "exp-compareComplexFunction-OUT-maxima-i.out", comma);
;;              write_data(x,           "exp-compareComplexFunction-OUT-maxima-x.out", comma);
;;
;;  Step 1c) Maple code:
;;  
;;              x:=[seq(x, x=-10.05 .. 10.05,0.1)]:
;;              a:=Matrix(nops(x),nops(x)):
;;              for i to nops(x) do for j to nops(x) do a[i, j] := evalf(GAMMA(x[i]+I*x[j]), 15) end do end do;
;;              ExportMatrix("exp-compareComplexFunction-OUT-maple-r.out", map(Re, a), target=csv);          
;;              ExportMatrix("exp-compareComplexFunction-OUT-maple-i.out", map(Im, a), target=csv);  
;;              ExportMatrix("exp-compareComplexFunction-OUT-maple-x.out", Matrix(x),  target=csv);
;;
;;  Step 1d) Fix the output files:
;;           After the file is written, search and replace all 'e' characters with 'd' characters -- so when we read the data in we will get double floats.
;;  Step 2)  Read in that data
;;  Step 3)  Put that data into a dquad object
;;  Step 4)  Compute values for the MJRCALC function on the same grid
;;  Step 5)  Compute percentage delta between the Maxima value and the *MJRCALC* value
;;  Step 6a) Graph the percentage delta
;;  Step 6b) Dump the dquad to a vtk file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun drawDelta (cas &optional (type :i))
  (let ((*READ-DEFAULT-FLOAT-FORMAT* 'double-float))
    
    (let* ((path "/home/richmit/world/my_prog/lispStuff/lispy/")
           (axl (first (mjr_util_read-csv-file (concatenate 'string path "exp-compareComplexFunction-OUT-" cas "-x.out")
                                               :filter-func #'read-from-string
                                               :header-lines-to-skip 0)))
           (axd (make-array   (length axl) :initial-contents axl))
           (dqd (mjr_dquad_make-from-axis "real" axd "imag" axd))
           (fvl (mjr_arr_binary-map2 (mjr_util_read-csv-file (concatenate 'string path "exp-compareComplexFunction-OUT-" cas "-r.out")
                                                             :return-as-array 't
                                                             :filter-func #'read-from-string
                                                             :header-lines-to-skip 0)
                                     (mjr_util_read-csv-file (concatenate 'string path "exp-compareComplexFunction-OUT-" cas "-i.out")
                                                             :return-as-array 't
                                                             :filter-func #'read-from-string
                                                             :header-lines-to-skip 0)
                                     #'complex)))
      ;; Add a data value for the independent variable: "z"
      (mjr_dquad_add-data-from-map dqd #'complex :axes '(0 1) :ano-nam "z" :ano-typ :ano-typ-complex)
      ;; Step 3) Add the value from the cas generated matrices we pulled in from disk
      (mjr_dquad_add-data dqd fvl :ano-nam (concatenate 'string cas "-value") :ano-typ :ano-typ-complex)
      ;; Step 4) Add the value of the *MJRCALC* function on the same grid
      (mjr_dquad_add-data-from-map dqd #'mjr_numu::mjr_numu_gamma-lanczos-9  :data "z" :ano-nam "mjrcalc-value" :ano-typ :ano-typ-complex)
      ;;(mjr_dquad_add-data-from-map dqd #'mjr_numu::mjr_numu_gamma-lanczos-15 :data "z" :ano-nam "mjrcalc-value" :ano-typ :ano-typ-complex)
      ;;(mjr_dquad_add-data-from-map dqd #'mjr_numu::mjr_numu_gamma-spouge     :data "z" :ano-nam "mjrcalc-value" :ano-typ :ano-typ-complex)
      ;; Step 5) Add the percentage difference from the cas value -- assuming the cas value is the better one
      (mjr_dquad_add-data-from-map dqd (lambda (a b) (* 100 (/ (abs (- a b)) (abs a))))
                                   :data (list (concatenate 'string cas "-value") "mjrcalc-value")
                                   :ano-nam (concatenate 'string cas "-delta")
                                   :ano-typ :ano-typ-real)
      ;; Step 6a) Graph the difference
      (mjr_gnupl_dquad dqd :data (concatenate 'string cas "-delta") :type type)
      ;; Step 6b) Graph the difference
      (mjr_vtk_from-dquad (concatenate 'string path "exp-compareComplexFunction-OUT-" cas "-delta.vtk") dqd))))


(drawDelta "maxima" :f)

;; (drawDelta "maple" :f)




;; How you might compare two real funcs
;; (let* ((range 1)
;;        (func  #'cos)
;;        (func2  #'mjr_rtrig_cos-f) ;; (lambda (x) (mjr_poly_eval poly x))
;;        (dq    (mjr_fsamp_dq-func-r123-r123 (list func func2) :xdat (list :start 0.d0 :end (* range 2 pi) :len 10000))))
;;            (mjr_dquad_add-data-from-map dq #'-   :data '(0 1)  :ano-nam "delta")
;;            (mjr_dquad_add-data-from-map dq (lambda (x y) (if (zerop x) 0 (min 100 (abs (* 100 (/ (- y x) x)))))) :data '(0 1) :ano-nam "pdelta")
;;            (mjr_gnupl_dquad dq :data '(0 1) :ylim '(-10 10))
;;            ;(mjr_gnupl_dquad dq :data "pdelta" :ylim '(-1 101))
;;            ;(mjr_gnupl_dquad dq :data "delta" :ylim '(-.001 .001))
;;            (format 't "Range: ~a~%" range)
;;            (format 't "Max Err: ~f~%" (abs (reduce #'mjr_numu_abs-max (mjr_dquad_get-data-array dq "delta")))))
