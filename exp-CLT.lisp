;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      exp-CLT.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Theoretical family of probability distributions of sums of an input PDF represented as a historgram.@EOL
;; @std       Common Lisp
;; @see       
;; @copyright 
;;  @parblock
;;  Copyright (c) 1993,1995,1996,1997,2003,2008,2009,2010,2012, 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defun exCLT (rv-dat wt-dat max-nsum)
  "Compute PDFs for the repeated sum of a random variable

rv-dat is an array of discrete random variable values (non-negative integers). wt-dat is an array of weights i.e. the probability, density, or count.  This
function transforms the weights into a PDF by dividing the weights by a constant value so that they will all sum to 1.

A family of max-nsum theoretical probability distributions is returned, each as a column of the returned 2D array.  The i'th PDF is the PDF resulting from
summing i independent copies of the input PDF.

This function allows us to see the central limit theorem in action if we divide each derived random variable by i.  It can be a real eye-opener to see just
how quickly, or slowly, a particular distribution's mean sampling distribution approaches normality!

The algorithm for doing the PDF computations is iterative using the i'th PDF for the computation of the (i+1)'th PDF.  This renders the PDFs very quickly when
compared to a naive implementation.  Even with a good algorithm, this kind of computation is best done in a fast language (C++, Fortran, or a fast LISP
implementation like SCBL)."
  (let* ((cvp      nil)                                                      ; Column j has the PDF for nsum==j+1
         (mx-in-ut (reduce #'max rv-dat))                                    ; Maximum value in input histogram
         (num-buck (1+ mx-in-ut))                                            ; Size of buckets in our base histogram
         (max-buck (make-array (1+ max-nsum) :initial-element 0))            ; (aref max-buck j) has the max ut for nsum=j+1
         (cut-tall (1+ (* max-nsum num-buck))))                              ; Number of rows in cvp
    (setq cvp (make-array (list cut-tall max-nsum) :initial-element 0))      ; Initialize cvp to 0
    (loop for ut across rv-dat                                               ; Populate first column of cvp (nsum==1)
          for wt across wt-dat                                                 ; After this loop, rv-dat & wt-dat are never used
          when (<= 0 ut)                                                       ; Ignore negative values
          do (incf (aref cvp ut 0) wt))                                        ; incf in case we have duplicate rv-dat values
    (loop with tot-p = (reduce #'+ (mjr_arr_get-col cvp 0))                  ; Fix the PDF so the total probably sums to 1  
          for i from 0 to mx-in-ut                                             ; Multiply all weight values by the same
          do (setf (aref cvp i 0) (/ (aref cvp i 0) tot-p)))                   ; factor.
    (setf (aref max-buck 0) (1- num-buck))                                   ; Set the max bucket index for col 0
    (loop for nsum from 2 upto max-nsum                                      ; Compute the PDFs for the remaining nsum values
          for nsum-1 = (1- nsum)                                               ; Store nsum-1 away for later use
          do (format 't "NSUM: ~3d~%" nsum)                                    ; Can take a while, print status
          do (loop for i from 0 upto (aref max-buck 0)                         ; Value for nsum=1 case
                   for piv = (aref cvp i 0)                                      ; Prob for nsum=1 col
                   do (loop for j from 0 upto (aref max-buck (1- nsum-1))        ; Value for nsum-1 case
                            for pjv = (aref cvp j (1- nsum-1))                   ; Prob for nsum-1 case
                            do (incf (aref cvp (+ i j) nsum-1) (* piv pjv))      ; Add up joint probability
                            do (setf (aref max-buck nsum-1) (+ i j)))))          ; Update max value so far (slow, but easy)
    cvp))                                                                    ; Return the result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exCLT-CSV (in-file out-file max-nsum)
  "Read a histogram from a CSV file, compute sum PDFs with exCLT, and write result to a CSV file.

This function first reads in a CSV file containing a histogram.  The first column should be the value of a discreet random
variable (a non-negative integer).  The second column is the weights."
  (let* ((in-hist  (mjr_util_read-csv-file                                   ; Load histograms from disk
                    in-file                                                    ; The file with the data
                    :return-as-array 't                                        ; Return a matrix, not a list of lists
                    :filter-func #'mjr_string_read-as-lisp))                   ; Convert input strings into numbers
         (rv-dat   (mjr_arr_get-col in-hist 0))                              ; Column with variable values
         (wt-dat   (mjr_arr_get-col in-hist 1))                              ; Column with weights/probabilities
         (cvp      (exCLT rv-dat wt-dat max-nsum)))                          ; Compute PDFs
    (mjr_util_write-csv-file cvp out-file                                    ; Write out the PDFs in CSV form
                             :titles (loop for i from 1 upto max-nsum          ; Put numeric column titles on the first
                                           collect (format nil "n.~d" i)))     ; line of the file.
    cvp))                                                                    ; Return the result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical application: Most lower level statistics texts will describe an application of the central limit theorem to the sampling distribution from
;; arbitrary populations.  The fundamental result is that the sampling distribution will approach normality as the sample size grows to infinity.  Students
;; are normally given advice like "a sample of 30 is sufficient".  The data in exCLT-IN.csv describes a very non-normal PDF.  The following code will read in
;; that distribution, and compute the sampling distribution from sample sizes up to 30.  The R code in exCLT-AUX.R illustrates how to use the results.

(defvar cvp)                                                                 ; Global var to store the results
(null (setq cvp (exCLT-csv "exp-CLT-IN.csv" "exCLT-OUT.csv" 30)))            ; Run the func, and store the results
(format 't "Data stored in variable CVP~%")                                  ; Remind the user about the CVP variable.
(format 't "Data written to file exCLT-OUT.csv~%")                           ; Remind the user about the output file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interesting compute farm/grid/cloud application: If the random variable is serial job CPU utilization (an integer from 0 to 100) and the weights are the
;; number of wall clock seconds consumed by jobs with that job CPU utilization, then the output PDFs will represent HOST utilization (when we divide by the
;; number of CPUs on the host) for various job counts.
