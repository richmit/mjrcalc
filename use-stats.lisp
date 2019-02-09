;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-stats.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Statistics: Averages, histograms, sub-samples, simple linear regression.@EOL
;; @std       Common Lisp
;; @see       tst-stats.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,1997,1998,2004,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_stats_summary: add 25th percentile, 75th percentile.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_STATS
  (:USE :COMMON-LISP
        :MJR_UTIL
        :MJR_VVEC
        :MJR_PWF)
  (:DOCUMENTATION "Brief: Statistics: Averages, histograms, sub-samples, simple linear regression.;")
  (:EXPORT #:mjr_stats_help
           #:mjr_stats_avg #:mjr_stats_subtotal
           #:mjr_stats_summary #:mjr_stats_fmt-summary
           #:mjr_stats_linear-regression
           #:mjr_stats_subsample
           #:mjr_stats_hist #:mjr_stats_fmt-hist
           ))

(in-package :MJR_STATS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_stats_help ()
  "Help for MJR_STATS:  STATistical Stuff

Simple statistical tools with a focus is on the kinds of quick computations one might preform before making the effort to push the data into a more robust
statistical tool like R."
  (documentation 'mjr_stats_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_stats_avg (&rest x)
  "Compute the arithmetic mean of the input arguments (a group of sequences and/or numbers).
For complex inputs, the average of the real and complex parts are independently computed."
  (cond ((null x)                                  (error "mjr_stats_avg: Empty list!"))
        ((and (= 1 (length x)) (listp (first x)))  (/ (reduce '+ (first x)) (length (first x))))
        ('t                                        (mjr_stats_avg (apply #'mjr_util_super-concatenate 'list x)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_stats_subtotal (&rest x)
  "Compute the running sum of the input arguments (a group of sequences and/or numbers)."
  (cond ((null x)                                  (error "mjr_stats_avg: Empty list!"))
        ((and (= 1 (length x)) (listp (first x)))  (loop for y in (first x) for psum = y then (+ y psum) collect psum))
        ('t                                        (mjr_stats_subtotal (apply #'mjr_util_super-concatenate 'list x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_stats_summary (the-list &key out-filter in-filter)
  "Compute various statistics for a list of numbers.

:OUT-FILTER is a function to be applied to all results.  The most typical use is to convert the result data type. If missing, then the output will be left in
its natural state. Some special function symbols are supported:

   :if    => 'float will be applied to all non-integer numbers
   :f     => same as 'float
   :r     => same as 'rational

IN-FILTER is also a function, but it is is applied to each element of the input list.  This is normally used to transform the data before computing the stats
-- exp, log, and expt are common choices.  The special symbols :if, :f, and :r do not work for the input filter.

Statistics returned (in an associative array) are:
  :sum    - Sum of values
  :suml   - Sum of the natural logarithms of all positive values
  :lgmean - Logarithm of the geometric mean
  :gmean  - Geometric mean if smaller than 24154952.
  :sumsq  - Sum of the square of the values
  :sumabs - Sum of the absolute values
  :min    - Minimum Value
  :max    - Maximum value
  :n      - Number of values
  :pn     - Number of positive values
  :nn     - Number of negative values
  :nz     - Number of zero values (n-pn-nn)
  :mean   - Arithmetic mean
  :sd     - Standard deviation
  :var    - Variance
  :sdp    - Population Standard deviation
  :varp   - Population Variance
  :aooc   - Number of observations out of ascending order.  aooc == 0 => list was sorted
  :dooc   - Number of observations out of descending order.  dooc == 0 => list was sorted
  :median - Median (if list sorted i.e. aooc==0 or dooc=0)
  :unique - Number of unique values (aooc==0 or dooc=0)
  :nmodes - Number of modes in the data (aooc=0 || dooc=0)
  :smode  - Biggest Mode -- The biggest mode of multi-modal data (aooc==0 && nmodes>1)
  :bmode  - Smallest Mode -- The smallest mode candidate (dooc=0 && nmodes>1)
  :mode   - The mode (see smode, bmode, & nmodes) (nmodes=1)
  :modef  - The mode frequency (see mode, smode, bmode, & nmodes) (nmodes>1)"
(if (and (listp the-list) (or (listp (first the-list)) (vectorp (first the-list))))
    (mapcar 'mjr_stats_summary the-list)
  (let* ((the-list   (if (listp the-list) the-list (loop for x across the-list collect x)))
         (input-size (length the-list))
         (the-flist  (if in-filter (mapcar in-filter the-list) the-list)))
    (loop for x in the-flist and px = nil then x;; Note: px = previous x, and nil the first time
      with the-mode       = (cons nil 0)
      with mode-candidate = (cons nil 0)
      with the-nmodes     = 0
      sum      (* x x)                 into the-sumsq
      sum      (abs x)                 into the-sumabs
      sum      x                       into the-sum
      sum      (if (> x 0) (log x) 0)  into the-suml
      minimize x                       into the-min
      maximize x                       into the-max
      count    't                      into the-n
      count    (> x 0)                 into the-pn
      count    (< x 0)                 into the-nn
      count    (and px (> px x))       into the-aooc    ;; Out of ascending order count (=0 => sorted, >0 => not sorted)
      count    (and px (< px x))       into the-dooc    ;; Out of descending order count (=0 => sorted, >0 => not sorted)
      count    (not (and px (= px x))) into the-unique  ;; Number of unique elements if the-occ=0
      do (if (or (zerop the-aooc) (zerop the-dooc))
             (progn (if (and px (= px x))
                        (setq mode-candidate (if (car mode-candidate) (cons x (+ (cdr mode-candidate) 1)) (cons x 2))))
                    (if  (and (car mode-candidate) (or (= input-size the-n) (and px (not (= px x)))))
                        (progn (if (> (cdr mode-candidate) (cdr the-mode))
                                   (setq the-mode       mode-candidate
                                         the-nmodes     1
                                         mode-candidate (cons nil 0)))
                               (if (= (cdr mode-candidate) (cdr the-mode))
                                   (setq the-nmodes (+ the-nmodes 1)))))));; Compute the mode
      finally (return (if (< 0 the-n)
                          (progn
                            (setq out-filter (cond ((eq out-filter :if)  (lambda (y) (if (integerp y) y (float y))))
                                                   ((eq out-filter :f)   'float)
                                                   ((eq out-filter :r)   'rational)
                                                   ('t                    out-filter)))
                            (let ((the-ret-tags (list :n))
                                  (the-ret-vals (list the-n)))
                              (if (>= the-n 1)
                                  (let ((the-nz (- the-n (+ the-pn the-nn))))
                                    (nconc the-ret-tags (list :sumsq    :sumabs :suml    :pn    :nn    :nz))
                                    (nconc the-ret-vals (list the-sumsq the-sumabs the-suml the-pn the-nn the-nz))))
                              (if (>= the-n 2)
                                  (let* ((the-mean (/ the-sum the-n))
                                         (the-var  (- (/ the-sumsq the-n) (* the-mean the-mean)))
                                         (the-sd   (sqrt the-var))
                                         (the-varp (- (/ the-sumsq (- the-n 1)) (* the-mean the-mean)))
                                         (the-sdp  (sqrt the-varp)))
                                    (nconc the-ret-tags (list :sum    :min    :max    :mean    :sd    :var    :aooc    :dooc    :sdp    :varp))
                                    (nconc the-ret-vals (list the-sum the-min the-max the-mean the-sd the-var the-aooc the-dooc the-sdp the-varp))))
                              (if (>= the-pn 2)
                                  (let ((the-lgmean (* (/ 1 the-pn) the-suml)))
                                    (nconc the-ret-tags (list :lgmean))
                                    (nconc the-ret-vals (list the-lgmean))
                                    (if (< the-lgmean 16)
                                        (let ((the-gmean (exp the-lgmean)))
                                          (nconc the-ret-tags (list :gmean))
                                          (nconc the-ret-vals (list the-gmean))))))
                              (if (= the-n 2)
                                  (progn (nconc the-ret-tags (list :unique))
                                         (nconc the-ret-vals (list the-unique))
                                         (if (= the-unique 1)
                                             (progn (nconc the-ret-tags (list :mode))
                                                    (nconc the-ret-vals (list the-min))))));; the-min is one of two identical values in this case!
                              (if (and (>= the-n 3)  (or (zerop the-aooc) (zerop the-dooc)))
                                  (progn
                                    (if (car the-mode)
                                        (progn
                                          (if (= the-nmodes 1)
                                              (progn (nconc the-ret-tags (list :mode))
                                                     (nconc the-ret-vals (list (car the-mode))))
                                            (progn (if (zerop the-dooc)
                                                       (let ((the-bmode (car the-mode)))
                                                         (nconc the-ret-tags (list :bmode    :nmodes))
                                                         (nconc the-ret-vals (list the-bmode the-nmodes))))
                                                   (if (zerop the-aooc)
                                                       (let ((the-smode (car the-mode)))
                                                         (nconc the-ret-tags (list :smode    :nmodes))
                                                         (nconc the-ret-vals (list the-smode the-nmodes))))))
                                          (let ((the-modef (cdr the-mode)))
                                            (nconc the-ret-tags (list :modef))
                                            (nconc the-ret-vals (list the-modef)))))
                                    (let ((the-median (if (oddp the-n)
                                                          (nth (- (ceiling (/ the-n 2)) 1) the-flist)
                                                        (/ (+ (nth (- (/ the-n 2) 1) the-flist) (nth (/ the-n 2) the-flist)) 2))))
                                      (nconc the-ret-tags (list :median    :unique))
                                      (nconc the-ret-vals (list the-median the-unique)))))
                              (if (and (>= the-n 4) (or (zerop the-aooc) (zerop the-dooc)))
                                  nil
                                )
                              (pairlis the-ret-tags (mapcar (lambda (y) (if out-filter (funcall out-filter y) y)) the-ret-vals))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_stats_fmt-summary (the-stats &key the-cats)
  "Format the output of stats as a string"
  (if (listp (cdr (first the-stats)))
      (apply #'concatenate 'string (mapcar (lambda (x) (mjr_stats_fmt-summary x :the-cats the-cats)) the-stats))
    (let ((the-stats-str (format nil "~%")))
      (loop for (cat-symbol . cat-contents) in '((:SUMS   . (:sum :suml :sumsq :sumabs))
                                                 (:COUNTS . (:n :pn :nn :nz :unique :aooc :dooc))
                                                 (:SPREAD . (:min :max :sd :var :sdp :varp))
                                                 (:CENTER . (:mean :mode :bmode :smode :modef :nmodes :median :gmean :lgmean)))
        when (or (not the-cats) (member cat-symbol (if (listp the-cats) the-cats (list the-cats))))
        do (progn
             (setq the-stats-str (concatenate 'string the-stats-str (format nil "~8a " (format nil "~s>" cat-symbol))))
             (loop for stat in cat-contents
               do (if (assoc stat the-stats)
                      (setq the-stats-str (concatenate 'string the-stats-str (format nil "  ~s: ~s;" stat (cdr (assoc stat the-stats)))))))
             (setq the-stats-str (concatenate 'string the-stats-str (format nil "~%")))
             ))
      the-stats-str)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_stats_linear-regression (list-x list-y &key (x-tform #'identity) (y-tform #'identity))
  "Compute the linear regression between the two lists -- yes they must be lists, not sequences.

Returns m, b, and a statistical analysis of the residuals.

Use :x-tform and :y-tform to transform the data as required.  Some transforms yielding various common curve fits
  * ln(y) ln(x) y = exp(b)*x^m        power
  * ln(y) x     y = exp(b)*exp(m*x)   exponential
  * y     x     y = m*x+b             linear
  * y     ln(x) y = ln(exp(b)*x^m)    logarithmic
  * y     x^2   y = m*x^2+b           quadratic
  * y^2   x     y = sqrt(m*x+b)       square root"
  (multiple-value-bind
        (sum-x sum-y sum-x2 sum-xy n) (loop for xo in list-x
                                            for yo in list-y
                                            for x = (funcall x-tform xo)
                                            for y = (funcall y-tform yo)
                                            sum (* x x)  into sum-x2
                                            sum x        into sum-x
                                            sum y        into sum-y
                                            sum (* x y)  into sum-xy
                                            count 't     into n
                                            finally (return (values sum-x sum-y sum-x2 sum-xy n)))
    (let* ((m  (/ (- (* n sum-xy) (* sum-x sum-y)) (- (* n sum-x2) (* sum-x sum-x))))
           (b  (/ (- sum-y (* m sum-x)) n))
           (fs (mjr_stats_summary (mapcar (lambda (x y) (- (+ b (* m x)) y)) list-x list-y))))
      (values m b fs))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_stats_subsample (data-vector sample-size &key (with-replacement 't))
  "Select, with replacement, SAMPLE-SIZE from the DATA-VECTOR.
This implementation uses the built in RANDOM function."
  (let ((new-data (make-array sample-size))
        (dat-len  (length data-vector)))
    (if with-replacement
        (dotimes (i sample-size new-data)
          (setf (aref new-data i) (aref data-vector (random dat-len))))
        (if (< (/ dat-len 4) sample-size)
            (subseq (let* ((new-seq   (copy-seq data-vector))                            ;; Large sample
                           (num-swaps (* 5 dat-len)))
                      (dotimes (i num-swaps new-seq)
                        (rotatef (aref new-seq (random dat-len))
                                 (aref new-seq (random dat-len)))))
                    0 sample-size)
            (let ((sel-map  (make-array dat-len :element-type 'bit :initial-element 0))) ;; Small sample
              (dotimes (i sample-size new-data)
                (setf (aref new-data i)
                      (aref data-vector (loop for j = (random dat-len)
                                              when (= 0 (aref sel-map j))
                                              do (progn (setf (aref sel-map j) 1)
                                                        (return j)))))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_stats_hist (data &key (density nil)
                       (interval-type :interval-type-left-closed)
                       breaks)
  "Compute histogram.  Returns a list with the histogram, breaks vector, count below breaks, & count above breaks.

if :density is non-NIL, then the output will be normalized to area 1.

The DATA argument should be a vector for performance reasons, but lists are accepted too.

Example: compute a random vector, compute a histogram for vector, construct a function for histogram, and graph resulting function.
  (let ((x0 1)
        (x1 10))
    (destructuring-bind (c b u o) (mjr_stats_hist (mjr_prng_vector 10000 #'mjr_prng_float-cc x0 x1) 
                                                  :breaks (list :start x0 :end x1 :len 20) 
                                                  :density 't)
      (let ((f (mjr_pwf_func-step b c :left-tail-value u :right-tail-value o)))
        (mjr_gnupl_dquad (mjr_fsamp_dq-func-r123-r123 f
                                                      :xdat (list :start (1- x0) :end (1+ x1) :len 1000))
                         :ylim '(0 1)))))"
  (let* ((data       (if (vectorp data) data (concatenate 'vector data)))
         (breaks     (mjr_vvec_to-vec (or breaks (list :start (reduce #'min data)
                                                       :end   (reduce #'max data)
                                                       :len   20))))
         (nbrk       (1- (length breaks)))
         (low-count  0)
         (high-count 0)
         (all-count  (make-array nbrk :initial-element 0)))
    (loop for d across data
          for b = (mjr_pwf_search-interval-mesh interval-type breaks d)
          do (case b
               (-1        (incf low-count))
               (-2        (incf high-count))
               (otherwise (incf (aref all-count b)))))
    (if density
        (let* ((area   0)
               (bwids  (make-array nbrk)))
          (loop for i from 0 upto (1- nbrk)
                do (let ((wid (- (aref breaks (1+ i)) (aref breaks i))))
                     (incf area (* wid (aref all-count i)))
                     (setf (aref bwids i) wid)))
          (loop for i from 0 upto (1- nbrk)
                do (setf (aref all-count i) (/ (aref all-count i) area)))))
    (list all-count breaks low-count high-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_stats_fmt-hist (hist)
  "Format the output of MJR_STATS_HIST as a string.

The first argument is assumed to be in the format returned by MJR_STATS_HIST."
  (let* ((histogram  (first  hist))
         (breaks     (second hist))
         (low-count  (third  hist))
         (high-count (fourth hist))
         (prt-wid    (if breaks (+ 3 (mjr_util_max-print-width breaks))))
         (max-width  128)
         (scale      (/ (reduce #'max histogram) max-width)))
    (declare (ignore low-count high-count)) ;; Suppress compiler warnings
    (with-output-to-string (str-out)
      (format str-out "~%")
      (loop for h across histogram
            for i from 0
            for j from 1
            when breaks
            do (format str-out
                       (concatenate 'string "~" (format nil "~d" prt-wid) "a ~"  (format nil "~d" prt-wid) "a: ")
                       (aref breaks i)
                       (aref breaks j))
            do (loop for k from 1 upto max-width
                     do (format str-out (if (<= k (truncate h scale)) "#" " "))
                     finally (format str-out " :~%"))))))
