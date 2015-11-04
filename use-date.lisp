;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-date.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2013 by Mitch Richling.  All rights reserved.
;; @brief     Handy date stuff.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_DATE
  (:USE :COMMON-LISP
        :MJR_A
        :MJR_STRING)
  (:DOCUMENTATION "Brief: Handy date stuff.;")
  (:EXPORT #:mjr_date_month-name #:mjr_date_day-name
           #:mjr_date_jdate2alternate
           #:mjr_date_convert
           #:mjr_date_now
           #:mjr_date_add-delta #:mjr_date_delta
           #:mjr_date_unx-convert
           #:mjr_date_us-tz-utc-offset                   
           ))

(in-package :MJR_DATE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_help ()
  "Help for MJR_DATE:  Date Time

Funcs:
 mjr_date_month-name mjr_date_day-name   Handy functions converting numbers into names
 mjr_date_now                            Get current date
 mjr_date_jdate2alternate                Convert Julian date to an alternate form (like Modified Julian Date)
 mjr_date_convert                        Convert between date formats.  See doc string for supported formats.
    :dt-list :dt-string :dt-posix,
    :dt-lisp :dt-julian
 mjr_date_add-delta mjr_date_delta         Time differences
 mjr_date_unx-convert                    Convert between UNIX integer dates and a string (time2int.pl & time2int.pl)
 mjr_date_us-tz-utc-offset               Time zones
    :tz-adt  :tz-ast  :tz-edt :tz-est
    :tz-cdt  :tz-cst  :tz-mdt :tz-mst
    :tz-pdt  :tz-pst  :tz-akdt
    :tz-akst :tz-hadt :tz-hast"
  (documentation 'mjr_date_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_us-tz-utc-offset (tz)
  "Return the offset, in hours, between the US timezone given by TZ and UTC.

If this function returns N for TZ, then UTC - N = Time in TZ"
  (cdr (assoc tz
              '(( :TZ-ADT  . -3 )     ;;  "Atlantic Daylight Time"        
                ( :TZ-AST  . -4 )     ;;  "Atlantic Standard Time"        
                ( :TZ-EDT  . -4 )     ;;  "Eastern Daylight Time"         
                ( :TZ-EST  . -5 )     ;;  "Eastern Standard Time"         
                ( :TZ-CDT  . -5 )     ;;  "Central Daylight Time"         
                ( :TZ-CST  . -6 )     ;;  "Central Standard Time"         
                ( :TZ-MDT  . -6 )     ;;  "Mountain Daylight Time"        
                ( :TZ-MST  . -7 )     ;;  "Mountain Standard Time"        
                ( :TZ-PDT  . -7 )     ;;  "Pacific Daylight Time"         
                ( :TZ-PST  . -8 )     ;;  "Pacific Standard Time"         
                ( :TZ-AKDT . -8 )     ;;  "Alaska Daylight Time"          
                ( :TZ-AKST . -9 )     ;;  "Alaska Standard Time"          
                ( :TZ-HADT . -9 )     ;;  "Hawaii-Aleutian Daylight Time" 
                ( :TZ-HAST . -10))))) ;;  "Hawaii-Aleutian Standard Time" 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_jdate2alternate (jdate &optional (aform :MJD))
  "Convert a Julian date (first return is same type as JDATE, second is DOUBLE-FLOAT) into one of the alternate forms:
  * MJD - Modified Julian Day      (The default)
  * JDN - Julian Day Number
  * CJD - Chronological Julian Day
  * RJD - Reduced Julian Day
  * TJD - Truncated Julian Day"
  (let ((ajdate (cond ((eq aform :JDN) (floor jdate))
                      ((eq aform :CJD) (floor (+ jdate 1/2)))
                      ((eq aform :RJD) (- jdate 2400000))
                      ((eq aform :MJD) (- jdate 4800001/2))
                      ((eq aform :TJD) (- jdate 4880001/2)))))
    (values ajdate (coerce ajdate 'double-float))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_month-name (month-number-or-name &key (january-index 1) (abbreviate-name nil))
  "Convert between integers and month names.
If MONTH-NUMBER-OR-NAME is a string, then the month number will be returned.  If it is an integer, then the name is returned.
When MONTH-NUMBER-OR-NAME is a string a prefix match is made against known month names.
JANUARY-INDEX sets the index corresponding to January, default is 1.
ABBREVIATE-NAME is only used when MONTH-NUMBER-OR-NAME is an integer.  If non-nil, then the returned name will be abbreviated."
  (if (not (integerp january-index))
      (error "mjr_date_month-name: JANUARY-INDEX must be an integer!"))
  (let ((mnames '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")))
    (cond ((stringp month-number-or-name)
           (let* ((the-pos (position-if (lambda (x) (mjr_string_starts-with month-number-or-name x)) mnames)))
             (if the-pos
                 (+ the-pos january-index))))
          ((integerp month-number-or-name)
           (progn
             (if (not (and (<= (- month-number-or-name january-index)) (>= (1- (length mnames)) (- month-number-or-name january-index))))
                 (error "mjr_date_month-name: MONTH-NUMBER-OR-NAME index is out of range!"))
             (if abbreviate-name
                 (subseq (nth (- month-number-or-name january-index) mnames) 0 3)
                 (nth (- month-number-or-name january-index) mnames))))
          ('t
           (error "mjr_date_month-name: Invalid MONTH-NUMBER-OR-NAME argument!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_day-name (day-number-or-name &key (monday-index 1) (abbreviate-name nil))
  "Convert between integers and day names.
If DAY-NUMBER-OR-NAME is a string, then the day number will be returned.  If it is an integer, then the name is returned.
When DAY-NUMBER-OR-NAME is a string a prefix match is made against known day names.
MONDAY-INDEX sets the index corresponding to Monday, default is 1.
ABBREVIATE-NAME is only used when DAY-NUMBER-OR-NAME is an integer.  If non-nil, then the returned name will be abbreviated."
  (if (not (integerp monday-index))
      (error "mjr_date_day-name: MONDAY-INDEX must be an integer!"))
  (let ((dnames '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (cond ((stringp day-number-or-name)
           (let* ((the-pos (position-if (lambda (x) (mjr_string_starts-with day-number-or-name x)) dnames)))
             (if the-pos
                 (+ the-pos monday-index))))
          ((integerp day-number-or-name)
           (progn
             (if (not (and (<= (- day-number-or-name monday-index)) (>=  (1- (length dnames))  (- day-number-or-name monday-index))))
                 (error "mjr_date_day-name: DAY-NUMBER-OR-NAME index is out of range!"))
             (if abbreviate-name
                 (subseq (nth (- day-number-or-name monday-index) dnames) 0 3)
                 (nth (- day-number-or-name monday-index) dnames))))
          ('t
           (error "mjr_date_day-name: Invalid DAY-NUMBER-OR-NAME argument!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_convert (from-fmt to-fmt &rest rest-args)
  "Convert between date formats:
  * :dt-list   -- list: date components: year, month, day, hour, min, second, zone
                - Time zone is in hours, and may be an integer, rational, or floating point.  The rest are integers.
                - Time zone may be nil indicating 'the current time zone'
  * :dt-string -- string: containing one of several ISO-like date formats:
                 * YYYY-MM-DD HH:MM:SS       -- The normal format, default time zone
                 * YYYY-MM-DD HH:MM:SSZ      -- UTC time zone
                 * YYYY-MM-DD HH:MM:SS+HH    -- Positive Time zone offset with just hours
                 * YYYY-MM-DD HH:MM:SS+HH:MM -- Positive Time zone offset with hours and minutes
                 * YYYY-MM-DD HH:MM:SS-HH    -- Negative Time zone offset with just hours
                 * YYYY-MM-DD HH:MM:SS-HH:MM -- Negative Time zone offset with hours and minutes
  * :dt-posix  -- integer: POSIX (UNIX or C) time integer
  * :dt-lisp   -- integer: LISP universal time integer
  * :dt-julian -- number:  'Julian Date'
When converting an integer based date to an :dt-list or :dt-string, a timezone may be added as a final argument -- otherwise, the current time zone will be
assumed.  The LISP function GET-UNIVERSAL-TIME will return the current :dt-lisp.

Theory of operation:
  A set of 4 bidirectional conversions are hand coded (:dt-string <-> :dt-list <-> :dt-lisp <-> :dt-posix <-> :dt-julian).  From these four, all 20 other
  conversions are obtained via composition of conversions.  As some conversions make use of a second argument that is the time zone, and the conversions that
  don't need this argument must simply ignore it if it is present."
  (flet ((f2= (x y) (and (equalp from-fmt x) (equalp to-fmt y))))
    (cond ((f2= :dt-posix  :dt-lisp)   (+ (or (first rest-args) 0) 2208988800))
          ((f2= :dt-lisp   :dt-posix)  (- (or (first rest-args) 0) 2208988800))
          ((f2= :dt-posix  :dt-julian) (+ (/ (or (first rest-args) 0) 86400) 4881175/2))
          ((f2= :dt-julian :dt-posix)  (* 86400 (- (or (first rest-args) 0) 4881175/2)))
          ((f2= :dt-lisp   :dt-list)   (let* ((time-number (truncate (or (first rest-args) 0)))
                                              (left-over   (second (multiple-value-list (truncate (or (first rest-args) 0)))))
                                              (in-tz       (second rest-args)))
                                         (if (not (zerop left-over))
                                             (print "mjr_date_convert: WARNING: Fractional seconds lost in conversion!"))
                                         (if (> 0 time-number)
                                             (error "mjr_date_convert: Dates before 1900 not supported for :dt-list and :dt-string"))
                                         ;; MJR TODO NOTE mjr_date_convert: add an alternate solution for this conversion for old dates...
                                         (multiple-value-bind (second minute hour dom month year dow dst-p out-tz)
                                             (if in-tz
                                                 (decode-universal-time time-number (- in-tz))
                                                 (decode-universal-time time-number))
                                           (declare (ignore dow dst-p)) ;; Keep compiler happy. :)
                                           (list year month dom hour minute second (- out-tz))))) ; POSIX TZs are negative
          ((f2= :dt-list   :dt-lisp)   (apply #'encode-universal-time
                                              (append (loop for i from 5 downto 0 collect (or (nth i (first rest-args)) 0))
                                                      (if (nth 6 (first rest-args)) (list (- (nth 6 (first rest-args))))))))
          ((f2= :dt-string :dt-list)   (let* ((the-str (first rest-args))
                                              (str-len (length the-str))
                                              (the-ints nil)
                                              (end-z?   (eq #\Z (elt (reverse (string-upcase the-str)) 0)))
                                              (p        -1))
                                         (loop for i from 1 upto 8 ; Extract the parts
                                               do (multiple-value-bind (nxt-int nxt-pos)
                                                      (parse-integer the-str :start (min (+ (if (= i 7) 0 1) p) str-len) :junk-allowed 't)
                                                    (setq the-ints (append the-ints (list nxt-int))
                                                          p        nxt-pos)))
                                         (mapcar ; Figure out what to do about time zone, then replace nil with 0
                                          (lambda (x) (or x 0))
                                          (cond (end-z?            (append (subseq the-ints 0 6) (list 0)))
                                                ((nth 7 the-ints)  (append (subseq the-ints 0 6)
                                                                           (list (+ (nth 6 the-ints)
                                                                                    (* (signum (nth 6 the-ints)) (/ (nth 7 the-ints) 60))))))
                                                ((nth 6 the-ints)  (subseq the-ints 0 7))
                                                ('t                (subseq the-ints 0 6))))))
          ((f2= :dt-list   :dt-string) (let* ((tz (nth 6 (first rest-args))))
                                         (concatenate 'string
                                                      (apply #' format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                                                                (loop repeat 6 for v in (first rest-args) collect (or v 0)))
                                                      (cond ((null tz)     "")
                                                            ((zerop tz)    "Z")
                                                            ((integerp tz) (format nil "~a~2,'0d" (if (<= 0 tz) "+" "-") (abs tz)))
                                                            ((numberp tz)  (apply #'format nil "~a~2,'0d:~2,'0d"
                                                                                  (if (<= 0 tz) "+" "-")
                                                                                  (subseq (multiple-value-list (mjr_a_d2dms (abs tz))) 0 2)))))))
          ('t                          (let* ((good-c  '(:dt-string :dt-list :dt-lisp :dt-posix :dt-julian))
                                              (xpos    (position from-fmt good-c))
                                              (ypos    (position to-fmt good-c)))
                                         (if (not (and (integerp xpos) (integerp ypos)))
                                             (error "mjr_date_convert: Unknown date format")
                                             (let ((nxt-fmt (cond ((< xpos ypos)  (nth (+ xpos 1) good-c))
                                                                  ((> xpos ypos)  (nth (- xpos 1) good-c))
                                                                  ('t             nil))))
                                               (if nxt-fmt
                                                   (mjr_date_convert nxt-fmt to-fmt (apply #' mjr_date_convert from-fmt nxt-fmt rest-args) (second rest-args))
                                                   (first rest-args)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_now (&optional (date-fmt :dt-string))
  "Return current date in the given formats.

Formats:
  * :dt-list   -- list: date components: year, month, day, hour, min, second, zone
  * :dt-string -- ISO-like date string
  * :dt-posix  -- integer: POSIX (UNIX or C) time integer
  * :dt-lisp   -- integer: LISP universal time integer
  * :dt-julian -- number:  'Julian Date'"
  (if (eq :dt-lisp date-fmt)
      (get-universal-time)
      (mjr_date_convert :dt-lisp date-fmt (get-universal-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_add-delta (the-date the-delta &key (date-units :dt-string) (delta-units "days") (return-units :dt-string) (return-zone nil))
  (let ((the-date          (if the-date
                               (mjr_date_convert date-units :dt-lisp the-date)
                               (get-universal-time)))
        (the-delta-seconds (cond ((or (string-equal delta-units "day")  (string-equal delta-units "days"))  (* the-delta 24 60 60))
                                 ((or (string-equal delta-units "hour") (string-equal delta-units "hours")) (* the-delta 60 60))
                                 ((or (string-equal delta-units "min")  (string-equal delta-units "mins"))  (* the-delta 60))
                                 ((or (string-equal delta-units "sec")  (string-equal delta-units "secs"))  the-delta)
                                 ('t                                    (error "mjr_date_add-delta: Unsupported unit!")))))
    (mjr_date_convert :dt-lisp return-units (+ the-date the-delta-seconds) return-zone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_delta (date1 date2 &key (date1-units :dt-string) (date2-units :dt-string) (delta-units "days"))
  (let ((date1             (if date1
                               (mjr_date_convert date1-units :dt-lisp date1)
                               (get-universal-time)))
        (date2             (if date2
                               (mjr_date_convert date2-units :dt-lisp date2)
                               (get-universal-time)))
        (the-delta-factor  (cond ((or (string-equal delta-units "day")  (string-equal delta-units "days"))  (* 1 24 60 60))
                                 ((or (string-equal delta-units "hour") (string-equal delta-units "hours")) (* 1 60 60))
                                 ((or (string-equal delta-units "min")  (string-equal delta-units "mins"))  (* 1 60))
                                 ((or (string-equal delta-units "sec")  (string-equal delta-units "secs"))  1)
                                 ('t                                    (error "mjr_date_add-delta: Unsupported unit!")))))
    (/ (- date2 date1) the-delta-factor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_date_unx-convert (&optional anArg)
  "Convert between UNIX integer date and a string date.  Returns integer, local date string, and Zulu date string

Replicates the functionality of the Perl scripts (time2int.pl & time2int.pl) that I use all the time."
  (if (integerp anArg)
      (values anArg
              (mjr_date_convert :dt-posix :dt-string anArg)
              (mjr_date_convert :dt-posix :dt-string anArg 0))
      (mjr_date_unx-convert (mjr_date_convert :dt-string :dt-posix anArg))))

