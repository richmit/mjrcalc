;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-string.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1998,2008,2013 by Mitch Richling.  All rights reserved.
;; @brief     String utilities.@EOL
;; @Keywords  lisp interactive chars table
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_STRING
  (:USE :COMMON-LISP
        :MJR_CHAR)
  (:DOCUMENTATION "Brief: String utilities.;")
  (:EXPORT #:mjr_string_starts-with

           #:mjr_string_match-integer #:mjr_string_match-float #:mjr_string_match-rational

           #:mjr_string_ltrim #:mjr_string_rtrim #:mjr_string_trim          

           #:mjr_string_parse-number #:mjr_string_parse-c-identifier

           #:mjr_string_split-if #:mjr_string_split

           #:mjr_string_join

           #:mjr_string_read-as-lisp
           ))

(in-package :MJR_STRING)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_read-as-lisp (the-string)
  "Evaluate the given string as lisp code, and return it's value."
  (with-input-from-string (stream the-string) (read stream nil)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_starts-with (small-string big-string &key (case-sensitive nil))
  "Return non-nil if BIG-STRING begins with SMALL-STRING."
  (let* ((small-string (string small-string))
         (big-string   (string big-string))
         (small-length (length small-string))
         (big-length   (length big-string)))
    (if (<= small-length big-length)
        (if case-sensitive
            (string=      small-string (subseq big-string 0 small-length))
            (string-equal small-string (subseq big-string 0 small-length))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_match-integer (the-string)
  "Is the-string an integer (possibly signed)"
; MJR TODO NOTE <2011-10-30 17:09:12 CDT> mjr_string_match-integer: Add :START and :END
  (if (> (length the-string) 0)
      (if (find (aref the-string 0) "-+")
          (and (mjr_char_digitsp (subseq the-string 1))
               (> (length the-string) 1))
          (mjr_char_digitsp the-string))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_join (separator &rest rest)
  "Join the strings with SEPARATOR between.  Returns a string.
Notes:
  * SEPARATOR may be a character or string"
  (format nil
          (concatenate 'string "~{~A~^" (if (stringp separator) separator (make-string 1 :initial-element separator)) "~}")
          (cond ((stringp (first rest)) rest)
                ((listp   (first rest)) (first rest))
                ((vectorp (first rest)) (concatenate 'list (first rest)))
                ('t                     (print "mjr_string_join: Bad argument")))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_split-if (the-string split-predicate &optional max-find)
  "Split the string up on the characters for shich SPLIT-PREDICATE is non-NIL.  Returns a list of strings."
  (cond ((and max-find (< max-find 1))            (error "mjr_string_split-if: MAX-FIND must be greater than 0 if provided"))
        ((and max-find (not (numberp max-find)))  (error "mjr_string_split-if: MAX-FIND must be NIL or a number"))
        ((and max-find (not (integerp max-find))) (error "mjr_string_split-if: MAX-FIND must be NIL or an integer")))
  (loop with cur-pos = 0
        with str-len = (length the-string)
        for num-fnd from 1
        for nxt-pos = (position-if split-predicate the-string :start cur-pos)
        collect (subseq the-string cur-pos (or nxt-pos str-len))
        until (null nxt-pos)
        until (and max-find (>= num-fnd max-find))
        do (setq cur-pos (1+ nxt-pos))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_split (the-string split-char &optional max-find)
  "Split the string up on the given character.  Returns a list of strings.

NOTE: If split-char is a single character string, everything still works.
NOTE: to split on commas, then use #\, for the split char.
SEE: cl-ppcre:split for an equivalent using a RegEx"
  (cond ((characterp split-char) (mjr_string_split-if the-string (lambda (c) (equal c split-char)) max-find))
        ((stringp split-char)    (mjr_string_split    the-string (aref split-char 0)               max-find))
        ('t                      (error "mjr_string_split: SPLIT-CHAR must be a character or string"))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_match-float (the-string)
; MJR TODO NOTE <2011-10-30 17:09:12 CDT> mjr_string_match-float: Add :START and :END
  "Is the-string a floating point number (possibly signed)"
  (let* ((the-string (substitute #\E #\D (substitute #\E #\L (string-upcase the-string))))
         (tmp1 (mjr_string_split the-string #\E)))
    (cond ((= 1 (length tmp1)) (let ((tmp2 (mjr_string_split the-string #\.)))                ;; no e
                                 (cond ((= 1 (length tmp2)) (mjr_string_match-integer the-string))    ;;   no .
                                       ((= 2 (length tmp2)) (and (mjr_char_digitsp (second tmp2)) ;;   hav .
                                                                 (if (and (> (length (first tmp2)) 0)
                                                                          (find (aref (first tmp2) 0) "-+"))
                                                                     (and (> (length the-string) 2)
                                                                          (mjr_char_digitsp (subseq (first tmp2) 1)))
                                                                     (and (> (length the-string) 1)
                                                                          (mjr_char_digitsp (first tmp2)))))))))
          ((= 2 (length tmp1)) (and (mjr_string_match-float   (first tmp1))                           ;; hav e
                                    (mjr_string_match-integer (second tmp1)))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_match-rational (the-string)
; MJR TODO NOTE <2011-10-30 17:09:12 CDT> mjr_string_match-rational: Add :START and :END
  "Is the-string a rational number (possibly signed)"
  (let ((tmp1 (mjr_string_split the-string #\/)))
    (cond ((= 1 (length tmp1)) (mjr_string_match-integer the-string))         ;;   no /
          ((= 2 (length tmp1)) (and (mjr_string_match-integer (first  tmp1))  ;;   hav /
                                    (mjr_char_digitsp     (second tmp1))
                                    (> (length (first  tmp1)) 0)
                                    (> (length (second tmp1)) 0))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_ltrim (the-string &optional (chars-to-trim #'mjr_char_whitespacep))
  "Trim THE-STRING on the left.  Returns THE-STRING if nothing to trim, returns a new string otherwise."
  (let ((pos (position-if (lambda (x) (not (funcall chars-to-trim x))) the-string)))
    (if pos
        (if (zerop pos)
            the-string
            (subseq the-string pos))
        "")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_rtrim (the-string &optional (chars-to-trim #'mjr_char_whitespacep))
  "Trim THE-STRING on the right.  Returns THE-STRING if nothing to trip, returns a new string otherwise."
  (let ((pos (position-if (lambda (x) (not (funcall chars-to-trim x))) the-string :from-end 't)))
    (if pos
        (if (= (1+ pos) (length the-string))
            the-string
            (subseq the-string 0 (1+ pos)))
        "")))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_trim (the-string &optional (chars-to-trim #'mjr_char_whitespacep))
  ;; MJR TODO NOTE <2011-10-25 20:58:30 CDT> mjr_string_trim: optimize
  "Trim THE-STRING on the right and left"
  (mjr_string_rtrim (mjr_string_ltrim the-string chars-to-trim) chars-to-trim))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_parse-number (the-string &key no-rat no-float (start 0))
  "Return the index of the character immediately following the number at the head of THE-STRING, or NIL if none found.
The algorithm is greedy -- i.e. largest string that matches is found.
:NO-RAT and :NO-FLOAT can suppress the matching of rationals and floating point numbers respectively.
Rationals, integers, floating point Numbers are all supported.
If a number is found, then a second return value repreents the type of the number found (:integer, :rational, :float)"
  (let ((len (length the-string))
        (typ :integer))
    (if (> len start)
        (let ((cnt (loop with dot-count = 0
                         with exp-count = 0
                         with dig-count = 0
                         with frc-count = 0
                         for i from start upto (1- len)
                         for c = (aref the-string i)
                         while (or
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (mjr_char_digitsp c)                                    ;; Current char is a digit
                                     (incf dig-count))                                       ;; increment digit counter
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (equal c #\.)                                           ;; Current char is a dot
                                     (not no-float)                                          ;; We are allowed to return floats
                                     (= 0 exp-count)                                         ;; no exponent yet
                                     (or                                                     ;; char before or after is digit
                                      (and (> i 0)                                           ;;   At least 1 char before current
                                           (mjr_char_digitsp (aref the-string (1- i))))      ;;     and it is a digit
                                      (and (< i (- len 2))                                   ;;   At least 1 char after current
                                           (mjr_char_digitsp (aref the-string (1+ i)))))     ;;     and it is a digit
                                     (= 1 (incf dot-count))                                  ;; never found a dot before
                                     (setq typ :float))                                      ;; Set return type to float
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (find c "eEdD")                                         ;; Current char is an exponent char
                                     (< 0 dig-count)                                         ;; We previously found at least 1 digit
                                     (not no-float)                                          ;; We are allowed to return floats
                                     (or                                                     ;; followed by a digit or a sign and digit
                                      (and (< i (- len 1))                                   ;;   At least one more char
                                           (mjr_char_digitsp (aref the-string (1+ i))))      ;;     and it is a digit
                                      (and (< i (- len 2))                                   ;;   At least two more char
                                           (find (aref the-string (1+ i)) "+-")              ;;     and first is a sign
                                           (mjr_char_digitsp (aref the-string (+ 2 i)))))    ;;     and second is a digit
                                     (= 1 (incf exp-count))                                  ;; This is our first exponent char
                                     (setq typ :float))                                      ;; Set return type to float
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (equal c #\/)                                           ;; Current char is a /
                                     (< i (- len 1))                                         ;; We are not at the end of the string
                                     (< 0 dig-count)                                         ;; We previously found at least 1 digit
                                     (not no-rat)                                            ;; We are allowed to return rationals
                                     (mjr_char_digitsp (aref the-string (1+ i)))             ;; Next char is a digit
                                     (= 1 (incf frc-count))                                  ;; This is our first /
                                     (setq typ :rational))                                   ;; Set return type to rational
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (= i start)                                             ;; On first char
                                     (find c "+-")                                           ;; Current char is a sign (+ or -)
                                     (or                                                     ;; followed by a digit or a dot and digit
                                      (and (< i (- len 1))                                   ;;   At least one more char
                                           (mjr_char_digitsp (aref the-string (1+ i))))      ;;     and it is a digit
                                      (and (< i (- len 2))                                   ;;   At least two more char
                                           (equal (aref the-string 1) #\.)                   ;;     and first is a dot
                                           (mjr_char_digitsp (aref the-string (+ 2 i))))))   ;;     and second is a digit
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                (and (= exp-count 1)                                         ;; We have found an exponent char (eEdD) already
                                     (find c "+-")                                           ;; Current char is a sign
                                     (< i (- len 1))                                         ;; We are not at the end of the string
                                     (find (aref the-string (1- i)) "eEdD")                  ;; The last char was an exponent char
                                     (mjr_char_digitsp (aref the-string (1+ i))))            ;; Next char is a digit
                                )
                         count 't)))
          (if (not (zerop cnt))
              (values (1- (+ start cnt)) typ))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_string_parse-c-identifier (the-string &key (start 0))
  "Return the index of the last character of the c-identifier at the head of THE-STRING, or NIL if none found
The algorithm is greedy -- i.e. largest string that matches is found."
  (let ((len (length the-string)))
    (if (and (> len start)
             (not (mjr_char_digitsp (aref the-string start))))
        (let ((cnt (loop for i from start upto (1- len)
                         for c = (aref the-string i)
                         while (mjr_char_in-class c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
                         count 't)))
          (if (not (zerop cnt))
              (1- (+ start cnt)))))))


; 0         0         0         0         0         0         0         0         0         0         1         1         1         1         1         1         1         1         1         1
; 0         1         2         3         4         5         6         7         8         9         0         1         1         3         4         5         6         7         8         9
; 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
;"  now + is the tiem   323  for  42343 all good    men 243 23 to come to the aid of their   country   "
