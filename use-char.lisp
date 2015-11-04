;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-char.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Character (ASCII & EBCIDIC) tools.@EOL@EOL
;; @std       Common Lisp
;; @see       tst-char.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1998,2008,2011,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CHAR 
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Character (ASCII & EBCIDIC) tools.;")
  (:EXPORT #:mjr_char_help

           #:mjr_char_table

           #:mjr_char_int2ch
           #:mjr_char_ch2int

           #:mjr_char_in-class

           #:mjr_char_digitsp
           #:mjr_char_lettersp
           #:mjr_char_uppercasep
           #:mjr_char_lowercasep
           #:mjr_char_whitespacep
           ))

(in-package :MJR_CHAR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_help ()
  "Help for MJR_CHAR:

The focus of this package is on characters (think ASCII):
  * Portable tools to convert to manipulate ASCII and EBCIDIC character sets.
  * Character classes (digits, letters, etc...)
  * Character reference for humans (i.e. ASCII and EBCIDIC tables)"
  (documentation 'mjr_char_help 'function))

(defparameter *char-sets*
  (list '(:cs-ascii  . ("NUL" "SOH" "STX" "ETX" "EOT" "NEQ" "ACK" "BEL" "BS" "HT"   "NL"  "VT"  "NP" "CR"  "SO"   "SI"
                        "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM"  "SUB" "ESC" "FS" "GS"  "RS"   "US"
                        "SP"  "!"   "\""  "#"   "$"   "%"   "&"   "'"   "("   ")"   "*"   "+"   ","   "-"   "."   "/"
                        "0"   "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   ":"   ";"   "<"   "="   ">"   "?"
                        "@"   "A"   "B"   "C"   "D"   "E"   "F"   "G"   "H"   "I"   "J"   "K"   "L"   "M"   "N"   "O"
                        "P"   "Q"   "R"   "S"   "T"   "U"   "V"   "W"   "X"   "Y"   "Z"   "["   "\\"  "]"   "^"   "_"
                        "`"   "a"   "b"   "c"   "d"   "e"   "f"   "g"   "h"   "i"   "j"   "k"   "l"   "m"   "n"   "o"
                        "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"   "y"   "z"   "{"   "|"   "}"   "~"   "DEL"))
        '(:cs-ebcdic . ("NUL" "SOH" "STX" "ETX" "PF" "HT" "LC" "DEL" "GE" "RLF" "SMM" "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2"
                        "TM" "RES" "NL" "BS" "IL" "CAN" "EM" "CC" "CU1" "IFS" "IGS" "IRS" "IUS" "DS" "SOS" "FS" nil "BYP" "LF"
                        "ETB" "ESC" nil nil "SM" "CU2" nil "ENQ" "ACK" "BEL" nil nil "SYN" nil "PN" "RS" "UC" "EOT" nil nil nil
                        "CUB" "DC4" "NAK" nil "SUB" "SP" nil nil nil nil nil nil nil nil nil nil "."  "<" "(" "+" "|" nil nil
                        nil nil nil nil nil nil nil nil "!"  "$" "*" ")" ";" nil "-" "/" nil nil nil nil nil nil nil nil nil ","
                        "%" "_" ">" "?"  nil nil nil nil nil nil nil nil nil "`" ":" "#" "@" "'" "=" "\"" nil "a" "b" "c" "d"
                        "e" "f" "g" "h" "i" nil nil nil nil nil nil nil "j" "k" "l" "m" "n" "o" "p" "q" "r" nil nil nil nil nil
                        nil nil "~" "s" "t" "u" "v" "w" "x" "y" "z" nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                        nil nil nil nil nil nil nil "{" "A" "B" "C" "D" "E" "F" "G" "H" "I" nil nil nil nil nil nil "}" "J" "K"
                        "L" "M" "N" "O" "P" "Q" "R" nil nil nil nil nil nil "\\" nil "S" "T" "U" "V" "W" "X" "Y" "Z" nil nil nil
                        nil nil nil "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" nil nil nil nil nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_int2ch (n &key (char-set :cs-ascii) (result-type 'string))
  "Convert an integer into a character string in the given character set.  

If a character code maps to a non-printable character, then the returned string is a typical representation of the value -- DEL for the delete character for
example (ASCII 127). The return is nil if the integer can not be converted.

If :result-type is 'string, then a string is returned.  Otherwise a character is returned.

The :char-set may be :cs-ascii or :cs-ebcdic.  For :cs-ebcdic only characters with ASCII equivalents are converted."
  (let ((char-lst (cdr (assoc char-set *char-sets*))))
    (if (null char-lst)
        (error "mjr_char_int2ch: CHAR-SET must be one of :CS-ASCII or :CS-EBCDIC. ")
        (let ((char (nth n char-lst)))
          (if (equalp result-type 'string)
              char
              (and char (aref char 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_ch2int (char &key (char-set :cs-ascii))
  "Convert a character into an integer in the given character set.  

Note CHAR may be a character string representing a non-printable character like DEL (ASCII 127). The return is nil if the integer can not be converted.

The :char-set may be :cs-ascii or :cs-ebcdic.  For :cs-ebcdic only characters with ASCII equivalents are converted."
  (let ((char-lst (cdr (assoc char-set *char-sets*))))
    (if (null char-lst)
        (error "mjr_char_int2ch: CHAR-SET must be one of :CS-ASCII or :CS-EBCDIC. ")
        (let ((char (if (stringp char) char (string char))))
            (position char char-lst :test #'string=)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_table (&key (base :d) (rows 16) (char-set :cs-ascii))
  "Print out an ASCII table.

Arguments:

  :base is used to specify the base of the numeric output.  It may be one of :d (decimal), :h (hex), or :o (oct).  It may also be an integer (16, 10, or 8).

  :rows is used to specify the number of rows used for the print out.  The default is 16.  Other values that yield attractive print outs are 8, 32, 64, 128,
  and 256.

  :char-set is used to specify the character set.  It may be one of :cs-ascii or :cs-ebcdic.  In the case of :cs-ebcdic, only the characters that have
  corresponding ASCII equivalents are printed."
  (let ((base (if (numberp base)
                  (second (assoc base '((8 :o) (10 :d) (16 :h))))
                  base)))
    (cond ((not (member char-set '(:cs-ascii :cs-ebcdic)))   (error "mjr_char_table: CHAR-SET must be one of :CS-ASCII or :CS-EBCDIC. "))
          ((not (member base '(:h :o :d)))             (error "mjr_char_table: BASE should be one of :h, :d, or :o!")))
    (let* ((num-chrs (if (eq char-set :cs-ascii) 128 256))
           (rows     (min rows num-chrs))
           (num-cols (/ num-chrs rows)))
      ;; Titles
      (format 't "| ")
      (dotimes (col-num num-cols)
        (format 't (cdr (assoc base '((:h . "CHR Hx | ") (:o . "CHR Oct | ")  (:d . "CHR Dec | "))))))
      (format 't "~&")
      ;; Char table
      (dotimes (row-num rows)
        (format 't "| ")
        (dotimes (col-num num-cols)
          (let ((cur-char (+ row-num (* col-num rows))))
            (if (< cur-char num-chrs)
                (format 't (cdr (assoc base '((:h . "~3a ~2,'0x | ") (:o . "~3a ~3,'0o | ")  (:d . "~3a ~3d | "))))
                        (mjr_char_int2ch cur-char :char-set char-set)
                        cur-char))))
        (format 't "~&")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_in-class (the-string-or-char class-string)
  "non-nil iff the-string-or-char is a in CLASS-STRING or a string containing only elements of CLASS-STRING.
Note this function will be vacuously true for any empty string."
  (or (and (characterp the-string-or-char)
           (find the-string-or-char class-string))
      (and (stringp the-string-or-char)
           (every (lambda (char) (find char class-string)) the-string-or-char))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_digitsp (the-string-or-char)
  "non-nil iff the-string-or-char is a digit or a string containing only digits.
Note this function will be vacuously true for any empty string."
  (mjr_char_in-class the-string-or-char "0123456789"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_lettersp (the-string-or-char)
  "non-nil iff the-string-or-char is a letter or a string containing only letters.
Note this function will be vacuously true for any empty string."
  (mjr_char_in-class the-string-or-char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_uppercasep (the-string-or-char)
  "non-nil iff the-string-or-char is a uppercase letter or string containing only uppercase letters.
Note this function will be vacuously true for any empty string."
  (mjr_char_in-class the-string-or-char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_lowercasep (the-string-or-char)
  "non-nil iff the-string-or-char is a lowercase letter or string containing only lowercase letters.
Note this function will be vacuously true for any empty string."
  (mjr_char_in-class the-string-or-char "abcdefghijklmnopqrstuvwxyz"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_char_whitespacep (the-string-or-char)
  "non-nil iff the-string-or-char is whitespace character or string containing only whitespace characters.
Note this function will be vacuously true for any empty string."
  (mjr_char_in-class the-string-or-char #( #\  #\Tab #\Newline #\Return )))

