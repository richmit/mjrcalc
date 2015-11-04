;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-string.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Unit Tests.@EOL
;; @std       Common Lisp
;; @see       use-string.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_STRING-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_STRING :MJR_PRNG))

(in-package :MJR_STRING-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_read-as-lisp
  (assert-equalp '(+ 1 1) (mjr_string_read-as-lisp "(+ 1 1)"))
  (assert-equalp 2        (eval (mjr_string_read-as-lisp "(+ 1 1)")))
  (assert-equalp 2        (mjr_string_read-as-lisp "2"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_starts-with
  (assert-true          (mjr_string_starts-with "foo" "foobar"))
  (assert-true          (mjr_string_starts-with "foo" "foo"))
  ;; Test case insensitivity
  (assert-true          (mjr_string_starts-with "foo" "FOOBAR"))
  (assert-true          (mjr_string_starts-with "foo" "FOO"))
  ;; Symbols.
  (assert-true          (mjr_string_starts-with :foo  "FOOBAR"))
  (assert-true          (mjr_string_starts-with :foo  "FOO"))
  (assert-true          (mjr_string_starts-with "foo" :foobar))
  (assert-true          (mjr_string_starts-with "foo" :foo))
  (assert-true          (mjr_string_starts-with :foo  :FOOBAR))
  (assert-true          (mjr_string_starts-with :foo  :FOO))
  ;; Case sensitive
  (assert-false         (mjr_string_starts-with "foo" "FOOBAR" :case-sensitive 't))
  (assert-false         (mjr_string_starts-with "foo" "FOO"    :case-sensitive 't))
  ;; Not matches
  (assert-false         (mjr_string_starts-with "foo" "fozbar"))
  (assert-false         (mjr_string_starts-with "foo" "fo"))
  ;; Errors
  (assert-error 'error  (mjr_string_starts-with 1     "1 bug"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_match-rational
  (assert-true (mjr_string_match-rational "1/2"))
  (assert-true (mjr_string_match-rational "12/2"))
  (assert-true (mjr_string_match-rational  "1/22"))

  (assert-false (mjr_string_match-rational  ""))
  (assert-false (mjr_string_match-rational  "+"))
  (assert-false (mjr_string_match-rational  "-"))

  (assert-false (mjr_string_match-rational  "++123/232"))
  (assert-false (mjr_string_match-rational  "-+123/232"))
  (assert-false (mjr_string_match-rational  "+-123/232"))
  (assert-false (mjr_string_match-rational  "--123/232"))

  (assert-false (mjr_string_match-rational "23/"))
  (assert-false (mjr_string_match-rational "/23"))
  (assert-false (mjr_string_match-rational "/"))
  (assert-false (mjr_string_match-rational "+/"))
  (assert-false (mjr_string_match-rational "-/"))

  (let ((bignumber (expt 10 100)))
    (dotimes (i 2000)
      (assert-true (mjr_string_match-rational (format nil "~s" (/ (mjr_prng_int-cc (- bignumber) bignumber) (mjr_prng_int-cc 1 bignumber)))))
      (assert-true (mjr_string_match-rational (format nil "~s" (mjr_prng_int-cc (- bignumber) bignumber))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_match-integer
  (assert-true (mjr_string_match-integer "+123"))
  (assert-true (mjr_string_match-integer "-123"))
  (assert-true (mjr_string_match-integer  "123"))

  (assert-false (mjr_string_match-integer  ""))
  (assert-false (mjr_string_match-integer  "+"))
  (assert-false (mjr_string_match-integer  "-"))

  (assert-false (mjr_string_match-integer  "++123"))
  (assert-false (mjr_string_match-integer  "-+123"))
  (assert-false (mjr_string_match-integer  "+-123"))
  (assert-false (mjr_string_match-integer  "--123"))

  (assert-false (mjr_string_match-integer  "123."))
  (assert-false (mjr_string_match-integer "123e4"))
  (assert-false (mjr_string_match-integer  "1-23"))
  (assert-false (mjr_string_match-integer  "1+23"))
  (assert-false (mjr_string_match-integer  "123-"))
  (assert-false (mjr_string_match-integer  "123+"))

  (let ((bignumber (expt 10 100)))
    (dotimes (i 5000)
      (assert-true (mjr_string_match-integer (format nil "~s" (mjr_prng_int-cc (- bignumber) bignumber))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_match-float
  (assert-true (mjr_string_match-float "+123"))
  (assert-true (mjr_string_match-float "-123"))
  (assert-true (mjr_string_match-float  "123"))
  (assert-true (mjr_string_match-float "+123e+3"))
  (assert-true (mjr_string_match-float "-123e-3"))
  (assert-true (mjr_string_match-float  "123e3"))
  (assert-true (mjr_string_match-float "+1.23"))
  (assert-true (mjr_string_match-float "-1.23"))
  (assert-true (mjr_string_match-float  "1.23"))
  (assert-true (mjr_string_match-float "+1.23e+3"))
  (assert-true (mjr_string_match-float "-1.23e-3"))
  (assert-true (mjr_string_match-float  "1.23e3"))

  (assert-true (mjr_string_match-float  "123."))
  (assert-true (mjr_string_match-float  ".123"))
  (assert-true (mjr_string_match-float  "-123."))
  (assert-true (mjr_string_match-float  "-.123"))
  (assert-true (mjr_string_match-float  "+123."))
  (assert-true (mjr_string_match-float  "+.123"))

  (assert-true (mjr_string_match-float "+123E+3"))
  (assert-true (mjr_string_match-float "-123E-3"))
  (assert-true (mjr_string_match-float  "123E3"))
  (assert-true (mjr_string_match-float "+1.23E+3"))
  (assert-true (mjr_string_match-float "-1.23E-3"))
  (assert-true (mjr_string_match-float  "1.23E3"))
  (assert-true (mjr_string_match-float "+123D+3"))
  (assert-true (mjr_string_match-float "-123D-3"))
  (assert-true (mjr_string_match-float  "123D3"))
  (assert-true (mjr_string_match-float "+1.23D+3"))
  (assert-true (mjr_string_match-float "-1.23D-3"))
  (assert-true (mjr_string_match-float  "1.23D3"))
  (assert-true (mjr_string_match-float "+123L+3"))
  (assert-true (mjr_string_match-float "-123L-3"))
  (assert-true (mjr_string_match-float  "123L3"))
  (assert-true (mjr_string_match-float "+1.23L+3"))
  (assert-true (mjr_string_match-float "-1.23L-3"))
  (assert-true (mjr_string_match-float  "1.23L3"))
  (assert-true (mjr_string_match-float "+123d+3"))
  (assert-true (mjr_string_match-float "-123d-3"))
  (assert-true (mjr_string_match-float  "123d3"))
  (assert-true (mjr_string_match-float "+1.23d+3"))
  (assert-true (mjr_string_match-float "-1.23d-3"))
  (assert-true (mjr_string_match-float  "1.23d3"))
  (assert-true (mjr_string_match-float "+123l+3"))
  (assert-true (mjr_string_match-float "-123l-3"))
  (assert-true (mjr_string_match-float  "123l3"))
  (assert-true (mjr_string_match-float "+1.23l+3"))
  (assert-true (mjr_string_match-float "-1.23l-3"))
  (assert-true (mjr_string_match-float  "1.23l3"))

  (assert-false (mjr_string_match-float  "123e"))
  (assert-false (mjr_string_match-float  "."))
  (assert-false (mjr_string_match-float  ".e1"))
  (assert-false (mjr_string_match-float  "1-23"))
  (assert-false (mjr_string_match-float  "1+23"))
  (assert-false (mjr_string_match-float  "123-"))
  (assert-false (mjr_string_match-float  "123+"))

  (dotimes (i 1000)
    (assert-true (mjr_string_match-float (format nil "~s" (mjr_prng_float-cc -1d300 1d300))))
    (assert-true (mjr_string_match-float (format nil "~s" (mjr_prng_float-cc -1d30 1d30)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_join
  (assert-equalp "hello,world"       (mjr_string_join #\,   "hello" "world"     ))
  (assert-equalp "hello"             (mjr_string_join #\,   "hello"             ))
  (assert-equalp "hello,,world"      (mjr_string_join #\,   "hello" "" "world"  ))

  (assert-equalp "hello,world"       (mjr_string_join ","   "hello" "world"     ))
  (assert-equalp "hello"             (mjr_string_join ","   "hello"             ))
  (assert-equalp "hello,,world"      (mjr_string_join ","   "hello" "" "world"  ))

  (assert-equalp "hello;world"       (mjr_string_join ";"   "hello" "world"     ))
  (assert-equalp "hello"             (mjr_string_join ";"   "hello"             ))
  (assert-equalp "hello;;world"      (mjr_string_join ";"   "hello" "" "world"  ))

  (assert-equalp "hello;:;world"     (mjr_string_join ";:;" "hello" "world"     ))
  (assert-equalp "hello"             (mjr_string_join ";:;" "hello"             ))
  (assert-equalp "hello;:;;:;world"  (mjr_string_join ";:;" "hello" "" "world"  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_split
  (assert-equalp '("hello" "world")  (mjr_string_split "hello,world"  #\,))
  (assert-equalp '("hello world")    (mjr_string_split "hello world"  #\,))
  (assert-equalp '("hello world" "") (mjr_string_split "hello world," #\,))
  (assert-equalp '("" "hello world") (mjr_string_split ",hello world" #\,))

  (assert-equalp '("hello" "world")  (mjr_string_split "hello,world"  ","))
  (assert-equalp '("hello world")    (mjr_string_split "hello world"  ","))
  (assert-equalp '("hello world" "") (mjr_string_split "hello world," ","))
  (assert-equalp '("" "hello world") (mjr_string_split ",hello world" ","))

  (assert-equalp '("hello" "world")  (mjr_string_split "hello,world"  #\, nil))
  (assert-equalp '("hello world")    (mjr_string_split "hello world"  #\, nil))
  (assert-equalp '("hello world" "") (mjr_string_split "hello world," #\, nil))
  (assert-equalp '("" "hello world") (mjr_string_split ",hello world" #\, nil))

  (assert-equalp '("hello" "world")  (mjr_string_split "hello,world"  "," nil))
  (assert-equalp '("hello world")    (mjr_string_split "hello world"  "," nil))
  (assert-equalp '("hello world" "") (mjr_string_split "hello world," "," nil))
  (assert-equalp '("" "hello world") (mjr_string_split ",hello world" "," nil))

  (assert-equalp '("hello world")    (mjr_string_split "hello world," #\, 1))
  (assert-equalp '("")               (mjr_string_split ",hello world" #\, 1))

  (assert-equalp '("hello world")    (mjr_string_split "hello world," "," 1))
  (assert-equalp '("")               (mjr_string_split ",hello world" "," 1))

  ;; Special case
  (assert-equalp '("hello" "world")  (mjr_string_split "hello world" " "))
  (assert-equalp '("hello" "world")  (mjr_string_split "hello world" " a"))

  ;; Errors
  (assert-error 'error  (mjr_string_split "hello world" 'a))
  (assert-error 'error  (mjr_string_split "hello world" :a))
  (assert-error 'error  (mjr_string_split "hello world" 1))
  (assert-error 'error  (mjr_string_split "hello world," #\, 0))
  (assert-error 'error  (mjr_string_split ",hello world" #\, 0))
  (assert-error 'error  (mjr_string_split "hello world," #\, 0.3))
  (assert-error 'error  (mjr_string_split ",hello world" #\, 0.3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_ltrim
    (assert-equalp "hello,world "     (mjr_string_ltrim " hello,world "       ))
    (assert-equalp "hello,world "     (mjr_string_ltrim "hello,world "        ))
    (assert-equalp "hello,world"      (mjr_string_ltrim " hello,world"        ))
    (assert-equalp "hello,world   "   (mjr_string_ltrim "   hello,world   "   ))

    (assert-equalp "hello, world "    (mjr_string_ltrim " hello, world "      ))
    (assert-equalp "hello, world "    (mjr_string_ltrim "hello, world "       ))
    (assert-equalp "hello, world"     (mjr_string_ltrim " hello, world"       ))
    (assert-equalp "hello, world   "  (mjr_string_ltrim "   hello, world   "  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_rtrim
    (assert-equalp " hello,world"     (mjr_string_rtrim " hello,world "       ))
    (assert-equalp "hello,world"      (mjr_string_rtrim "hello,world "        ))
    (assert-equalp " hello,world"     (mjr_string_rtrim " hello,world"        ))
    (assert-equalp "   hello,world"   (mjr_string_rtrim "   hello,world   "   ))

    (assert-equalp " hello, world"    (mjr_string_rtrim " hello, world "      ))
    (assert-equalp "hello, world"     (mjr_string_rtrim "hello, world "       ))
    (assert-equalp " hello, world"    (mjr_string_rtrim " hello, world"       ))
    (assert-equalp "   hello, world"  (mjr_string_rtrim "   hello, world   "  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_trim
    (assert-equalp "hello,world"   (mjr_string_trim " hello,world "         ))
    (assert-equalp "hello,world"   (mjr_string_trim "hello,world "          ))
    (assert-equalp "hello,world"   (mjr_string_trim " hello,world"          ))
    (assert-equalp "hello,world"   (mjr_string_trim "   hello,world   "     ))

    (assert-equalp "hello, world"  (mjr_string_trim " hello, world "        ))
    (assert-equalp "hello, world"  (mjr_string_trim "hello, world "         ))
    (assert-equalp "hello, world"  (mjr_string_trim " hello, world"         ))
    (assert-equalp "hello, world"  (mjr_string_trim "   hello, world   "    ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_split-if
  (assert-equalp '("a" "b" "c" "d" "e" "f") (mjr_string_split-if "a,b,c,d;e,f" (lambda (x) (or (equalp x #\,) (equalp x #\;)))))
  (assert-equalp '("a" "b" "c" "d;e" "f")   (mjr_string_split-if "a,b,c,d;e,f" (lambda (x) (or (equalp x #\,)))))
  (assert-equalp '("a,b,c,d;e,f")           (mjr_string_split-if "a,b,c,d;e,f" (lambda (x) (or (equalp x #\:)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_parse-c-identifier
  (assert-equalp nil (mjr_string_parse-c-identifier "12345"))
  (assert-equalp 5   (mjr_string_parse-c-identifier "a12345"))
  (assert-equalp 2   (mjr_string_parse-c-identifier "abc"))
  (assert-equalp 2   (mjr_string_parse-c-identifier "abc "))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_string_parse-number
  (assert-equalp (values 4 :INTEGER) (mjr_string_parse-number "12345"))
  (assert-equalp (values 6 :FLOAT)   (mjr_string_parse-number "12345.4"))
  (assert-equalp NIL                 (mjr_string_parse-number " 12345.4"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)
