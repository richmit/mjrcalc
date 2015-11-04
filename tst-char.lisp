;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-char.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Tests for :mjr_char.@EOL
;; @std       Common Lisp
;; @see       use-char.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_CHAR-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_CHAR))

(in-package :MJR_CHAR-TESTS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_table 
  (assert-equal 2771 (length (with-output-to-string (*standard-output* nil) (mjr_char_table  :char-set :cs-ebcdic))))
  (assert-equal 1411 (length (with-output-to-string (*standard-output* nil) (mjr_char_table  :char-set :cs-ascii))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_int2ch
  (assert-equal "A"   (mjr_char_int2ch 65 ))
  (assert-equal "&"   (mjr_char_int2ch 38 ))
  (assert-equal "a"   (mjr_char_int2ch 97 ))
  (assert-equal "DEL" (mjr_char_int2ch 127))
  (assert-equal "SP"  (mjr_char_int2ch 32 ))
  (assert-equal "Z"   (mjr_char_int2ch 90 ))
  (assert-equal "z"   (mjr_char_int2ch 122))

  (assert-equal #\A   (mjr_char_int2ch 65  :result-type 'char))
  (assert-equal #\&   (mjr_char_int2ch 38  :result-type 'char))
  (assert-equal #\a   (mjr_char_int2ch 97  :result-type 'char))
  (assert-equal #\Z   (mjr_char_int2ch 90  :result-type 'char))
  (assert-equal #\z   (mjr_char_int2ch 122 :result-type 'char))

  (assert-equal "A"   (mjr_char_int2ch 193 :char-set :cs-ebcdic))
  (assert-equal "Z"   (mjr_char_int2ch 233 :char-set :cs-ebcdic))
  (assert-equal "a"   (mjr_char_int2ch 129 :char-set :cs-ebcdic))
  (assert-equal "z"   (mjr_char_int2ch 169 :char-set :cs-ebcdic))

  (assert-equal #\A   (mjr_char_int2ch 193 :char-set :cs-ebcdic :result-type 'char))
  (assert-equal #\Z   (mjr_char_int2ch 233 :char-set :cs-ebcdic :result-type 'char))
  (assert-equal #\a   (mjr_char_int2ch 129 :char-set :cs-ebcdic :result-type 'char))
  (assert-equal #\z   (mjr_char_int2ch 169 :char-set :cs-ebcdic :result-type 'char))

  ;; The default is :cs-ascii
  (dotimes (i 255)
    (assert-equal (mjr_char_int2ch i :char-set :cs-ascii) (mjr_char_int2ch i)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_ch2int
  (assert-equal 65  (mjr_char_ch2int "A"   ))
  (assert-equal 38  (mjr_char_ch2int "&"   ))
  (assert-equal 97  (mjr_char_ch2int "a"   ))
  (assert-equal 127 (mjr_char_ch2int "DEL" ))
  (assert-equal 32  (mjr_char_ch2int "SP"  ))
  (assert-equal 90  (mjr_char_ch2int "Z"   ))
  (assert-equal 122 (mjr_char_ch2int "z"   ))

  (assert-equal 65  (mjr_char_ch2int #\A   ))
  (assert-equal 38  (mjr_char_ch2int #\&   ))
  (assert-equal 97  (mjr_char_ch2int #\a   ))
  (assert-equal 90  (mjr_char_ch2int #\Z   ))
  (assert-equal 122 (mjr_char_ch2int #\z   ))

  (assert-equal 65  (mjr_char_ch2int "A"   :char-set :cs-ascii))
  (assert-equal 38  (mjr_char_ch2int "&"   :char-set :cs-ascii))
  (assert-equal 97  (mjr_char_ch2int "a"   :char-set :cs-ascii))
  (assert-equal 127 (mjr_char_ch2int "DEL" :char-set :cs-ascii))
  (assert-equal 32  (mjr_char_ch2int "SP"  :char-set :cs-ascii))
  (assert-equal 90  (mjr_char_ch2int "Z"   :char-set :cs-ascii))
  (assert-equal 122 (mjr_char_ch2int "z"   :char-set :cs-ascii))

  (assert-equal 65  (mjr_char_ch2int #\A   :char-set :cs-ascii))
  (assert-equal 38  (mjr_char_ch2int #\&   :char-set :cs-ascii))
  (assert-equal 97  (mjr_char_ch2int #\a   :char-set :cs-ascii))
  (assert-equal 90  (mjr_char_ch2int #\Z   :char-set :cs-ascii))
  (assert-equal 122 (mjr_char_ch2int #\z   :char-set :cs-ascii))

  (assert-equal 193 (mjr_char_ch2int "A"   :char-set :cs-ebcdic))
  (assert-equal 233 (mjr_char_ch2int "Z"   :char-set :cs-ebcdic))
  (assert-equal 129 (mjr_char_ch2int "a"   :char-set :cs-ebcdic))
  (assert-equal 169 (mjr_char_ch2int "z"   :char-set :cs-ebcdic))

  (assert-equal 193 (mjr_char_ch2int #\A   :char-set :cs-ebcdic))
  (assert-equal 233 (mjr_char_ch2int #\Z   :char-set :cs-ebcdic))
  (assert-equal 129 (mjr_char_ch2int #\a   :char-set :cs-ebcdic))
  (assert-equal 169 (mjr_char_ch2int #\z   :char-set :cs-ebcdic))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_digitsp
    (assert-true  (mjr_char_digitsp"1234"))
    (assert-true  (mjr_char_digitsp"14"  ))
    (assert-true  (mjr_char_digitsp""    ))

    (assert-false (mjr_char_digitsp" 1234"))
    (assert-false (mjr_char_digitsp" 14"  ))
    (assert-false (mjr_char_digitsp" "    ))
    (assert-false (mjr_char_digitsp"1 234"))
    (assert-false (mjr_char_digitsp"1 4"  ))
    (assert-false (mjr_char_digitsp"1234 "))
    (assert-false (mjr_char_digitsp"14 "  ))
    (assert-false (mjr_char_digitsp"a1234"))
    (assert-false (mjr_char_digitsp"a14"  ))
    (assert-false (mjr_char_digitsp"a"    ))
    (assert-false (mjr_char_digitsp"1a234"))
    (assert-false (mjr_char_digitsp"1a4"  ))
    (assert-false (mjr_char_digitsp"1234a"))
    (assert-false (mjr_char_digitsp"14a"  ))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_lettersp 
    (assert-true  (mjr_char_lettersp "abcd"))
    (assert-true  (mjr_char_lettersp "cd"  ))
    (assert-true  (mjr_char_lettersp ""    ))

    (assert-false (mjr_char_lettersp " abcd"))
    (assert-false (mjr_char_lettersp " cd"  ))
    (assert-false (mjr_char_lettersp " "    ))
    (assert-false (mjr_char_lettersp "d abc"))
    (assert-false (mjr_char_lettersp "q b"  ))
    (assert-false (mjr_char_lettersp "abcd "))
    (assert-false (mjr_char_lettersp "qd "  ))
    (assert-false (mjr_char_lettersp "4abcd"))
    (assert-false (mjr_char_lettersp "4ab"  ))
    (assert-false (mjr_char_lettersp "4"    ))
    (assert-false (mjr_char_lettersp "a3cde"))
    (assert-false (mjr_char_lettersp "b4a"  ))
    (assert-false (mjr_char_lettersp "abcd4"))
    (assert-false (mjr_char_lettersp "ab4"  ))

    (assert-true  (mjr_char_lettersp "ABCD"))
    (assert-true  (mjr_char_lettersp "CD"  ))

    (assert-false (mjr_char_lettersp " ABCD"))
    (assert-false (mjr_char_lettersp " CD"  ))
    (assert-false (mjr_char_lettersp " "    ))
    (assert-false (mjr_char_lettersp "D ABC"))
    (assert-false (mjr_char_lettersp "Q B"  ))
    (assert-false (mjr_char_lettersp "ABCD "))
    (assert-false (mjr_char_lettersp "QD "  ))
    (assert-false (mjr_char_lettersp "4ABCD"))
    (assert-false (mjr_char_lettersp "4AB"  ))
    (assert-false (mjr_char_lettersp "4"    ))
    (assert-false (mjr_char_lettersp "A3CDE"))
    (assert-false (mjr_char_lettersp "B4A"  ))
    (assert-false (mjr_char_lettersp "ABCD4"))
    (assert-false (mjr_char_lettersp "AB4"  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_uppercasep
    (assert-true  (mjr_char_uppercasep "ABCD"))
    (assert-true  (mjr_char_uppercasep "CD"  ))
    (assert-true  (mjr_char_uppercasep ""    ))

    (assert-false (mjr_char_uppercasep " abcd"))
    (assert-false (mjr_char_uppercasep " cd"  ))
    (assert-false (mjr_char_uppercasep " "    ))
    (assert-false (mjr_char_uppercasep "d abc"))
    (assert-false (mjr_char_uppercasep "q b"  ))
    (assert-false (mjr_char_uppercasep "abcd "))
    (assert-false (mjr_char_uppercasep "qd "  ))
    (assert-false (mjr_char_uppercasep "4abcd"))
    (assert-false (mjr_char_uppercasep "4ab"  ))
    (assert-false (mjr_char_uppercasep "4"    ))
    (assert-false (mjr_char_uppercasep "a3cde"))
    (assert-false (mjr_char_uppercasep "b4a"  ))
    (assert-false (mjr_char_uppercasep "abcd4"))
    (assert-false (mjr_char_uppercasep "ab4"  ))

    (assert-false (mjr_char_uppercasep "abcd"))
    (assert-false (mjr_char_uppercasep "cd"  ))

    (assert-false (mjr_char_uppercasep " ABCD"))
    (assert-false (mjr_char_uppercasep " CD"  ))
    (assert-false (mjr_char_uppercasep " "    ))
    (assert-false (mjr_char_uppercasep "D ABC"))
    (assert-false (mjr_char_uppercasep "Q B"  ))
    (assert-false (mjr_char_uppercasep "ABCD "))
    (assert-false (mjr_char_uppercasep "QD "  ))
    (assert-false (mjr_char_uppercasep "4ABCD"))
    (assert-false (mjr_char_uppercasep "4AB"  ))
    (assert-false (mjr_char_uppercasep "4"    ))
    (assert-false (mjr_char_uppercasep "A3CDE"))
    (assert-false (mjr_char_uppercasep "B4A"  ))
    (assert-false (mjr_char_uppercasep "ABCD4"))
    (assert-false (mjr_char_uppercasep "AB4"  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test mjr_char_lowercasep
    (assert-true  (mjr_char_lowercasep "abcd"))
    (assert-true  (mjr_char_lowercasep "cd"  ))
    (assert-true  (mjr_char_lowercasep ""    ))

    (assert-false (mjr_char_lowercasep " abcd"))
    (assert-false (mjr_char_lowercasep " cd"  ))
    (assert-false (mjr_char_lowercasep " "    ))
    (assert-false (mjr_char_lowercasep "d abc"))
    (assert-false (mjr_char_lowercasep "q b"  ))
    (assert-false (mjr_char_lowercasep "abcd "))
    (assert-false (mjr_char_lowercasep "qd "  ))
    (assert-false (mjr_char_lowercasep "4abcd"))
    (assert-false (mjr_char_lowercasep "4ab"  ))
    (assert-false (mjr_char_lowercasep "4"    ))
    (assert-false (mjr_char_lowercasep "a3cde"))
    (assert-false (mjr_char_lowercasep "b4a"  ))
    (assert-false (mjr_char_lowercasep "abcd4"))
    (assert-false (mjr_char_lowercasep "ab4"  ))

    (assert-false (mjr_char_lowercasep "ABCD"))
    (assert-false (mjr_char_lowercasep "CD"  ))

    (assert-false (mjr_char_lowercasep " ABCD"))
    (assert-false (mjr_char_lowercasep " CD"  ))
    (assert-false (mjr_char_lowercasep " "    ))
    (assert-false (mjr_char_lowercasep "D ABC"))
    (assert-false (mjr_char_lowercasep "Q B"  ))
    (assert-false (mjr_char_lowercasep "ABCD "))
    (assert-false (mjr_char_lowercasep "QD "  ))
    (assert-false (mjr_char_lowercasep "4ABCD"))
    (assert-false (mjr_char_lowercasep "4AB"  ))
    (assert-false (mjr_char_lowercasep "4"    ))
    (assert-false (mjr_char_lowercasep "A3CDE"))
    (assert-false (mjr_char_lowercasep "B4A"  ))
    (assert-false (mjr_char_lowercasep "ABCD4"))
    (assert-false (mjr_char_lowercasep "AB4"  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(run-tests)

