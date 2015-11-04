;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      tst-mxp.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 2015 by Mitch Richling.  All rights reserved.
;; @brief     Tests for :mjr_mxp.@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_MXP-TESTS (:USE :COMMON-LISP :LISP-UNIT :MJR_MXP))

(in-package :MJR_MXP-TESTS)

;; Examples
;; * INFIX:  "3+4*2/(1-5)^2^3"
;;   RPL:    "3 4 2 * 1 5 - 2 3 ^ ^ / +"
;;   TOKENS: '("3" "+" "4" "*" "2" "/" "(" "1" "-" "5" ")" "^" "2" "^" "3")
;;   TREE:   '("+" "3" ("^" ("/" ("*" "4" "2") ("-" "1" "5")) ("^" "2" "3")))
;;   LISP:   '(+ 3 (^ (/ (* 4 2) (- 1 5)) (^ 2 3)))
;;
;; * INFIX: "2*SIN(3)"
;;   TOKENS: '("2" "*" "SIN" "(" "3"  ")")
;;   LISP:   '("*" "2" ("SIN" "3"))
;;
;; * INFIX: "2*ATAN(3, 4)"
;;   LISP: '("*" "2" ("ATAN" "3" "4"))
;;
;; * INFIX: a*(x+1)+3*cos(y)
;;   LISP: '("+" ("*" "a" ("+" "x" "1")) ("*" "3" ("cos" "y")))

;;----------------------------------------------------------------------------------------------------------------------------------
(define-test mjr_mxp_infix-to-tree
    (assert-equalp 1                                                 (mjr_mxp_infix-to-tree "1"            ))
    (assert-equalp 1.0                                               (mjr_mxp_infix-to-tree "1."           ))
    (assert-equalp 1.1                                               (mjr_mxp_infix-to-tree "1.1"          ))
    (assert-equalp 1e3                                               (mjr_mxp_infix-to-tree "1e3"          ))
    (assert-equalp 1e-3                                              (mjr_mxp_infix-to-tree "1e-3"         ))
    (assert-equalp 1/2                                               (mjr_mxp_infix-to-tree "1/2"          ))
    (assert-equalp '("-" 1)                                          (mjr_mxp_infix-to-tree "-1"           ))
    (assert-equalp '("+" 1)                                          (mjr_mxp_infix-to-tree "+1"           ))
    (assert-equalp '("*" 1 ("+" 2))                                  (mjr_mxp_infix-to-tree "1 * +2"       ))
    (assert-equalp '("*" 1 ("-" 2))                                  (mjr_mxp_infix-to-tree "1 * -2"       ))
    (assert-equalp '("-" ("+" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a + b-c"      ))
    (assert-equalp '("+" "a" ("-" "b"))                              (mjr_mxp_infix-to-tree "a+-b"         ))
    (assert-equalp '("+" "a" ("-" "b"))                              (mjr_mxp_infix-to-tree "a+ -b"        ))
    (assert-equalp '("+" "a" ("-" "b"))                              (mjr_mxp_infix-to-tree "a+ - b"       ))
    (assert-equalp '("+" "a" ("-" "b"))                              (mjr_mxp_infix-to-tree "a + - b"      ))
    (assert-equalp '("+" "a" ("-" "b"))                              (mjr_mxp_infix-to-tree "a +- b"       ))
    (assert-equalp '("+" "a" "b")                                    (mjr_mxp_infix-to-tree "a+b"          ))
    (assert-equalp '("+" "a" ("*" "b" "c"))                          (mjr_mxp_infix-to-tree "a+b*c"        ))
    (assert-equalp '("+" ("+" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a+b+c"        ))
    (assert-equalp '("-" ("+" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a+b-c"        ))
    (assert-equalp '("+" ("-" ("+" "a" "b") "c") "d")                (mjr_mxp_infix-to-tree "a+b-c+d"      ))
    (assert-equalp '("-" ("-" ("+" "a" "b") "c") "d")                (mjr_mxp_infix-to-tree "a+b-c-d"      ))
    (assert-equalp '("-" "a" "b")                                    (mjr_mxp_infix-to-tree "a-b"          ))
    (assert-equalp '("*" "a" "b")                                    (mjr_mxp_infix-to-tree "a*b"          ))
    (assert-equalp '("*" ("*" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a*b*c"        ))
    (assert-equalp '("+" ("*" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a*b+c"        ))
    (assert-equalp '("/" "a" "b")                                    (mjr_mxp_infix-to-tree "a/b"          ))
    (assert-equalp '("^" "a" "b")                                    (mjr_mxp_infix-to-tree "a^b"          ))
    (assert-equalp '("+" 1 ("*" 2 ("^" 3 4)))                        (mjr_mxp_infix-to-tree "1+2*3^4"      ))
    (assert-equalp '("+" ("+" 1 ("*" 2 ("^" 3 4))) 5)                (mjr_mxp_infix-to-tree "1+2*3^4+5"    ))
    (assert-equalp '("+" ("*" 2 ("^" 3 4)) 1)                        (mjr_mxp_infix-to-tree "2*3^4+1"      ))
    (assert-equalp '("+" 2 ("*" ("^" 3 4) 5))                        (mjr_mxp_infix-to-tree "2+3^4*5"      ))
    (assert-equalp '("^" 2 ("^" 3 4))                                (mjr_mxp_infix-to-tree "2^3^4"        ))
    (assert-equalp '("+" ("^" "x" 2) ("^" "y" 2))                    (mjr_mxp_infix-to-tree "x^2 + y^2"    ))
    (assert-equalp '("/" ("+" 1 2) 3)                                (mjr_mxp_infix-to-tree "(1+2)/3"      ))
    (assert-equalp '("*" ("+" 1 2) 3)                                (mjr_mxp_infix-to-tree "(1+2)*3"      ))
    (assert-equalp '("/" ("+" 1 2) ("-" 3 4))                        (mjr_mxp_infix-to-tree "(1+2)/(3-4)"  ))
    (assert-equalp '("*" ("+" 1 2) ("-" 3 4))                        (mjr_mxp_infix-to-tree "(1+2)*(3-4)"  ))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "(a=b)"        ))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "((a=b))"      ))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "(((a=b)))"    ))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "((((a=b))))"  ))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "(((((a=b)))))"))
    (assert-equalp '("=" "a" "b")                                    (mjr_mxp_infix-to-tree "a=b"          ))
    (assert-equalp '("*" 1 ("+" 2 3))                                (mjr_mxp_infix-to-tree "1*(2+3)"      ))
    (assert-equalp '("+" 1 2/3)                                      (mjr_mxp_infix-to-tree "1+2/3"        ))
    (assert-equalp '("foo" "a" "b" "c")                              (mjr_mxp_infix-to-tree "foo(a,b,c)"   ))
    (assert-equalp '("foo" "a" "b")                                  (mjr_mxp_infix-to-tree "foo(a,b)"     ))
    (assert-equalp '("foo" "a" ("bar" "b"))                          (mjr_mxp_infix-to-tree "foo(a,bar(b))"))
    (assert-equalp '("foo" "a")                                      (mjr_mxp_infix-to-tree "foo(a)"       ))
    (assert-equalp '("+" ("*" "a" ("b" "c")) "d")                    (mjr_mxp_infix-to-tree "a*b(c)+d"     ))
    (assert-equalp '("+" "a" ("*" ("b" "c") "d"))                    (mjr_mxp_infix-to-tree "a+b(c)*d"     ))
    (assert-equalp '("+" ("+" "a" ("b" "c")) "d")                    (mjr_mxp_infix-to-tree "a+b(c)+d"     ))
    (assert-equalp '("+" "d" ("*" "a" ("b" "c")))                    (mjr_mxp_infix-to-tree "d+a*b(c)"     ))
    (assert-equalp '("+" ("+" "a") "b")                              (mjr_mxp_infix-to-tree "+a+b"         ))
    (assert-equalp '("+" ("-" "a") "b")                              (mjr_mxp_infix-to-tree "-a+b"         ))
    (assert-equalp '("-" ("-" "a") "b")                              (mjr_mxp_infix-to-tree "-a-b"         ))
    (assert-equalp '("-" ("-" ("-" "a") "b") "c")                    (mjr_mxp_infix-to-tree "-a-b-c"       ))
    (assert-equalp '("/" ("*" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a*b/c"        ))
    (assert-equalp '("-" ("+" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a+b-c"        ))
    (assert-equalp '("-" ("-" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a-b-c"        ))
    (assert-equalp '("*" ("/" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a/b*c"        ))
    (assert-equalp '("/" ("/" "a" "b") "c")                          (mjr_mxp_infix-to-tree "a/b/c"        ))
    (assert-equalp '("^" "a" ("^" "b" "c"))                          (mjr_mxp_infix-to-tree "a^b^c"        ))
    (assert-equalp '("^" ("a" "d") ("^" "b" "c"))                    (mjr_mxp_infix-to-tree "a(d)^b^c"     ))
    (assert-equalp '("*" "a" ("^" "b" "c"))                          (mjr_mxp_infix-to-tree "a*(b)^c"      ))
    (assert-equalp '("^" ("*" "a" "b") "c")                          (mjr_mxp_infix-to-tree "(a*b)^c"      ))
    (assert-equalp '("*" ("^" "b" "c") "d")                          (mjr_mxp_infix-to-tree "b^(c)*d"      ))
    (assert-equalp '("^" "b" ("*" "c" "d"))                          (mjr_mxp_infix-to-tree "b^(c*d)"      ))

    ;; Errors
    (assert-error 'error (mjr_mxp_infix-to-tree "foo((bar,baz))" ))  ;; double parens are a problem
    (assert-error 'error (mjr_mxp_infix-to-tree "foo(bar,baz"    ))
    (assert-error 'error (mjr_mxp_infix-to-tree "foo(bar,baz))"  ))
    (assert-error 'error (mjr_mxp_infix-to-tree "foo[bar,baz]"   ))
    (assert-error 'error (mjr_mxp_infix-to-tree "[foo,bar]"      ))
    (assert-error 'error (mjr_mxp_infix-to-tree "=bar"           ))
    (assert-error 'error (mjr_mxp_infix-to-tree "*bar"           ))
    (assert-error 'error (mjr_mxp_infix-to-tree ""               ))
    (assert-error 'error (mjr_mxp_infix-to-tree ")a"             ))
    (assert-error 'error (mjr_mxp_infix-to-tree "]a"             ))
    )

;;----------------------------------------------------------------------------------------------------------------------------------
(run-tests)
