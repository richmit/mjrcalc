;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-mxp.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Mathematical eXPressions library.@EOL
;; @std       Common Lisp
;; @see       tst-mxp.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1996,2010,2011,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @todo      Add this kind of code to other packages (mnleq.lisp, deq.lisp, etc..).@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_MXP
  (:USE :COMMON-LISP
        :MJR_STRING
        :MJR_CHAR)
  (:DOCUMENTATION "Brief: Mathematical eXPressions library.;")
  (:EXPORT #:mjr_mxp_help
           ;; Conversion into MXP-Tree Expressions
           #:mjr_mxp_infix-to-tree #:mjr_mxp_lisp-to-tree
           ;; Conversion from MXP-Tree Expressions
           #:mjr_mxp_tree-to-infix #:mjr_mxp_tree-to-lambda #:mjr_mxp_tree-to-code #:mjr_mxp_print
           ;; Conversion between non-MXP objects
           #:mjr_mxp_string-or-func-to-lambda
           #:mjr_mxp_string-or-func-to-list-o-lambda
           #:mjr_mxp_trees-to-values-lambda
           ;; Evaluation of MXP-Tree Expressions
           #:mjr_mxp_tree-eval-as-lisp
           ;; Expression Tree Kind Predicates
           #:mjr_mxp_number?  #:mjr_mxp_not-number?  #:mjr_mxp_symbol?  #:mjr_mxp_not-symbol?  #:mjr_mxp_atom?  #:mjr_mxp_not-atom?
           #:mjr_mxp_atom= #:mjr_mxp_expr=
           ;; Expression Component access
           #:mjr_mxp_op #:mjr_mxp_op-in?  #:mjr_mxp_args #:mjr_mxp_nargs #:mjr_mxp_nth-arg
           ;; Expression Construction
           #:mjr_mxp_c #:mjr_mxp_cc #:mjr_mxp_map
           ;; Expression argument operations
           #:mjr_mxp_map-args #:mjr_mxp_some
           ;; Expression substitution
           #:mjr_mxp_substitute-par #:mjr_mxp_substitute-seq #:mjr_mxp_substitute-atom-par
           ;; Expression Complete Subexpression queries
           #:mjr_mxp_free-of #:mjr_mxp_has
           ;; NOT EXPORTED
           ;; #:mjr_mxp_op-precedence
           ;; #:mjr_mxp_op-right-assocp
           ;; #:mjr_mxp_lisp-op-to-math-op
           ;; #:mjr_mxp_math-constants-to-lisp-constants
           ))

(in-package :MJR_MXP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_help ()
  "MXP (Mathematical eXPressions)

This package provides the core functionality required:

 1) transform an infix-string into a mxp-tree.  That is to say, convert a LISP string representing an infix mathematical expression into a LISP tree (nested
    lists) representing the logical structure of the expression

 2) Convert a mxp-tree into other forms (like a LISP function, LISP code, or an infix-string)

 3) Preform 'structural' manipulations on an mxp-tree.  The word 'structural' is used to imply that this library preforms changes to an expression at the
    structural level and not the mathematical level.  i.e. it is not a CAS, but rather a tool a CAS might use to manage expressions.

 4) Provide some handy functions combining some of the above operations in ways commonly used by other code (i.e. directly transform an infix-string
    representing a mathematical function into a LISP function)

 While this package is primary useful for providing back-end functionality to other packages (like MJR_NLEQ, VTK.LISP, and POV.LISP), I have decided to design
 it for interactive use and expose it at the top level because it is occasionally handy to convert an equation from a text book or other source into LISP's
 prefix notation.

Definitions:
  atom
      A symbol (MJR_MXP_SYMBOL?) or a non-complex number (MJR_MXP_NUMBER?).  See: MJR_MXP_ATOM?
  mxp-tree
      A symbol (MJR_MXP_SYMBOL?), a non-complex number (MJR_MXP_NUMBER?), or an composite expression tree (MJR_MXP_NOT-ATOM?).
  composite expression
      An mxp-tree that is not an atom (MJR_MXP_NOT-ATOM?) -- they are nested lists (i.e. trees)
  infix-string
      A string containing a representation of a mathematical expression in c-like syntax (only MJR_MXP_INFIX-TO-TREE takes these)"
  (documentation 'mjr_mxp_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_op-precedence (op)
  "Take a string with a binary operator name, and return the integer indicating its precedence."
  (let ((op (if (and op (listp op))
                (cdr op)
                op)))
    (cond ((string-equal "^" op)    8)
          ((string-equal "*" op)    7)
          ((string-equal "/" op)    7)
          ((string-equal "+" op)    6)
          ((string-equal "-" op)    6)
          ((string-equal "=" op)    5)
          ((string-equal ":" op)    4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun mjr_mxp_op-type (op)
;;  ""
;;  (if (= 1 (length op))
;;      (cond ((mjr_char_in-class op "*/+-") UNARY/BINARY)
;;            ((mjr_char_in-class op "^=:")  BINARY)
;;            ((mjr_char_in-class op "!")    POSTFIX))
;;        FUNCTION))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_op-right-assocp (op)
  "Take a string with a binary operator name, and return non-NIL if it is right associative."
  (let ((op (if (and op (listp op))
                (cdr op)
                op)))
    (string-equal "^" op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokenizer:
;;    * Gobble leading white-space
;;
;;    * Look for + and - that are not part of an integer.
;;
;;    * Look for a number.
;;
;;    * Look for 1 char things
;;
;;      For :tokt-op-infix, :tokt-op-postfix, :tokt-op-prefix, & :tokt-function the number of arguments must be determined.  For :tokt-function, the tokenizer
;;      figures it out.  For + or -, we need to figure out if it is binary or unary.  If a - or + character is read, and the previous token read is a number,
;;      symbol, or a closing parenthesis, the meaning is taken to be a binary -.  OTOH, if the previous token is an operator, function arg sep, open paren, or
;;      if there is no previous token, then the meaning is taken to be unary -.
;;
;;    * Look for symbols (variables & functions): start with [_a-zA-Z] only contain  [_a-zA-Z0-9]
;;
;;      If the next non-whitespcae char after a symbol is a (, then the symbol is a prefix function name.  If it is a function, march along the string keeping
;;      track of how many parens are still open (we start with at least one that was at the end of the function name), and we count commas we find while we
;;      are 1 paren level deep.  Stop when paren level is 0 -- i.e. we found the last paren.  Record the number of commas as the number of arguments-1.
;;
;; Tokens
;;        * float     :tokt-number                   short-float double-float
;;        * rational  :tokt-number                   rational number
;;        * integer   :tokt-number                   integer (fixnum, bugnum, etc...)
;;        * !         :tokt-op-postfix          9    factoral
;;        * ^         :tokt-op-infix     left   8    expt
;;        * *         :tokt-op-infix     right  7    mult
;;        * /         :tokt-op-infix     right  7    divide
;;        * +         :tokt-op-infix     right  6    add
;;                    :tokt_op-prefix           9    noop
;;        * -         :tokt-op-infix     right  6    subtract
;;                    :tokt_op-prefix           9    negate
;;        * =         :tokt-op-infix     left   5    equality
;;        * :         :tokt-op-infix     left   4    assignment
;;        * (         :tokt-parn-open                Open paren
;;        * )         :tokt-parn-close               Close paren
;;        * ,         :tokt-f-separator              Separates arguments in prefix function calls
;;        * variable  :tokt-symbol                   Variables
;;        * function  :tokt-function                 prefix functions
;;
;; parser algorithm:
;;  For each token (TOK)
;;     * If the TOK represents a number, convert to real lisp number and push that number on B-STACK
;;     * If the TOK is a symbol, push TOK on B-STACK
;;     * If the TOK is a unary postfix operator, push  '(1 . TOK) onto B-STACK
;;     * If the TOK is a unary prefix operator, push '(1 . TOK) on to A-STACK
;;     * If the TOK is a function token, push '(#args . TOK) it on to A-STACK
;;     * If the TOK is a function argument separator (a comma)
;;           * Pop element off A-STACK and push it onto B-STACK, until (car A-STACK) is an open paren
;;     * If TOK is a binary operator then:
;;           * If TOK is left-associative
;;               while (car A-STACK) is an operator of higher or equal precedence than TOK, do (push (pop A-STACK) B-STACK)
;;             else (If TOK is right-associative)
;;               while (car A-STACK) is an operator of higher precedence than TOK, (push (pop A-STACK) B-STACK)
;;           * Push TOK onto A-STACK
;;     * If TOK is an opening paren, then push it onto A-STACK
;;     * If TOK is a closing paren:
;;           * Pop operators off A-STACK and push them onto B-STACK, until (car A-STACK) is a opening paren.
;;           * Pop the opening paren off the A-STACK.
;;           * If the token at the top of the A-STACK is a function token, pop it and push it onto B-STACK
;;  When all the tokens have been read:
;;     * While there are still operator tokens in the A-STACK, pop the operator off A-STACK, and push it onto B-STACK
(defun mjr_mxp_infix-to-tree (the-string &key (start 0) (show-progress nil))
  "Convert THE-STRING into an MXP"
  (let ((len     (length the-string))
        (b-stack  nil)
        (a-stack nil))
    (loop with i = start
          with cur-chr = nil
          for prv-tok-typ = nil then cur-tok-typ
          for cur-tok-end = nil
          for cur-tok-typ = nil
          for arg-cnt = nil
          do (setq i (position-if-not #'mjr_char_whitespacep the-string :start i))
          do (setq cur-chr (if i (aref the-string i)))
          when i
          do (cond ((setq cur-tok-end (if (find cur-chr "+-") i))     (if (member prv-tok-typ (list :tokt-number
                                                                                                    :tokt-parn-close
                                                                                                    :tokt-symbol))
                                                                          (setq cur-tok-typ :tokt-op-infix)
                                                                          (setq cur-tok-typ :tokt-op-prefix)))
                   ((setq cur-tok-end (mjr_string_parse-number
                                       the-string :start i))          (setq cur-tok-typ :tokt-number))
                   ((setq cur-tok-end (if (char= cur-chr #\() i))     (setq cur-tok-typ :tokt-parn-open))
                   ((setq cur-tok-end (if (char= cur-chr #\)) i))     (setq cur-tok-typ :tokt-parn-close))
                   ((setq cur-tok-end (if (char= cur-chr #\,) i))     (setq cur-tok-typ :tokt-f-separator))
                   ((setq cur-tok-end (if (char= cur-chr #\!) i))     (setq cur-tok-typ :tokt-op-postfix))
                   ((setq cur-tok-end (if (find cur-chr "^*/=:") i))  (setq cur-tok-typ :tokt-op-infix))
                   ((setq cur-tok-end (mjr_string_parse-c-identifier
                                       the-string :start i))          (let* ((chr-loc (and (< cur-tok-end (1- len))
                                                                                           (position-if-not #'mjr_char_whitespacep
                                                                                                            the-string
                                                                                                            :start (1+ cur-tok-end))))
                                                                             (arg-len -2)
                                                                             (num-sep (if (and (numberp chr-loc)
                                                                                               (char= #\( (aref the-string chr-loc)))
                                                                                          (loop with par-dep = 0
                                                                                                for i from chr-loc upto (1- len)
                                                                                                for c = (aref the-string i)
                                                                                                do (incf arg-len)
                                                                                                do (cond ((char= #\( c) (incf par-dep))
                                                                                                         ((char= #\) c) (decf par-dep)))
                                                                                                count (and (= par-dep 1) (char= #\, c))
                                                                                                do (if (and (not (zerop par-dep)) (= i (1- len)))
                                                                                                       (error "mjr_mxp_infix-to-tree: Unbalanced parentheses after position: ~d!" cur-tok-end))
                                                                                                while (< 0 par-dep)))))
                                                                        (if num-sep
                                                                            (setq arg-cnt     (+ (if (zerop arg-len) 0 1) num-sep)
                                                                                  cur-tok-typ :tokt-function)
                                                                            (setq cur-tok-typ :tokt-symbol)))))
          do (if show-progress
                 (format 't "LEXER: ~15a ~5a ~5a ~5a -> '~a'~%" cur-tok-typ i cur-tok-end arg-cnt (if (and i cur-tok-end) (subseq the-string i (1+ cur-tok-end)))))
          when (and i cur-tok-end)
          do (let ((cur-tok-str (subseq the-string i (1+ cur-tok-end))))
               (case cur-tok-typ
                 (:tokt-number      (push (eval (with-input-from-string (stream (format nil "~a" cur-tok-str)) (read stream nil))) b-stack))
                 (:tokt-symbol      (push cur-tok-str b-stack))
                 (:tokt-op-postfix  (push (cons 1 cur-tok-str) b-stack))
                 (:tokt-op-prefix   (push (cons 1 cur-tok-str) a-stack))
                 (:tokt-function    (push (cons (- arg-cnt) cur-tok-str) a-stack))
                 (:tokt-f-separator (loop until (and (stringp (car a-stack)) (string-equal "(" (car a-stack)))
                                          do (push (pop a-stack) b-stack)))
                 (:tokt-op-infix    (let ((tok-persidence (mjr_mxp_op-precedence cur-tok-str)))
                                      (if (mjr_mxp_op-right-assocp cur-tok-str)
                                          (loop while (and (car a-stack)
                                                           (and (car a-stack) (listp (car a-stack)) (< 0 (caar a-stack))) ;;  means it is an operator
                                                           (< tok-persidence (mjr_mxp_op-precedence (car a-stack))))
                                                do (push (pop a-stack) b-stack))
                                          (loop while (and (car a-stack)
                                                           (and (car a-stack) (listp (car a-stack)) (< 0 (caar a-stack))) ;; means it is an operator
                                                           (<= tok-persidence (mjr_mxp_op-precedence (car a-stack))))
                                                do (push (pop a-stack) b-stack)))
                                      (push (cons 2 cur-tok-str) a-stack)))
                 (:tokt-parn-open   (push cur-tok-str a-stack))
                 (:tokt-parn-close  (progn (loop until (or (null a-stack) (and (stringp (car a-stack)) (string-equal "("  (car a-stack))))
                                                 do (push (pop a-stack) b-stack))
                                           (if (null a-stack)
                                               (error "mjr_mxp_infix-to-tree: Syntax error in expression: Unable to find enough open parenthesis."))
                                           (pop a-stack)
                                           (if (and (car a-stack) (listp (car a-stack)) (> 0 (caar a-stack))) ;; means it is a function
                                               (push (pop a-stack) b-stack)))))
               (if show-progress
                   (format 't "PARSR: BSTACK: ~120a ASTACK: ~80a~%" b-stack a-stack))
               (setq i (1+ cur-tok-end)))
          do (if (not i)
                 (error "mjr_mxp_infix-to-tree: Syntax error in expression: Unable to find a token."))
          do (if (and (not cur-tok-end) (< i (1- len)))
                 (error "mjr_mxp_infix-to-tree: Syntax error in expression: Unconsumed tokens starting at position ~d." i))
          until (>= i len))
    (if show-progress
        (format 't "LEXER: complete..~%"))
    (loop while (and (car a-stack) (listp (car a-stack))) ;; listp means it is an operator or function...
          do (push (pop a-stack) b-stack)
          (if show-progress
              (format 't "PARSR: BSTACK: ~120a ASTACK: ~80a~%"  b-stack a-stack)))
    (if show-progress
        (format 't "PARSR: complete..~%"))
    (if a-stack
        (error "mjr_mxp_infix-to-tree: Syntax error in expression: Unconsumed tokens left over: '~a'." (mjr_string_join "' '" a-stack)))
    (loop initially (setq a-stack nil)
          finally (progn (if show-progress
                             (format 't "SYNTH: complete..~%"))
                         (if (cadr a-stack)
                             (error "mjr_mxp_infix-to-tree: Syntax error in expression: Had stuff left over: '~a'." (mjr_string_join "' '" (cdr a-stack))))
                         (return (car a-stack)))
          for tok in (reverse b-stack)
          do (if show-progress
                 (format 't "SYNTH: TOK: '~a'~%" tok))
          if (listp tok)
          do (let* ((arg (car  tok))
                    (sym (cdr tok)))
               (push (apply #'list
                            sym
                            (reverse (loop for i from 1 upto (abs arg)
                                           for a = (pop a-stack)
                                           do (if (null a)
                                                  (error "mjr_mxp_infix-to-tree: Too few arguments for: '~a'" sym))
                                           collect a)))
                     a-stack))
          else
          do (push tok a-stack)
          do (if show-progress
                 (format 't "SYNTH: BSTACK: ~120a ASTACK: ~80a~%"  b-stack a-stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_lisp-to-tree (lisp-code)
  "Transforms the LISP expression in LISP-CODE into a mxp-tree"
  ;; Example: (mjr_mxp_lisp-to-tree '(+ a "234" b c (* d 345 f))) => ("+" "A" 234 "B" "C" ("*" "D" 345 "F"))
  (if (and lisp-code (listp lisp-code))
      (mapcar #'mjr_mxp_lisp-to-tree lisp-code)
      (typecase lisp-code
        (string     (let* ((len (length lisp-code))
                           (pos (mjr_string_parse-number lisp-code)))
                      (if (and pos (= (1- len) pos))
                          (mjr_string_read-as-lisp lisp-code)
                          lisp-code)))
        (number     lisp-code)
        (character  (string lisp-code))
        (symbol     (symbol-name lisp-code))
        (otherwise  (format nil "~a" lisp-code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_tree-eval-as-lisp (expr)
  "Transform mxp-tree into LISP code, and evaluate it -- Get a number if everything is defined."
  (eval (mjr_string_read-as-lisp (format nil "~a" expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_number? (expr)
  "non-NIL if EXPR is a number"
  (numberp expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_not-number? (expr)
  "non-NIL if EXPR is not a number"
  (not (numberp expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_symbol? (expr)
  "non-NIL if EXPR is a symbol"
  (stringp expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_not-symbol? (expr)
  "non-NIL if EXPR is not a symbol"
  (not (mjr_mxp_symbol? expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_atom? (expr)
  "non-NIL if EXPR is a symbol or a number -- i.e. if EXPR is a mxp-tree, it is not an composite expression."
  (or (mjr_mxp_symbol? expr) (mjr_mxp_number? expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_not-atom? (expr)
  "non-NIL if EXPR is not a string or number -- i.e. if EXPR is a mxp-tree, then it must be a composite expression."
  ;; Logically, this is: (not (or (mjr_mxp_symbol? expr) (mjr_mxp_number? expr)))
  ;; That said, the only other things we can have are lists starting with an operator.  So we do this instead:
  (and expr (listp expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_atom= (atom1 atom2)
  "non-NIL if arguments are both atoms and equal."
  (or (and (mjr_mxp_number? atom1)
           (mjr_mxp_number? atom2)
           (= atom1 atom2))
      (and (mjr_mxp_symbol? atom1)
           (mjr_mxp_symbol? atom2)
           (equal atom1 atom2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_expr= (expr1 expr2)
  "non-NIL if the expressions are equal."
  (equal expr1 expr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_op (expr)
  "Top operator if EXPR is a composite expression, and NIL otherwise."
  (if (mjr_mxp_not-atom? expr)
      (car expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_op-in? (expr op)
  "non-NIL if the top operator of EXPR is in OP if OP is a list of strings or equal to OP if OP is a string.
Note that if EXPR is not a composite expression, then the return is NIL."
  (if (mjr_mxp_not-atom? expr)
      (if (listp op)
          (member (car expr) op :test #'mjr_mxp_atom=)
          (mjr_mxp_atom= op (car expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_args (expr)
  "Args for the top operator, or NIL if EXPR is an atom."
  (cdr expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_nargs (expr)
  "Number of args for the top operator, or NIL if EXPR is not a composite expression."
  (if (mjr_mxp_not-atom? expr)
      (length (mjr_mxp_args expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_nth-arg (expr n)
  "nth arg for the top operator, or NIL if EXPR is not a composite expression.  Error if N is out of bounds or not an integer."
  (cond ((not (integerp n)) (error "mjr_mxp_nth-arg: N must be an integer."))
        ((> 0 n)            (error "mjr_mxp_nth-arg: N must be a non-negative integer.")))
  (if (mjr_mxp_not-atom? expr)
      (let ((the-arg (nth n (mjr_mxp_args expr))))
        (if (and (not the-arg) (< (1- (length (mjr_mxp_args expr))) n))
            (error "mjr_mxp_nth-arg: N must be less than the length of the argument list of EXPR.")
            the-arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_some (expr func &rest fargs)
  "non-NIL if FUNC applied to an argument of EXPR is non-NIL for at least one argument of EXPR."
  (if (mjr_mxp_not-atom? expr)
      (if fargs
          (some (lambda (x) (apply func x fargs)) (mjr_mxp_args expr))
          (some func                              (mjr_mxp_args expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_map-args (expr func &rest fargs)
  "Form a list by applying FUNC to each argument of EXPR."
  (if fargs
      (mapcar (lambda (x) (apply func x fargs)) (mjr_mxp_args expr))
      (mapcar func                              (mjr_mxp_args expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_map (expr func &rest fargs)
  "Apply FUNC to EXPR args and construct new expression with same top operator."
  (apply #'list (first expr) (apply #'mjr_mxp_map-args expr func fargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_copy-expr (expr)
  "Copy an expression."
  (typecase expr
    (list       (copy-tree expr))
    (string     (copy-seq  expr))
    (number     expr)
    (otherwise  (progn (warn "mjr_mxp_copy-expr: Argument was not a valid MXP")
                       expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_c (op &rest args)
  "Construct new expression with OP applied to the arguments following OP."
  (apply #'list op args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_cc (op &rest arg-lists)
  "Construct new expression with OP applied to elements the concatenation of the arguments after OP."
  (if arg-lists
      (if (cdr arg-lists)
          (apply #'mjr_mxp_c op (apply #'concatenate 'list arg-lists)) ;; More than one element in arg-lists
          (apply #'mjr_mxp_c op (car arg-lists)))                      ;; One element in arg-lists
      (mjr_mxp_c op)))                                                 ;; arg-lists is empty -- is this an error?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_math-op-to-lisp-op (expr)
  "Transform traditional mathematical function names to lisp function names (^ => expt)"
  (flet ((op-trans (op) (if (string-equal "^" op)
                            "expt"
                            op)))
    (cond ((mjr_mxp_atom? expr) expr)
          ('t                   (mjr_mxp_cc (op-trans (mjr_mxp_op expr))
                                            (mjr_mxp_map-args expr #'mjr_mxp_math-op-to-lisp-op))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_lisp-op-to-math-op (expr)
  "Transform lisp function names to traditional mathematical ones (expt => ^)"
  (flet ((op-trans (op) (if (string-equal "expt" op)
                            "^"
                            op)))
    (cond ((mjr_mxp_atom? expr) expr)
          ('t                   (mjr_mxp_cc (op-trans (mjr_mxp_op expr))
                                            (mjr_mxp_map-args expr #'mjr_mxp_lisp-op-to-math-op))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_tree-to-lambda (expr &rest args)
  "Transform mxp-tree into LISP code, and make it the body of a function taking VARS."
  (eval (mjr_string_read-as-lisp (format nil "(lambda ~a ~a )" args (mjr_mxp_math-op-to-lisp-op expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_tree-to-code (expr)
  "Transform mxp-tree into LISP code."
  (eval (mjr_string_read-as-lisp (format nil "(quote ~a)" (mjr_mxp_math-op-to-lisp-op expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_trees-to-values-lambda (expr-list arg-list)
  "Like mjr_mxp_tree-to-lambda, but first argument is a list of expressions and second is a list of arguments."
  (eval (mjr_string_read-as-lisp
         (format nil "(lambda (~{ ~A~}) (values ~{ ~A~}))" arg-list (mapcar #'mjr_mxp_math-op-to-lisp-op expr-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_string-or-func-to-lambda (func-or-string &rest vars)
  "If FUNC-OR-STRING is a string, then transform it into a lambda on VARS.  Otherwise return FUNC-OR-STRING as is."
  (if (stringp func-or-string)
      (apply #'mjr_mxp_tree-to-lambda (mjr_mxp_math-op-to-lisp-op (mjr_mxp_infix-to-tree func-or-string)) vars)
      func-or-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_string-or-func-to-list-o-lambda (funcs-or-strings &rest vars)
  "Like MJR_MXP_INFIX-TO-TREE, but work on lists of objects.  If not given a list, still return a list of one element."
  (mapcar (lambda (f-or-s) (apply #'mjr_mxp_string-or-func-to-lambda f-or-s vars))
          (if (listp funcs-or-strings) funcs-or-strings (list funcs-or-strings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_tree-to-infix (expr &optional (prv-prec -1))
  "Transform mxp-tree into an infix encoded string.  This is not well tested!!!

Some care has been taken to make the expression less ugly.  Many unnecessary parenthesis are culled, and funny lisp function names are replaced by traditional
operators (expt => ^ for example).  Some things are funny like unary minus and postfix operators.  Frankly I don't use this enough to care about it much. :)"
  (if (mjr_mxp_atom? expr)
      (format nil "~a" expr)
      (let* ((expr      (mjr_mxp_lisp-op-to-math-op expr))
             (cur-op    (mjr_mxp_op expr))
             (cur-prec  (mjr_mxp_op-precedence cur-op))
             (need-parn (or (null cur-prec) (null prv-prec) (< cur-prec prv-prec))))
        (concatenate 'string
                     (if need-parn "(" "")
                     (if (= 1 (mjr_mxp_nargs expr))
                         (format nil "~a(~a)" cur-op (mjr_mxp_nth-arg expr 0))
                         (mjr_string_join cur-op
                                          (mjr_mxp_map-args expr #'mjr_mxp_tree-to-infix cur-prec)))
                     (if need-parn ")" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_substitute-atom-par (expr &rest atom-exprs-pairs)
  "Replace atoms.  Example: '(* x y) x '(+ 4 z) y '(+ x y)) => (* (+ 4 z) (+ x y))
Faster than mjr_mxp_substitute-par."
  (if (mjr_mxp_atom? expr)
      (loop for lhs in atom-exprs-pairs by #'cddr
            for rhs in (cdr atom-exprs-pairs) by #'cddr
            do (if (mjr_mxp_expr= expr lhs)
                   (return (mjr_mxp_copy-expr rhs)))
            finally (return expr))
      (apply #'mjr_mxp_map expr #'mjr_mxp_substitute-atom-par atom-exprs-pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_substitute-par (expr &rest exprs-pairs)
  "Replace atoms.  Example: '(* (+ a b) c) a b b q) => (* (+ b q) c)"
  (let ((nexpr (if (mjr_mxp_atom? expr)
                   expr
                   (apply #'mjr_mxp_map expr #'mjr_mxp_substitute-par exprs-pairs))))
    (loop for lhs in exprs-pairs       by #'cddr
          for rhs in (cdr exprs-pairs) by #'cddr
          do (if (mjr_mxp_expr= nexpr lhs)
                 (return (mjr_mxp_copy-expr rhs)))
          finally (return nexpr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_substitute-seq (expr &rest exprs-pairs)
  "Replace atoms.  Example: '(* (+ a b) c) a b b q) => (* (+ q q) c)"
  (loop for lhs in exprs-pairs       by #'cddr
        for rhs in (cdr exprs-pairs) by #'cddr
        for nexpr = (mjr_mxp_substitute-par expr lhs rhs) then (mjr_mxp_substitute-par nexpr lhs rhs)
        finally (return nexpr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_has (expr small-expr)
  "EXPR contains SMALL-EXPR as a 'complete sub-expression'."
  (if (mjr_mxp_expr= small-expr expr)
      't
      (if (mjr_mxp_not-atom? expr)
          (mjr_mxp_some expr #'mjr_mxp_has small-expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_free-of (expr small-expr)
  "EXPR doesn't contain SMALL-EXPR as a 'complete sub-expression'."
  (not (mjr_mxp_has expr small-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_mxp_math-constants-to-lisp-constants (expr)
  "Transform traditional mathematical constant names (%e) to lisp values."
  (mjr_mxp_substitute-atom-par expr
                               "%e"  (log 1)
                               "%pi" pi
                               "%i" (complex 0 1)))
