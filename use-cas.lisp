;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-cas.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1996,2010-2013 by Mitch Richling.  All rights reserved.
;; @brief     Very basic computer algebra on :MJR_MXP objects.@EOL
;; @Keywords  lisp interactive computer algebra system cas
;; @Std       Common Lisp
;;
;;            TODO:
;;              * Attach assumptions to expressions in order to allow more simplification.
;;                Example: if we know the a,x,y are real and a is positive, then a^x*a^y => a^(x+y)
;;                Example: If we know foo is commutative, then foo(a,b)+foo(b,a) => 2*foo(a,b)
;;              * Pattern matching language to match and modify expressions
;;              * See functions commented out in the export expression
;;              * Convert expressions to/from polys & mpolys 
;;              * Add high level functions that do nice stuff
;;                * Compute derivative and simplify
;;                * Expand completely and then collect
;;              * Symbolic solution for quadratic and cubic
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(defpackage :MJR_CAS
  (:USE :COMMON-LISP
        :MJR_COMBE
        :MJR_MXP)
  (:DOCUMENTATION "Brief: Very basic computer algebra on :MJR_MXP objects.;")
  (:EXPORT #:mjr_cas_help
           ;; Mathematical eXPression (MXP) Algebraic Manipulation (mostly used for automatic simplification, but useful alone)
           #:mjr_cas_wack-division #:mjr_cas_wack-pos #:mjr_cas_nary-comb #:mjr_cas_commute-eval-num #:mjr_cas_commute-eval-num-in-prod
           #:mjr_cas_commute-eval-num-in-mjr #:mjr_cas_wack-int-fact #:mjr_cas_fix-int-expt #:mjr_cas_collect-over-prod
           #:mjr_cas_collect-over-sum #:mjr_cas_expand-prod1 #:mjr_cas_expand-prod #:mjr_cas_expand-expt
           ;; Higher order Mathematical eXPression (MXP) Algebraic Manipulation
           #:mjr_cas_canonize #:mjr_cas_trig-expand #:mjr_cas_alg-expand #:mjr_cas_alg-collect #:mjr_cas_do-till-noop #:mjr_cas_diff
           #:mjr_cas_diff-list
           ;; Automatically Simplified Expression (ASE) Manipulation
           #:mjr_cas_isolate 
           ;; Hard stuff I will probably never do:
           ;; TODO: #:mjr_cas_ratsimp              ;; Simplify and canonize rational expressions
           ;; TODO: #:mjr_cas_trigsimp             ;; Simplify and canonize trigonometric
           ;; TODO: #:mjr_cas_solve                ;; Solve, isolate, a variable
           ;; TODO: #:mjr_cas_integrate            ;; Symbolic integration (one var)
           ;; TODO: #:mjr_cas_dsolve               ;; Solve differential equations
           ))

(in-package :MJR_CAS)

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_help ()
  "CAS (Computer Algebra System)

This library performs computer algebraic computations on MXP objects (see :MJR_MXP).

!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!

This library is more of a playground for me to experiment with the basic algorithms and ideas of computer algebra -- a field
that is just stuffed full with some of the most beautiful algorithms in all of computer science.  While it is really more of a
toy than a useful package, it can be pressed into useful service for simple things like automatic differentiation for nonlinear
root finding and optimization.  I hope to expand this library over time....

References:
  Joel S. Cohen (2002); Computer Algebra and Symbolic Computation: Elementary Algorithms; ISBN: 1568811586
  Joel S. Cohen (2003); Computer Algebra and Symbolic Computation: Mathematical Methods; ISBN: 1568811594

!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!
!EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL! !EXPERIMENTAL!"
  (documentation 'mjr_cas_help 'function))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_do-till-noop (expr func &rest fargs)
  "Iteratively apply FUNC to EXPR till it stops changing.  Return result of FUNC applications and number of iterations needed."
  (loop for prv = nil  then nxt
        for nxt = expr then (apply func nxt fargs)
        for cnt from 0
        finally (return (values nxt cnt))
        until (mjr_mxp_expr= nxt prv)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_wack-pos (expr)
  "Replace unary + operators.  Example: (+ a) => a"
  (cond ((mjr_mxp_atom? expr)                                       expr)
        ((and (mjr_mxp_op-in? expr "+") (= 1 (mjr_mxp_nargs expr))) (mjr_cas_wack-pos (mjr_mxp_nth-arg expr 0)))
        ('t                                                         (mjr_mxp_map expr #'mjr_cas_wack-pos))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_wack-division (expr)
  "Replace binary / operators.  Example: (/ a b) => (* a (expt b -1))"
  (cond ((mjr_mxp_atom? expr)      expr)
        ((mjr_mxp_op-in? expr "/") (let* ((args  (mjr_mxp_map-args expr #'mjr_cas_wack-division))
                                          (arg1  (first args))
                                          (nargs (length args)))
                                     (case nargs
                                       (0         (error "mjr_cas_wack-division: Too few arguments for /."))
                                       (1         (if (numberp arg1)
                                                      (/ arg1)
                                                      (mjr_mxp_c "^" arg1 -1)))
                                       (otherwise (apply #'mjr_mxp_c "*" arg1 (loop for x in (cdr args)
                                                                                    collect (if (numberp x)
                                                                                                (/ x)
                                                                                                (mjr_mxp_c "^" x -1))))))))
        ('t                        (mjr_mxp_map expr #'mjr_cas_wack-division))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_wack-minus (expr)
  "Replace - operators with multiplication by -1.
Examples: (- a)   => (* -1 a)
          (- a b) => (+ a (- b))"
  (cond ((mjr_mxp_atom? expr)      expr)
        ((mjr_mxp_op-in? expr "-") (let* ((args  (mjr_mxp_map-args expr #'mjr_cas_wack-minus))
                                          (arg1  (first args))
                                          (nargs (length args)))
                                     (case nargs
                                       (0         (error "mjr_cas_wack-minus: Too few arguments for -."))
                                       (1         (if (numberp arg1)
                                                      (- arg1)
                                                      (mjr_mxp_c "*" -1 arg1)))
                                       (otherwise (apply #'mjr_mxp_c "+" arg1 (loop for x in (cdr args)
                                                                                    collect (if (numberp x)
                                                                                                (- x)
                                                                                                (mjr_mxp_c "*" -1 x))))))))
        ('t                        (mjr_mxp_map expr #'mjr_cas_wack-minus))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_nary-comb (expr &optional (op '("+" "*")))
  "Reduce nary expression to larger ones. Example: (+ (+ a b) c) => (+ a b c)"
  (cond ((mjr_mxp_atom? expr)     expr)
        ((mjr_mxp_op-in? expr op) (let ((cur-op (mjr_mxp_op expr)))
                                    (apply #'mjr_mxp_cc cur-op (mapcar (lambda (x)
                                                                         (if (mjr_mxp_op-in? x cur-op)
                                                                             (mjr_mxp_args x)
                                                                             (list x)))
                                                                       (mjr_mxp_map-args expr #'mjr_cas_nary-comb op)))))
        ('t                    (mjr_mxp_map expr #'mjr_cas_nary-comb op))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_fix-int-expt (expr)
  "Fix expt calls when the exponent is an integer.
Rules:
  - exp(x) => %e^x  (NOT IMPLIMENTED YET)
  - sqrt(x) => x^(1/2)
  - a^n (n is an integer) => a can not be a number, product, or power
  - a^(m/n) (a a number, n/m a reduced fraction) => Evaluate a^m and raise to 1/n."
  (cond ((mjr_mxp_atom? expr)         expr)
        ((mjr_mxp_op-in? expr "sqrt") (mjr_mxp_c "^" (mjr_cas_fix-int-expt (mjr_mxp_nth-arg expr 0)) 1/2))
        ;;((mjr_mxp_op-in? expr "exp")  (mjr_mxp_c "^" "%e" (mjr_cas_fix-int-expt (mjr_mxp_nth-arg expr 0))))
        ((mjr_mxp_op-in? expr "^")    (destructuring-bind (a1 a2) (mjr_mxp_map-args expr #'mjr_cas_fix-int-expt)
                                        (if (integerp a2)
                                            (cond ((mjr_mxp_number? a1)    (expt a1 a2))
                                                  ((= 1 a2)                a1)
                                                  ((mjr_mxp_op-in? a1 "*") (mjr_mxp_cc "*"  ;; (expt (* a b c d) x) => (* (expt a x) (expt b x) (expt c x) (expt d x))
                                                                                       (mjr_mxp_map-args a1 (lambda (x) (mjr_mxp_c "^" x a2)))))
                                                  ((mjr_mxp_op-in? a1 "^") (destructuring-bind (b1 b2) (mjr_mxp_args a1) ;; (expt (expt b1 b2) a2) => (expt b1 (* b2 a2))
                                                                             (mjr_mxp_c "^" b1 (mjr_mxp_c "*" b2 a2))))
                                                  ('t                      (mjr_mxp_c "^" a1 a2)))
                                            (if (and (numberp a1) (rationalp a2))
                                                (mjr_mxp_c "^" (expt a1 (numerator a2)) (/ (denominator a2)))  ;; (^ a n/m) => (^ (^ a n) 1/m) => (^ a^n 1/m)
                                                (mjr_mxp_c "^" a1 a2)))))
        ('t                           (mjr_mxp_map  expr #'mjr_cas_fix-int-expt))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_wack-int-fact (expr)
  "Get rid of factorial calls on positive integers.  Example (! 4) => 12"
  (cond ((mjr_mxp_atom? expr)                        expr)
        ((and (mjr_mxp_op-in? expr "!")
              (= 1 (length (mjr_mxp_args expr)))
              (let ((a1 (car (mjr_mxp_args expr))))
                (and (integerp a1) (<= 0 a1))))      (mjr_combe_! (car (mjr_mxp_args expr))))
        ('t                                          (mjr_mxp_map expr #'mjr_cas_wack-int-fact))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_commute-eval-num (expr op f annihilators identities)
  "Reduce numbers and put number at front of list.  Example (+ 1 a 2 b 3 c) => (+ 6 a b c)"
  (cond ((mjr_mxp_atom? expr)     expr)
        ((mjr_mxp_op-in? expr op) (let* ((new-args (mjr_mxp_map-args expr #'mjr_cas_commute-eval-num op f annihilators identities))
                                         (nval     (reduce f (remove-if #'mjr_mxp_not-number? new-args)))
                                         (exprs    (remove-if #'mjr_mxp_number? new-args)))
                                    (cond ((or (and (listp annihilators) (member nval annihilators))
                                               (and (numberp annihilators) (= annihilators nval)))   nval)
                                          ((or (and (listp identities) (member nval identities))
                                               (and (numberp identities) (= identities nval)))       (if exprs
                                                                                                         (if (cdr exprs)
                                                                                                             (mjr_mxp_cc op exprs) ;; two or more
                                                                                                             (car exprs))          ;; one
                                                                                                         nval))                    ;; NONE
                                          ('t                                                        (if exprs
                                                                                                         (apply #'mjr_mxp_c  ;; two or more
                                                                                                                op
                                                                                                                nval
                                                                                                                exprs)
                                                                                                         nval)))))        ;; none
        ('t                       (mjr_mxp_map  expr #'mjr_cas_commute-eval-num op f annihilators identities))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_commute-eval-num-in-prod (expr)
  "Multiply up numeric terms in products and put result as first argument of each product. Example (* 1 a 2 3 c) => (* 6 a c)"
  (mjr_cas_commute-eval-num expr "*" #'* 0 1))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_commute-eval-num-in-sum (expr)
  "Sum up numeric terms in sums and put result as first argument of each sum.  Example (+ 1 a 2 b 3 c) => (+ 6 a b c)"
  (mjr_cas_commute-eval-num expr "+" #'+ nil 0))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_sort-args (expr &optional (op '("+" "*")))
  "Sort the arguments of complete sub-expressions with top op of op.  Example (+ 2 b a c (* y x)) => (+ 2 a b c (+ x y))"
  (labels ((eless (a b) (cond ((and (mjr_mxp_number? a) (mjr_mxp_number? b))     (< a b))
                              ((and (mjr_mxp_number? a) (mjr_mxp_not-number? b)) 't)
                              ((and (mjr_mxp_number? b) (mjr_mxp_not-number? a)) nil)
                              ((and (mjr_mxp_symbol? b) (mjr_mxp_symbol? a))     (string< a b))
                              ((and (mjr_mxp_symbol? a) (mjr_mxp_not-symbol? b)) 't)
                              ((and (mjr_mxp_symbol? b) (mjr_mxp_not-symbol? a)) nil)
                              ('t                                                (let ((ao (mjr_mxp_op a))
                                                                                       (bo (mjr_mxp_op b)))
                                                                                   (if (mjr_mxp_expr= ao bo)
                                                                                       (let* ((aa  (mjr_mxp_args a))
                                                                                              (ba  (mjr_mxp_args b))
                                                                                              (eqs (mapcar #'mjr_mxp_expr= aa ba))
                                                                                              (pos (position-if #'null eqs)))
                                                                                         (if pos
                                                                                             (eless (nth pos aa) (nth pos ba))
                                                                                             (let ((al (length aa))
                                                                                                   (bl (length ba)))
                                                                                               (< al bl))))
                                                                                       (string< ao bo)))))))
    (cond ((mjr_mxp_atom? expr)      expr)
          ((mjr_mxp_op-in? expr op) (mjr_mxp_cc (mjr_mxp_op expr) (sort (mjr_mxp_map-args expr #'mjr_cas_sort-args op) #'eless)))
          ('t                       (mjr_mxp_map expr #'mjr_cas_sort-args op)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_trig-expand (expr)
  "Replace all trig functions with expressions involving sin and cos only.  Example (tan x) => (* (sin x) (expt (cos x) -1))"
  (if (mjr_mxp_atom? expr)
      expr
      (if (and (= 1 (mjr_mxp_nargs expr))
               (or (mjr_mxp_op-in? expr "tan") (mjr_mxp_op-in? expr "cot") (mjr_mxp_op-in? expr "sec") (mjr_mxp_op-in? expr "csc")))
          (let ((newargs (mjr_mxp_map-args expr #'mjr_cas_trig-expand)))
            (cond ((mjr_mxp_op-in? expr "tan") (mjr_mxp_c "*" (mjr_mxp_cc "sin" newargs) (mjr_mxp_c "^" (mjr_mxp_cc "cos" newargs) -1)))
                  ((mjr_mxp_op-in? expr "cot") (mjr_mxp_c "*" (mjr_mxp_cc "cos" newargs) (mjr_mxp_c "^" (mjr_mxp_cc "sin" newargs) -1)))
                  ((mjr_mxp_op-in? expr "sec") (mjr_mxp_c "^" (mjr_mxp_cc "cos" newargs) -1))
                  ((mjr_mxp_op-in? expr "csc") (mjr_mxp_c "^" (mjr_mxp_cc "sin" newargs) -1))))
          (mjr_mxp_map expr #'mjr_cas_trig-expand))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_collect-over-sum (expr)
  "Collect like products over sums.  Example: n*a+m*a => (n+m)*a"
  ;; This is a very slow function -- very poorly implimented, but it was easy to write. :)
  ;; Requires that the produts in the sum have numbers at the front.
  (flet ((bnp (x) (if (and (mjr_mxp_op-in? x "*") (numberp (cadr x)))
                      (list (cadr x) (cddr x))
                      (list 1        (list x)))))
    (cond ((mjr_mxp_atom? expr)      expr)
          ((mjr_mxp_op-in? expr "+") (let* ((args (mjr_mxp_map-args expr #'mjr_cas_collect-over-sum))
                                            (newa (if args
                                                      (loop for fst = (pop args)
                                                            for (fst-n fst-a) = (bnp fst)
                                                            while fst 
                                                            do (setq args (loop for cur in args
                                                                                for (cur-n cur-a) = (bnp cur)
                                                                                if (and (numberp cur-n) (mjr_mxp_expr= fst-a cur-a))
                                                                                do (incf fst-n cur-n)
                                                                                else
                                                                                collect cur))
                                                            collect (if (= 1 fst-n)
                                                                        (car fst-a)
                                                                        (apply #'mjr_mxp_c "*" fst-n fst-a))))))
                                       (if (and (car newa) (not (cdr newa)))
                                           (car newa)                 ;; 1 element
                                           (apply #'mjr_mxp_c "+" newa))))  ;; 0 or more than 1 element
          ('t                        (mjr_mxp_map expr #'mjr_cas_collect-over-sum)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_collect-over-prod (expr)
  "Collect integer exponents similar bases in products.  Example: a^n*a^m => a^(n+m)"
  ;; This function is O(n^2) but could be MUCH faster if the expression were sorted.  Still, it was easy to write. :)
  (flet ((bnp (x) (if (mjr_mxp_op-in? x "^")
                      (list (mjr_mxp_nth-arg x 0) (mjr_mxp_nth-arg x 1))
                      (list x                     1))))
    (cond ((mjr_mxp_atom? expr)      expr)
          ((mjr_mxp_op-in? expr "*") (let* ((args (mjr_mxp_map-args expr #'mjr_cas_collect-over-prod))
                                            (newa (if args
                                                      (loop for fst = (pop args)
                                                            for (fst-b fst-p) = (bnp fst)
                                                            while fst
                                                            when (integerp fst-p)
                                                            do (setq args (loop for cur in args
                                                                                for (cur-b cur-p) = (bnp cur)
                                                                                if (and (integerp cur-p) (mjr_mxp_expr= fst-b cur-b))
                                                                                do (incf fst-p cur-p)
                                                                                else
                                                                                collect cur))
                                                            collect (if (= 1 fst-p)
                                                                        fst-b
                                                                        (mjr_mxp_c "^" fst-b fst-p))))))
                                       (if (and (car newa) (not (cdr newa)))
                                           (car newa)                ;; 1 element
                                           (mjr_mxp_cc "*" newa))))  ;; 0 or more than 1 element
        ('t                        (mjr_mxp_map expr #'mjr_cas_collect-over-prod)))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_canonize (expr)
  "Reduce to canonical CAS form by applying mjr_cas_canonize1 repeatedly"
  (labels ((mjr_cas_canonize1 (expr) (mjr_cas_collect-over-sum
                                      (mjr_cas_collect-over-prod
                                       (mjr_cas_sort-args
                                        (mjr_cas_commute-eval-num-in-prod  ;; Can crete integers (when sums collapse to a number)
                                         (mjr_cas_commute-eval-num-in-sum  ;; Can crete integers (when sums collapse to a number)
                                          (mjr_cas_nary-comb
                                           (mjr_cas_wack-pos
                                            (mjr_cas_wack-minus            ;; Can create products
                                             (mjr_cas_fix-int-expt         ;; Can create products
                                              (mjr_cas_wack-division       ;; Can crete exponents
                                               (mjr_cas_wack-int-fact      ;; Can create integers
                                                expr)))))))))))))
    (mjr_cas_do-till-noop expr #'mjr_cas_canonize1)))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_diff (expr &rest vars)
  "Iteratively compute the partial derivative with respect to each element of VARS in turn.
For example, if VARS is '(x x), then the second derivative with respect to x will be returned.

This function is HUGE.  The reasons are two fold: 1) When EXPR is canonical, this function makes some effort to return results
that do not contain operators forbidden in canonical expressions (i.e. no - or / operators are in the results unless they were in
the input already -- not to say that the results are canonical, just that we try and not make things to bad.  2) Some effort is
also made to avoid horrible expressions that arise from the use of overly general differentiation rules like f(x)^g(x) for
things like x^2.  Still, the results will be quite messy, and will benefit from canonization."
  (cond ((null vars) (error "mjr_cas_diff: At least one variable must be provided.")))
  (if (cdr vars)
      (loop for var in vars
            for the-expr = (mjr_cas_diff expr var) then (mjr_cas_diff the-expr var)
            finally (return the-expr))
      (let ((var (car vars)))
        (if (mjr_mxp_atom= expr var)
            1
            (if (or (mjr_mxp_atom? expr) (mjr_mxp_free-of expr var))
                0
                (let ((op1 (mjr_mxp_op expr)))
                  (if (= 1 (mjr_mxp_nargs expr))
                      (let ((a1 (mjr_mxp_nth-arg expr 0)))
                        (mjr_mxp_c "*" (mjr_cas_diff a1 var) (let ((fdrule (assoc op1
                                                                                  '(("log"   . ("^" "x" -1))
                                                                                    ("exp"   . ("exp" "x"))
                                                                                    ("abs"   . ("*" "x" ("^" ("abs" "x") -1)))
                                                                                    ("sqrt"  . ("*" 1/2 ("^" "x" -1/2)))
                                                                                    ("cos"   . ("*" -1 ("sin" "x")))
                                                                                    ("sin"   . ("cos" "x"))
                                                                                    ("tan"   . ("^" ("sec" "x") 2))
                                                                                    ("sec"   . ("*" ("sec" "x") ("tan" "x")))
                                                                                    ("cot"   . ("*" -1 ("^" ("csc" "x") 2)))
                                                                                    ("csc"   . ("*" -1 ("cot" "x") ("csc" "x")))
                                                                                    ("acos"  . ("*" -1 ("^" ("+" 1 ("*" -1 ("^" "x" 2))) -1/2)))
                                                                                    ("asin"  . ("^" ("+" 1 ("*" -1 ("^" "x" 2))) -1/2))
                                                                                    ("atan"  . ("^" ("+" 1 ("^" "x" 2)) -1))
                                                                                    ("asec"  . ("*" ("^" "x" -2) ("^" ("+" 1 ("*" -1 ("^" "x" -2))) -1/2)))
                                                                                    ("acot"  . ("*" -1 ("^" ("+" 1 ("^" "x" 2)) -1)))
                                                                                    ("acsc"  . ("*" -1 ("^" "x" -2) ("^" ("+" 1 ("*" -1 ("^" "x" -2))) -1/2)))
                                                                                    ("cosh"  . ("*" -1 ("sinh" "x")))
                                                                                    ("sinh"  . ("cosh" "x"))
                                                                                    ("tanh"  . ("^" ("sech" "x") 2))
                                                                                    ("sech"  . ("*" -1 ("sech" "x") ("tanh" "x")))
                                                                                    ("coth"  . ("*" -1 ("^" ("csch" "x") 2)))
                                                                                    ("csch"  . ("*" -1 ("coth" "x") ("csch" "x")))
                                                                                    ("acosh" . ("^" ("+" -1 ("^" "x" 2)) -1/2))
                                                                                    ("asinh" . ("^" ("+" 1 ("^" "x" 2)) -1/2))
                                                                                    ("atanh" . ("^" ("+" 1 ("*" -1 ("^" "x" 2))) -1))
                                                                                    ("asech" . ("*" -1 ("^" "x" -2) ("^" ("+" -1 ("^" "x" -2)) -1/2)))
                                                                                    ("acoth" . ("*" -1 ("^" ("+" -1 ("^" "x" 2)) -1)))
                                                                                    ("acsch" . ("*" -1 ("^" "x" -2) ("^" ("+" 1 ("^" "x" -2)) -1/2)))
                                                                                    )
                                                                                  :test #'mjr_mxp_atom=)))
                                                               (if fdrule
                                                                   (mjr_mxp_substitute-atom-par (cdr fdrule) "x" a1)
                                                                   (error "YIKES: unary duno: '~a'" expr)))))
                      (let ((args (mjr_mxp_args expr)))
                        (cond ((mjr_mxp_atom= op1 "+")  (mjr_mxp_map expr #'mjr_cas_diff var))                                       ;; D(f1+f2+..)     -> D(f1)+D(f2)+..
                              ((mjr_mxp_atom= op1 "-")  (apply #'mjr_mxp_c "-" (mjr_cas_diff (car args) var)                         ;; D(f-g1-g2-..)   -> D(f)-D(g1)-D(g2)-..
                                                               (mapcar (lambda (x) (mjr_cas_diff x var)) (cdr args))))
                              ((mjr_mxp_atom= op1 "*")  (mjr_mxp_cc "+" (loop for (diff-term . right-terms) on args                  ;; D(f*g)          -> D(f)*g+D(g)*f
                                                                              collect (mjr_mxp_cc "*" left-terms (list (mjr_cas_diff diff-term var)) right-terms)
                                                                              collect diff-term into left-terms)))
                              ((mjr_mxp_atom= op1 "/")  (apply #'mjr_mxp_c "-" (mjr_mxp_c "/"                                       ;; D(f/(g1*g2*..))  -> D(f)/(g1*g2*..)- f*D(g1)/(g1^2*g2*..)- f*D(g2)/(g1*g2^2*..)-..
                                                                                           (mjr_cas_diff (car args) var) (mjr_mxp_cc "*" (cdr args)))
                                                               (loop for diff-term in (cdr args)
                                                                     collect (apply #'mjr_mxp_c "/" (mjr_mxp_c "*" (car args) (mjr_cas_diff diff-term var)) diff-term (cdr args)))))
                              ((mjr_mxp_atom= op1 "^")    (destructuring-bind (a1 a2) args
                                                            (if (mjr_mxp_number? a2)
                                                                (mjr_mxp_c "*" a2 (mjr_cas_diff a1 var) (mjr_mxp_c "^" a1 (1- a2)))  ;; D(f^n)          -> n*f^(n-1)*D(f)
                                                                (mjr_mxp_c "*"                                                       ;; D(f^g)          -> f^g*(log(f)*D(g)+g*D(f)/f)
                                                                           (mjr_mxp_c "^" a1 a2)
                                                                           (mjr_mxp_c "+"
                                                                                      (mjr_mxp_c "*" (mjr_mxp_c "log" a1) (mjr_cas_diff a2 var))
                                                                                      (mjr_mxp_c "*" a2 (mjr_cas_diff a1 var) (mjr_mxp_c "^" a1 -1)))))))
                              ('t                  (error "YIKES: binary duno: '~a'" expr)))))))))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_expand-prod1 (expr)
  "Distribute products over a sum in each product.  Example: (* a (+ b c) (+ d e)))) => (+ (* b a (+ d e)) (* c a (+ d e)))"
  (cond ((and (mjr_mxp_some expr (lambda (x) (mjr_mxp_op-in? x "+")))
              (mjr_mxp_op-in? expr "*")) (let* ((da-sum  nil)
                                             (da-rst  (loop with ns = 't
                                                            for e in (mjr_mxp_args expr)
                                                            when (if (and ns (mjr_mxp_op-in? e "+"))
                                                                     (setq da-sum (mjr_mxp_args e)
                                                                           ns nil)
                                                                     't)
                                                            collect e)))
                                         (mjr_mxp_cc "+" (mapcar (lambda (x) (apply #'mjr_mxp_c "*" x da-rst)) da-sum))))
        ((mjr_mxp_atom? expr)            expr)
        ('t                              (mjr_mxp_map expr #'mjr_cas_expand-prod1))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_expand-prod (expr)
  "Distribute products over sums.  Example: (* a (+ b c) (+ d e)))) => (+ (+ (* d b a) (* e b a)) (+ (* d c a) (* e c a)))"
  (mjr_cas_do-till-noop expr #'mjr_cas_expand-prod1))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_expand-expt (expr &optional (max-exponent 100))
  "Transform exponents into products if the integer exponent is <=max-exponent.  Example: (expt a 2) => (* a a)"
  (cond ((mjr_mxp_atom? expr)
         expr)
        ((and (mjr_mxp_op-in? expr "^") (let ((a2 (mjr_mxp_nth-arg expr 1))) (and (integerp a2) (<= a2 max-exponent))))
         (mjr_mxp_cc "*" (loop with a1 = (mjr_mxp_nth-arg expr 0)
                               for i from 1 upto (mjr_mxp_nth-arg expr 1)
                               collect a1)))
        ('t
         (mjr_mxp_map  expr #'mjr_cas_expand-expt))))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_alg-expand (expr)
  "Distribute products over sums and transform exponents into products.
Example: (* (expt a 2) (+ b a)) => (+ (* b (expt a 2)) (* a (expt a 2)))"
  (mjr_cas_do-till-noop (mjr_cas_do-till-noop expr #'mjr_cas_expand-expt) #'mjr_cas_expand-prod1))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_alg-collect (expr)
  "Collect like products over sums, and exponents over products."
  (mjr_cas_do-till-noop (mjr_cas_do-till-noop expr #'mjr_cas_collect-over-sum) #'mjr_cas_collect-over-prod))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_diff-list (expr var num-diff)
  "Return a list with original expression and NUM-DIFF derivatives"
  (loop for i from 1 upto (1+ num-diff)
        for e = expr then (mjr_cas_diff expr var)
        collect e))

;;----------------------------------------------------------------------------------------------------------------------------------
(defun mjr_cas_isolate (expr sub-expr)
  "Isolate SUB-EXPR (which must only appear one time in EXPR) from EXPR (which must be an ASE).
This function directly manipulates EXPR much like an Algebra I student might who didn't know about the quadratic equation. :)"
  (destructuring-bind (op lhs rhs) expr
    (if (not (mjr_mxp_atom= op "="))
        (setq lhs 0
              rhs expr))
    (if (mjr_mxp_has rhs sub-expr)
        (rotatef lhs rhs))
    (if (mjr_mxp_has rhs sub-expr)
        (error "mjr_cas_isolate: Found sub-expr on both sides of equation!"))
    (if (mjr_mxp_free-of lhs sub-expr)
        (error "mjr_cas_isolate: Could not find sub-expr in equation!"))
    (loop with s = 0
          do (if (or (mjr_mxp_atom? lhs) (and (mjr_mxp_not-symbol? sub-expr) (mjr_mxp_expr= lhs sub-expr)))
                 (return (mjr_mxp_c "=" lhs rhs))
                 (let ((op   (mjr_mxp_op   lhs))
                       (args (mjr_mxp_args lhs)))
                   (cond ((mjr_mxp_atom= op "+") (setq rhs (apply #'mjr_mxp_c "+" rhs (loop for ex in args
                                                                                                 initially (setq lhs nil)
                                                                                                 when (mjr_mxp_free-of ex sub-expr)
                                                                                                 collect (mjr_mxp_c "*" -1 ex)
                                                                                                 else
                                                                                                 do (if lhs
                                                                                                        (error "mjr_cas_isolate: Could not isolate sub-expr in equation (S2)!")
                                                                                                        (setq lhs ex))))))
                         ((mjr_mxp_atom= op "*") (setq rhs (apply #'mjr_mxp_c "*" rhs (loop for ex in args
                                                                                                 initially (setq lhs nil)
                                                                                                 when (mjr_mxp_free-of ex sub-expr)
                                                                                                 collect (mjr_mxp_c "^" ex -1)
                                                                                                 else
                                                                                                 do (if lhs
                                                                                                        (error "mjr_cas_isolate: Could not isolate sub-expr in equation (P2)!")
                                                                                                        (setq lhs ex))))))
                         ((mjr_mxp_atom= op "^") (let* ((b  (first  args))
                                                        (p  (second args))
                                                        (hb (mjr_mxp_has b sub-expr))
                                                        (hp (mjr_mxp_has p sub-expr)))
                                                   (if (and hb hp)
                                                       (error "mjr_cas_isolate: Could not isolate sub-expr in equation (E2)!")
                                                       (if hb
                                                           (psetq rhs (if (evenp p)
                                                                          (mjr_mxp_c "*" (format nil "s~d" (incf s)) (mjr_mxp_c "^" rhs (mjr_mxp_c "^" p -1)))
                                                                          (mjr_mxp_c "^" rhs (mjr_mxp_c "^" p -1)))
                                                                  lhs b)
                                                           (psetq rhs (mjr_mxp_c "*" (mjr_mxp_c "log" rhs) (mjr_mxp_c "^" (mjr_mxp_c "log" b) -1))
                                                                  lhs b)))))
                         ('t                     (error "mjr_cas_isolate: Could not isolate sub-expr in equation (UF)!"))))))))
