;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-combe.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008,2011,2013 by Mitch Richling.  All rights reserved.
;; @brief     Enumerative Combinatorics: Counting combinatorial objects.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_COMBE
  (:USE :COMMON-LISP
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Enumerative Combinatorics: Counting combinatorial objects.;")
  (:EXPORT #:mjr_combe_help
           #:mjr_combe_!
           #:mjr_combe_!!
           #:mjr_combe_perm-with-replacement
           #:mjr_combe_perm
           #:mjr_combe_comb-with-replacement
           #:mjr_combe_comb
           #:mjr_combe_central-comb
           #:mjr_combe_multinomial
           #:mjr_combe_stirling2nd
           #:mjr_combe_stirling1st-unsigned
           #:mjr_combe_stirling1st
           #:mjr_combe_euler1st
           #:mjr_combe_bell
           #:mjr_combe_catalan
           #:mjr_combe_12way-lu-all
           #:mjr_combe_12way-uu-all
           #:mjr_combe_12way-ll-ge1
           #:mjr_combe_12way-ul-ge1
           #:mjr_combe_inversion-number
           #:mjr_combe_descent-number
           #:mjr_combe_k-weak-compositions
           #:mjr_combe_k-compositions
           #:mjr_combe_partition-into-parts-of-size-ge-k
           #:mjr_combe_k-partitions
           #:mjr_combe_partitions
           #:mjr_combe_derangements
           #:mjr_combe_rencontres
           #:mjr_combe_iverson-bracket-le
           ))

(in-package :MJR_COMBE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_help ()
  "Help for MJR_COMBE:

This package contains a collection of functions related to 'Enumerative Combinatorics' -- i.e. 'counting'.  Basic combinatorial functions like the factorial
and the binomial coefficient functions, but with a pure counting focus.  Also included are less generic functions for the direct solutions to various counting
problems (the 12-fold way & several balls-n-urns problems).  The use case is two fold:

  * Assist in solving common counting problems that come up in my day to day work
  * Recreational combinatorics problems.

This counting focus can lead to some surprises.  For example, the function that computes 'N choose K' (MJR_COMBE_COMB) in this package is different from the
traditional 'binomial coefficient function' in that MJR_COMBE_COMB only takes integer arguments, and is zero when K>N or K<0 (See: MJR_NUMU_BINOMIAL).
Another example is the factorial function (MJR_COMBE_!) in this package only supports integer arguments (see MJR_NUMU_GAMMA).

  Twelvefold way:

        |-------------+-----------+---------------------------------+------------------------------+------------------------|
        |                                                    12-fold way (N=#balls, K=#urns)                                |
        |-------------+-----------+---------------------------------+------------------------------+------------------------|
        | balls       | urns      | all                             | le1                          | ge1                    |
        |-------------+-----------+---------------------------------+------------------------------+------------------------|
        | labeled     | labeled   | mjr_combe_perm-with-replacement | mjr_combe_perm               | mjr_combe_12way-ll-ge1 |
        |             |           | mjr_combe_bell (N=K)            |                              | mjr_combe_! (N=K)      |
        | unlabeled   | labeled   | mjr_combe_comb-with-replacement | mjr_combe_comb               | mjr_combe_12way-ul-ge1 |
        | labeled     | unlabeled | mjr_combe_12way-lu-all          | mjr_combe_iverson-bracket-le | mjr_combe_stirling2nd  |
        | unlabeled   | unlabeled | mjr_combe_12way-uu-all          | mjr_combe_iverson-bracket-le | mjr_combe_k-partitions |
        |-------------+-----------+---------------------------------+------------------------------+------------------------|

  Non-twelvefold

        * mjr_combe_!!
        * mjr_combe_central-comb
        * mjr_combe_multinomial
        * mjr_combe_stirling1st-unsigned
        * mjr_combe_stirling1st
        * mjr_combe_euler1st
        * mjr_combe_catalan
        * mjr_combe_inversion-number
        * mjr_combe_descent-number
        * mjr_combe_k-weak-compositions
        * mjr_combe_k-compositions
        * mjr_combe_partition-into-parts-of-size-ge-k
        * mjr_combe_partitions
        * mjr_combe_derangements
        * mjr_combe_rencontres

Note: Some of the implementations here are quite slow."
  (documentation 'mjr_combe_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_iverson-bracket-le (a b)
  "Return 1 if a<=b, and 0 otherwise.

Interpretations:
  * mjr_combe_12way-lu-le1
  * The number ways of placing N labeled balls into N unlabeled urns such that each ball  has no more than one ball
  * mjr_combe_12way-ul-le1
  * The number ways of placing N unlabeled balls into N labeled urns such that each ball  has no more than one ball"
  (mjr_numu_iverson-bracket #'<= a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_! (n)
  "Compute the factorial.

Interpretations:
  * mjr_combe_12way-ll-ge1 with N=K
  * The number ways of placing N labeled balls into N labeled urns such that each urn has at least 1 ball
  * The number ways of permuting N labeled objects"
  (cond ((not (integerp n))   (error "mjr_combe_!: Input must be an integer!"))
        ((< n 0)              (error "mjr_combe_!: Input must be non-negative!")))
  (if (< n 2)
      1
      (mjr_numu_prod :start 1 :end n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_!! (n)
  "Compute the double factorial."
  (cond ((not (integerp n))   (error "mjr_combe_!!: Input must be an integer!"))
        ((< n 0)              (error "mjr_combe_!!: Input must be non-negative!")))
  (cond ((< n 2) 1)
        ((= n 2) 2)
        ('t      (mjr_numu_prod :start (if (oddp n) 1 2) :end n :step 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_perm (n k)
  "Compute the permutation function, $n^{\underline{k}}$ (falling power).

Interpretations:
  * $$\vphantom{P}_nP_k = \vphantom{P}^nP_k = P_{n,k} = (n)_k = n^{\underline{k}} = \mathtt{FallingFactoral(}n,k\mathtt{)}$$
  * mjr_combe_12way-ll-le1
  * The number ways of placing N labeled balls into K labeled urns such that each urn has no more than one ball
  * Ways to select K items from N items where order matters"
  ;; MJR TODO NOTE mjr_combe_perm: Add a "falling power" version of this function that takes non-integer N
  (cond ((not (integerp n))   (error "mjr_combe_perm: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_perm: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_perm: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_perm: Second argument must be non-negative!")))
  (cond ((= k 0) 1)
        ((= n 0) 0)
        ((< n k) 0) ;; Can't pick more than we have!
        ('t      (mjr_numu_prod :start (1+ (- n k)) :end n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_perm-with-replacement (n k)
  "Compute the permutation function with replacements, $n^k$.

Interpretations:
  * mjr_combe_12way-ll-all
  * The number ways of placing N labeled balls into K labeled urns
  * Ways to select K items from N items with replacement where order matters"
  (cond ((not (integerp n))   (error "mjr_combe_perm-with-replacement: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_perm-with-replacement: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_perm-with-replacement: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_perm-with-replacement: Second argument must be non-negative!")))
  (expt n k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_comb (n k)
  "Compute the combination function without replacements, $\binom{n}{k}$.

Interpretations:
  * mjr_combe_12way-ul-le1
  * The number of ways to place N unlabeled balls into K labeled urns such that each urn has at least one ball.
  * Ways to select K items from N items where order doesn't matter

NOTE: This is the same as the binomial coefficient for integer arguments
NOTE: This function is relatively fast, and avoids large intermediate values during the computation"
                                        ; MJR TODO NOTE mjr_combe_comb: Add a version of this function that takes non-integer arguments.  Call it "mjr_combe_binom"
  (cond ((not (integerp n))   (error "mjr_combe_comb: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_comb: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_comb: Second argument must be an integer!")))
  (let ((n-k (- n k)))
    (cond ((= k 0)    1)
          ((= n 0)    0)
          ((< k 0)    0)
          ((< n k)    0) ;; 0 ways to select more objects than we have
          ((= n k)    1)
          ((< n-k k)  (mjr_numu_prod :start 1 :end n-k :seq-fun (lambda (i) (/ (+ k i)   i))))
          ('t         (mjr_numu_prod :start 1 :end k   :seq-fun (lambda (i) (/ (+ n-k i) i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_comb-with-replacement (n k)
  "Compute the combination function with replacements, $\left(\binom{n}{k}\right)$.

Interpretations (when :WITH-REPLACEMENT is non-NIL):
  * mjr_combe_12way-ul-all
  * The number of ways to place N unlabeled balls into K labeled urns such that each urn has at least one ball.
  * Ways to select K items from N items with replacement where order doesn't matter"
  (cond ((not (integerp n))   (error "mjr_combe_comb-with-replacement: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_comb-with-replacement: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_comb-with-replacement: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_comb-with-replacement: Second argument must be non-negative!")))
  (mjr_combe_comb (1- (+ n k)) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_central-comb (n)
  "Central binomial coefficient for integer arguments: $\binom{2n}{n}$"
  (cond ((not (integerp n)) (error "mjr_combe_central-comb: Argument must be an integer!"))
        ((< n 0)            (error "mjr_combe_central-comb: Argument must be non-negative!")))
  (cond ((= n 0) 1)
        ('t      (mjr_numu_prod :start 1 :end n :seq-fun (lambda (i) (/ (+ n i) i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_multinomial (n &rest k-list)
  "Compute the multinomial function.

Interpretations:
  * The number of ways of putting N labeled balls into M labeled urns, with K_i balls into urn U_i for i=1...M
  * The number of ways to permute a multiset of N elements with K_i are the multiplicities of each of the distinct M elements.
    Example: MISSISSIPPI -- M=4, K_1=1 (M), K_2=4 (S), K_3=2 (P), K_4=4 (I)
  * The coefficient on the term x_1^k_1*x_2^k_2*...*x_M^k_M in the expansion of (x_1+x_2+...+x_M)^n"
  (cond ((not (integerp n))                 (error "mjr_combe_multinomial: First argument must be an integer!"))
        ((< n 0)                            (error "mjr_combe_multinomial: First argument must be non-negative!"))
        ((notevery #'integerp k-list)       (error "mjr_combe_multinomial: Arguments after the first must be an integers!"))
        ((some (lambda (x) (< x 0)) k-list) (error "mjr_combe_multinomial: Arguments after the first must be non-negative!")))
  (let ((k-sum (reduce #'+ k-list)))
    (cond ((= 0 k-sum)  1)
          ((= n 0)      0)
          ((< n k-sum)  0) ;; 0 ways to select more objects than we have
          ('t           (mjr_numu_prod :start 1
                                       :end n
                                       :seq-fun (lambda (i) (/ (if (<= i n) i 1)
                                                               (loop with prod = 1
                                                                     for k in k-list
                                                                     finally (return prod)
                                                                     when (<= i k)
                                                                     do (setf prod (* prod i))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_stirling2nd (n k)
  "Compute the Stirling numbers of the second kind, $S(n,k)$

Interpretations:
  * The number of ways to partition a set of N objects into K non-empty subsets
  * mjr_combe_12way-lu-ge1
  * The number ways of placing N unlabeled balls into N labeled urns such that each ball has at least one ball
References:
  Allenby and Slomson (2010); How to Count: An Introduction to Combinatorics; p48,61"
  (cond ((not (integerp n))   (error "mjr_combe_stirling2nd: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_stirling2nd: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_stirling2nd: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_stirling2nd: Second argument must be non-negative!")))
  (cond ((and (= n 0) (= k 0)) 1)
        ((or  (= n 0) (= k 0)) 0)
        ((or  (= n k) (= k 1)) 1)
        ((> k n)               0)
        ((and (> n 1) (= k 2)) (1- (expt 2 (1- n))))
        ((and (> n 2) (= k 3)) (/ (- (expt 3 (1- n)) (expt 2 n) -1) 2))
        ((and (> n 3) (= k 4)) (+ (/ (expt 4 n) 24) (/ (expt 3 n) -6) (/ (expt 2 n) 4) -1/6))
        ((= n (1+ k))          (mjr_combe_comb n 2))
        ('t                    (/ (mjr_numu_sum :start 0
                                                :end k
                                                :seq-fun (lambda (j) (* (if (oddp j) -1 1)
                                                                        (mjr_combe_comb k j)
                                                                        (expt (- k j) n))))
                                  (mjr_combe_! k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_stirling1st-unsigned (n k)
  "Compute the Stirling numbers of the first kind, $c(n,k)$

Interpretations:

  * The number of permutations of N objects with exactly K orbits (cycles)

    i.e. The symmetric group on 4 objects has s(4, 2) = 11 elements with 2 cycles.

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol 1; pp 18"
  (cond ((not (integerp n))   (error "mjr_combe_stirling1st-unsigned: First argument must be an integer!"))
        ((not (integerp k))   (error "mjr_combe_stirling1st-unsigned:: Second argument must be an integer!")))
  (cond ((and (= n 0) (= k 0)) 1)
        ((< n 1)               0)
        ((< k 1)               0)
        ((= n k)               1)
        ((> k n)               0)
        ((= n 1)               (mjr_combe_! (1- n)))
        ((= n (1+ k))          (mjr_combe_comb n 2))
        ('t                    (+ (* (1- n) (mjr_combe_stirling1st-unsigned (1- n) k))
                                  (mjr_combe_stirling1st-unsigned (1- n) (1- k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_stirling1st (n k)
  "Compute the Stirling numbers of the first kind, $s(n,k)$

Interpretations:
  * ???

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol 1; pp 18"
  (cond ((not (integerp n))   (error "mjr_combe_stirling1st: First argument must be an integer!"))
        ((not (integerp k))   (error "mjr_combe_stirling1st: Second argument must be an integer!")))
  (* (if (oddp n) -1 1)
     (if (oddp k) -1 1)
     (mjr_combe_stirling1st-unsigned n k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_euler1st (n k)
  "The number of permutations of the numbers $1\cdots n$ which have exactly $k$ descents. See: MJR_COMBE_DESCENT-NUMBER

Interpretations:
  * The number of permutations of the numbers $1\cdots n$ which have exactly $k$ descents.
  * The number of permutations of the numbers $1\cdots n$ in which have exactly $k$ ascents."
  (cond ((not (integerp n))   (error "mjr_combe_euler1st: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_euler1st: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_euler1st: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_euler1st: Second argument must be non-negative!")))
  (cond ((=  k 0)        1)
        ((=  k (1- n))   1)
        ((>= k n)        0)
        ((< (- n k 1) k) (mjr_combe_euler1st n (- n k 1)))
        ('t              (mjr_numu_sum :start 0 :end k
                                       :seq-fun (lambda (i) (* (if (oddp i) -1 1)
                                                               (mjr_combe_comb (1+ n) i)
                                                               (expt (- (1+ k) i) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_bell (n)
  "Compute the N'th bell number.

Interpretations:
  * mjr_combe_12way-ll-all with N=K
  * The number ways of placing N labeled balls into N indistinguishable urns.
  * The number of partitions of a SET with N members."
  (cond ((not (integerp n)) (error "mjr_combe_bell: Argument must be an integer!"))
        ((< n 0)            (error "mjr_combe_bell: Argument must be non-negative!")))
  (loop with idxs = (vector 0 1 2)
        with datv = (make-array (list 3 (+ 3 n)) :initial-element 1)
        for row-num from 0 upto (1+ n)
        do (setf (aref datv (aref idxs 2) 0) (aref datv (aref idxs 1) row-num))
        do (loop for col-num from 1 upto (1+ row-num)
                 do (setf (aref datv (aref idxs 2) col-num) (+ (aref datv (aref idxs 1) (1- col-num))
                                                               (aref datv (aref idxs 2) (1- col-num)))))
        do (rotatef (aref idxs 2) (aref idxs 0) (aref idxs 1))
        finally (return (aref datv (aref idxs 2) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_catalan (n)
  "Recursive implementation

Interpretations:
  * The number of vectors $(a_1,...,a_{2n})\\in\\{1,-1\\}^{2n}$ such that $\\sum_1^k a_k\\ge0$ for $k\\in\\{1,...,2n\\}$
  * The number of non-negative integer vectors $(a_1,...,a_n)$ such that $\\sum_1^n a_n=n$ and $\\sum_1^k a_k\\le k$ for $0\\le k<n$

References:
  Allenby and Slomson (2010); How to Count: An Introduction to Combinatorics; p72,80"
  (cond ((not (integerp n)) (error "mjr_combe_catalan: Argument must be an integer!"))
        ((< n 0)            (error "mjr_combe_catalan: Argument must be non-negative!")))
  (if (< n 2)
      1
      (* (/ (- (* 4 n) 2) (1+ n)) (mjr_combe_catalan (1- n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_12way-lu-all (n k)
  ;; MJR TODO NOTE <2011-11-11 13:54:55 CST> mjr_combe_12way-lu-all: CHECK THIS!!
  "Number of ways to put N labeled balls into K unlabeled urns.

Interpretations:
  * mjr_combe_12way-ll-all
  * Number of ways to put N labeled balls into K unlabeled urns."
  (cond ((not (integerp n))   (error "mjr_combe_12way-ll-all: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_12way-ll-all: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_12way-ll-all: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_12way-ll-all: Second argument must be non-negative!")))
  (mjr_numu_sum :start 1 :end k :seq-fun (lambda (i) (mjr_combe_stirling2nd n i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_12way-ll-ge1 (n k)
  "The number ways of placing N labeled balls into N labeled urns such that each ball has at least one ball

Interpretations:
  * mjr_combe_12way-ll-ge1
  * The number ways of placing N labeled balls into N labeled urns such that each ball has at least one ball"
  (cond ((not (integerp n))   (error "mjr_combe_12way-ll-ge1: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_12way-ll-ge1: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_12way-ll-ge1: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_12way-ll-ge1: Second argument must be non-negative!")))
  (* (mjr_combe_! k) (mjr_combe_stirling2nd n k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_12way-ul-ge1 (n k)
  "The number ways of placing N unlabeled balls into N labeled urns such that each ball has at least one ball

Interpretations:
  *   * mjr_combe_12way-ul-ge1
  * The number ways of placing N unlabeled balls into N labeled urns such that each ball has at least one ball"
  (cond ((not (integerp n))   (error "mjr_combe_12way-ul-ge1: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_12way-ul-ge1: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_12way-ul-ge1: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_12way-ul-ge1: Second argument must be non-negative!")))
  (mjr_combe_comb-with-replacement k (- n k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_inversion-number (seq)
  "Count the number of pairs $(s_i, s_j)$ in the finite sequence $(s_1,...,s_n)$ such that $s_i > s_j$ when $i<j$"
  (loop with vec = (if (vectorp seq) seq (concatenate 'vector seq))
        with len = (length vec)
        for i from 0 upto (- len 2)
        for si across vec
        sum (loop for j from (1+ i) upto (- len 1)
                  count (> (aref vec i) (aref vec j)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_descent-number (seq)
  "Count the number of pairs $(s_{i-1}, s_i)$ in the finite sequence $(s_1,...,s_n)$ such that $s_{i-1} > s_i$"
  (if (vectorp seq)
      (loop for si-1 = nil then si
            for si across seq
            count (and si-1 (> si-1 si)))
      (loop for si-1 = nil then si
            for si in seq
            count (and si-1 (> si-1 si)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_k-weak-compositions (n k)
  "Number of k component weak compositions.

Interpretations:
  * Number of ways to form the sum $\\sum_1^k x_k=n$ where $x_k$ are non-negative integers -- order matters.
  * Number of ways to put $n$ balls of $k$ colors into one urn.  At least $n$ balls of each color is available in the pool.  The order in which they are
    placed in the urn is irrelevant, just the final counts.

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol I"
  (cond ((not (integerp n))   (error "mjr_combe_k-weak-compositions: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_k-weak-compositions: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_k-weak-compositions: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_k-weak-compositions: Second argument must be non-negative!")))
  (mjr_combe_comb (1- (+ n k)) (1- k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_k-compositions (n k)
  "Number of k component compositions.

Interpretations:
  * Number of ways to form the sum $\\sum_1^k x_k=n$ where $x_k$ are positive integers -- order matters.
  * Number of ways to put $n$ balls of $k$ colors into one urn such that at least 1 ball of each color is in the urn at the end.  At least $n$ balls of each
    color is available in the pool. The order in which they are placed in the urn is irrelevant, just the final counts.

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol I"
  (cond ((not (integerp n))   (error "mjr_combe_k-compositions: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_k-compositions: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_k-compositions: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_k-compositions: Second argument must be non-negative!")))
  (mjr_combe_comb (1- n) (1- k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_k-partitions (n k)
  "Number of partitions of N into K parts

See MJR_COMBE_PARTITIONS for a definition of partition.

Interpretations:
  * Number of partitions of N in which the greatest part is K (1<=K<=N).
  * Number of partitions of N into K positive parts (1<=K<=N)
  * Number of ways to place N unlabeled balls into K unlabeled urns such that each urn has at least 1 ball

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol I"
  (cond ((not (integerp n))   (error "mjr_combe_k-partitions: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_k-partitions: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_k-partitions: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_k-partitions: Second argument must be non-negative!")))
  (cond ((= k n)      1) ;; Gets k=n=0
        ((> k n)      0)
        ((= k 0)      0)
        ((= n 0)      0)
        ((= k 1)      1)
        ((= k (1+ n)) 1)
        ('t           (+ (mjr_combe_k-partitions (1- n)  (1- k))
                         (mjr_combe_k-partitions (- n k) k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_partition-into-parts-of-size-ge-k (n k)
  "Number of partitions of N such that all parts are at least as big as K.

See MJR_COMBE_PARTITIONS for a definition of partition.

References:
  Richard P. Stanley (1997); Enumerative Combinatorics Vol I"
  (cond ((not (integerp n))   (error "mjr_combe_partition-into-parts-of-size-ge-k: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_partition-into-parts-of-size-ge-k: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_partition-into-parts-of-size-ge-k: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_partition-into-parts-of-size-ge-k: Second argument must be non-negative!")))
  (cond ((= k  n)      1)
        ((<= k 0)      (mjr_combe_partition-into-parts-of-size-ge-k n 1))  ;; k=n=0 in previous case
        ((<= n 0)      0)
        ((> k  n)      0)
        ('t            (+ (mjr_combe_partition-into-parts-of-size-ge-k n  (1+ k))
                          (mjr_combe_partition-into-parts-of-size-ge-k (- n k) k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_12way-uu-all (n k)
  "Number of ways to put N unlabeled balls into K unlabeled urns.

Interpretations:
  * mjr_combe_12way-uu-all
  * Number of ways to put N unlabeled balls into K unlabeled urns."
  (cond ((not (integerp n))   (error "mjr_combe_12way-uu-all: First argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_12way-uu-all: First argument must be non-negative!"))
        ((not (integerp k))   (error "mjr_combe_12way-uu-all: Second argument must be an integer!"))
        ((< k 0)              (error "mjr_combe_12way-uu-all: Second argument must be non-negative!")))
  (mjr_numu_sum :start 1 :end k :seq-fun (lambda (i) (mjr_combe_k-partitions i n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_partitions (n)
  "The number of partitions of N.

A partition of a number, $n$, is a sum of positive integers equaling $n$ where sums differing only in the order of the summands are not counted as distinct."
  (cond ((not (integerp n)) (error "mjr_combe_partition: Argument must be an integer!"))
        ((< n 0)            (error "mjr_combe_partition: Argument must be non-negative!")))
  (if (< n 2)
      1
      (let ((tmp (make-array (list (max 4 (1+ n)) n) :initial-element 0)))
        (setf (aref tmp 1 1) 1)
        (setf (aref tmp 2 1) 1)
        (setf (aref tmp 3 1) 2)
        (loop for x from 4 upto n
              do (loop for y from (floor x 2) downto 1
                       do (setf (aref tmp x y) (+ 1 (aref tmp (- x y) y) (aref tmp x (1+ y))))))
        (1+ (aref tmp n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_derangements (n)
  "Number of derangements of $n$ objects -- i.e. number of permutations of $n$ objects that fix no element.

Also called 'rencontres numbers with $k=0$', 'de Montmort numbers', or the 'subfactorial function' (ocassionally written as $!n$).

References:
  Allenby and Slomson (2010); How to Count: An Introduction to Combinatorics; p60"
  (cond ((not (integerp n))   (error "mjr_combe_derangements: Argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_derangements: Argument must be non-negative!")))
  (if (zerop n)
      1
      (let ((fact-i 1))
        (* (1+ (mjr_numu_sum :start 1 :end n :seq-fun (lambda (i) (/ (if (oddp i) -1 1)
                                                                     (setq fact-i (* fact-i i))))))
           fact-i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_combe_rencontres (n k)
  "Rencontres numbers -- i.e. number of permutations of $n$ objects that fix $k$ elements.

References:
  http://en.wikipedia.org/wiki/Rencontres_numbers"
  (cond ((not (integerp n))   (error "mjr_combe_rencontres: Argument must be an integer!"))
        ((< n 0)              (error "mjr_combe_rencontres: Argument must be non-negative!")))
  (cond ((= n k)       1)
        ((= n (1- k))  0)
        ((< n k)       0)
        ('t            (let ((fact-i 1)) ;; $D(n, k) = \\frac{n!}{k!} \\sum_{i=0}^{n-k} \\frac{(-1)^i}{i!}$
                         (* (1+ (mjr_numu_sum :start 1 :end (- n k) :seq-fun (lambda (i) (/ (if (oddp i) -1 1)
                                                                                            (setq fact-i (* fact-i i))))))
                            (/ (mjr_combe_! n)
                               (mjr_combe_! k)))))))
