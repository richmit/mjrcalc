;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-perm.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004 by Mitch Richling.  All rights reserved.
;; @brief     Permutation group computation.@EOL
;; @Std       Common Lisp
;;
;;            
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_PERM
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Permutation group computation.;")
  (:EXPORT #:mjr_perm_help
           #:mjr_perm_swapping-number #:mjr_perm_evenp #:mjr_perm_oddp #:mjr_perm_sgn
           #:mjr_perm_order
           #:mjr_perm_check-perm
           #:mjr_perm_* #:mjr_perm_/ 
           #:mjr_perm_eval
           #:mjr_perm_apply-to-array2d #:mjr_perm_apply-to-sequence
           #:mjr_perm_cycle-structure-list #:mjr_perm_cycle-structure-lengths
           #:mjr_perm_make-identity #:mjr_perm_make-random
           ))

(in-package :MJR_PERM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_help ()
"Help for MJR_PERM:

This library performs some basic permutation group computations that are of use for the other computations.  In keeping with the 'utility' focus, most of the
permutations are zero-indexed; however, a few of the functions allow this to be changed (the index-base argument) to support more interactive use.  That said,
it is unlikely that this library will ever be extended into a general permutation group computation tool -- use GAP for that. :)"
  (documentation 'mjr_perm_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_check-perm (perm &key (index-base 0))
  "Return non-nil if the perm is valid, and nil otherwise.

A permutation may be invalid for any number of reasons: 1) It is not a vector, 2) It contains non-integers, 3) It contains out of range integers, 4) It
contains duplicate integers."
  (if (vectorp perm)
      (let* ((len     (length perm))
             (idx-cnt (make-array len :initial-element 0)))
        (if (> len 0)
            (loop for i  from 0 ; Check the perm, and compute inverse
                  for jr  across perm       ; Raw element value
                  for j = (- jr index-base) ; Zero based value
                  when (not (integerp j))            do (return nil)
                  when (or (< j 0) (> j (1- len)))   do (return nil)
                  do (incf (aref idx-cnt j))
                  when (< 1 (aref idx-cnt j))        do (return nil)
                  finally (return 't))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_make-identity (len)
  "Make an identity permutation of size LEN."
  (let ((new-seq   (make-array len)))
    (dotimes (i len new-seq)
      (setf (aref new-seq i) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_make-random (seq-len &optional (num-swaps nil))
  "Make a random permutation of size SEQ-LEN."
  (let ((new-seq   (mjr_perm_make-identity seq-len))
        (num-swaps (or num-swaps (* 3 seq-len))))
    (dotimes (i num-swaps new-seq)
      (rotatef (aref new-seq (random seq-len)) (aref new-seq (random seq-len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_eval (perm x &key (index-base 0))
  "Evaluate a permutation at x. 

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (aref perm (- x index-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_cycle-structure-list (perm &key (index-base 0))
  "Returns a list of lists such that each sub-list represents a cycle in the disjoint cycle structure of PERM.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (loop with used-bit-map = (make-array (length perm) :initial-element 0)
        for nxt-idx       = (position 0 used-bit-map)
        while nxt-idx
        collect (loop for nxter   = nxt-idx then nxterer
                      for nxterer = (- (aref perm nxter) index-base)
                      do (setf (aref used-bit-map nxter) 1)
                      collect (+ nxter index-base)
                      while (not (= nxt-idx nxterer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_cycle-structure-lengths (perm &key (index-base 0))
  "Return a list with the length of each cycle in the disjoint cycle structure of PERM.  The list is NOT ordered.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (mapcar #'length (mjr_perm_cycle-structure-list perm :index-base index-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_swapping-number (perm &key (index-base 0))
  "Return the number of inversions, or nil if the permutation is invalid.

See MJR_PERM_CHECK-PERM for more information about invalid permutations."
  (loop for i in (mjr_perm_cycle-structure-lengths perm :index-base index-base)
        sum (1- i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_oddp (perm &key (index-base 0))
  "Return non-nil if the perm is odd.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (oddp (loop for i in (mjr_perm_cycle-structure-lengths perm :index-base index-base)
              sum (1- i))))  ;; a cycle of length n can be written as n-1 transpositions (two cycles).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_evenp (perm &key (index-base 0))
  "Return non-nil if the perm is even.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (not (mjr_perm_oddp perm :index-base index-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_sgn (perm)
  "Return the signature (or sign) of the permutation (-1 if odd, 1 if even).

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (if (mjr_perm_evenp perm) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_* (&rest perms)
  "Compute the product (composition) of the given permutations.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (if (= 2 (length perms))
      (let* ((perm1 (first perms))
             (perm2 (second perms))
             (len   (length perm1))
             (prod (make-array len :initial-element 0)))
        (dotimes (i len prod)
          (setf (aref prod i) (mjr_perm_eval perm1 (mjr_perm_eval perm2 i)))))
      (reduce #'mjr_perm_* perms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_order (perm &key (index-base 0))
  "Return the order of the perm.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (apply #'lcm (mjr_perm_cycle-structure-lengths perm :index-base index-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_/ (&rest perms)
  "Compute the inverse of a single argument, or divide multiple arguments.

This function performs no error checking -- use MJR_PERM_CHECK-PERM first if PERM might be invalid."
  (if (= 1 (length perms))
      (let* ((perm1 (first perms))
             (len   (length perm1))
             (p-inv (make-array len :initial-element 0)))
        (loop for i from 0
              for j across perm1
              do (setf (aref p-inv j) i)
              finally (return p-inv)))
      (if (= 2 (length perms))
          (mjr_perm_* (first perms) (mjr_perm_/ (second perms)))
          (reduce #'mjr_perm_/ perms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_apply-to-array2d (perm the-array &optional perm-cols)
  "Permute the rows (or columns if PERM-COLS is non-nil) of the given array via the given permutation"
  (let* ((rows (array-dimension the-array 0))
         (cols (array-dimension the-array 1))
         (new-array (make-array (list rows cols) :initial-element 0)))
    (dotimes (i rows new-array)
      (dotimes (j cols)
        (setf (aref new-array i j) (aref the-array (if perm-cols i (mjr_perm_eval perm i)) (if perm-cols (mjr_perm_eval perm j) j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_perm_apply-to-sequence (perm the-seq)
  "Permute the elements of the given seducing (vector or list) via the given permutation"
  (cond ((vectorp the-seq)   (let* ((len     (length the-seq))
                                    (new-vec (make-array len)))
                               (dotimes (i len new-vec)
                                 (setf (aref new-vec i) (aref the-seq (mjr_perm_eval perm i))))))
        ((listp the-seq)     (loop with len = (length the-seq)
                                   for i from 0 upto (1- len)
                                   collect (nth (mjr_perm_eval perm i) the-seq)))
        ('t                  (error "mjr_perm_apply-to-sequence: Unsupported type (need list or vector)!"))))

