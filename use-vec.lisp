;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-vec.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2008-2013 by Mitch Richling.  All rights reserved.
;; @brief     Mathematical vectors.@EOL
;; @Std       Common Lisp
;;
;;            This library is designed primarily to support the mat_ library; however, it is also useful as an aid in hand
;;            computation with vectors.  This packages grows as I require new functionality, and it is far from a complete vector
;;            arithmetic package.
;;            
;;            TODO: Bind vectors together to form a matrix
;;            TODO: Coordinate conversions (rectangular, spherical, cylindrical)
;;            TODO: Rotate vector
;;            TODO: Make it so that a "mathematical vector" can be a "LISP List", but always return "LISP Vectors"
;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_VEC
  (:USE :COMMON-LISP
        :MJR_CMP
        :MJR_EPS
        :MJR_CHK
        :MJR_UTIL
        :MJR_NUMU
        :MJR_VVEC)
  (:DOCUMENTATION "Brief: Mathematical vectors.;")
  (:EXPORT #:mjr_vec_every-idx #:mjr_vec_e? 
           #:mjr_vec_make-const #:mjr_vec_make-from-func #:mjr_vec_make-seq #:mjr_vec_make-e 
           #:mjr_vec_ewuo #:mjr_vec_ewbo
           #:mjr_vec_dot #:mjr_vec_triple-cross #:mjr_vec_cross #:mjr_vec_- #:mjr_vec_+ #:mjr_vec_/ #:mjr_vec_*
           #:mjr_vec_norm-infinity #:mjr_vec_norm-one #:mjr_vec_norm-two #:mjr_vec_norm-two-squared 
           #:mjr_vec_zap-eps #:mjr_vec_float #:mjr_vec_rationalize #:mjr_vec_normalize
           #:mjr_vec_print #:mjr_vec_code
           #:mjr_vec_proj
           #:mjr_vec_orthogonalize
           #:mjr_vec_<
           ))

(in-package :MJR_VEC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_make-const (len-or-vector &optional (constant 0))
  "Create a constant vector.  If missing, constant=0."
  (let ((len (if (vectorp len-or-vector) (length len-or-vector) len-or-vector)))
    (if (> 1 len)
        (error "mjr_vec_make-const: Invalid vector size specified!"))
    (make-array len :initial-element constant)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_make-from-func (func &key len start end step)
  "Create a vector by evaluating FUNC on an arithmetic sequence.  
POINTS, START, END, STEP, and LEN are processed by MJR_VVEC_KW-NORMALIZE.  If LEN is a non-empty sequence (list/vector), 
then the length of that sequence will be used for the length."
  (mjr_vvec_gen-0sim 'vector (list :map-fun func :start start :end end :step step :len len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_make-seq (&key points start end step len)
  "Compute a sequence, and put the result in a vector.
See the function MJR_VVEC_KW-NORMALIZE for details of how the arguments specify the sequence."
  (mjr_vvec_gen-0sim 'vector (list :start start :end end :step step :points points :len len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_dot (vec1 vec2 &optional (hermitian 't))
  "Compute the dot product of the given vectors.  If vectors are of different lengths, then
the product will only be for the number of elements in the shortest vector."
  (loop for x1 across vec1
        for x2 across vec2
        sum (* (if hermitian (conjugate x1) x1) x2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_cross (&rest vecs)
  "Compute the cross product of the given vectors.  Only the first 3 elements of the vectors
are used. Returns NIL if any vector is too short."
  (if (< 2 (length vecs))
      (reduce #'mjr_vec_cross vecs)
      (let ((arg1 (first vecs))
            (arg2 (second vecs)))
        (if (not (and (= 3 (length arg1)) (= 3 (length arg2))))
            (error "mjr_vec_cross: Only vectors of length three are supported!"))
        ;; The aref calls below will error when arg1 or arg2 are not a vector
        (vector (- (* (aref arg1 1) (aref arg2 2)) (* (aref arg1 2) (aref arg2 1)))
                (- (* (aref arg1 2) (aref arg2 0)) (* (aref arg1 0) (aref arg2 2)))
                (- (* (aref arg1 0) (aref arg2 1)) (* (aref arg1 1) (aref arg2 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_triple-cross (vec1 vec2 vec3)
  "Compute the triple cross  product of the given vectors $(a \cdot ( b \times c))$.  If vectors are of different lengths, then
the product will only use the first three elements."
  (- (+ (* (aref vec1 0) (aref vec2 1) (aref vec3 2))
        (* (aref vec1 1) (aref vec2 2) (aref vec3 0))
        (* (aref vec1 2) (aref vec2 0) (aref vec3 1)))
     (* (aref vec1 0) (aref vec2 2) (aref vec3 1))
     (* (aref vec1 1) (aref vec2 0) (aref vec3 2))
     (* (aref vec1 2) (aref vec2 1) (aref vec3 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_ewuo (vec op)
  "Apply unary function to each element of the vector."
  (typecase vec
    (number    (vector (funcall op vec)))
    (vector    (let ((len (length vec)))
                 (if (> len 0)
                     (mjr_vec_make-from-func (lambda (i) (funcall op (aref vec i))) :len vec)
                     #())))
    (otherwise (error "mjr_vec_ewuo: Only vectors are supported!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_ewbo (vec1 vec2 op)
  "Apply binary function to each of the elements of vec1 and vec2 in turn."
  (if (numberp vec1)
      (mjr_vec_ewbo (vector vec1) vec2 op)
      (if (numberp vec2)
          (mjr_vec_ewbo vec1 (vector vec2) op)
          (if (and (vectorp vec1) (vectorp vec2))
              (let ((l1 (length vec1))
                    (l2 (length vec2)))
                (if (and (> l1 0) (> l2 0))
                    (mjr_vec_make-from-func (lambda (i) (funcall op (aref vec1 i) (aref vec2 i))) :len (min l1 l2))
                    #()))
              (error "mjr_vec_ewuo: Only vectors are supported!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_* (&rest vecs)
  "Multiply given vectors and/or scalars.
vec*vec = multiply element-wise, s*vec & vec*s = multiply s by each element, s*s = multiply numbers."
  (if (< 2 (length vecs))
      (reduce #'mjr_vec_* vecs)
      (let ((arg1 (first vecs))
            (arg2 (second vecs)))
        (if arg2
            (if (numberp arg1)
                (if (numberp arg2)
                    (* arg1 arg2)                               ; Both are numbers
                    (mjr_vec_ewuo arg2 (lambda (x) (* x arg1))))    ; First is number, second is vector
                (if (numberp arg2)
                    (mjr_vec_ewuo arg1 (lambda (x) (* x arg2)))     ; Second is number, first is vector
                    (mjr_vec_ewbo arg1 arg2 #'*)))                  ; Both are vectors (so we do element-wise)
            arg1))))                                            ; One argument, return it like LISP built-in

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_/ (&rest vecs)
  "Divide given vectors and/or scalars.  If only one argument is provided, then invert it.
vec/vec = divide element-wise, vec/s = divide each element by s, s/vec = divide s by each element, s/s means = divide numbers,
/vec = invert the vector element-wise, /s = invert the number."
  (if (< 2 (length vecs))
      (reduce #'mjr_vec_/ vecs)
      (let ((arg1 (first vecs))
            (arg2 (second vecs)))
        (if arg2
            (if (numberp arg1)
                (if (numberp arg2)
                    (/ arg1 arg2)                               ; Both are numbers
                    (mjr_vec_ewuo arg2 (lambda (x) (/ arg1 x))))    ; First is number, second is vector
                (if (numberp arg2)
                    (mjr_vec_ewuo arg1 (lambda (x) (/ x arg2)))     ; Second is number, first is vector
                    (mjr_vec_ewbo arg1 arg2 #'/)))                  ; Both are vectors (so we do element-wise)
            (if (numberp arg1)
                (/ arg1)                                        ; One arg, a number
                (mjr_vec_ewuo arg1 '/))))))                         ; One arg, a vector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_- (&rest vecs)
  "Subtract given vectors and/or scalars.  If only one argument is provided, then negate it.
vec-vec = subtract element-wise, s-vec = subtract each element from s, vec-s = subtract s from each element, s-s = subtract numbers,
-vec = negate the vector (element-wise), -s = negate the number."
  (if (< 2 (length vecs))
      (reduce #'mjr_vec_- vecs)
      (let ((arg1 (first vecs))
            (arg2 (second vecs)))
        (if arg2
            (if (numberp arg1)
                (if (numberp arg2)
                    (- arg1 arg2)                               ; Both are numbers
                    (mjr_vec_ewuo arg2 (lambda (x) (- arg1 x))))    ; First is number, second is vector
                (if (numberp arg2)
                    (mjr_vec_ewuo arg1 (lambda (x) (- x arg2)))     ; Second is number, first is vector

                    (mjr_vec_ewbo arg1 arg2 #'-)))                  ; Both are vectors (so we do element-wise)
            (if (numberp arg1)
                (- arg1)                                        ; One arg, a number
                (mjr_vec_ewuo arg1 '-))))))                         ; One arg, a vector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_+ (&rest vecs)
  "Add given vectors and/or scalars.
vec+vec = add element-wise, s+vec & vec+s = add s to each element, s+s = add numbers."
  (if (< 2 (length vecs))
      (reduce #'mjr_vec_+ vecs)
      (let ((arg1 (first vecs))
            (arg2 (second vecs)))
        (if arg2
            (if (numberp arg1)
                (if (numberp arg2)
                    (+ arg1 arg2)                               ; Both are numbers
                    (mjr_vec_ewuo arg2 (lambda (x) (+ x arg1))))    ; First is number, second is vector
                (if (numberp arg2)
                    (mjr_vec_ewuo arg1 (lambda (x) (+ x arg2)))     ; Second is number, first is vector
                    (mjr_vec_ewbo arg1 arg2 #'+)))                  ; Both are vectors (so we do element-wise)
            arg1))))                                            ; One argument, return it like LISP built-in

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_zap-eps (vec &optional (eps 0.0001))
  "Zap small values from a vec.  If EPS is NIL, then *MJR_EPS_EPS* will be used.
This function uses MJR_EPS_=0 to identify zero elements."
  (mjr_vec_ewuo vec (lambda (x) (if (mjr_eps_=0 x eps) 0 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_float (vec)
  "Convert vec elements from rational/complex rational float/complex float"
  (mjr_vec_ewuo vec (lambda (x) (if (complexp x)
                                   (complex (float (realpart x)) (float (imagpart x)))
                                 (if (numberp x) (float x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_rationalize (vec)
  "Convert vec elements from float/float complex into rational/complex rational"
  (mjr_vec_ewuo vec (lambda (x) (if (complexp x)
                                   (complex (rationalize (realpart x)) (rationalize (imagpart x)))
                                 (if (numberp x) (rationalize x) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_norm-infinity (vec)
  "Compute the infinity-norm of the given vector"
  (reduce 'max (mjr_vec_ewuo vec (lambda (x) (abs x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_norm-one (vec)
  "Compute the one-norm of the given vector."
  (let* ((nvec (mjr_vec_ewuo vec (lambda (x) (abs x))))
         (nvl  (length nvec)))
    (if (> nvl 0)
        (reduce '+ nvec)
        (error "mjr_vec_norm-one: Only non-empty vectors supported"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_norm-two (vec &optional eps0)
  "Compute the two-norm (standard Euclidean distance) of the given vector.  If given a number, return the ABS.

The EPS0 argument is used with MJR_CHK_!=0 to avoid dividing by zero.

Empty vectors are length zero.

This function is careful to avoid floating point overflow when possible, but pays a price in performance.  Underflow is still a problem. Also note that it
still gets wrong answers for some combinations of inputs ; however, it is better than the naive implementation."
  (if (numberp vec)
      (abs vec)
      (let ((len (length vec)))
        (if (>= 1 len)
            (if (= 1 len)
                (abs (aref vec 0))                                                        ;; Vectors of one element are easy
                (error "mjr_vec_norm-two: Vector must be of dimension greater than 0")))
            (multiple-value-bind (maxi)                                                   ;; Find index of element with largest magnitude
                (mjr_vvec_map-maxi (list :end (1- len)
                                         :map-fun (lambda (i) (abs (aref vec i)))))
              (let ((maxv (aref vec maxi)))                                               ;; value of largest element
                (if (mjr_chk_!=0 maxv eps0)                                               ;; avoid /0
                    (* (abs maxv)                                                         ;; Compute length
                       (mjr_numu_sqrt                                                     ;; Sum of the squares of the ratios
                        (1+ (loop for x across vec
                                  for i from 0
                                  when (not (= maxi i))
                                  sum (expt (abs (/ x maxv)) 2)))))
                    (mjr_numu_sqrt (loop for xi across vec
                                         sum (* xi xi)))))))))                            ;; It was close to the zero vector, so use easy formula

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_norm-two-squared (vec)
  "Compute the two-norm squared (the standard Euclidean distance squared) of the given vector."
  (let* ((nvec (mjr_vec_ewuo vec (lambda (x) (expt (abs x) 2))))
         (nvl  (length nvec)))
    (if (> nvl 0)
        (reduce '+ nvec)
        (error "mjr_vec_norm-two-squared: Only non-empty vectors supported"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_normalize (vec &optional norm)
  "Normalize the vector"
      (mjr_vec_/ vec   (if norm
                           (funcall norm vec)
                           (mjr_vec_norm-two vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_make-e (n &key (len 3) (zero-value 0) (one-value 1))
  "Make the Nth element of the LEN-dimensional standard basis."
  (mjr_vec_make-from-func (lambda (i) (if (= i n) one-value zero-value)) :len len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_every-idx (pred vec)
  "Non-nil if PRED is non-nil for every index (i).  Note difference from the built-in EVERY function"
  (let ((len  (length vec)))
    (dotimes (i len (not nil))
      (if (not (funcall pred i)) (return-from mjr_vec_every-idx nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_e? (vec n &optional eps)
  "Non-nil if the vec is the Nth element of the standard basis.
This function uses MJR_CMP_= with EPS against vector elements."
  (if (>= (length vec) n)
      (mjr_vec_every-idx (lambda (i) (if (= i n) (mjr_cmp_= 1 (aref vec i) eps) (mjr_cmp_= 0 (aref vec i) eps))) vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_print (vector &optional fmt-str)
  "Print a vector.  If missing, the FMT-STR will be auto-sized (right justified ~S specifier).

If a VECTOR is not a vector, then it will be printed out with ~S~& regardless of what was given for FMT-STR.  Returns VECTOR so that this function may be
inserted inside nested function calls."
  (if (vectorp vector)
      (let ((len (length vector)))
        (if (not fmt-str)
            (setq fmt-str (concatenate 'string "~" (format nil "~d" (+ 3 (mjr_util_max-print-width vector "~s"))) "<~S~>")))
        (format 't "~%")
        (dotimes (i len)
          (format 't fmt-str (aref vector i)))
        (format 't "~%"))
    (format 't "~S~&" vector))
  vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_code (vec &key (lang :lang-matlab))
  "Print out the vector using the syntax of the selected programming language or computational environment."
  (if (vectorp vec)
      (let* ((len  (length vec))
             (len0 (1- len))
             (str   "")
             (bams  (case lang
                      (:lang-povray       (list "<"       "" "" "," ">"   ","))
                      ((:lang-maxima    
                        :lang-matlab     
                        :lang-octave     
                        :lang-idl
                        :lang-gap
                        :lang-gp
                        :lang-pari
                        :lang-pari/gp)    (list "["       "" "" "," "];"  ","))
                      (:lang-hp48         (list "["       "" "" "," "]"   ","))
                      (:lang-mathematica  (list "{"       "" "" "," "};"  ","))
                      (:lang-python       (list "("       "" "" "," ");"  ","))
                      (:lang-maple        (list "vector[" "" "" "," "]);" ","))
                      (:lang-ruby         (list "Vector[" "" "" "," "]"   ","))
                      (:lang-r            (list "c("      "" "" "," ")"   ","))
                      (:lang-lisp         (list "#1a("    "" "" " " ")"   " "))
                      ('t                 (list ""        "" "" " " " "   " ")))))
        (dotimes (i len str)
          (setq str (concatenate 'string str (format nil "~a~a~a"
                                                     (cond ((= i 0)                         (nth 0 bams))
                                                           ((= i len0)                      (nth 1 bams))
                                                           ('t                              (nth 2 bams)))
                                                     (mjr_numu_code (aref vec i) :lang lang)
                                                     (cond ((= i 0)                         (nth 3 bams))
                                                           ((= i len0)                      (nth 4 bams))
                                                           ('t                              (nth 5 bams))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_proj (u v)
  "Compute proj_u(v) == the projection of v along u."
  (mjr_vec_/ (mjr_vec_* (mjr_vec_dot u v) u) (mjr_vec_norm-two-squared u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_orthogonalize (vec-list &key (unitize nil) (method :cgs))
  "Orthogonalize the set of vectors using the specified algorithm.
Use :method to specify the algorithm:
  :cgs -- Classical Gram-Schmidt (Don't use this for floating point vectors as it is not stable....)
Set :unitize to non-nil to unitize the basis (i.e. make them orthonormal)"
  (cond ((eq method :cgs)   (loop with new-vec-list = (list (if unitize (mjr_vec_normalize (first vec-list)) (first vec-list)))
                                  for cv in (cdr vec-list)
                                  do (nconc new-vec-list (list (loop with nv = cv
                                                                     for j in new-vec-list
                                                                     do (setq nv (mjr_vec_- nv (mjr_vec_proj j cv)))
                                                                     finally (return (if unitize (mjr_vec_normalize nv) nv)))))
                                  finally (return new-vec-list)))
        ('t                 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_vec_< (a b &key (order :lex))
  "Vector orderings

All of the orderings are suitable for use as monomial orderings for commutative algebra computations -- the names used are typical"
  (if (not (= (length a) (length b))) (error "mjr_vec_<: Vectors must be of the same length"))
  (cond ((eq order :lex)     (loop for ai across a                  ;; lex: Lexicographic order
                                   for bi across b
                                   when (not (= ai bi))
                                   do (return (< ai bi))
                                   finally (return nil)))
        ((eq order :revlex)  (loop for ai across a                  ;; revlex: Reverse lexicographic order
                                   for bi across b
                                   when (not (= ai bi))
                                   do (return (> ai bi))
                                   finally (return nil)))
        ((eq order :grlex)   (let ((sa (reduce #'+ a))              ;; grlex: maximal order dominates, lex breaks ties
                                   (sb (reduce #'+ b)))
                               (cond ((< sa sb) 't)
                                     ((= sa sb) (mjr_vec_< a b :order :lex))
                                     ('t        nil))))
        ((eq order :grevlex) (let ((sa (reduce #'+ a))              ;; grevlex: minimal order dominates, revlex breaks ties
                                   (sb (reduce #'+ b)))
                               (cond ((> sa sb) 't)
                                     ((= sa sb) (mjr_vec_< a b :order :revlex))
                                     ('t        nil))))
        ('t                  (error "mjr_vec_<: Unknown ordering!"))))

