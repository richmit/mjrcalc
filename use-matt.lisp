;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-matt.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Generate and compute with various test matrices.@EOL
;; @std       Common Lisp
;; @see       tst-matt.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2004,2010,2012,2013,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_MATT
  (:USE :COMMON-LISP
        :MJR_MAT
        :MJR_VEC
        :MJR_CMP
        :MJR_ARR
        :MJR_COMBE
        :MJR_PRNG
        :MJR_NUMU)
  (:DOCUMENTATION "Brief: Generate and compute with various test matrices.;")
  (:EXPORT #:mjr_matt_help
           #:mjr_matt_make-test
           #:mjr_matt_det-test
           #:mjr_matt_make-random
           ))

(in-package :MJR_MATT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_matt_help ()
  "Help for MJR_MATT: Test MATrix

This package focuses on various 'test' matrices -- i.e. matrices that have an exploitable structure allowing specialized algorithms.  This code is not in a
-test package because these matrices frequently come up in applications, and because they are very handy during the software development process."
  (documentation 'mjr_matt_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_matt_det-test (the-sm &key x y a b n m f)
  "Compute the determinant of special matrices.  See mjr_matt_make-test for details regarding arguments."
  (let* ((m     (or m n))
         (n     (or n m))
         (x     (or x (and m (mjr_vec_make-seq :start 1 :end n)) y))
         (y?    (not (null y)))
         (y     (or y x))
         (len-x (and x (length x))))
    (or y?)
    (case the-sm
      (:mp-hilbert        (/ (mjr_numu_prod :start 0 :end (1- n) :seq-fun
                                            (lambda (k) (* (1+ (* 2 k))
                                                           (expt (mjr_combe_comb (* 2 k) k) 2))))))
      (:mp-vandermonde    (mjr_numu_prod :start 0 :end (- len-x 2) :seq-fun
                                         (lambda (i) (mjr_numu_prod :start (1+ i) :end (- len-x 1) :seq-fun
                                                                    (lambda (j) (- (aref x j) (aref x i)))))))
      (:mp-hankel         (* (if (oddp (floor (/ n 2))) -1 1) (expt n n)))
      (:mp-pei            (* (+ n b) (expt b (1- n))))
      (:mp-cauchy         (/ (mjr_numu_prod :start 0 :end (- len-x 2) :seq-fun
                                            (lambda (i) (mjr_numu_prod :start (+ i 1) :end (1- len-x) :seq-fun
                                                                       (lambda (j) (* (- (aref x j) (aref x i))
                                                                                      (- (aref y j) (aref y i)))))))
                             (mjr_numu_prod :start 0 :end (1- len-x):seq-fun
                                            (lambda (i) (mjr_numu_prod :start 0 :end (1- len-x) :seq-fun
                                                                       (lambda (j) (+ (aref x i) (aref y j))))))))
      (:mp-pascal         1)
      (:mp-clement        (if (oddp n)
                              0
                              (if (= n 2)
                                  -1
                                  (* (if (oddp (/ N 2)) -1 1) (mjr_numu_prod :start 0 :len (/ n 2) :step 2 :seq-fun
                                                                             (lambda (i) (* (+ 1 i) (- n i 1))))))))
      (:mp-hilbert-i      (/ (apply #'mjr_matt_det-test :mp-hilbert     :x x :y y :a a :b b :n n :m m :f f)))
      (:mp-vandermonde-i  (/ (apply #'mjr_matt_det-test :mp-vandermonde :x x :y y :a a :b b :n n :m m :f f)))
      (:mp-hankel-i       (/ (apply #'mjr_matt_det-test :mp-hankel      :x x :y y :a a :b b :n n :m m :f f)))
      (:mp-pei-i          (/ (apply #'mjr_matt_det-test :mp-pei         :x x :y y :a a :b b :n n :m m :f f)))
      (:mp-cauchy-i       (/ (apply #'mjr_matt_det-test :mp-cauchy      :x x :y y :a a :b b :n n :m m :f f)))
      (:mp-pascal-i       1)
      (otherwise          (error "mjr_matt_det-test: Unknown matrix type requested!!")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_matt_make-random (the-sm &key a b k n m)
  "Compute random matrix with special form

  Notation
     a,b   -- Scalars
     k,n,m -- Integers

  Symbol            Arguments   Description
  :mp-real           m,n,a,b     Random mxn matrix with elements uniformly distributed on in [a,b)
                                    Only one of a and b are provided => the range will be [0,a), [0,b), (a,0], or (b,0]
                                    If both a and b are missing => the range will be [0.0, 1.0)
                                    If only one of m and n are provided => T will be square
  :mp-diagonal       m,n,a,b     Random diagonal matrix (off diagonal elements are zero)
  :mp-l-triangular   m,n,a,b     Random lower triangular matrix (above diagonal elements are zero)
  :mp-u-triangular   m,n,a,b     Random upper triangular matrix (below diagonal elements are zero)
  :mp-m-diagonal     k,m,n,a,b   Random multi-diagonal matrix. k controls number of diagonals
                                    Ex: k=1=>diagonal, k=2=>tridiagonal, etc...  k=2 by default
  :mp-complex        m,n,a,b     Random mxn complex matrix.  a & b control the ranges for both realpart and imagpart
  :mp-rational       m,n,a,b     Random mxn rational matrix. a & b control the ranges for both top and bottom. Default a=b=10
  :mp-symmetric      m,n,a,b     Random square, real, symetric matrix with max(m,n) rows
  :mp-hermitian      m,n,a,b     Random square, complex, hermitian matrix with max(m,n) rows"
  (let* ((m     (or m n))
         (n     (or n m)))
    (case the-sm
      (:mp-real          (let ((rll  (min (or (and a b) 0) (or a b 1.0)))
                               (rul  (max (or (and a b) 0) (or a b 1.0))))
                           (mjr_mat_make-from-func (lambda (i j) (progn i j (mjr_prng_tbd-uniform-co rll rul))) :rows m :cols n)))
      (:mp-diagonal      (let ((rll  (min (or (and a b) 0) (or a b 1.0)))
                               (rul  (max (or (and a b) 0) (or a b 1.0))))
                           (mjr_mat_make-from-func (lambda (i j) (if (= i j) (mjr_prng_tbd-uniform-co rll rul) 0)) :rows m :cols n)))
      (:mp-l-triangular  (let ((rll  (min (or (and a b) 0) (or a b 1.0)))
                               (rul  (max (or (and a b) 0) (or a b 1.0))))
                           (mjr_mat_make-from-func (lambda (i j) (if (>= i j) (mjr_prng_tbd-uniform-co rll rul) 0)) :rows m :cols n)))
      (:mp-u-triangular  (let ((rll  (min (or (and a b) 0) (or a b 1.0)))
                               (rul  (max (or (and a b) 0) (or a b 1.0))))
                           (mjr_mat_make-from-func (lambda (i j) (if (<= i j) (mjr_prng_tbd-uniform-co rll rul) 0)) :rows m :cols n)))
      (:mp-m-diagonal    (let ((rll  (min (or (and a b) 0) (or a b 1.0)))
                               (rul  (max (or (and a b) 0) (or a b 1.0)))
                               (k    (if k (1- k) 1)))
                           (mjr_mat_make-from-func (lambda (i j) (if (< k (abs (- i j))) 0 (mjr_prng_tbd-uniform-co rll rul))) :rows m :cols n)))
      (:mp-complex       (mjr_arr_binary-map2 (mjr_matt_make-random :mp-real  :m m :n n :a a :b b)
                                              (mjr_matt_make-random :mp-real  :m m :n n :a a :b b)
                                              #'complex))
      (:mp-rational      (let ((rll  (min (or (and a b) 0) (or a b 10)))
                               (rul  (max (or (and a b) 0) (or a b 10))))
                           (mjr_mat_make-from-func (lambda (i j) (progn i j (/ (mjr_prng_tbd-uniform-co rll rul)
                                                                               (loop for r = (mjr_prng_tbd-uniform-co rll rul)
                                                                                     when (not (zerop r))
                                                                                     do (return r))))) :rows m :cols n)))
      (:mp-symmetric     (let* ((rll    (min (or (and a b) 0) (or a b 1.0)))
                                (rul    (max (or (and a b) 0) (or a b 1.0)))
                                (siz    (max m n))
                                (newmat (make-array (list siz siz))))
                           (loop for r from 0 upto (1- siz)
                                 finally (return newmat)
                                 do (loop for c from r upto (1- siz)
                                          do (let ((rnum (mjr_prng_tbd-uniform-co rll rul)))
                                               (setf (aref newmat r c) rnum
                                                     (aref newmat c r) rnum))))))
      (:mp-hermitian     (let* ((rll    (min (or (and a b) 0) (or a b 1.0)))
                                (rul    (max (or (and a b) 0) (or a b 1.0)))
                                (siz    (max m n))
                                (newmat (make-array (list siz siz))))
                           (loop for r from 0 upto (1- siz)
                                 finally (return newmat)
                                 do (loop for c from r upto (1- siz)
                                          do (let ((rnum (complex (mjr_prng_tbd-uniform-co rll rul) (mjr_prng_tbd-uniform-co rll rul))))
                                               (if (= c r)
                                                   (setf (aref newmat c r) (realpart rnum))
                                                   (setf (aref newmat r c) rnum
                                                         (aref newmat c r) (conjugate rnum))))))))
      (otherwise         (error "mjr_matt_make-random: Unknown matrix type (~s) requested!!" the-sm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_matt_make-test (the-sm &key x y a b k n m f)
  "Compute several special matrices.

  Notation
     T[i,j] = row i, col j (zero based)
     X,Y   -- Vectors of length n
     a,b   -- Scalars
     k,n,m -- Integers
     f,g   -- Functions

  Symbol            Arguments   Description
  :mp-sampling      X,Y         T[i,j]=X[i]/(X[i]-X[j]) for i!=j and T[j,j]=sum(X[i, j], i!=j)
                                    T has rational entries.  The eigenvalues are 0, 1, ..., n (n=length X), and they
                                    are all ill-conditioned.
  :mp-cauchy        X,Y         T[i,j]=1/(X[i]+Yj])
  :mp-cauchy        X           T[i,j]=1/(X[i]+X[j])
  :mp-cauchy        n           T[i,j]=1/((i+1)+(j+1))
                                    T is totally positive if 0<X[i]<X[j] & 0<Y[i]<Y[j] for i<j
                                    Totally Positive: All submatrices and minors have positive determinant.
                                    T will be singular if any X[i]=X[j], Y[i]=Y[j] for i!=j or if X[i]+Y[j]=0 for any i,j.
                                    References:
                                      Robert Gregory, David Karney (1969); A Collection of Matrices for Testing Computational Algorithms; Ex 3.26
                                      Nicholas Higham; Accuracy and Stability of Numerical Algorithms
                                      Donald Knuth; The Art of Computer Programming, Volume 1, Fundamental Algorithms, Second Edition; Page 36
                                      Olga Taussky, Marvin Marcus. Eigenvalues of finite matrices. In Survey of Numerical Analysis
                                      Evgeny Tyrtyshnikov; Cauchy-Toeplitz matrices and some applications; Linear Algebra and Applications. Volume 149
  :mp-cauchy-i      X,Y         Inverse of :mp-cauchy
  :mp-cauchy-i      X           Inverse of :mp-cauchy
  :mp-cauchy-i      n           Inverse of :mp-cauchy
  :mp-parter        n           T[i,j]=1/(i-j+1/2)
                                    T is a special case of the Cauchy matrix with X=(1:n)+1/2 & Y=-(1:n).  The diagonal and superdiagonal are constant:
                                    T[i,i]=2 and T[i,i-1]=-2.  T is Toeplitz, persymmetric.  Most of the singular values are very close to Pi.
                                    References:
                                      Evgeny Tyrtyshnikov; Cauchy-Toeplitz matrices and some applications; Linear Algebra and Applications. Volume 149
  :mp-parter-i      n           Inverse of :mp-parter
                                    NOTE: Implemented via :mp-cauchy-i
  :mp-hilbert       n           T[i,j]=1/(i-1+j)
                                    Condition number grows like exp(3.5*n) T is symmetric positive definite, totally positive, and Hankel.  The Hilbert matrix
                                    is a Cauchy matrix with X=(1:n)-1 & Y=1:n
                                    References:
                                      M.D. Choi (1983); Tricks or treats with the Hilbert matrix; Amer. Math. Monthly 90; Page 301-312.
                                      Nicholas Higham; Accuracy and Stability of Numerical Algorithms
                                      Morris Newman, John Todd; The evaluation of matrix inversion programs; Journal of SIAM. Vol 6
                                      Donald Knuth; The Art of Computer Programming, Volume 1, Fundamental Algorithms, Second Edition; Page 37
  :mp-hilbert-i     n           Inverse of :mp-hilbert matrix
                                    T has integer entries.
  :mp-ris           n           T[i,j]=(1/2)/(n-i-j-1/2)
                                    T is a Cauchy matrix with X=(3+2*5)-2*(1:n) & Y=-2*(1:n).  T is Hankel, symmetric, normal, diagonalizable. The eigenvalues
                                    of T cluster around PI/2 and -PI/2.
                                    Reference:
                                      John Nash; Compact Numerical Methods for Computers: Linear Algebra and Function Minimization
  :mp-ris-i         n           Inverse of :mp-ris
                                    NOTE: Implemented via :mp-cauchy-i
  :mp-westlake      n           T[1,j]=(1+min(i,j))/(1+max(i,j))
                                    T is symmetric, normal, diagonalizable, positive definite, and totally non-negative.  The condition number of T lies
                                    between n and 4*n*n. Frequently called Lehmer; however, the one called Lehmer in Matlab is NOT this matrix.
                                    References:
                                      Morris Newman, John Todd; The evaluation of matrix inversion programs; Journal of SIAM. Vol 6
                                      John Todd; Basic Numerical Mathematics, Volume 2: Numerical Algebra
  :mp-westlake-i    n           Inverse of :mp-westlake.
                                    T is tridiagonal.
  :mp-minij         n           T[1,j]=min((i+1),(j+1))
                                    T has integer entries, is positive definite, symmetric, normal, diagonalizable.
                                    References:
                                      Robert Gregory, David Karney (1969); A Collection of Matrices for Testing Computational Algorithms; Ex 3.12 & 4.14
                                      John Todd; Basic Numerical Mathematics, Volume 2: Numerical Algebra
                                      Joan Westlake; A Handbook of Numerical Matrix Inversion and Solution of Linear Equations.
  :mp-minij-i       n           T[i,j]=-1 when j=i-1 or j=i+1, T[i,j]=2 when j=i and j!=n-1, T[i,j]=1 when j=i and j=n-1, T[i,j]=0 otherwise
                                    Inverse of :mp-minij.
                                    T has integer entries, is symmetric, normal, diagonalizable, and tridiagonal.
  :mp-pei           n,b         T[j,j]=b+1 and T[i,j]=1 otherwise
                                    T is symmetric, normal, diagonalizable, persymmetric, positive definite when 0<b, Toeplitz, circulant, singular only when
                                    b=0 or b=-n, and becomes more ill-conditioned as b->0.  T has two eigenvalues: b+n (of multiplicity 1) and b (of
                                    multiplicity n-1).  The determinant of T is (n+b)*b^(n-1).
                                    References:
                                      Morris Newman, John Todd; The evaluation of matrix inversion programs; Journal of SIAM. Vol 6
                                      ML Pei; A test matrix for inversion procedures; Communications of the ACM. Volume 5
                                      Joan Westlake; A Handbook of Numerical Matrix Inversion and Solution of Linear Equations.
  :mp-pei-i         n,b         Inverse of :mp-pei
  :mp-vandermonde   X           T[i,j]=X[i]^j
  :mp-vandermonde   n           T[i,j]=(i+1)^j  (i.e. X=1:n)
                                    T is singular if, and only if, X[i]=X[j] for some i!=j.  T is generally ill-conditioned.
                                    References:
                                      Robert Gregory, David Karney (1969); A Collection of Matrices for Testing Computational Algorithms
                                      Nicholas Higham; Stability analysis of algorithms for solving confluent Vandermonde-like systems
  :mp-vandermonde-i X           Inverse of :mp-vandermonde
  :mp-vandermonde-i n           Inverse of :mp-vandermonde
  :mp-hankel        X,Y         Hankel with first row equal to X and last column equal to Y
  :mp-hankel        X           Hankel with first row equal to X and zeros on sub-skew diagonals
  :mp-hankel        n           Hankel with first row equal to 1:n and zeros on sub-skew diagonals (As if X=1:n)
  :mp-hankel        f,n         T[i,j]=f(i+j)
                                    NOTE: Hankel <=> constant skew-diagonals <=> T[i,j]=T[i-1,j+1] <=> T[i,j]=f(i+j) for f:Z->C T is symmetric, normal,
                                    diagonalizable, constant along anti-diagonals.
  :mp-toeplitz      X,Y         T[i,j]=X[i-j] when i>=j, and T[i,j]=Y[j-i] otherwise
                                    NOTE: Toeplitz <=> constant along diagonals.  Toeplitz matrices are completely defined by the first column and first row.
                                    The first column of T is X, and the first row of T is Y.  Note that X[0]=Y[0], but if they do not, then the value of X[0]
                                    'wins'. T is persymmetric.
  :mp-toeplitz      X           T[i,j]=X[i-j] when i>=j, and T[i,j]=X[j-i] otherwise -- i.e. Y is set to X.
  :mp-toeplitz      n           X & Y are set to 1:n
  :mp-gauss-1       k,m,n       Gauss transform matrix KxK that exchanges row N and M
  :mp-gauss-2       k,n,a       Gauss transform matrix KxK that multiplies row N by A
  :mp-gauss-3       X,m,n       Gauss transform matrix (row operation of type 3) that will zero out x[n] with x[m].
  :mp-gauss-3       k,m,n,a     Gauss transform matrix KxK that will: x[n]<-x[m]*a+x[n]
  :mp-givens-2      X,m,n       Givens transform matrix that will zero out x[n] with x[m].
  :mp-givens-2      k,m,n,a     Givens transform matrix (KxK) with cos(a) and sin(a)
  :mp-givens-2      k,m,n,a,b   Givens transform matrix (KxK) with cos=a and sin=b
  :mp-householder   X,Y         Householder transform matrix that will transform X into Y -- |X|==|Y|.
  :mp-householder   X,Y,n       Householder transform matrix that will zero out x except for x[n] and any x[i] with y[i] non-NILL.
  :mp-householder   X,n         Householder transform matrix that will zero out x except for x[n].
  :mp-householder-2 X,m,n       Householder transform matrix that will zero out x[n] with x[m].
  :mp-householder-2 k,m,n,a     Householder transform matrix (KxK) with cos(a) and sin(a)
  :mp-householder-2 k,m,n,a,b   Householder transform matrix (KxK) with cos=a and sin=b
  :mp-hankel-cat    n           (mjr_mat_make-from-func (lambda (i j) (mjr_combe_catalan (+ i j -2))) :start 1 :end N);  DET=1; TODO: implement!!
  :mp-hankel-cat-1  n           (mjr_mat_make-from-func (lambda (i j) (mjr_combe_catalan (+ i j -2))) :start 1 :end N);  DET=1; TODO: implement!!
  :mp-clement       n           T[i,i+1]=i+1, T[i,i-1] = n-i, other elements of T are 0. -- i.e. 1..m-1 on superdiagonal, the reverse on subdiagonal.
                                Determinant is zero.  Eigenvalus are: -N, -N+2, -N+4, ..., N-2, N where N=n-1."
  (let* ((m     (or m n))
         (n     (or n m))
         (x?    (not (null x)))
         (x     (or x (and m (mjr_vec_make-seq :start 1 :end n)) y))
         (y?    (not (null y)))
         (y     (or y x))
         (len-x (and x (length x))))
    (case the-sm
      (:mp-clement         (mjr_mat_make-from-func (lambda (i j) (cond ((= j (1+ i)) (+ 1 i))
                                                                       ((= i (1+ j)) (- m i))
                                                                       ('t   0)))
                                                  :rows m :cols n))
      (:mp-sampling        (let* ((newmat (mjr_mat_make-from-func (lambda (i j) (if (= i j)
                                                                                    0
                                                                                    (/ (aref x i) (- (aref x i) (aref x j)))))
                                                                  :rows len-x :cols len-x)))
                             (loop for i from 0
                                   for col in (mjr_arr_get-cols newmat)
                                   do (setf (aref newmat i i) (loop for y across col sum y)))
                             newmat))
      (:mp-cauchy          (mjr_mat_make-from-func (lambda (i j) (let ((bot (+ (aref x i) (aref y j))))
                                                                   (if (mjr_cmp_=0 bot)
                                                                       (return-from mjr_matt_make-test nil)
                                                                       (/ bot))))
                                                   :rows len-x))
      (:mp-cauchy-i        (mjr_mat_make-from-func
                            (lambda (i j)
                              (/ (mjr_numu_prod :start 0 :end (1- len-x) :seq-fun (lambda (k) (* (+ (aref x j) (aref y k)) (+ (aref x k) (aref y i)))))
                                 (* (+ (aref x j) (aref y i))
                                    (mjr_numu_prod :start 0 :end (1- len-x) :seq-fun (lambda (k) (if (= k j) 1 (- (aref x j) (aref x k)))))
                                    (mjr_numu_prod :start 0 :end (1- len-x) :seq-fun (lambda (k) (if (= k i) 1 (- (aref y i) (aref y k))))))))
                            :rows len-x))
      (:mp-parter          (mjr_matt_make-test :mp-cauchy   :x (mjr_vec_+ 1/2 (mjr_vec_make-seq :start 1 :end n)) :y (mjr_vec_- (mjr_vec_make-seq :start 1 :end n))))
      (:mp-parter-i        (mjr_matt_make-test :mp-cauchy-i :x (mjr_vec_+ 1/2 (mjr_vec_make-seq :start 1 :end n)) :y (mjr_vec_- (mjr_vec_make-seq :start 1 :end n))))
      (:mp-vandermonde     (mjr_mat_make-from-func (lambda (i j) (expt (aref x i) j)) :rows len-x))
      (:mp-vandermonde-i   (let* ((nml (mjr_mat_make-from-func (lambda (i j)
                                                                 (cond ((< i j)
                                                                        0)
                                                                       ((and (zerop i) (zerop j))
                                                                        1)
                                                                       ('t
                                                                        (mjr_numu_prod :start 0 :end i :seq-fun
                                                                                       (lambda (k) (if (= k j) 1 (/ (- (aref x j) (aref x k)))))))))
                                                               :rows len-x))
                                  (nmu (mjr_mat_make-zero len-x len-x)))
                             (if (and nml nmu)
                                 (progn
                                   (setf (aref nmu 0 0) 1)
                                   (loop for j from 1 upto (1- len-x)
                                         do (setf (aref nmu 0 j) (mjr_numu_prod :start 0 :end (1- j) :seq-fun (lambda (k) (* (if (oddp j) -1 1) (aref x k))))))
                                   (loop for i from 1 upto (1- len-x)
                                         do (loop for j from 1 upto (1- len-x)
                                                  do (setf (aref nmu i j) (- (aref nmu (1- i) (1- j)) (* (aref nmu i (1- j)) (aref x (1- j)))))))
                                   (mjr_mat_* nmu nml)))))
      (:mp-minij           (mjr_mat_make-from-func (lambda (i j) (min (1+ i) (1+ j))) :rows n))
      (:mp-minij-i         (mjr_mat_make-from-func (lambda (i j) (cond ((or (= j (1- i)) (= j (1+ i)))   -1)
                                                                       ((and (= j i) (not (= j (1- n))))  2)
                                                                       ((and (= j i) (= j (1- n)))        1)
                                                                       ('t                                0)))
                                                   :rows n))
      (:mp-westlake        (mjr_mat_make-from-func (lambda (i j) (/ (1+ (min i j)) (1+ (max i j)))) :rows n))
      (:mp-westlake-i      (let ((newmat (mjr_mat_make-zero n n)))
                             (loop for i from 0 upto (- n 2)
                                   do (setf (aref newmat i i) (/ (* 4 (expt (1+ i) 3)) (1- (* 4 (1+ i) (1+ i))))))
                             (setf (aref newmat (1- n) (1- n)) (/ (* n n) (1- (* 2 n))))
                             (loop for i from 0 upto (- n 2)
                                   for v = (/ (- (* (1+ i) (+ i 2))) (1+ (* 2 (1+ i))))
                                   do (setf (aref newmat i (1+ i)) v)
                                   do (setf (aref newmat (1+ i) i) v))
                             newmat))
      (:mp-pei             (mjr_mat_make-from-func (lambda (i j) (if (= i j) (1+ b) 1)) :rows n))
      (:mp-pei-i           (mjr_mat_make-from-func (lambda (i j) (if (= i j)
                                                                     (/ (1- (+ b n)) (- (* (1+ b) (1- (+ b n))) (1- n)))
                                                                     (/ (- (1- (+ n 0)) (* (1+ b) (1- (+ b (+ n 0))))))))
                                                   :rows n))
      (:mp-ris             (mjr_mat_make-from-func (lambda (i j) (/ 1/2 (- n i j 1/2))) :rows n))
      (:mp-ris-i           (mjr_matt_make-test :mp-cauchy-i
                                               :x (mjr_vec_+ (+ 3 (* 2 n)) (mjr_vec_* -2 (mjr_vec_make-seq :start 1 :end n)))
                                               :y (mjr_vec_* -2 (mjr_vec_make-seq :start 1 :end n))))
      (:mp-hilbert         (mjr_mat_make-from-func (lambda (i j) (/ (+ i j 1))) :rows n))
      (:mp-hilbert-i       (mjr_mat_make-from-func (lambda (i j) (* (if (oddp (+ i j 2)) -1 1)
                                                                    (+ i j 1)
                                                                    (mjr_combe_comb (+ n i) (- n j 1))
                                                                    (mjr_combe_comb (+ n j) (- n i 1))
                                                                    (expt (mjr_combe_comb (+ i j) i) 2)))
                                                   :rows n))
      (:mp-hankel          (if f
                               (mjr_mat_make-from-func (lambda (i j) (funcall f (+ i j))) :rows (or n (and x (length x))))
                               (mjr_mat_make-from-func (lambda (i j) (if (< (+ i j) len-x)
                                                                         (aref x (+ i j))
                                                                         (if y? (aref y (- (+ i j 1) len-x)) 0))) :rows len-x)))
      (:mp-toeplitz        (mjr_mat_make-from-func (lambda (i j) (if (>= i j) (aref x (- i j)) (aref y (- j i)))) :rows len-x))
      (:mp-pascal          (mjr_mat_make-from-func (lambda (i j) (mjr_combe_comb (+ i j) i)) :rows n))
      (:mp-gauss-1         (mjr_mat_apply-gauss-1!! (mjr_mat_make-identity k)     m n))
      (:mp-gauss-2         (mjr_mat_apply-gauss-2!! (mjr_mat_make-identity k)     a n))
      (:mp-gauss-3         (if k
                               (mjr_mat_apply-gauss-3!!       (mjr_mat_make-identity k)     a m n)
                               (let ((f (- (/ (aref x n) (aref x m)))))
                                 (mjr_mat_apply-gauss-3!!     (mjr_mat_make-identity len-x) f m n))))
      (:mp-givens-2        (cond (k         (mjr_mat_apply-givens-2!!      (mjr_mat_make-identity k)     a b m n))
                                 (x?        (multiple-value-bind (c s) (mjr_mat_orthogonal-zero-pair (aref x m) (aref x n) 1)
                                              (mjr_mat_apply-givens-2!!    (mjr_mat_make-identity len-x) c s m n)))
                                 ('t        (let ((c (cos a))
                                                  (s (sin a)))
                                              (mjr_mat_apply-givens-2!!    (mjr_mat_make-identity len-x) c s m n)))))
      (:mp-householder-2   (cond (k         (mjr_mat_apply-householder-2!!      (mjr_mat_make-identity k)     a b m n))
                                 (x?        (multiple-value-bind (c s) (mjr_mat_orthogonal-zero-pair (aref x m) (aref x n) 1)
                                              (mjr_mat_apply-householder-2!!    (mjr_mat_make-identity len-x) c s m n)))
                                 ('t        (let ((c (cos a))
                                                  (s (sin a)))
                                              (mjr_mat_apply-householder-2!!    (mjr_mat_make-identity len-x) c s m n)))))
      (:mp-householder     (let ((newmat (mjr_mat_make-identity len-x)))
                             (multiple-value-bind (gamma u) (mjr_mat_householder-many x y n)
                               (mjr_mat_apply-householder-many!! newmat gamma u))))
      (otherwise           (error "mjr_matt_make-test: Unknown matrix type requested!!")))))
