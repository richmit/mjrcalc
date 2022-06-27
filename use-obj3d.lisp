;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-obj3d.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     OBJ 3d files.@EOL
;; @std       Common Lisp
;; @see       N/A
;; @copyright
;;  @parblock
;;  Copyright (c) 1995, 2013, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
(defpackage :MJR_OBJ3D
  (:USE :COMMON-LISP
        :MJR_DSIMP)
  (:DOCUMENTATION "Brief: Create OBJ3D files.;")
  (:EXPORT #:mjr_obj3d_help
           #:mjr_obj3d_from-dsimp
           ))

(in-package :MJR_OBJ3D)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_obj3d_help ()
  "Help for MJR_OBJ3D:

Library for produceing OBJ 3D files -- from DSIMP objects.  In the future other source types and/or more complex PLY files may be added."
  (documentation 'mjr_obj3d_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_obj3d_from-dsimp (out-file dsimp)
  "Generate an OBJ file 2-simplices from dsimp.

The use primary use case is to easily get a triangulation into more artistically oriented 3D modeling tools like Blender and meshlab.
This function is also handy for quickly looking at a surface with 3D object viewers like g3dviewer before loading into more cumbersome tools.

See the :POV and :VTK packages for more sophisticated rendering options and data export options respectively.

Typical example of use:

  (mjr_dsimp_dump2d-obj-file \"surf.obj\"
                             (mjr_fsamp_ds-func-r123-r123 (lambda (x y) (* .5 (abs (- (expt (complex x y) 3) 1))))
                                                          :xdat '(:start -1.1 :end 1.1 :len 50)
                                                          :ydat '(:start -1.1 :end 1.1 :len 50)
                                                          :arg-mode :arg-number))"
  (with-open-file (dest out-file  :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for pnt across (mjr_dsimp_get-simplex-array dsimp 0)
          do (format dest "v ~f ~f ~f~%" (aref pnt 0) (aref pnt 1) (aref pnt 2)))
    (loop for  tri across (mjr_dsimp_get-simplex-array dsimp 2)
          do (format dest "f ~d ~d ~d~%" (1+ (aref tri 0)) (1+ (aref tri 1)) (1+ (aref tri 2))))))
