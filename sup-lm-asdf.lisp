;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      sup-lm-asdf.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Load and use-package all *MJRCALC* packages via ADSF.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1998,2002,2004,2007,2010,2011,2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @filedetails
;;
;;  If you have problems with ASDF loading *MJRCALC* on your platform, I suggest that you try the following:
;;      1) Put the *MJRCALC* source in "~/.local/share/common-lisp/source/mjrcalc/ -- or make a link.
;;      2) Try HACK-B below -- change the conditional to include your lisp
;;      3) Put a copy of the newest ASDF in ~/.local/share/common-lisp/source/asdf/asdf.lisp
;;      4) Try HACK-A below -- change the conditional to include your lisp
;;
;; By way of an example of how one might set things up, here is what I do.  I keep this code base in the directory ~/core/lispy, and I use quicklisp.  To
;; setup a brand new environment I first install quicklisp, and then do the following (in bash):
;;     if [ -e $HOME/core/lispy ] ; then
;;       mkdir -p ~/.local/share/common-lisp/source/
;;       ln -s ~/core/lispy ~/.local/share/common-lisp/source/mjrcalc
;;     else
;;       echo "ERROR: Could not find mjrcalc!!"
;;     fi
;; Then I fire up a LISP, I use SBCL, and do this:
;;     (load "/home/richmit/quicklisp/asdf.lisp")
;;     (load "/home/richmit/.local/share/common-lisp/source/mjrcalc/sup-lm-asdf.lisp")
;;
;; You don't need to use quicklisp just to load this package with ASDF.  Suppose you have installed ASDF in the directory
;; ~/.local/share/common-lisp/source/asdf/asdf.lisp.  You would first link mjrcalc into your ASDF tree like this (in bash):
;;     if [ -e $HOME/core/lispy ] ; then
;;       ln -s ~/core/lispy ~/.local/share/common-lisp/source/mjrcalc
;;     else
;;       echo "ERROR: Could not find mjrcalc!!"
;;     fi
;; Then you would fire up a LISP, and do this:
;;     (load "~/.local/share/common-lisp/source/asdf/asdf.lisp")
;;     (load "~/.local/share/common-lisp/source/mjrcalc/sup-lm-asdf.lisp")
;;
;;  Testing History:
;;
;;      |------------+---------------------+-----------------+---------------------+--------+-------+--------------------------------------------------|
;;      | Test  Date | Lisp Implementation |    Lisp Version | OS Version          | Status | Hacks | Notes                                            |
;;      |------------+---------------------+-----------------+---------------------+--------+-------+--------------------------------------------------|
;;      | 2015-02-01 | ABCL                |           1.3.1 | debian 7.7          | WORKS  |       |                                                  |
;;      | 2015-02-01 | Allegro CL Express  |             9.0 | debian 7.7          | WORKS  | A & B |                                                  |
;;      | 2015-02-01 | CMUCL               |             20f | debian 7.7          | WORKS  |       |                                                  |
;;      | 2015-02-01 | ECL                 |          11.1.1 | debian 7.7          | WORKS  | B     |                                                  |
;;      | 2015-02-01 | GCL                 |           2.6.7 | debian 7.7          | BROKEN |       |                                                  |
;;      | 2015-02-01 | GCL                 |           2.6.9 | debian 7.7          | BROKEN |       |                                                  |
;;      | 2015-02-01 | LispWorks Personal  |           6.1.1 | debian 7.7          | WORKS  |       |                                                  |
;;      | 2015-02-01 | SBCL                |           1.2.8 | debian 7.7          | WORKS  |       |                                                  |
;;      | 2015-02-01 | SBCL                |          1.2.11 | debian 7.7          | WORKS  |       |                                                  |
;;      | 2015-02-01 | SBCL                | 1.0.57.0.debian | debian 7.7          | WORKS  | A     |                                                  |
;;      | 2015-02-01 | clisp               |            2.49 | debian 7.7          | WORKS  | A     |                                                  |
;;      |------------+---------------------+-----------------+---------------------+--------+-------+--------------------------------------------------|
;;      | 2015-09-16 | ABCL                |           1.3.2 | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | CCL                 |            1.10 | debian 8.2          | WORKS  | A     |                                                  |
;;      | 2015-09-16 | CMUCL               |             20d | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | CMUCL               |             20f | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | CMUCL               |     21a 2015-09 | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | ECL                 |          15.3.7 | debian 8.2          | WORKS  | B     |                                                  |
;;      | 2015-09-16 | SBCL                |          1.2.14 | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | SBCL                |    1.2.4.debian | debian 8.2          | WORKS  |       |                                                  |
;;      | 2015-09-16 | clisp               |            2.49 | debian 8.2          | WORKS  | A     |                                                  |
;;      |------------+---------------------+-----------------+---------------------+--------+-------+--------------------------------------------------|
;;      | 2019-01-30 | SBCL                |          1.4.14 | Windows 10 Pro 1809 | WORKS  | B     | Running in MSYS2 shell                           |
;;      | 2019-01-30 | SBCL                |   1.3.14.debian | Debian 9.6          | WORKS  |       | Windows store Debian App/WSL/Windows 10 Pro 1809 |
;;      |------------+---------------------+-----------------+---------------------+--------+-------+--------------------------------------------------|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HACK-A
;; If you have a LISP with an old ASDF.  Change to suit your environment.
#+(and (not ASDF3)
       (or CCL CLISP)) (let ((asdf-file (find-if #'probe-file '("~/common-lisp/asdf.lisp"
                                                                "~/.local/share/common-lisp/source/asdf.lisp"
                                                                "~/.local/share/common-lisp/source/asdf/asdf.lisp"
                                                                "asdf.lisp"
                                                                "~/quicklisp/asdf.lisp"
                                                                "C:/msys64/home/richmit/quicklisp/quicklisp/"
                                                                "../olispy/asdf/asdf.lisp"))))
                         (if asdf-file
                             (progn (load asdf-file)
                                    (warn "sup-lm-asdf: Loading newer ASDF from: ~s" asdf-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :asdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HACK-B
;; Some LISPS don't load things from subdirectories of the standard ASDF locations.  Change to suit your environment.
#+(or ECL ALLEGRO CCL) (let ((mjrcalc-file (find-if #'probe-file '("~/.local/share/common-lisp/source/mjrcalc/"
                                                                   "C:/msys64/home/richmit/.local/share/common-lisp/source/mjrcalc/"
                                                                   "~/common-lisp/mjrcalc/"))))
                         (if (and mjrcalc-file
                                  (not (find mjrcalc-file asdf:*central-registry* :test #'string-equal)))
                             (push mjrcalc-file asdf:*central-registry*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:load-system :mjrcalc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mjr_meta::mjr_meta_use-packages :base-path (directory-namestring (asdf:system-source-directory :mjr_meta)))
