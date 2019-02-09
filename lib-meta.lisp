;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      lib-meta.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Meta package for working with *MJRCALC* packages.@EOL
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
;; @todo      unit tests!@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_META
  (:USE :COMMON-LISP)
  (:DOCUMENTATION "Brief: Meta package for working with *MJRCALC* packages.;")
  (:EXPORT #:mjr_meta_help
           #:mjr_meta_load-packages
           #:mjr_meta_use-packages
           #:mjr_meta_compile-packages
           #:mjr_meta_asdfify-packages
           #:mjr_meta_dotify-packages
           #:mjr_meta_test-packages
           ;; Not Exported
           ;; #:mjr_meta_scan-packages
           ;; #:*prefixes-to-consider-default*
           ))

(in-package :MJR_META)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_help ()
  "Help for the :MJR_META package:

INTRODUCTION
^^^^^^^^^^^^

  This package is for working with other packages in *MJRCALC* -- finding, loading, compiling, testing, etc...

  Unless the functions in this package are run within the directory containing the *MJRCALC* source, the :BASE-PATH argument must be provided to let the
  functions know where the *MJRCALC* source is.  The value is the path, with trailing directory separator at the end.  For example:
  '/the/full/path/to/mjrcalc/'

  If you use SLIME, then you can 'cd' your LISP with the comma command or from the REPL with the SWANK:SET-DEFAULT-DIRECTORY function.

  Most people will use ASDF to load/compile the packages included in *MJRCALC* so the only thing of use in this package is the function MJR_META_USE-PACKAGES
  which will do a 'use-package' for all packages designed for interactive use.

  For people who do not use ASDF, this package can do a few useful things:

    * Find *MJRCALC* packages on the disk
    * Load *MJRCALC* packages (source or objects) taking into account package dependencies
    * Compile *MJRCALC* packages

  For developers extending *MJRCALC*, a few other items may be of use:

    * Generate ASDF files
    * Generate dot code to generate package dependency graphs

EXPORTED PACKAGE CONTENTS
^^^^^^^^^^^^^^^^^^^^^^^^^

   * FUN: mjr_meta_help ............... Displays this help document
   * FUN: mjr_meta_load-packages ...... Load scanned packages (source code or object files)
   * FUN: mjr_meta_use-packages ....... Use loaded packages
   * FUN: mjr_meta_compile-packages ... Compile loaded packages
   * FUN: mjr_meta_asdfify-packages ... Write ASDF files (.asd) for each package
   * FUN: mjr_meta_dotify-packages .... Write the dot files used to generate dependency graphs

LOAD'N THE CODE WITH ASDF OR QUICKLISP
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  With ASDF or Quicklisp, you can easily load up just the packages you need (like :MJR_MAT and its dependencies), or you can load up everything with :mjrcalc.
  For example, you might load everything with ASDF like so:

     (require :asdf)
     (asdf:load-system :mjrcalc)

  With Quicklisp the process is very similar (first make sure Quicklisp is loaded):

     (ql:quickload :mjrcalc)

  When used interactively, the next step is to do a 'use-package' on the loaded parts of *MJRCALC* designed for interactive use.  This may be done like so:

     (mjr_meta::mjr_meta_use-packages :base-path (directory-namestring (asdf:system-source-directory :mjr_meta)))

  The above process of loading and 'using' *MJRCALC* packages may be archived by loading the file 'sup-lm-asdf.lisp'.

  Note that you need a relatively recent version of ASDF for all this to work -- v3 or newer.  If you are using Quicklisp, then you are OK. ;)

LOAD'N THE CODE
^^^^^^^^^^^^^^^

  Using ASDF or Quicklisp is the normal way I expect most people will load the packages, but they may be loaded using :MJR_META like so:

     (mjr_meta::mjr_meta_load-packages)
     (mjr_meta::mjr_meta_use-packages)

  You can do all of the above by loading the file 'sup-lm-meta.lisp'.

COMPILE'N THE CODE
^^^^^^^^^^^^^^^^^^

  While most users will probably use ASDF to manage compiled versions of the code, the :MJR_META package can be to compile the code without ASDF or
  Quicklisp/.  This is all a bit of a hack, but works -- mostly. ;)

  Before compiling the code it must first be loaded.  The complete sequence looks like this:

     (mjr_meta::mjr_meta_load-packages)
     (mjr_meta::mjr_meta_compile-packages)

  Then the object files may be loaded, on most LISPs at least, like so:

     (mjr_meta_load-packages :force-source-load nil)

  On some LISPs you might need to provide the :OBJ-EXTENSION argument to specify the file extension used by object files.  Note that on most lisps, you can
  leave this the empty string and the LISP will load the object code if it is up to date and the source otherwise.

CHANGE'N THE CODE
^^^^^^^^^^^^^^^^^

  If you change the package dependencies, then you may want to update the ASDF and dot files.  This may be accomplished like so:

     (mjr_meta::mjr_meta_load-packages)
     (mjr_meta::mjr_meta_asdfify-packages)
     (mjr_meta::mjr_meta_dotify-packages)

PACKAGE AND SYMBOL NAMING CONVENTIONS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The package names all match the following Perl regular expression:

      /MJR_([a-z]+)/

  Where $1 is the 'package identifier' and 'MJR' is the prefix I use on all of my packages.

  All functions and macro names match a similar Perl regular expression:

      /MJR_([a-z]+)_([^_]+)

  All parameter names match a similar Perl regular expression, but they have ear-muffs:

      /\*MJR_([a-z]+)_([^_]+\*$)

  In both symbol regular expressions, $1 is the 'package identifier' -- i.e. all external names begin with the package name followed by an underscore.  The $2
  could be thought of as the 'unqualified symbol name' within the package.  Note that external symbols have PRECISELY two underscores when we follow the rules
  above.  In the code, package symbols are generally referred to in lowercase.

  Using the above conventions, a typical function might be named:

      mjr_package:mjr_package_function-name

  This redundancy is designed to provide namespace isolation at the language level and at the interactive REPL level even if all the packages are 'USED'.
  This second form of namespace separation makes auto-completion work nicely in clisp, SLIME, and rlwrap.  The biggest disadvantage is that it lengthens
  function names in source code files.

  Unit test packages have names that look like

      /MJR_([a-z]+)-TESTS$/

  The test will be for a package named MJR_$1 (i.e. with package identifier of $1).

  Unit test files contain tests, data, and functions.  These symbols have names that match the ASSOCIATED package they test.  Ex: :MJR_FOO-TESTS might contain
  a function named MJR_FOO_BAR-NAIVE.  Note that this function looks like it might belong to the package :MJR_FOO!

  An alternate implementation for a function found in a unit test package have names that end with '-naive[0-9]'.

  Each normal package (i.e. not a unit test package), begins with a defpackage form:

      (defpackage :MJR_PKG
        (:USE :COMMON-LISP
              :MJR_PKG1
              :MJR_PKG2
              ... )
        (:DOCUMENTATION \"Standard *MJRCALC* package documentation string\")
        (:EXPORT :MJR_FUNC_1
                 :MJR_FUNC_2
                 ...))
      (in-package :MJR_PKG)

  The order of the forms contained within the defpackage is important.  The :USE, :DOCUMENTATION, and :EXPORT components may only appear in this order, and,
  when present, they must appear immediately after the package name.  The :USE component is required -- all *MJRCALC* packages use :COMMON-LISP.

  In the above, :MJR_PKG is the name of the package -- i.e. the package identifier is 'PKG'.  The package symbol and dependency symbols should be UPPERCASE.
  This structure allows the following code to dynamically discover all package dependencies, and then load the packages in an order consistent with those
  package dependencies.

FILE NAME CONVENTIONS
^^^^^^^^^^^^^^^^^^^^^

  The package identifier relates to the source code files holding the code and unit tests.  Let $1 be as in the first regexp above, then:

    -  lib-<PACKAGE_ID>.lisp .............. non-interactive library for PACKAGE_ID
    -  use-<PACKAGE_ID>.lisp .............. interactive library for PACKAGE_ID
    -  tst-<PACKAGE_ID>.lisp .............. lisp-unit tests for PACKAGE_ID
    - utst-<PACKAGE_ID>.lisp .............. u tests for PACKAGE_ID
    -  tst-<PACKAGE_ID>-REG-<TAG>.* ....... Regression golden test file for PACKAGE_ID
    -  tst-<PACKAGE_ID>-OUT-<TAG>.* ....... Test output file for PACKAGE_ID
    -  dev-<PACKAGE_ID>.lisp .............. Development library for PACKAGE_ID.  Used for alpha packages.
    -  pre-<PACKAGE_ID>.lisp .............. Pre-Production component.  Used for beta packages.
    -  add-<PACKAGE_ID>.lisp .............. User add-on component.  I will never use this prefix, so end users can.

  Other files, not related to packages, include:

    - sup-<TOOL_ID>.* ..................... support code
    - exp-<EXAMP_ID>.lisp ................. example Lisp code
    - exp-<EXAMP_ID>-IN-<TAG1>.<EXT1> ..... input file
    - exp-<EXAMP_ID>-OUT-<TAG2>.<EXT2> .... output file
    - exp-<EXAMP_ID>-AUX-<TAG3>.<EXT3> .... Auxiliary file (ex: a povray file that will render an output file)
    - exp-<EXAMP_ID>-ART-<TAG4>.<EXT4> .... An artifact generated from an additional process (ex: image generated by povray)"
  (documentation 'mjr_meta_help 'function))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *prefixes-to-consider-default* '("use-" "lib-" "add-" "pre-")
  "Default value for prefixes-to-consider argument")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_parse-package-doc (docstring)
  "Parse standard *MJRCALC* package documentation strings."
  (loop with ppos = 0
        for npos = (search ";" docstring :start2 ppos)
        until (null npos)
        collect (let* ((tmp (string-trim " $" (subseq docstring ppos npos)))
                       (cpo (search ":" tmp)))
                  (cons (string-trim " " (subseq tmp 0 cpo))
                        (string-trim " " (subseq tmp (1+ cpo)))))
        do (setq ppos (1+ npos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_scan-packages (&key prefixes-to-consider show-progress base-path)
  "Scan the source directory for packages.

Prefixes: use- lib- add- pre- dev- tst-

The value of PREFIXES-TO-CONSIDER must be :pc-src or :pc-tst"
  (let ((prefixes-to-consider (or prefixes-to-consider *prefixes-to-consider-default*)))
    (loop for (pkg-path pkg-file) in (loop for pfx in prefixes-to-consider
                                           append (loop for cur-path-name in (directory (concatenate 'string base-path pfx "*.lisp"))
                                                        for cur-path-name-string = (file-namestring cur-path-name)
                                                        when (and (> (length cur-path-name-string) 5)
                                                                  (string= ".lisp" cur-path-name-string :start2 (- (length cur-path-name-string) 5))
                                                                  (some (lambda (pfx) (equalp 0 (search pfx cur-path-name-string)))
                                                                        prefixes-to-consider))
                                                        collect (list (namestring cur-path-name) cur-path-name-string)))
          for pkg-head = (with-open-file (in-file-stream pkg-path :direction :input) (read in-file-stream))
          for pkg-name = (and pkg-head
                              (listp pkg-head)
                              (equal 'defpackage (first pkg-head))
                              (second pkg-head))
          for pkg-uses = (and pkg-head
                              (listp pkg-head)
                              (equal 'defpackage (first pkg-head))
                              (listp (third pkg-head))
                              (equal :USE (first (third pkg-head)))
                              (remove-if-not (lambda (pkg) (equalp 0 (search "MJR_" (symbol-name pkg)))) (cdr (third pkg-head))))
          for (pkg-docs) = (and pkg-head
                                (listp pkg-head)
                                (equal 'defpackage (first pkg-head))
                                (listp (fourth pkg-head))
                                (equal :DOCUMENTATION (first (fourth pkg-head)))
                                (let ((docs (mjr_meta_parse-package-doc (second (fourth pkg-head)))))
                                  (list (cdr (assoc "Brief" docs :test #'string=)))))
          when (and pkg-head
                    pkg-name
                    pkg-file
                    (equalp 0 (search "MJR_" (symbol-name pkg-name))))
          do (if show-progress (format 't "Found package: ~20a ::: ~s~%" pkg-name pkg-path))
          collect (list pkg-name pkg-path pkg-file pkg-uses pkg-docs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_load-packages (&key prefixes-to-consider (force-source-load 't) (object-extension "") (max-deps-tree-depth 10) show-progress base-path)
  "Load *MJRCALC* packages."
  (let* ((all-load-s-time      (float (/ (get-internal-real-time) internal-time-units-per-second)))
         (prefixes-to-consider (or prefixes-to-consider *prefixes-to-consider-default*))
         (pkg-loaded           nil)
         (pkg-data             (mjr_meta_scan-packages :prefixes-to-consider prefixes-to-consider :base-path base-path))
         (num-want-load        (length pkg-data))
         (num-loaded           (loop for i from 1 upto max-deps-tree-depth
                                     for num-loaded-this-pass = (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in pkg-data
                                                                      when (and (not (member pkg-name pkg-loaded))
                                                                                (every (lambda (pkg-use) (member pkg-use pkg-loaded)) pkg-uses))
                                                                      count (let ((load-s-time (float (/ (get-internal-real-time) internal-time-units-per-second))))
                                                                              (if show-progress (format 't "LOADING(~d): ~15a" i pkg-name))
                                                                              (if force-source-load
                                                                                  (load pkg-path)
                                                                                  (load (concatenate 'string
                                                                                                     (subseq pkg-path 0 (- (length pkg-path) 5))
                                                                                                     object-extension)))
                                                                              (push pkg-name pkg-loaded)
                                                                              (let ((load-e-time (float (/ (get-internal-real-time) internal-time-units-per-second))))
                                                                                (if show-progress (format 't " ~7,2fs :: ~a~%" (- load-e-time load-s-time) pkg-uses)))
                                                                              1))
                                     sum num-loaded-this-pass
                                     until (= 0 num-loaded-this-pass))))
    (let ((all-load-e-time (float (/ (get-internal-real-time) internal-time-units-per-second))))
      (format 't "Loaded ~d of ~d packages in ~,2fs~%" num-loaded num-want-load (- all-load-e-time all-load-s-time))
      (if (not (= num-loaded num-want-load))
          (format 't "Not Loaded: ~a~%" (set-difference (mapcar #'car pkg-data) pkg-loaded))))
    num-loaded))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_use-packages (&key prefixes-to-consider show-progress base-path)
  "Preforms a use-package for each loaded *MJRCALC* package that is intended for interactive use."
  (let* ((prefixes-to-consider (or prefixes-to-consider *prefixes-to-consider-default*))
         (num-used             (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in (mjr_meta_scan-packages :prefixes-to-consider prefixes-to-consider :base-path base-path)
                                     when (and (find-package pkg-name)
                                               (not (equalp 0 (search "lib-" pkg-file))))
                                     do (if show-progress (format 't "Using package: ~a~%" pkg-name))
                                     and
                                     count (use-package pkg-name))))
    (format 't "Used ~d packages!~%" num-used)
    num-used))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_compile-packages (&key prefixes-to-consider show-progress base-path quiet-compile)
  "Compile all packages in *MJRCALC*."
  (let* ((prefixes-to-consider (or prefixes-to-consider *prefixes-to-consider-default*))
         (num-compiled         (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in (mjr_meta_scan-packages :prefixes-to-consider prefixes-to-consider :base-path base-path)
                            when (find-package pkg-name)
                            do (if show-progress (format 't "Compiling package: ~a~%" pkg-name))
                            and
                            count (if quiet-compile
                                      (compile-file pkg-path :verbose nil :print nil)
                                      (compile-file pkg-path)))))
    (format 't "Compiled ~d packages!~%" num-compiled)
    num-compiled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_asdfify-packages (&key prefixes-to-consider show-progress base-path)
  "Create ASDF files for all packages and the MJRCALC 'package'."
  (let* ((prefixes-to-consider (or prefixes-to-consider *prefixes-to-consider-default*))
         (pkg-auth             "Mitch Richling <https://www.mitchr.me/>")
         (pkg-lic              "See the BSD-style license in LICENSE.TXT")
         (mjr-ver              (format nil "~d" (- (get-universal-time) 2208988800)))
         (pkg-data             (mjr_meta_scan-packages :prefixes-to-consider prefixes-to-consider :base-path base-path))
         (num-asdfify          (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in pkg-data
                            for pkg-asdf  = (concatenate 'string (string-downcase (symbol-name pkg-name)) ".asd")
                            for pkg-bfile = (subseq pkg-file 0 (- (length pkg-file) 5))
                            for new-asd-str = (format nil
                                                      "(defsystem~% ~w~% :description ~w~% :version ~w~% :author ~w~% :licence ~w~% :defsystem-depends-on ~w~% :components ((:file ~w))~%)~%"
                                                      (string-downcase (symbol-name pkg-name))
                                                      (or pkg-docs "YIKES!!!")
                                                      mjr-ver
                                                      pkg-auth
                                                      pkg-lic
                                                      pkg-uses
                                                      pkg-bfile)
                            for old-asd-str = (and (probe-file pkg-asdf)
                                                   (with-open-file (stream pkg-asdf :direction :input)
                                                     (let ((seq (make-string (file-length stream))))
                                                       (read-sequence seq stream)
                                                       seq)))
                            for new-old-same = (if old-asd-str
                                                   (let* ((nas1e (search ":version \"" new-asd-str))
                                                          (nas2b (search ":author \""  new-asd-str))
                                                          (oas1e (search ":version \"" new-asd-str))
                                                          (oas2b (search ":author \""  old-asd-str)))
                                                     (and nas1e nas2b oas1e oas2b
                                                          (string= (subseq new-asd-str 0 nas1e) (subseq old-asd-str 0 oas1e))
                                                          (string= (subseq new-asd-str nas2b)   (subseq old-asd-str oas2b)))))
                            sum (if new-old-same
                                    (progn (if show-progress (format 't "Skipping (no change) .asd file for package: ~a~%" pkg-name))
                                           0)
                                    (progn (if show-progress (format 't "Writeing (it changed) .asd file for package: ~a~%" pkg-name))
                                           (with-open-file (stream pkg-asdf :direction :output :if-exists :supersede :if-does-not-exist :create)
                                             (format stream "~a" new-asd-str))
                                           1)))))
    (with-open-file (stream "mjrcalc.asd" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream
              "(defsystem~% ~w~% :description ~w~% :version ~w~% :author ~w~% :licence ~w~% :defsystem-depends-on ~w~%)~%"
              "mjrcalc"
              "META package depending on every *MJRCALC*.package -- load this to load them all!"
              mjr-ver
              pkg-auth
              pkg-lic
              (mapcar #'car pkg-data)))
    (format 't "ASDFify'ed ~d packages!~%" num-asdfify)
    (format 't "ASDFify'ed 1 meta package!~%")
    num-asdfify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_dotify-packages (&key (out-file-name "gr.dot") show-progress base-path)
  "Create dot code for dependency graphs.

Spit out a dot file with package dependencies (default file name is 'gr.dot')

Different ways to render a graph with graphviz:

  $ circo -T pdf -o gr.pdf gr.dot ; xpdf -z page gr.pdf
  $ dot -T pdf -o gr.pdf gr.dot ; xpdf -z page gr.pdf
  $ fdp -T pdf -o gr.pdf gr.dot ; xpdf -z page gr.pdf
  $ neato -T pdf -o gr.pdf gr.dot ; xpdf -z page gr.pdf
  $ twopi -T pdf -o gr.pdf gr.dot ; xpdf -z page gr.pdf

See: sup-updDepImage.sh for how the dependency graphs on the web site are generated."
  (with-open-file (stream out-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "digraph mjrDeps {~%")
    (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in (mjr_meta_scan-packages :prefixes-to-consider '("use-" "lib-" "pre-" "dev-" "tst-") :base-path base-path)
          for pkg-name-string = (let ((s (symbol-name pkg-name)))
                                  (if (and (< 5 (length s)) (string-equal (reverse "-TESTS") (reverse s) :start2 0 :end2 6))
                                      (subseq s 0 (- (length s) 6))
                                      s))
          for pkg-color       = (cond ((string-equal "tst-" pkg-file :start2 0 :end2 4) "mediumpurple1")
                                      ((string-equal "use-" pkg-file :start2 0 :end2 4) "lightblue")
                                      ((string-equal "lib-" pkg-file :start2 0 :end2 4) "palegreen2")
                                      ((string-equal "pre-" pkg-file :start2 0 :end2 4) "gold")
                                      ((string-equal "dev-" pkg-file :start2 0 :end2 4) "pink"))
          do (if show-progress (format 't "Doting package: ~a~%" pkg-name))
          do (format stream "    \"~a\" [color=~a, style=filled, label=\"~a\"];~%" pkg-name pkg-color pkg-name-string)
          do (loop for cur-pkg-dep in pkg-uses
                   do (format stream "    \"~a\" -> \"~a\";~%" pkg-name cur-pkg-dep)))
      (format stream "}~%")
      (format 't "Wrote ~a~%" out-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_meta_test-packages (&key tests-to-run dry-run show-progress base-path)
  "Load and run unit tests.

  * tests-to-run   -- NIL run all tests.  Otherwise a list of test package symbols (ex: '(:MJR_VEC-TESTS :MJR_VVEC-TESTS))
  * dry-run        -- Non-NIL means to skip running the tests.  Most useful with :show-progress 't
  * :show-progress -- Non-NIL to print progress
  * base-path      -- The path to look for source files (CWD by default)"
  (if (not (find-package :LISP-UNIT))
      (format 't "LISP-UNIT not loaded.~%USE: (load \"../olispy/lisp-unit.lisp\")~%")
      (let ((loaded-pkgs  (mapcar #'package-name (list-all-packages)))
            (tests-to-run (mapcar (lambda (s) (let ((s (if (stringp s)
                                                           s
                                                           (symbol-name s))))
                                                (if (and (< 5 (length s)) (string-equal (reverse "-TESTS") (reverse s) :start2 0 :end2 6))
                                                    s
                                                    (concatenate 'string s "-TESTS"))))
                                  tests-to-run)))
        (loop for (pkg-name pkg-path pkg-file pkg-uses pkg-docs) in (mjr_meta_scan-packages :prefixes-to-consider '("tst-") :base-path base-path)
              for tst-for-pkg-name = (let ((s (symbol-name pkg-name)))
                                       (if (and (< 5 (length s)) (string-equal (reverse "-TESTS") (reverse s) :start2 0 :end2 6))
                                           (subseq s 0 (- (length s) 6))
                                           (error "mjr_meta_test-packages: Found '~s' file with bad package name '~s'." pkg-file pkg-name)))
              do (if (not (or (not tests-to-run) (find (symbol-name pkg-name) tests-to-run :test #'string-equal)))
                     (if show-progress (format 't "Excluded Tests: ~a~%" pkg-name))
                     (if (not (find tst-for-pkg-name loaded-pkgs :test #'string-equal))
                         (if show-progress (format 't "Skipping Tests: ~a~%" pkg-name))
                         (progn (if show-progress (format 't "Running  Tests: ~a~%" pkg-name))
                                (if (not dry-run)
                                    (handler-case
                                        (load pkg-path)
                                      (error (e) (print e)))))))))))
