;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      sup-lm-asdf.lisp
;; @author    Mitch Richling<http://www.mitchr.me>
;; @Copyright Copyright 1998,2002,2004,2007,2010,2011,2015 by Mitch Richling.  All rights reserved.
;; @brief     Load and use-package all *MJRCALC* packages via ADSF.@EOL
;; @Keywords  lisp load mjr library
;; @Std       Common Lisp
;; @Tested    2015-02-01 SBCL               1.0.57.0.debian  debian 7.7  WORKS with HACK-A
;; @Tested    2015-02-01 SBCL               1.2.8            debian 7.7  WORKS 
;; @Tested    2015-02-01 SBCL               1.2.11           debian 7.7  WORKS 
;; @Tested    2015-02-01 ECL                11.1.1           debian 7.7  WORKS
;; @Tested    2015-02-01 GCL                2.6.7            debian 7.7  BROKEN
;; @Tested    2015-02-01 GCL                2.6.9            debian 7.7  BROKEN
;; @Tested    2015-02-01 clisp              2.49             debian 7.7  WORKS with HACK-A
;; @Tested    2015-02-01 LispWorks Personal 6.1.1            debian 7.7  WORKS
;; @Tested    2015-02-01 Allegro CL Express 9.0              debian 7.7  WORKS with HACK-A & HACK-B
;; @Tested    2015-02-01 ABCL               1.3.1            debian 7.7  WORKS
;; @Tested    2015-02-01 CMUCL              20f              debian 7.7  WORKS
;;            
;;            If you have problems with ASDF loading *MJRCALC* on your platform, I suggest that you try the following:
;;                1) Put the *MJRCALC* source in "~/.local/share/common-lisp/source/mjrcalc/ -- or make a link.
;;                2) Try HACK-B below -- change the conditional to include your lisp
;;                3) Put a copy of the newest ASDF in ~/.local/share/common-lisp/source/asdf/asdf.lisp
;;                4) Try HACK-A below -- change the conditional to include your lisp
;;
;;            The following lines are here for cut-n-paste help. :)
;;                (load "/home/richmit/.local/share/common-lisp/source/asdf/asdf.lisp")
;;                (load "/home/richmit/.local/share/common-lisp/source/mjrcalc/sup-lm-asdf.lisp")
;;
;;            If you have quicklisp installed, then you can likely use this:
;;                (load "/home/richmit/quicklisp/asdf.lisp")
;;                (load "/home/richmit/.local/share/common-lisp/source/mjrcalc/sup-lm-asdf.lisp")

;;----------------------------------------------------------------------------------------------------------------------------------

;;----------------------------------------------------------------------------------------------------------------------------------
;; HACK-A
;; If you have a LISP with an old ASDF (like the default one that ships with clisp), then here is how you might go about getting an
;; ASDF loaded from this file.  Change the conditional to suit your lisp.
#+clisp (let ((asdf-file (find-if #'probe-file '("~/common-lisp/asdf.lisp"
                                                 "~/.local/share/common-lisp/source/asdf.lisp"
                                                 "~/.local/share/common-lisp/source/asdf/asdf.lisp"
                                                 "asdf.lisp"
                                                 "~/quicklisp/asdf.lisp"
                                                 "../olispy/asdf/asdf.lisp"))))
          (if asdf-file 
              (progn (load asdf-file)
                     (warn "sup-lm-asdf: Loading newer ASDF..."))))

;;----------------------------------------------------------------------------------------------------------------------------------
(require :asdf)

;;----------------------------------------------------------------------------------------------------------------------------------
;; HACK-B
;; Some LISPS don't load things from subdirectories of the standard ASDF locations (Like some versions of Allegro apparently).  Here
;; is how you might work around such a problem.
#+(or ECL ALLEGRO) (let ((mjrcalc-file (find-if #'probe-file '("~/.local/share/common-lisp/source/mjrcalc/"
                                                               "~/common-lisp/mjrcalc/"))))
                     (if mjrcalc-file
                         (push mjrcalc-file asdf:*central-registry*)))


;;----------------------------------------------------------------------------------------------------------------------------------
(asdf:load-system :mjrcalc)

;;----------------------------------------------------------------------------------------------------------------------------------
(mjr_meta::mjr_meta_use-packages :base-path (directory-namestring (asdf:system-source-directory :mjr_meta)))
