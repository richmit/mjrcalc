;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      sup-lm-meta.lisp
;; @author    Mitch Richling<http://www.mitchr.me>
;; @Copyright Copyright 1998,2002,2004,2007,2010,2011,2015 by Mitch Richling.  All rights reserved.
;; @brief     Load and use-package all *MJRCALC* packages via META.@EOL
;; @Keywords  lisp load mjr library
;; @Std       Common Lisp
;; @Tested    2015-01-29 SBCL      1.0.57.0.debian  debian 7.7  WORKS
;; @Tested    2015-01-29 ECL       11.1.1           debian 7.7  WORKS
;; @Tested    2015-01-29 GCL       2.6.7            debian 7.7  BROKEN
;; @Tested    2015-01-29 clisp     2.49             debian 7.7  WORKS
;; @Tested    2015-01-29 LispWorks 6.1.1            debian 7.7  WORKS
;;
;;            (load "sup-lm-meta.lisp")

;;----------------------------------------------------------------------------------------------------------------------------------
(load "lib-meta.lisp")
(mjr_meta::mjr_meta_load-packages :show-progress 't)
(mjr_meta::mjr_meta_use-packages)
