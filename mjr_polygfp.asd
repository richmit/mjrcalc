(defsystem
 "mjr_polygfp"
 :description "Univariate polynomials over prime order fields -- i.e. GF(p)[x]."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_GFP :MJR_GPOLY)
 :components ((:file "use-polygfp"))
)
