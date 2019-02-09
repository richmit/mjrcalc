(defsystem
 "mjr_gfp"
 :description "Interactive GF(p) library -- modular arithmatic."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_INTU)
 :components ((:file "lib-gfp"))
)
