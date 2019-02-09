(defsystem
 "mjr_opt"
 :description "Univariate function optimization."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_PRNG)
 :components ((:file "use-opt"))
)
