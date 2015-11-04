(defsystem
 "mjr_opt"
 :description "Univariate function optimization."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_PRNG)
 :components ((:file "use-opt"))
)
