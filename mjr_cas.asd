(defsystem
 "mjr_cas"
 :description "Very basic computer algebra on :MJR_MXP objects."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COMBE :MJR_MXP)
 :components ((:file "use-cas"))
)
