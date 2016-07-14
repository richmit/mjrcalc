(defsystem
 "mjr_ndiff"
 :description "Numerical differentiation."
 :version "1468113368"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_POLY :MJR_INTRP)
 :components ((:file "use-ndiff"))
)
