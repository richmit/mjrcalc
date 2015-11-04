(defsystem
 "mjr_ndiff"
 :description "Numerical differentiation."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_POLY :MJR_INTRP)
 :components ((:file "use-ndiff"))
)
