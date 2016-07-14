(defsystem
 "mjr_const"
 :description "Physical constants -- with units."
 :version "1468113368"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_UNITS :MJR_STRING :MJR_IA)
 :components ((:file "use-const"))
)
