(defsystem
 "mjr_numu"
 :description "Numerical utilities."
 :version "1441685900"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_CHK :MJR_UTIL :MJR_VVEC)
 :components ((:file "use-numu"))
)
