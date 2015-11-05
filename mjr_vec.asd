(defsystem
 "mjr_vec"
 :description "Mathematical vectors."
 :version "1446689596"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_EPS :MJR_CHK :MJR_UTIL :MJR_NUMU
                        :MJR_VVEC)
 :components ((:file "use-vec"))
)
