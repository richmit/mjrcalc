(defsystem
 "mjr_nleqm"
 :description "Multiple Non-linear EQuation root location."
 :version "1446689596"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_EPS :MJR_VEC :MJR_MAT :MJR_UTIL
                        :MJR_COMBC)
 :components ((:file "use-nleqm"))
)
