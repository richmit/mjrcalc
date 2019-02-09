(defsystem
 "mjr_optm"
 :description "Multivariate function OPTimization."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_COMBC :MJR_VEC :MJR_MAT :MJR_UTIL)
 :components ((:file "use-optm"))
)
