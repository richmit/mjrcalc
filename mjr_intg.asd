(defsystem
 "mjr_intg"
 :description "Numerical integration of univariate real functions."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_VVEC :MJR_UTIL :MJR_PRNG :MJR_EPS :MJR_MXP)
 :components ((:file "use-intg"))
)
