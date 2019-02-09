(defsystem
 "mjr_mat"
 :description "Matrix math library."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VEC :MJR_CMP :MJR_EPS :MJR_NUMU :MJR_UTIL :MJR_POLY :MJR_PERM :MJR_VVEC :MJR_ARR)
 :components ((:file "use-mat"))
)
