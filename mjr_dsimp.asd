(defsystem
 "mjr_dsimp"
 :description "Data sets on SIMPlicial complexes."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VVEC :MJR_DQUAD :MJR_UTIL :MJR_COLOR :MJR_ARR :MJR_VEC :MJR_GEOM :MJR_UTIL :MJR_ANNOT)
 :components ((:file "use-dsimp"))
)
