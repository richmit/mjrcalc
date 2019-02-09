(defsystem
 "mjr_fsamp"
 :description "Sample functions and store structured data."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VVEC :MJR_NUMU :MJR_COMBC :MJR_GEOM :MJR_ARR :MJR_UTIL :MJR_DQUAD :MJR_DSIMP :MJR_POLY)
 :components ((:file "use-fsamp"))
)
