(defsystem
 "mjr_pov"
 :description "Write Povray files!"
 :version "1441565463"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VEC :MJR_ARR :MJR_GEOM :MJR_COLOR :MJR_ANNOT :MJR_UTIL :MJR_DQUAD :MJR_DSIMP)
 :components ((:file "use-pov"))
)
