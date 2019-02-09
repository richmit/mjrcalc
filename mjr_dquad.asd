(defsystem
 "mjr_dquad"
 :description "Data sets on QUADrilateral (rectilinear) grids."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VVEC :MJR_COMBC :MJR_ARR :MJR_UTIL :MJR_COLORIZE :MJR_COLOR :MJR_ANNOT)
 :components ((:file "use-dquad"))
)
