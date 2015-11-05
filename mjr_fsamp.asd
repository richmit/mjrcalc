(defsystem
 "mjr_fsamp"
 :description "Data sets on QUADrilateral (rectilinear) grids."
 :version "1446689596"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_VVEC :MJR_NUMU :MJR_COMBC :MJR_ARR :MJR_UTIL
                        :MJR_DQUAD :MJR_DSIMP :MJR_POLY)
 :components ((:file "pre-fsamp"))
)
