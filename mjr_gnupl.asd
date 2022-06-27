(defsystem
 "mjr_gnupl"
 :description "Plotting DQUADs, DSIMPs, and RPTREEs with GNUPlot."
 :version "1656293669"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_UTIL :MJR_COLOR :MJR_DQUAD :MJR_DSIMP :MJR_ANNOT :MJR_VVEC :MJR_ARR)
 :components ((:file "use-gnupl"))
)
