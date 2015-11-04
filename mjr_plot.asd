(defsystem
 "mjr_plot"
 :description "Plotting with backend interface to GNUPlot."
 :version "1438713726"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_POLY :MJR_UTIL :MJR_IMG :MJR_COLOR :MJR_ANNOT :MJR_VVEC :MJR_MXP :MJR_COMBC :MJR_ARR)
 :components ((:file "use-plot"))
)
