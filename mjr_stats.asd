(defsystem
 "mjr_stats"
 :description "Statistics: Averages, histograms, sub-samples, simple linear regression."
 :version "1549598006"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_UTIL :MJR_VVEC :MJR_PWF)
 :components ((:file "use-stats"))
)
