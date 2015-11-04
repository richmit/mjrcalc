(defsystem
 "mjr_stats"
 :description "Statistics: Averages, histograms, sub-samples, simple linear regression."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_UTIL :MJR_VVEC :MJR_QMESH)
 :components ((:file "use-stats"))
)
