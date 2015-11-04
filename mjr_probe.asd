(defsystem
 "mjr_probe"
 :description "Empirical probability distriubtions."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_PRNG :MJR_QMESH)
 :components ((:file "use-probe"))
)
