(defsystem
 "mjr_probe"
 :description "Empirical probability distriubtions."
 :version "1549598006"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_PRNG :MJR_PWF)
 :components ((:file "use-probe"))
)
