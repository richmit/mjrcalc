(defsystem
 "mjr_prime"
 :description "Computational Number Theory."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_INTU :MJR_PRNG)
 :components ((:file "use-prime"))
)
