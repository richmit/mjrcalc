(defsystem
 "mjr_prng"
 :description "Uniform random deviate generators."
 :version "1468113368"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_ARR)
 :components ((:file "use-prng"))
)
