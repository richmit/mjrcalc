(defsystem
 "mjr_prob"
 :description "Augments and supports :MJR_PROBAU."
 :version "1439525372"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_COMBE :MJR_CMP :MJR_CHK :MJR_PRNG :MJR_PROBU)
 :components ((:file "use-prob"))
)
