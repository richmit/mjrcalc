(defsystem
 "mjr_probau"
 :description "Balls And Urns probability distributions."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_COMBE :MJR_PROBU :MJR_PROB)
 :components ((:file "use-probau"))
)
