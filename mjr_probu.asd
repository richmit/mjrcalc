(defsystem
 "mjr_probu"
 :description "Computations on PDFs (Probability Distribution Functions)"
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_INTG :MJR_PRNG :MJR_NUMU)
 :components ((:file "use-probu"))
)
