(defsystem
 "mjr_matt"
 :description "Generate and compute with various test matrices."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_MAT :MJR_VEC :MJR_CMP :MJR_ARR :MJR_COMBE :MJR_PRNG :MJR_NUMU)
 :components ((:file "use-matt"))
)
