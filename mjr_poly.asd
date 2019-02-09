(defsystem
 "mjr_poly"
 :description "Univariate Polynomials over R or C."
 :version "1549330168"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NLEQ :MJR_NUMU :MJR_CMP :MJR_CHK :MJR_PRIME :MJR_VEC :MJR_PRNG :MJR_COMBE :MJR_INTRP :MJR_COMBC :MJR_VVEC :MJR_GPOLY :MJR_EPS)
 :components ((:file "use-poly"))
)
