(defsystem
 "mjr_intrp"
 :description "Polynomial interpolation."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_CMP :MJR_VVEC)
 :components ((:file "use-intrp"))
)
