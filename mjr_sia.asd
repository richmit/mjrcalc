(defsystem
 "mjr_sia"
 :description "Simple Interval Arithmetic (with guess)"
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_NUMU)
 :components ((:file "lib-sia"))
)
