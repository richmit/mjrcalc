(defsystem
 "mjr_sia"
 :description "Simple Interval Arithmetic (with guess)"
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_NUMU)
 :components ((:file "lib-sia"))
)
