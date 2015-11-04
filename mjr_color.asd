(defsystem
 "mjr_color"
 :description "Color theory (color space conversions and computations)."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_EPS :MJR_CMP :MJR_UTIL)
 :components ((:file "use-color"))
)
