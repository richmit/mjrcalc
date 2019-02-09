(defsystem
 "mjr_colorizer"
 :description "Colorization of continuous spaces."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COLOR :MJR_NUMU :MJR_UTIL)
 :components ((:file "use-colorizer"))
)
