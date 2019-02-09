(defsystem
 "mjr_colorize"
 :description "Interface for :MJR_COLORIZED and :MJR_COLORIZER."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COLORIZED :MJR_COLORIZER)
 :components ((:file "use-colorize"))
)
