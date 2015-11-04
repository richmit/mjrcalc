(defsystem
 "mjr_colorize"
 :description "Interface for :MJR_COLORIZED and :MJR_COLORIZER."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COLORIZED :MJR_COLORIZER)
 :components ((:file "use-colorize"))
)
