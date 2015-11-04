(defsystem
 "mjr_ia"
 :description "User interface wrapper for :MJR_SIA."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_SIA :MJR_CMP)
 :components ((:file "use-ia"))
)
