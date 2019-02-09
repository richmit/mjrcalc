(defsystem
 "mjr_tga"
 :description "Read and write TGA files."
 :version "1548900802"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COLOR :MJR_ANNOT :MJR_DQUAD)
 :components ((:file "use-tga"))
)
