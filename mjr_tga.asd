(defsystem
 "mjr_tga"
 :description "Read and write TGA files."
 :version "1441149856"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_COLOR :MJR_ANNOT :MJR_DQUAD)
 :components ((:file "use-tga"))
)
