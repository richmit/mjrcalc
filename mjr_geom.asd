(defsystem
 "mjr_geom"
 :description "Computational Geometry."
 :version "1468113368"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_CMP :MJR_VEC :MJR_COMBE :MJR_MAT :MJR_ARR
                        :MJR_NUMU)
 :components ((:file "use-geom"))
)
