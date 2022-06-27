(defsystem
 "mjr_ply3d"
 :description "Create PLY3D files."
 :version "1656293669"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_DSIMP :MJR_COLOR :MJR_ANNOT)
 :components ((:file "use-ply3d"))
)
