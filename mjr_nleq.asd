(defsystem
 "mjr_nleq"
 :description "Non-linear equation root finding."
 :version "1425518614"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_EPS :MJR_CHK :MJR_CMP :MJR_MXP :MJR_CAS)
 :components ((:file "use-nleq"))
)
