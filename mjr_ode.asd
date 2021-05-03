(defsystem
 "mjr_ode"
 :description "ODE (Ordinary Differential Equation) IVP (Initial Value Problem) solvers."
 :version "1443928638"
 :author "Mitch Richling <http://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_NUMU :MJR_VVEC :MJR_CHK :MJR_VEC :MJR_DQUAD :MJR_UTIL)
 :components ((:file "use-ode"))
)
