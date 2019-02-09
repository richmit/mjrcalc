(defsystem
 "mjrcalc"
 :description "META package depending on every *MJRCALC*.package -- load this to load them all!"
 :version "1549729851"
 :author "Mitch Richling <https://www.mitchr.me/>"
 :licence "See the BSD-style license in LICENSE.TXT"
 :defsystem-depends-on (:MJR_A :MJR_ANNOT :MJR_ARR :MJR_CAS :MJR_CHAR :MJR_CHEM :MJR_CHK :MJR_CMP :MJR_COLOR :MJR_COLORIZE :MJR_COLORIZED :MJR_COLORIZER :MJR_COMBC
                        :MJR_COMBE :MJR_CONST :MJR_DATE :MJR_DFT :MJR_DQUAD :MJR_DSIMP :MJR_EE :MJR_EPS :MJR_FSAMP :MJR_GEO :MJR_GEOM :MJR_GNUPL :MJR_IA :MJR_INTG
                        :MJR_INTRP :MJR_INTU :MJR_MAT :MJR_MATT :MJR_MXP :MJR_NDIFF :MJR_NLEQ :MJR_NLEQM :MJR_NUMU :MJR_ODE :MJR_OPT :MJR_OPTM :MJR_PERM :MJR_POLY
                        :MJR_POLYGFP :MJR_POV :MJR_PRIME :MJR_PRNG :MJR_PROB :MJR_PROBAU :MJR_PROBE :MJR_PROBU :MJR_PWF :MJR_RTRIG :MJR_STATS :MJR_STRING :MJR_SVG
                        :MJR_TGA :MJR_UNITS :MJR_UTIL :MJR_VEC :MJR_VTK :MJR_VVEC :MJR_GFP :MJR_GPOLY :MJR_META :MJR_SIA :MJR_QMCI)
)
