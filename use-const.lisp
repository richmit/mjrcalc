;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-const.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Physical constants -- with units.@EOL
;; @std       Common Lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1997-2002,2004-2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_CONST
  (:USE :COMMON-LISP
        :MJR_UNITS
        :MJR_STRING
        :MJR_IA)
  (:DOCUMENTATION "Brief: Physical constants -- with units.;")
  (:EXPORT #:mjr_const_help
           #:mjr_const_search))

(in-package :MJR_CONST)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_const_help ()
  "Help for MJR_CONST:  Physical constants -- with units."
  (documentation 'mjr_const_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_const_search (search-string &key units return-as-interval suppress-ambiguous-results prefix-search case-sensitive)
  "Search for physical constant data.
The return depends upon the search results:
  * Search results (one hit)       --  (values constant_value absolute_error units description)
                                        The constant_value number, interval, or canonized  unit expression
                                        The absolute_error will be a NUMBER or NIL (when the constant_value is exact)
                                        The units will be a STRING or canonized (see use-units.lisp)
                                        The description will be a single line STRING
  * Search finds nothing            --  NIL
  * Search results (multiple hits)  --  (values NIL number_of_hits list_of_matches)
                                        Never get here when :SUPPRESS-AMBIGUOUS-RESULTS is non-NIL
                                        Most other options ignored in this case.
Options
  * UNITS .................... convert to given units.  If 1, then units will be canonized.  (see: use-units.lisp)
                                   Incompatible with :RETURN-AS-INTERVAL.
  * RETURN-AS-INTERVAL ........... if non-NIL, express value as interval.  Otherwise as number.  See  (see ai.lisp)
                                   if one of :m, :ae, :fe, :pe, :s, :n; then corresponding interval format will be used
                                   Any other non-NIL value will yield an :ae format.
  * SUPPRESS-AMBIGUOUS-RESULTS ... if non-NIL, return first match.  Otherwise return list of matches.
  * PREFIX-SEARCH ................ if non-NIL, match first part of string.  Otherwise do sub-string search.
  * CASE-SENSITIVE ............... if non-NIL, matches are case sensitive.  Otherwise they are case insensitive.

If SEARCH-STRING. ends in a colon (:), then :SUPPRESS-AMBIGUOUS-RESULTS and :PREFIX-SEARCH are turned on."
  (let* ((const-list
          ;; Value                                Uncertainty           Unit                 Description
          (list
           (list 6378137L0                         nil                  "m"                  "Earth WGS-84_u2004 major (equatorial) radius")
           (list 6356752.314245L0                  nil                  "m"                  "Earth WGS-84_u2004 minor (polar) radius")
           (list -104.0L0                          nil                  "m"                  "Earth sea level min (EGM96_u2004 geoid & WGS-84_u2004 ellipsoid)")
           (list 85.0L0                            nil                  "m"                  "Earth sea level max (EGM96_u2004 geoid & WGS-84_u2004 ellipsoid)")
           (list 13.6L0                            0L0                  "m"                  "Earth (geographical) north pole elevation above ellipsoid (WGS-84)")
           (list 29.5L0                            0L0                  "m"                  "Earth (geographical) south pole elevation above ellipsoid (WGS-84)")
           (list 6384.4L0                          0L0                  "km"                 "Earth maximum radius (Chimborazo volcano in Ecuador")
           (list 384399L0                          0L0                  "km"                 "Earth Moon Distance (center-center)")
           (list 7.3477L22                         0L0                  "kg"                 "Moon Mass")
           (list 5.9742e24                         0L0                  "kg"                 "Earth Mass")
           (list 1737.1L0                          0L0                  "km"                 "Mean Moon Radius")
           (list 152097701                         0L0                  "km"                 "Earth orbit aphelion (far)")
           (list 147098074                         0L0                  "km"                 "Earth orbit perihelion (close)")
           (list 149597887.5                       0L0                  "km"                 "Earth orbit semi-major axis")
           (list 29.783L0                          0L0                  "km/s"               "Earth orbit average speed")
           (list 363104L0                          0L0                  "km"                 "Moon orbit perigee (close)")
           (list 405696L0                          0L0                  "km"                 "Moon orbit apogee (far)")
           (list 384399L0                          0L0                  "km"                 "Moon orbit semi-major axis")
           (list 27.321582L0                       0L0                  "days"               "Moon orbit orbital period")
           (list 1.022L0                           0L0                  "km/s"               "Moon orbit average speed")
           ;; The following are from the NIST (pulled 2008)
           (list 8.314472L0                        0.00000015L0         "J/(K*mol)"          "R: molar gas constant")
           (list 6.64465620L-27                    0.00000033L-27       "kg"                 "m_alpha: alpha particle mass")
           (list 1.00001498L-10                    0.00000090L-10       "m"                  "Angstrom star")
           (list 1.660538782L-27                   0.000000083L-27      "kg"                 "atomic mass constant")
           (list 3.206361533L-53                   0.000000081L-53      "C^3*m^3/J^2"        "atomic unit of 1st hyperpolarizablity")
           (list 6.23538095L-65                    0.00000031L-65       "C^4*m^4/J^3"        "atomic unit of 2nd hyperpolarizablity")
           (list 1.054571628L-34                   0.000000053L-34      "J*s"                "atomic unit of action")
           (list 1.602176487L-19                   0.000000040L-19      "C"                  "atomic unit of charge")
           (list 1.081202300L12                    0.000000027L12       "C/m^3"              "atomic unit of charge density")
           (list 6.62361763L-3                     0.00000017L-3        "A"                  "atomic unit of current")
           (list 8.47835281L-30                    0.00000021L-30       "C*m"                "atomic unit of electric dipole momentum")
           (list 5.14220632L11                     0.00000013L11        "V/m"                "atomic unit of electric field")
           (list 9.71736166L21                     0.00000024L21        "V/m^2"              "atomic unit of electric field gradient")
           (list 1.6487772536L-41                  0.0000000034L-41     "C^2*m^2/J"          "atomic unit of electric polarizablity")
           (list 27.21138386L0                     0.00000068L0         "V"                  "atomic unit of electric potential")
           (list 4.48655107L-40                    0.00000011L-40       "C*m^2"              "atomic unit of electric quadrupole momentum")
           (list 4.35974394L-18                    0.00000022L-18       "J"                  "atomic unit of energy")
           (list 8.23872206L-8                     0.00000041L-8        "N"                  "atomic unit of force")
           (list 0.52917720859L-10                 0.00000000036L-10    "m"                  "atomic unit of length")
           (list 1.854801830L-23                   0.000000046L-23      "J/T"                "atomic unit of magnetic dipole momentum")
           (list 2.350517382L5                     0.000000059L5        "T"                  "atomic unit of magnetic flux density")
           (list 7.891036433L-29                   0.000000027L-29      "J/T^2"              "atomic unit of magnetizability")
           (list 9.10938215L-31                    0.00000045L-31       "kg"                 "atomic unit of mass")
           (list 1.992851565L-24                   0.000000099L-24      "kg*m/s"             "atomic unit of momentum")
           (list 1.112650056L-10                   nil                  "F/m"                "atomic unit of permittivity")
           (list 2.1876912541L6                    0.0000000015L6       "m/s"                "atomic unit of velocity")
           (list 6.02214179L23                     0.00000030L23        "1/mol"              "N_a: Avogadro constant")
           (list 927.400915L-26                    0.000023L-26         "J/T"                "Bohr magneton")
           (list 0.52917720859L-10                 0.00000000036L-10    "m"                  "Bohr radius")
           (list 1.3806504L-23                     0.0000024L-23        "J/K"                "k: Boltzmann constant")
           (list 376.730313461L0                   nil                  "ohm"                "Z_0: characteristic impedance of vacuum")
           (list 2.8179402894L-15                  0.0000000058L-15     "m"                  "classical electron radius")
           (list 2.4263102175L-12                  0.0000000033L-12     "m"                  "Compton wavelength")
           (list 7.7480917004L-5                   0.0000000053L-5      "S"                  "conductance quantum")
           (list 483597.9L9                        nil                  "Hz/V"               "conventional value of Josephson constant")
           (list 25812.807L0                       nil                  "ohm"                "conventional value of von Klitzing constant")
           (list 1.00207699L-13                    0.00000028L-13       "m"                  "Cu x unit")
           (list 3.34358320L-27                    0.00000017L-27       "kg"                 "m_d: deuteron mass")
           (list 2.1402L-15                        0.0028L-15           "m"                  "deuteron rms charge radius")
           (list 8.854187817L-12                   nil                  "F/m"                "eps_0: electric constant")
           (list -1.758820150L11                   0.000000044L11       "C/kg"               "electron charge to mass quotient")
           (list 1.760859770L11                    0.000000044L11       "1/(s*T)"            "electron gyromagnetic ratio")
           (list -928.476377L-26                   0.000023L-26         "J/T"                "m_e: electron magnetic momentum")
           (list 1.15965218111L-3                  0.00000000074L-3     ""                   "electron magnetic momentum anomaly")
           (list 9.10938215L-31                    0.00000045L-31       "kg"                 "electron mass")
           (list 1.602176487L-19                   0.000000040L-19      "J"                  "electron volt")
           (list 1.073544188L-9                    0.000000027L-9       "u"                  "electron volt-atomic mass unit relationship")
           (list 3.674932540L-2                    0.000000092L-2       "E_h"                "electron volt-hartree relationship")
           (list 2.417989454L14                    0.000000060L14       "Hz"                 "electron volt-hertz relationship")
           (list 8.06554465L5                      0.00000020L5         "1/m"                "electron volt-inverse meter relationship")
           (list 1.602176487L-19                   0.000000040L-19      "J"                  "electron volt-joule relationship")
           (list 1.1604505L4                       0.0000020L4          "K"                  "electron volt-kelvin relationship")
           (list 1.782661758L-36                   0.000000044L-36      "kg"                 "electron volt-kilogram relationship")
           (list 1.602176487L-19                   0.000000040L-19      "C"                  "e: elementary charge")
           (list 96485.3399L0                      0.0024L0             "C/mol"              "F: Faraday constant")
           (list 96485.3401L0                      0.0048L0             "C_90/mol"           "Faraday constant for conventional electric current")
           (list 1.16637L-5                        0.00001L-5           "1/GeV^2"            "Fermi coupling constant")
           (list 7.2973525376L-3                   0.0000000050L-3      ""                   "fine-structure constant")
           (list 3.74177118L-16                    0.00000019L-16       "W*m^2"              "first radiation constant")
           (list 1.191042759L-16                   0.000000059L-16      "W*m^2/sr"           "first radiation constant for spectral radiance")
           (list 2.9212622986L-8                   0.0000000042L-8      "u"                  "hartree-atomic mass unit relationship")
           (list 27.21138386L0                     0.00000068L0         "eV"                 "hartree-electron volt relationship")
           (list 4.35974394L-18                    0.00000022L-18       "J"                  "Hartree energy")
           (list 6.579683920722L15                 0.000000000044L15    "Hz"                 "hartree-hertz relationship")
           (list 2.194746313705L7                  0.000000000015L7     "1/m"                "hartree-inverse meter relationship")
           (list 4.35974394L-18                    0.00000022L-18       "J"                  "hartree-joule relationship")
           (list 3.1577465L5                       0.0000055L5          "K"                  "hartree-kelvin relationship")
           (list 4.85086934L-35                    0.00000024L-35       "kg"                 "hartree-kilogram relationship")
           (list 5.00641192L-27                    0.00000025L-27       "kg"                 "m_h: helion mass")
           (list 4.4398216294L-24                  0.0000000064L-24     "u"                  "hertz-atomic mass unit relationship")
           (list 4.13566733L-15                    0.00000010L-15       "eV"                 "hertz-electron volt relationship")
           (list 3.335640951L-9                    nil                  "1/m"                "hertz-inverse meter relationship")
           (list 6.62606896L-34                    0.00000033L-34       "J"                  "hertz-joule relationship")
           (list 4.7992374L-11                     0.0000084L-11        "K"                  "hertz-kelvin relationship")
           (list 7.37249600L-51                    0.00000037L-51       "kg"                 "hertz-kilogram relationship")
           (list 12906.4037787L0                   0.0000088L0          "ohm"                "inverse of conductance quantum")
           (list 483597.891L9                      0.012L9              "Hz/V"               "Josephson constant")
           (list 6.70053641L9                      0.00000033L9         "u"                  "joule-atomic mass unit relationship")
           (list 6.24150965L18                     0.00000016L18        "eV"                 "joule-electron volt relationship")
           (list 2.29371269L17                     0.00000011L17        "E_h"                "joule-hartree relationship")
           (list 1.509190450L33                    0.000000075L33       "Hz"                 "joule-hertz relationship")
           (list 5.03411747L24                     0.00000025L24        "1/m"                "joule-inverse meter relationship")
           (list 7.242963L22                       0.000013L22          "K"                  "joule-kelvin relationship")
           (list 1.112650056L-17                   nil                  "kg"                 "joule-kilogram relationship")
           (list 9.251098L-14                      0.000016L-14         "u"                  "kelvin-atomic mass unit relationship")
           (list 8.617343L-5                       0.000015L-5          "eV"                 "kelvin-electron volt relationship")
           (list 3.1668153L-6                      0.0000055L-6         "E_h"                "kelvin-hartree relationship")
           (list 2.0836644L10                      0.0000036L10         "Hz"                 "kelvin-hertz relationship")
           (list 69.50356L0                        0.00012L0            "1/m"                "kelvin-inverse meter relationship")
           (list 1.3806504L-23                     0.0000024L-23        "J"                  "kelvin-joule relationship")
           (list 1.5361807L-40                     0.0000027L-40        "kg"                 "kelvin-kilogram relationship")
           (list 6.02214179L26                     0.00000030L26        "u"                  "kilogram-atomic mass unit relationship")
           (list 5.60958912L35                     0.00000014L35        "eV"                 "kilogram-electron volt relationship")
           (list 1.356392733L50                    0.000000068L50       "Hz"                 "kilogram-hertz relationship")
           (list 4.52443915L41                     0.00000023L41        "1/m"                "kilogram-inverse meter relationship")
           (list 8.987551787L16                    nil                  "J"                  "kilogram-joule relationship")
           (list 6.509651L39                       0.000011L39          "K"                  "kilogram-kelvin relationship")
           (list 1.3310250394L-15                  0.0000000019L-15     "u"                  "inverse meter-atomic mass unit relationship")
           (list 1.239841875L-6                    0.000000031L-6       "eV"                 "inverse meter-electron volt relationship")
           (list 299792458L0                       nil                  "Hz"                 "inverse meter-hertz relationship")
           (list 1.986445501L-25                   0.000000099L-25      "J"                  "inverse meter-joule relationship")
           (list 1.4387752L-2                      0.0000025L-2         "K"                  "inverse meter-kelvin relationship")
           (list 2.21021870L-42                    0.00000011L-42       "kg"                 "inverse meter-kilogram relationship")
           (list 543.102064L-12                    0.000014L-12         "m"                  "lattice parameter of silicon")
           (list 2.6867774L25                      0.0000047L25         "1/m^3"              "Loschmidt constant (273.15 K, 101.325 kPa)")
           (list 12.566370614L-7                   nil                  "N/A^2"              "mu_0: magnetic constant")
           (list 2.067833667L-15                   0.000000052L-15      "Wb"                 "magnetic flux quantum")
           (list 12L-3                             nil                  "kg/mol"             "molar mass of carbon-12")
           (list 22.710981L-3                      0.000040L-3          "m^3/mol"            "molar volume of ideal gas (273.15 K, 100 kPa)")
           (list 22.413996L-3                      0.000039L-3          "m^3/mol"            "V_m: molar volume of ideal gas (273.15 K, 101.325 kPa)")
           (list 12.0588349L-6                     0.0000011L-6         "m^3/mol"            "molar volume of silicon")
           (list 1.00209955L-13                    0.00000053L-13       "m"                  "Mo x unit")
           (list 11.73444104L-15                   0.00000030L-15       "m"                  "muon Compton wavelength")
           (list -4.49044786L-26                   0.00000016L-26       "J/T"                "muon magnetic momentum")
           (list 1.16592069L-3                     0.00000060L-3        ""                   "muon magnetic momentum anomaly")
           (list 1.88353130L-28                    0.00000011L-28       "kg"                 "m_mu: muon mass")
           (list 1.674927211L-27                   0.000000084L-27      "kg"                 "m_n: neutron mass")
           (list 6.67428L-11                       0.00067L-11          "m^3/(kg*s^2)"       "G: Newtonian constant of gravitation")
           (list 5.05078324L-27                    0.00000013L-27       "J/T"                "nuclear magneton")
           (list 6.62606896L-34                    0.00000033L-34       "J*s"                "h: Planck constant")
           (list 1.616252L-35                      0.000081L-35         "m"                  "Planck length")
           (list 2.17644L-8                        0.00011L-8           "kg"                 "Planck mass")
           (list 1.416785L32                       0.000071L32          "K"                  "Planck temperature")
           (list 5.39124L-44                       0.00027L-44          "s"                  "Planck time")
           (list 9.57883392L7                      0.00000024L7         "C/kg"               "proton charge to mass quotient")
           (list 1.3214098446L-15                  0.0000000019L-15     "m"                  "proton Compton wavelength")
           (list 2.675222099L8                     0.000000070L8        "1/(s*T)"            "proton gyromagnetic ratio")
           (list 1.410606662L-26                   0.000000037L-26      "J/T"                "proton magnetic momentum")
           (list 1.672621637L-27                   0.000000083L-27      "kg"                 "m_p: proton mass")
           (list 0.8768L-15                        0.0069L-15           "m"                  "proton rms charge radius")
           (list 3.6369475199L-4                   0.0000000050L-4      "m^2/s"              "quantum of circulation")
           (list 7.273895040L-4                    0.000000010L-4       "m^2/s"              "quantum of circulation times 2")
           (list 10973731.568527L0                 0.000073L0           "1/m"                "R_infinity: Rydberg constant")
           (list -1.1517047L0                      0.0000044L0          ""                   "Sackur-Tetrode constant (1 K, 100 kPa)")
           (list -1.1648677L0                      0.0000044L0          ""                   "Sackur-Tetrode constant (1 K, 101.325 kPa)")
           (list 1.4387752L-2                      0.0000025L-2         "m K"                "second radiation constant")
           (list 2.037894730L8                     0.000000056L8        "1/(s*T)"            "shielded helion gyromagnetic ratio")
           (list -1.074552982L-26                  0.000000030L-26      "J/T"                "shielded helion magnetic momentum")
           (list 2.675153362L8                     0.000000073L8        "1/(s*T)"            "shielded proton gyromagnetic ratio")
           (list 299792458                         nil                  "m/s"                "c: speed of light in vacuum")
           (list 9.80665L0                         nil                  "m/s^2"              "g: standard acceleration of gravity")
           (list 101325L0                          nil                  "Pa"                 "atm: standard atmosphere")
           (list 5.670400L-8                       0.000040L-8          "W/(m^2*K^4)"        "Stefan-Boltzmann constant")
           (list 0.69772L-15                       0.00011L-15          "m"                  "tau Compton wavelength")
           (list 3.16777L-27                       0.00052L-27          "kg"                 "tau mass")
           (list 0.6652458558L-28                  0.0000000027L-28     "m^2"                "Thomson cross section")
           (list 5.00735588L-27                    0.00000025L-27       "kg"                 "m_t: triton mass")
           (list 1.660538782L-27                   0.000000083L-27      "kg"                 "m_u: unified atomic mass unit")
           (list 25812.807557L0                    0.000018L0           "ohm"                "von Klitzing constant")
           (list 0.22255L0                         0.00056L0            ""                   "weak mixing angle")
           (list 5.878933L10                       0.000010L10          "Hz/K"               "b': Wien frequency displacement law constant")
           (list 2.8977685L-3                      0.0000051L-3         "m*K"                "b: Wien wavelength displacement law constant")))
         (end-colon                  (and (position #\: search-string) (= (1- (length search-string)) (position #\: search-string))))
         (prefix-search              (or prefix-search end-colon))
         (suppress-ambiguous-results (or suppress-ambiguous-results end-colon))
         (return-as-interval         (if (and return-as-interval (not (member return-as-interval (list :m :ae :fe :pe :s :n)))) :ae return-as-interval))
         (results                    (loop for c in const-list
                                           for good-match = (if prefix-search
                                                                (mjr_string_starts-with search-string (fourth c) :case-sensitive case-sensitive)
                                                                (search search-string (fourth c) :test (if case-sensitive #'string-equal #'string=)))
                                           when good-match
                                           collect c
                                           until (and good-match suppress-ambiguous-results)))
         (number-results (length results)))
    (if (and return-as-interval units) (error "mjr_const_search: :RETURN-AS-INTERVAL :UNITS can not both be non-NIL"))
    (if (= 1 number-results)
        (let ((val (first  (first results)))
              (err (second (first results)))
              (unt (third  (first results)))
              (str (fourth (first results))))
          (values (cond ((and err return-as-interval) (mjr_ia_convert (list ':ae val err) return-as-interval))
                        (units                        (mjr_units_convert val unt units))
                        ('t                           val))
                  err
                  (if units
                      (mjr_units_canonization units)
                      unt)
                  str))
        (values nil number-results (mapcar #'fourth results)))))
