;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:utf-8; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      use-units.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1997,1998,2004,2011,2013 by Mitch Richling.  All rights reserved.
;; @brief     Unit conversion tool.@EOL
;; @Std       Common Lisp
;;
;;            This package provides basic unit conversions and computations.  It takes an inherently algebraic approach utilizing
;;            LISPY expression rewriting and other symbolic manipulation with a somewhat hybrid LISP-like implementation.
;;            
;;            TODO: Implement direct conversions between two units :si-not-other without using SI units
;;            TODO: Implement arithmetic on unit expressions -- +, -, *, and /.
;;            TODO: Detect simple non-linear units, and convert using the offset term -- i.e. temperature conversions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_UNITS
  (:USE :COMMON-LISP
        :MJR_STRING)
  (:DOCUMENTATION "Brief: Unit conversion tool.;")
  (:EXPORT #:mjr_units_find-unit  
           #:mjr_units_canonization            
           #:mjr_units_substitute               
           #:mjr_units_to-si-fundamental
           #:mjr_units_convert                     
           #:mjr_units_compatible                  
           ))

(in-package :MJR_UNITS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_find-unit (unit-to-find &key (print-ambiguous-warnings 't) (error-on-bad-lookup 't) (case-sensitive 't))
  "Searches unit database for UNIT-TO-FIND, and return info.
Returns:
  * If UNIT-TO-FIND is a number or string that looks like a number, then a number is returned
    If UNIT-TO-FIND is a string, then an error might occur during parsing
  * If UNIT-TO-FIND is a symbol or string that looks like a unit name or abbreviation, a list of unit information is returned
    Info list:  (list unit_tag (list name, abrv, type, fundamental or other units, non-linear, pfx, class))
    If nothing is found, and :ERROR-ON-BAD-LOOKUP is nil, then NIL is returned
    If nothing is found, and :ERROR-ON-BAD-LOOKUP is non-NIL, then an error occurs.
Options:
  * :CASE-SENSITIVE           if-non-NIL and UNIT-TO-FIND is a string, matches are case sensitive -- case insensitive otherwise
  * :PRINT-AMBIGUOUS-WARNINGS if non-NIL, then warnings will be printed to STDOUT if multiple unit matches are found."
;; :si-not-other -- non-SI units defined in terms of SI units.  Simple, one unit expression for unit def.  That unit must NOT be another :si-not-other unit.
;; :si-not-sib   -- non-SI units defined in terms of SI base units
;; :si-tertiary  -- SI units that are simply synonyms of an SI unit
  (let ((units
         (list ;;name                            symb               type                                 fundamental or other units                     non-linear          pfx class
          (list "meter"                          '("m")             "length"                             '(* "m")                                                0          't  :si-fundamental )
          (list "kilogram"                       '("kg")            "mass"                               '(* "kg")                                               0          nil :si-fundamental )
          (list "second"                         '("s" "sec")       "time"                               '(* "s")                                                0          't  :si-fundamental )
          (list "ampere"                         '("A")             "electric_current"                   '(* "A")                                                0          't  :si-fundamental )
          (list "mole"                           '("mol")           "amount_of_substance"                '(* "mol")                                              0          't  :si-fundamental )
          (list "candela"                        '("cd")            "luminous_intensity"                 '(* "cd")                                               0          't  :si-fundamental )
          (list "kelvin"                         '("K" "degK")      "temperature"                        '(* "K")                                                0          nil :si-fundamental )
          (list "hertz"                          '("Hz")            "frequency"                          '(/ "s")                                                0          't  :si-derived     )
          (list "radian"                         '("rad")           "plane_angle"                        '(/ "m" "m")                                            0          't  :si-derived     )
          (list "steradian"                      '("sr")            "solid_angle"                        '(/ (* "m" "m") (* "m" "m"))                            0          't  :si-derived     )
          (list "newton"                         '("N")             "force"                              '(/ (* "kg" "m") (* "s" "s"))                           0          't  :si-derived     )
          (list "pascal"                         '("Pa")            "pressure"                           '(/ "kg" (* "m" "s" "s"))                               0          't  :si-derived     )
          (list "joule"                          '("J")             "energy"                             '(/ (* "kg" "m" "m") (* "s" "s"))                       0          't  :si-derived     )
          (list "watt"                           '("W")             "power"                              '(/ (* "kg" "m" "m") (* "s" "s" "s"))                   0          't  :si-derived     )
          (list "coulomb"                        '("C")             "electric_charge"                    '(* "s" "A")                                            0          't  :si-derived     )
          (list "volt"                           '("V")             "voltage"                            '(/ (* "kg" "m" "m") (* "s" "s" "s" "A"))               0          't  :si-derived     )
          (list "farad"                          '("F")             "electric_capacitance"               '(/ (* "s" "s" "s" "s" "A" "A") (* "kg" "m" "m"))       0          't  :si-derived     )
          (list "ohm"                            '("ohm")           "electric_resistance"                '(/ (* "kg" "m" "m") (* "s" "s" "s" "A" "A"))           0          't  :si-derived     )
          (list "siemens"                        '("S")             "electrical_conductance"             '(/ (* "s" "s" "s" "A" "A") (* "kg" "m" "m"))           0          't  :si-derived     )
          (list "weber"                          '("Wb")            "magnetic_flux"                      '(/ (* "kg" "m" "m") (* "s" "s" "A"))                   0          't  :si-derived     )
          (list "tesla"                          '("T")             "magnetic_field_strength"            '(/ "kg" (* "s" "s" "A"))                               0          't  :si-derived     )
          (list "henry"                          '("H")             "inductance"                         '(/ (* "kg" "m" "m") (* "s" "s" "A" "A"))               0          't  :si-derived     )
          (list "lumen"                          '("lm")            "luminous_flux"                      '(* cd)                                                 0          't  :si-derived     )
          (list "lux"                            '("lx")            "illuminance"                        '(/ cd (* "m" "m"))                                     0          't  :si-derived     )
          (list "becquerel"                      '("Bq")            "radioactivity"                      '(/ "s")                                                0          't  :si-derived     )
          (list "gray"                           '("Gy")            "absorbed_dose"                      '(/ (* "m" "m") (* "s" "s"))                            0          't  :si-derived     )
          (list "sievert"                        '("Sv")            "equivalent_dose"                    '(/ (* "m" "m") (* "s" "s"))                            0          't  :si-derived     )
          (list "katal"                          '("kat")           "catalytic_activity"                 '(/ "mol" "s")                                          0          't  :si-derived     )
          (list "square_meter"                   nil                "area"                               '(* "m" "m")                                            0          't  :si-secondary   )
          (list "cubic_meter"                    nil                "volume"                             '(/ "m" "m" "m")                                        0          't  :si-secondary   )
          (list "meter_per_second"               nil                "velocity"                           '(/ "m" "s")                                            0          't  :si-secondary   )
          (list "cubic_meter_per_second"         nil                "volumetric_flow"                    '(/ (* "m" "m" "m") "s")                                0          't  :si-secondary   )
          (list "radian_per_second"              nil                "angular_velocity"                   '(/ "m" "s")                                            0          't  :si-secondary   )
          (list "meter_per_second_squared"       nil                "acceleration"                       '(/ "m" (* "s" "s"))                                    0          't  :si-secondary   )
          (list "meter_per_second_cubed"         nil                "jolt"                               '(/ "m" (* "s" "s" "s"))                                0          't  :si-secondary   )
          (list "meter_per_quartic_second"       nil                "snap"                               '(/ "m" (* "s" "s" "s" "s"))                            0          't  :si-secondary   )
          (list "newton_second"                  nil                "momentum"                           '(/ (* "m" "kg") (* "s"))                               0          't  :si-secondary   )
          (list "newton_meter_second"            nil                "angular_momentum"                   '(/ (* "m" "m" "kg") (* "s"))                           0          't  :si-secondary   )
          (list "newton_meter"                   nil                "torque"                             '(/ (* "m" "m" "kg") (* "s" "s"))                       0          't  :si-secondary   )
          (list "newton_per_second"              nil                "yank"                               '(/ (* "m" "kg") (* "s" "s" "s"))                       0          't  :si-secondary   )
          (list "reciprocal_meter"               nil                "wavenumber"                         '(/ "m")                                                0          't  :si-secondary   )
          (list "kilogram_per_square_meter"      nil                "area_density"                       '(/ "kg" (* "m" "m"))                                   0          't  :si-secondary   )
          (list "kilogram_per_cubic_meter"       nil                "density"                            '(/ "kg" (* "m" "m" "m"))                               0          't  :si-secondary   )
          (list "cubic_meter_per_kilogram"       nil                "specific_volume"                    '(/ (* "m" "m" "m") "kg")                               0          't  :si-secondary   )
          (list "mole_per_cubic_meter"           nil                "amount_of_substance_concentration"  '(/ "mol" (* "m" "m" "m"))                              0          't  :si-secondary   )
          (list "cubic_meter_per_mole"           nil                "molar_volume"                       '(/ (* "m" "m" "m") "mol")                              0          't  :si-secondary   )
          (list "joule_second"                   nil                "action"                             '(/ (* "m" "m" "kg") "s")                               0          't  :si-secondary   )
          (list "joule_per_kelvin"               nil                "heat_capacity"                      '(/ (* "m" "m" "kg") (* "s" "s" "K"))                   0          't  :si-secondary   )
          (list "joule_per_kelvin_mole"          nil                "molar_heat_capacity"                '(/ (* "m" "m" "kg") (* "s" "s" "K" "mol"))             0          't  :si-secondary   )
          (list "joule_per_kilogram_kelvin"      nil                "specific_heat_capacity"             '(/ (* "m" "m") (* "s" "s" "K"))                        0          't  :si-secondary   )
          (list "joule_per_mole"                 nil                "molar_energy"                       '(/ (* "m" "m" "kg") (* "s" "s" "mol"))                 0          't  :si-secondary   )
          (list "joule_per_kilogram"             nil                "specific_energy"                    '(/ (* "m" "m") (* "s" "s"))                            0          't  :si-secondary   )
          (list "joule_per_cubic_meter"          nil                "energy_density"                     '(/ "kg" (* "m" "s" "s"))                               0          't  :si-secondary   )
          (list "newton_per_meter"               nil                "surface_tension"                    '(/ "kg" (* "s" "s"))                                   0          't  :si-secondary   )
          (list "watt_per_square_meter"          nil                "irradiance"                         '(/ "kg" (* "s" "s" "s"))                               0          't  :si-secondary   )
          (list "watt_per_meter_kelvin"          nil                "thermal_conductivity"               '(/ (* "m" "kg") (* "s" "s" "s" "K"))                   0          't  :si-secondary   )
          (list "square_meter_per_second"        nil                "kinematic_viscosity"                '(/ (* "m" "m") "s")                                    0          't  :si-secondary   )
          (list "pascal_second"                  nil                "dynamic_viscosity"                  '(/ "kg" (* "m" "s"))                                   0          't  :si-secondary   )
          (list "coulomb_per_square_meter"       nil                "electric_displacement_field"        '(/ (* "s" "A") (* "m" "m"))                            0          't  :si-secondary   )
          (list "coulomb_per_cubic_meter"        nil                "electric_charge_density"            '(/ (* "s" "A") (* "m" "m" "m"))                        0          't  :si-secondary   )
          (list "ampere_per_square_meter"        nil                "electric_current_density"           '(/ "A" (* "m" "m"))                                    0          't  :si-secondary   )
          (list "siemens_per_meter"              nil                "conductivity"                       '(/ (* "s" "s" "s" "A" "A") (* "m" "m" "m" "kg"))       0          't  :si-secondary   )
          (list "siemens_square_meter_per_mole"  nil                "molar_conductivity"                 '(/ (* "s" "s" "s" "A" "A") (* "mol" "kg"))             0          't  :si-secondary   )
          (list "farad_per_meter"                nil                "permittivity"                       '(/ (* "s" "s" "s" "s" "A" "A") (* "m" "m" "m" "kg"))   0          't  :si-secondary   )
          (list "henry_per_meter"                nil                "permeability"                       '(/ (* "m" "kg") (* "s" "s" "A" "A"))                   0          't  :si-secondary   )
          (list "volt_per_meter"                 nil                "electric_field_strength"            '(/ (* "m" "kg") (* "s" "s" "s" "A"))                   0          't  :si-secondary   )
          (list "ampere_per_meter"               nil                "magnetic_field_strength"            '(/ "A" "m")                                            0          't  :si-secondary   )
          (list "candela_per_square_meter"       nil                "luminance"                          '(/ cd (* "m" "m"))                                     0          't  :si-secondary   )
          (list "coulomb_per_kilogram"           nil                "exposure"                           '(/ (* "s" "A") "kg")                                   0          't  :si-secondary   )
          (list "gray_per_second"                nil                "absorbed_dose_rate"                 '(/ (* "m" "m") (* "s" "s" "s"))                        0          't  :si-secondary   )
          (list "ohm_meter"                      nil                "resistivity"                        '(/ (* "m" "m" "m" "kg") (* "s" "s" "s" "A" "A"))       0          't  :si-secondary   )
          (list "gram"                           '("g" "gm")        "mass"                               '(/ "kg" 1000)                                          0          't  :si-tertiary    )
          (list "inch"                           '("in_us" "in")    "length"                             '(* "m" 127/5000)                                       0          nil :si-not-sib     )
          (list "nautical_mile"                  '("nm")            "length"                             '(* "m" 1852)                                           0          nil :si-not-sib     )
          (list "liter"                          '("l")             "volume"                             '(* "m" "m" "m" 1/1000)                                 0          't  :si-not-sib     )
          (list "cc"                             '("cc")            "volume"                             '(* "m" "m" "m" 1000000)                                0          nil :si-not-sib     )
          (list "us_teaspoon"                    '("tsp_us" "tsp")  "volume"                             '(* "m" "m" "m" 4.92892159L-6)                          0          nil :si-not-sib     )
          (list "us_acre"                        nil                "area"                               '(* "m" "m" 62726400000/15499969)                       0          nil :si-not-sib     )
          (list "pound_mass"                     '("lb_mass")       "mass"                               '(* "kg" 4.5359237L-1)                                  0          nil :si-not-sib     )
          (list "slug"                           nil                "mass"                               '(* "kg" 1.45939029L1)                                  0          nil :si-not-sib     )
          (list "gram_force"                     nil                "force"                              '(/ (* 9.80665L-3 "kg" "m") (* "s" "s"))                0          nil :si-not-sib     )
          (list "pound"                         '("lb")             "force"                              '(/ (* 4.44822162L0 "kg" "m") (* "s" "s"))              0          nil :si-not-sib     )
          (list "minute"                         '("min")           "time"                               '(* "s" 60)                                             0          nil :si-not-sib     )
          (list "hour"                           nil                "time"                               '(* "s" 3600)                                           0          nil :si-not-sib     )
          (list "day"                            nil                "time"                               '(* "s" 86400)                                          0          nil :si-not-sib     )
          (list "fortnight"                      nil                "time"                               '(* "s" 1209600)                                        0          nil :si-not-sib     )
          (list "mile_per_hour"                  '("mph")           "velocity"                           '(/ (* "m" 1397/3125) "s")                              0          nil :si-not-sib     )
          (list "kilometer_per_hour"             '("kph")           "velocity"                           '(/ (* "m" 5/18) "s")                                   0          nil :si-not-sib     )
          (list "celsius"                        '("degC")          "temperature"                        '(* "K" 1)                                              -5463/20   nil :si-not-sib     )
          (list "fahrenheit"                     '("degF")          "temperature"                        '(* "K" 9/5)                                            -45967/100 nil :si-not-sib     )
          (list "rankine"                        '("R" "degR")      "temperature"                        '(* "K" 9/5)                                            0          nil :si-not-sib     )
          (list "angular_revolution"             nil                "plane_angle"                        '(* 6.283185307179586d0 "rad")                          0          nil :si-not-sib     )
          (list "ounce_wt"                       '("oz_wt")         "force"                              '(* "pound" 1/16)                                       0          nil :si-not-other   )
          (list "us_foot"                        '("ft_us" "ft")    "length"                             '(* "inch" 12)                                          0          nil :si-not-other   )
          (list "us_yard"                        '("yd_us" "yd")    "length"                             '(* "inch" 36)                                          0          nil :si-not-other   )
          (list "us_fathom"                      nil                "length"                             '(* "inch" 72)                                          0          nil :si-not-other   )
          (list "us_rod"                         nil                "length"                             '(* "inch" 198)                                         0          nil :si-not-other   )
          (list "us_chain"                       nil                "length"                             '(* "inch" 792)                                         0          nil :si-not-other   )
          (list "us_furlong"                     nil                "length"                             '(* "inch" 7920)                                        0          nil :si-not-other   )
          (list "us_mile"                        '("mi_us" "mi" )   "length"                             '(* "inch" 63360)                                       0          nil :si-not-other   )
          (list "degree"                         '("deg")           "plane_angle"                        '(* "angular_revolution"  1/360    )                    0          nil :si-not-other   )
          (list "gradians"                       '("grad")          "plane_angle"                        '(* "angular_revolution"  1/400    )                    0          nil :si-not-other   )
          (list "angular_tmil"                   nil                "plane_angle"                        '(* "angular_revolution"  1/6283   )                    0          nil :si-not-other   )
          (list "angular_mil"                    nil                "plane_angle"                        '(* "angular_revolution"  1/6400   )                    0          nil :si-not-other   )
          (list "angular_min"                    nil                "plane_angle"                        '(* "angular_revolution"  1/21600  )                    0          nil :si-not-other   )
          (list "angular_sec"                    nil                "plane_angle"                        '(* "angular_revolution"  1/1296000)                    0          nil :si-not-other   )
          (list "us_tablespoon"                  '("us_tbl" "tbl")  "volume"                             '(* "us_teaspoon"         3        )                    0          nil :si-not-other   )
          (list "us_ounce_fl"                    '("oz_fl")         "volume"                             '(* "us_teaspoon"         6        )                    0          nil :si-not-other   )
          (list "us_cup"                         nil                "volume"                             '(* "us_teaspoon"         48       )                    0          nil :si-not-other   )
          (list "us_pint"                        '("us_pt" "pt")    "volume"                             '(* "us_teaspoon"         96       )                    0          nil :si-not-other   )
          (list "us_quart"                       '("us_qt" "qt")    "volume"                             '(* "us_teaspoon"         192      )                    0          nil :si-not-other   )
          (list "us_gallon"                      '("us_gal" "gal")  "volume"                             '(* "us_teaspoon"         768      )                    0          nil :si-not-other   )))
        (prfxs (list (list "exbi"   "Ei"   (expt 2  60))
                     (list "pebi"   "Pi"   (expt 2  50))
                     (list "tebi"   "Ti"   (expt 2  40))
                     (list "gibi"   "Gi"   (expt 2  30))
                     (list "mebi"   "Mi"   (expt 2  20))
                     (list "kibi"   "Ki"   (expt 2  10))
                     (list "yotta"  "Y"    (expt 10 24))
                     (list "zetta"  "Z"    (expt 10 21))
                     (list "exa"    "E"    (expt 10 18))
                     (list "peta"   "P"    (expt 10 15))
                     (list "tera"   "T"    (expt 10 12))
                     (list "giga"   "G"    (expt 10 9))
                     (list "mega"   "M"    (expt 10 6))
                     (list "kilo"   "k"    (expt 10 3))
                     (list "hecto"  "h"    (expt 10 2))
                     (list "deca"   "da"   (expt 10 1))
                     (list "yocto"  "y"    (expt 10 -24))
                     (list "zepto"  "z"    (expt 10 -21))
                     (list "atto"   "a"    (expt 10 -18))
                     (list "femto"  "f"    (expt 10 -15))
                     (list "pico"   "p"    (expt 10 -12))
                     (list "nano"   "n"    (expt 10 -9))
                     (list "micro"  "u"    (expt 10 -6))
                     (list "milli"  "m"    (expt 10 -3))
                     (list "centi"  "c"    (expt 10 -2))
                     (list "deci"   "d"    (expt 10 -1)))))
    (cond ((symbolp unit-to-find)                      (mjr_units_find-unit (symbol-name unit-to-find)
                                                                            :print-ambiguous-warnings print-ambiguous-warnings
                                                                            :error-on-bad-lookup error-on-bad-lookup
                                                                            :case-sensitive nil))
          ((numberp unit-to-find)                      unit-to-find)
          ((not (stringp unit-to-find))                (error "mjr_units_find-unit: Unsupported argument type"))
          ((<= (length unit-to-find) 0)                (error "mjr_units_find-unit: Empty string is not a vlid unit"))
          ((find (aref unit-to-find 0) "-+0123456789") (mjr_string_read-as-lisp unit-to-find))
          ('t
           (let* ((test    (if case-sensitive #'string= #'string-equal))
                  (g-prfxs (loop for x in prfxs
                                 when (or (mjr_string_starts-with (first  x) unit-to-find :case-sensitive case-sensitive)
                                          (mjr_string_starts-with (second x) unit-to-find :case-sensitive case-sensitive))
                                 collect x))
                  (result  (delete-duplicates 
                            (loop for a-prefix-list in (append (list (list "" "" 1)) g-prfxs)
                                  for a-prefix-name = (first a-prefix-list)
                                  for a-prefix-abrv = (second a-prefix-list)
                                  for a-prefix-mult = (third a-prefix-list)
                                  append (loop for a-unit-list in units
                                               for a-unit-name = (first a-unit-list)
                                               for a-unit-abrv-list = (second a-unit-list)
                                               for a-unit-fullname = (concatenate 'string a-prefix-name a-unit-name)
                                               for a-unit-cononical-name = (if (first a-unit-abrv-list)
                                                                               (concatenate 'string a-prefix-abrv (first a-unit-abrv-list))
                                                                               (concatenate 'string a-prefix-name a-unit-name))
                                               for a-unit-cononical-data = (list a-unit-cononical-name (append (list a-prefix-mult) a-unit-list))
                                               when (or (sixth a-unit-list) (string= a-prefix-name ""))
                                               append (append  (if (funcall test a-unit-fullname unit-to-find)
                                                                   (list a-unit-cononical-data))
                                                               (loop for a-unit-abrv in a-unit-abrv-list
                                                                     for a-unit-fullabrv = (concatenate 'string a-prefix-abrv a-unit-abrv)
                                                                     when (funcall test a-unit-fullabrv unit-to-find)
                                                                     collect a-unit-cononical-data))))
                            :test (lambda (x y) (string= (first x) (first y))))))
             (if (and print-ambiguous-warnings (> (length result) 1))
                 (format 't "WARNING: Ambiguous ~a unit specification: ~s => ~s~%" 
                         (if case-sensitive "case sensitive" "case insensitive") unit-to-find (mapcar #'first result)))
             (if (and error-on-bad-lookup (null result))
                 (error "mjr_units_find-unit: Unknown unit symbol or string: ~s" unit-to-find))
             (first result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_canonization  (number-or-unit-thingy &optional nil-or-unit-thingy)
  "Return Canonical unit expressions and unit information alist when given various unit representations

Unit Representations

 * Canonical unit expression .. LISP list starting with '*, optionally followed by a number, and sequence of unit names or two element lists with the first
                                element '/ and the second a unit name

                                This is the format used internally for all computations and is the only form that will be returned from a unit expression
                                manipulation function.

 * Unit expression ............ LISP expression with * & / functions operating on unit names (strings), unit abbreviations (strings), and/or unit
                                symbols (list symbols representing unit abbreviations)

 * Algebraic unit expression .. LISP string representing an unit expression using an algebra-LIKE syntax

                                Note that a syntax similar to, but different from, traditional algebraic notation is used.  It is designed to provide a quick
                                way to express complex units.  These strings represent rational expressions with the top and bottom being pure multiplicative
                                products of symbols.  In this syntax, * indicates a product.  The ^ is for exponents.  The exponent only applies to the symbol
                                immediately preceding the ^.  Exponents must be integers. All symbols to the left of the / are on the top, and all symbols on
                                the right are on the bottom -- so a*b/c*d is (in traditional algebra syntax) (a*b)/(c*d).  A / with nothing on the left is the
                                same as having a 1 on the left -- i.e.  everything on the right is on the bottom of the fraction. parentheses are
                                ignored. Examples:

                                    * Frequency    =  1/s*s    = /s*s     = /s^2    
                                    * Area         =  m*m      = m*m/     = m^2   = m^2/
                                    * Acceleration =  m/s*s    = m/s^2    
                                    * Force        =  kg*m/s*s = kg*m/s^2

 * Unit name                   LISP string representing a unit name

 * Unit abbreviation           String representing a unit abbreviation

 * Unit symbol                 LISP Symbol representing a unit abbreviation

 * String number               Convert to a number"
  (let ((unit-thingy (or nil-or-unit-thingy number-or-unit-thingy))
        (multiplyer  (if nil-or-unit-thingy number-or-unit-thingy 1)))
    (if (or (numberp unit-thingy) (stringp unit-thingy) (and (not (listp unit-thingy)) (symbolp unit-thingy)))
        (mjr_units_canonization  
         multiplyer
         (if (or (symbolp unit-thingy)
                 (numberp unit-thingy)
                 (every (lambda (char) (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")) unit-thingy))
             (list '* unit-thingy)
             (flet ((buster (aList) (append (list '*) (loop for e1 in aList
                                                            for e2 = (mjr_string_split e1 #\^)
                                                            append (if (second e2)
                                                                       (loop for e3 from 1 upto (parse-integer (second e2))
                                                                             collect (first e2))
                                                                       e2)))))
               (let* ((tmp-str (concatenate 'string
                                            (loop for c across unit-thingy
                                                  when (find c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.+-/*_^")
                                                  collect c)))
                      (top-bot (mjr_string_split tmp-str #\/))
                      (top     (mjr_string_split (or (first top-bot)  "") #\*))
                      (bot     (mjr_string_split (or (second top-bot) "") #\*))
                      (topx    (buster top))
                      (botx    (buster bot)))
                 (list '/ topx botx)))))
        (if (and unit-thingy (listp unit-thingy) (equal '/ (car unit-thingy)))
            (mjr_units_canonization  multiplyer (append '(*) (list unit-thingy)))
            ;; Unsimplified canonization: (* a b c (/ d) e f g (/ h)...
            (let* ((new-exp
                    (loop for e in (cdr unit-thingy)
                          for lene = (and (listp e) (length e))
                          for i from 1
                          when (= i 1)
                          append '(*)
                          append (cond ((null e)                                    ;; nil                => nil
                                        nil)
                                       ((and (stringp e) (equal "" e))              ;; ""                 => 1
                                        (list 1))
                                       ((or (stringp e) (symbolp e))                ;; 's                 => s
                                        (list e))
                                       ((numberp e)                                 ;; 'n                 => n
                                        (list e))                                   ;; if e is a number, it is not =1 below
                                       ((not (listp e))                             ;; lists below
                                        (error "mjr_units_expression-canonicalization: exp bad"))
                                       ((equal (first e) '*)                        ;; '(* e1 e2 ...)     => e1 e2 ...
                                        (cdr e))
                                       ((not (equal (first e) '/))                  ;; / expressions below
                                        (error "mjr_units_expression-canonicalization: exp bad"))
                                       ((= lene 1)                                  ;; '(/ )              => 1
                                        (list 1))
                                       ((and (= lene 2) (equal "" (second e)))      ;; '(/ "")            => 1
                                        (list 1))
                                       ((and (= lene 2) (numberp (second e)))       ;; '(/ n)             => 1/n
                                        (list (/ (second e))))
                                       ((and (= lene 2) (not (listp (second e))))   ;; '(/ s)             => '(/ s)
                                        (list e))
                                       ((not (= lene 2))                            ;; '(/ e1 e2 ...)     => e1 (/ e2) ...
                                        (append (list (second e))                   ;; length 2 and (second e) is a list below
                                                (loop for e1 in (cddr e)
                                                      collect (list '/ e1))))
                                       ((equal '* (first (second e)))               ;; '(/ (* e1 e2 ...)) => '(/ 1 e1 e2 ...)
                                        (list (append '(/ 1) (cdr (second e)))))
                                       ((not (equal '/ (first (second e))))         ;; (second e) is a list starting with /
                                        (error "mjr_units_expression-canonicalization: EXP bad"))
                                       ((= 1 (length (second e)))                   ;; '(/ (/ ))          => 1
                                        (list 1))
                                       ((= 2 (length (second e)))                   ;; '(/ (/ e1))        => e1
                                        (list (second (second e))))
                                       ('t                                          ;; '(/ (/ e1 e2 ...)) => (/ e1) e2 ...
                                        (append (list (list '/ (second (second e))))
                                                (cddr (second e))))))))
              ;;(format 't "~s~%" new-exp)
              (if (tree-equal unit-thingy new-exp)
                  ;; Simply result before return: a/a == 1
                  (let ((unit-powr (make-hash-table :test 'equal))
                        (unit-info (make-hash-table :test 'equal)))
                    (loop for e in (cdr new-exp)
                          for (s p) = (if (listp e)
                                          (list (second e) -1)
                                          (list e 1))
                          do (let* ((info-s      (mjr_units_find-unit s))
                                    (canonized-s (if (numberp info-s) info-s (first info-s))))
                               (if (numberp canonized-s)
                                   (setf multiplyer (* multiplyer (cond ((> p 0) canonized-s)
                                                                        ((= p 0) 1)
                                                                        ((< p 0) (/ canonized-s))
                                                                        ('t      0)))) ;; Quiet compiler
                                   (progn (setf (gethash canonized-s unit-info) info-s)
                                          (if (gethash canonized-s unit-powr)
                                              (incf (gethash canonized-s unit-powr) p)
                                              (setf (gethash canonized-s unit-powr) p))))))
                    (values (append '(*)
                                    (if (not (equal multiplyer 1)) (list multiplyer))
                                    (loop for s being the hash-keys of unit-powr using (hash-value p)
                                          append (loop for i from 1 upto (abs p)
                                                       collect (cond ((> p 0) s)
                                                                     ((< p 0) (list '/ s))))))
                            (loop for x being the hash-values of unit-info
                                  collect x)))
                  ;; Do it again
                  (mjr_units_canonization  multiplyer new-exp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_substitute (unit-thingy &rest rest)
  "Substitute expressions for unit symbols in UNIT-THINGY."
  (let ((exp-c (mjr_units_canonization  unit-thingy))
        (rst-c (loop for (u r) in rest
                     collect (list (first (mjr_units_find-unit u)) r))))
    (identity (mjr_units_canonization  (loop for i from 1
                                                for e in (cdr exp-c)
                                                for (s p) = (if (listp e)
                                                                (list (second e) -1)
                                                                (list e 1))
                                                for (su sub) = (or (assoc s rst-c :test #'equal) (list s s))
                                                when (= i 1)
                                                collect '*
                                                collect (if (> p 0)
                                                            sub
                                                            (list '/ sub)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_to-si-fundamental (number-or-unit-thingy &optional nil-or-unit-thingy)
  "convert into base SI units"
  (multiple-value-bind (unit-expr unit-info) (mjr_units_canonization  number-or-unit-thingy nil-or-unit-thingy)
    (apply #'mjr_units_substitute
           unit-expr 
           (loop for (s s-info) in unit-info
                 for s-class = (eighth s-info)
                 when (or (not (member s-class '(:si-fundamental))) (not (= 1 (first s-info))))
                 collect (list s (if (equal s-class :si-not-other)
                                     (multiple-value-bind (crap hop-unit-list) (mjr_units_canonization  (fifth s-info))
                                       (declare (ignore crap))
                                       (mjr_units_substitute (fifth s-info) (list (first (first hop-unit-list)) (fifth (second (first hop-unit-list))))))
                                     (list '* (first s-info) (fifth s-info))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_convert (number-or-unit-thingy-from unit-thingy-from-or-unit-thingy-to &optional unit-thingy-to-or-nil)
  "Convert units.

Two use options:
 * number from-unit to-unit
 * from-unit to-unit

If the units are not compatible, the units on the result of the computation will not quite be what you ask for.  Note that the
value returned will ALWAYS be EQUAL to the original quantity."
  (flet ((nlu (x) (and (listp x)
                       (equal '* (first x))
                       (member (first (last x)) (list "R" "degF" "degC" "K") :test #'equal)
                       (or (= 2 (length x))
                           (and (= 3 (length x))
                                (numberp (second x)))))))
    (multiple-value-bind (unit-thingy-from unit-info-from)
        (mjr_units_canonization  number-or-unit-thingy-from (if unit-thingy-to-or-nil unit-thingy-from-or-unit-thingy-to))
      (multiple-value-bind (unit-thingy-to unit-info-to)
          (mjr_units_canonization  (or unit-thingy-to-or-nil unit-thingy-from-or-unit-thingy-to))
        (or unit-info-from unit-info-to)
        (if (and (nlu unit-thingy-from) (nlu unit-thingy-to))
            "This is a SIMPLE, non-linear unit conversion.  Not implemented yet. :)"
            (identity (mjr_units_canonization  (list '*
                                                        (mjr_units_canonization  (list '/
                                                                                          (mjr_units_to-si-fundamental unit-thingy-from)
                                                                                          (mjr_units_to-si-fundamental unit-thingy-to)))
                                                        unit-thingy-to))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_units_compatible (unit-thingy-1 unit-thingy-2)
  "non-NIL if the units are compatible"
  (let ((foo (mjr_units_canonization  (list '/
                                               (mjr_units_to-si-fundamental unit-thingy-1)
                                               (mjr_units_to-si-fundamental unit-thingy-2)))))
    (and (listp foo) 
         (or (and (= 2 (length foo)) (equal '* (first foo)) (numberp (second foo)))
             (and (= 1 (length foo)) (equal '* (first foo)))))))
