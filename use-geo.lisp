;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-geo.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     Geographic and cartographic computations.@EOL
;; @std       Common Lisp
;; @see       tst-geo.lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1997,2006,2008,2013,2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; @todo      mjr_earth_place-names.@EOL@EOL
;; @todo      projections.@EOL@EOL
;; @todo      magnetics.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_GEO
  (:USE :COMMON-LISP
        :MJR_A
        :MJR_CHK)
  (:DOCUMENTATION "Brief: Geographic and cartographic computations.;")
  (:EXPORT #:mjr_geo_help
           ;; Geometric stuff
           #:mjr_geo_angular-spherical-distance 
           #:mjr_geo_spherical-distance #:mjr_geo_spheroid-distance 
           ;; Real geographic stuff
           #:mjr_geo_geod-distance
           #:mjr_geo_geod-mean-radius
           #:mjr_geo_geod-quadratic-mean-radius
           #:mjr_geo_geod-volumetric-radius
           #:mjr_geo_geod-authalic-mean-radius
           ;; Not Exported
           ;; #:mjr_geo_spheroid-string-to-parameter
           ;; #:mjr_geo_spheroid-parameter-normalize
           ))

(in-package :MJR_GEO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_help ()
  "Help for MJR_GEO: GEOgraphic computation
Geographical computations like great circle distance between two points."
  (documentation 'mjr_geo_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_angular-spherical-distance (lat1 lon1 lat2 lon2)
  ;; Get the cosine of the angle of separation: sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2)
  (let ((ca (+ (* (sin lat1) (sin lat2)) (* (cos lat1) (cos lat2) (cos (- lon1 lon2))))))
    (if (<= (abs ca) 1)
        (acos ca))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_spherical-distance (lat1 lon1 lat2 lon2 r)
  (let ((a (mjr_geo_angular-spherical-distance lat1 lon1 lat2 lon2)))
    (if a 
        (* a r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_spheroid-parameter-normalize (majax minax invflt)
  "Normalize spheroid parameters a (major axis), b (minor axis), & rf (inverse flattening).
If any one is NIL, then it will be computed in terms of the others.  If MAJAX & INVFLT are both NILL or MINAX & INVFLT are both NIL, then MAJAX is assumed to
be equal to MINAX and INVFLT is assumed to be zero.  NIL will be returned if not enough parameters are provided."
  (let ((num-prm (count-if #'identity (list minax majax invflt))))
    (case num-prm
      (3           (if (mjr_chk_!= majax minax)
                       (values majax minax invflt)
                       (error "ERROR: mjr_geo_spheroid-parameter-normalize: inconsistent parameters (non-NIL invflt for sphere)")))
      (2           (values (or majax  (/ (* invflt minax) (1- invflt)))
                           (or minax  (/ (* majax (1- invflt)) invflt))
                           (or invflt (if (mjr_chk_!= majax minax)
                                          (/ majax (- majax minax))))))
      (1           (if invflt
                       (error "ERROR: mjr_geo_spheroid-parameter-normalize: inconsistent parameters (only invflt provided)")
                       (values (or majax minax) (or majax minax) nil)))
      (otherwise   (error "ERROR: mjr_geo_spheroid-parameter-normalize: inconsistent parameters (no values provided)")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_spheroid-distance (lat1 lon1 lat2 lon2 majax minax invflt)
  "Compute the distance between two points on the surface of a spheroid, or NIL if error.

The two points are given by lat/lon in rads.  Note that when the spheroid is approximating Earth's geometry, the geodetic literature generally refers to it as
an 'ellipsoids', but it is mathematically a oblate spheroid.

The spheroid is defined by at least two of the following: majax=major axis, minax=minor axis, and invflt=inverse flattening.  Only two are required with the
third being computed from the known two if needed.  For the non-spherical case, majax and invflt are directly used for the computation.  The minax parameter
will only be used in this case to compute one of the maxax or invflt if they are unknown.  For the spherical case (a==b) geoDist() is called.  Note that in
the spherical case, invflt can NOT be provided as it is infinity -- use NIL.

Reference: H. Andoyer (1950); Annuaire du Bureau des Longitudes pour; pp 145"
  (multiple-value-bind (a b rf) (mjr_geo_spheroid-parameter-normalize majax minax invflt)
    (declare (ignore b))
    (if (null invflt)
        (mjr_geo_spherical-distance lat1 lon1 lat2 lon2 a) ; spherical case..
        (let* ((f     (/ (+ lat1 lat2) 2.0d0))
               (g     (/ (- lat1 lat2) 2.0d0))
               (l     (/ (- lon1 lon2) 2.0d0))
               (cg   (cos g))
               (sg   (sin g))
               (cl   (cos l))
               (sl   (sin l))
               (cf   (cos f))
               (sf   (sin f))
               (s    (+ (expt (* sg cl) 2) (expt (* cf sl) 2)))
               (w    (asin (sqrt s)))
               (c    (- 1 s))
               (r    (* 3 (sqrt (* s c)))))
          (if (and (>= c 0.0d0) (mjr_chk_!=0 s) (mjr_chk_!=0 c))              
              (* a (+ (* 2 w)
                      (/ (- (* (/ (- r w) c) (expt (* sf cg) 2))
                            (* (/ (+ r w) s) (expt (* sg cf) 2)))
                         rf))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geo Computations Below This Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_spheroid-string-to-parameter (ellps)
  "Return a list ellipsoid parameters (a, b, & rf) as well as a note about the ellipsoid"
  (let ((ell-list
         (list
          ;;                 a            b             rf                 Notes
          (list "WGS84"      6378137.0          nil     298.257223563  ) ; WGS 84                       ;;;;;;;;;;;;; HERE FOR SPEED
          (list "MERIT"      6378137.0          nil     298.257        ) ; MERIT 1983
          (list "SGS85"      6378136.0          nil     298.257        ) ; Soviet Geodetic System 85
          (list "GRS80"      6378137.0          nil     298.257222101  ) ; GRS 1980(IUGG 1980)
          (list "IAU76"      6378140.0          nil     298.257        ) ; IAU 1976
          (list "airy"       6377563.396  6356256.910     nil          ) ; Airy 1830
          (list "APL4.9"     6378137.0          nil     298.25         ) ; Applied Physics 1965
          (list "NWL9D"      6378145.0          nil     298.25         ) ; Naval Weapons Lab 1965
          (list "mod_airy"   6377340.189  6356034.446     nil          ) ; Modified Airy
          (list "andrae"     6377104.43         nil     300.0          ) ; Andrae 1876 (Den., Iclnd.)
          (list "aust_SA"    6378160.0          nil     298.25         ) ; Australian Natl & S. Amer. 1969
          (list "GRS67"      6378160.0          nil     298.2471674270 ) ; GRS 67(IUGG 1967)
          (list "bessel"     6377397.155        nil     299.1528128    ) ; Bessel 1841
          (list "bess_nam"   6377483.865        nil     299.1528128    ) ; Bessel 1841 (Namibia)
          (list "clrk66"     6378206.4    6356583.8       nil          ) ; Clarke 1866
          (list "clrk80"     6378249.145        nil     293.4663       ) ; Clarke 1880 mod.
          (list "CPM"        6375738.7          nil     334.29         ) ; Comm. des Poids et Mesures 1799
          (list "delmbr"     6376428.0          nil     311.5          ) ; Delambre 1810 (Belgium)
          (list "engelis"    6378136.05         nil     298.2566       ) ; Engelis 1985
          (list "evrst30"    6377276.345        nil     300.8017       ) ; Everest 1830
          (list "evrst48"    6377304.063        nil     300.8017       ) ; Everest 1948
          (list "evrst56"    6377301.243        nil     300.8017       ) ; Everest 1956
          (list "evrst69"    6377295.664        nil     300.8017       ) ; Everest 1969
          (list "evrstSS"    6377298.556        nil     300.8017       ) ; Everest (Sabah & Sarawak)
          (list "fschr60"    6378166.0          nil     298.3          ) ; Fischer (Mercury Datum) 1960
          (list "fschr60m"   6378155.0          nil     298.3          ) ; Modified Fischer 1960
          (list "fschr68"    6378150.0          nil     298.3          ) ; Fischer 1968
          (list "helmert"    6378200.0          nil     298.3          ) ; Helmert 1906
          (list "hough"      6378270.0          nil     297.0          ) ; Hough
          (list "intl"       6378388.0          nil     297.0          ) ; International 1909 (Hayford)
          (list "krass"      6378245.0          nil     298.3          ) ; Krassovsky, 1942
          (list "kaula"      6378163.0          nil     298.24         ) ; Kaula 1961
          (list "lerch"      6378139.0          nil     298.257        ) ; Lerch 1979
          (list "mprts"      6397300.0          nil     191.0          ) ; Maupertius 1738
          (list "new_intl"   6378157.5    6356772.2       nil          ) ; New International 1967
          (list "plessis"    6376523.0    6355863.0       nil          ) ; Plessis 1817 (France)
          (list "SEasia"     6378155.0    6356773.3205    nil          ) ; Southeast Asia
          (list "walbeck"    6376896.0    6355834.8467    nil          ) ; Walbeck
          (list "WGS60"      6378165.0          nil     298.3          ) ; WGS 60
          (list "WGS66"      6378145.0          nil     298.25         ) ; WGS 66
          (list "WGS72"      6378135.0          nil     298.26         ) ; WGS 72
        ;;(list "WGS84"      6378137.0          nil     298.257223563  ) ; WGS 84
          (list "IERS1989"   6378136.0    6356751.302	298.257        ) ; IERS Conventions 1989
          (list "IERS2003"   6378136.6    6356751.9     298.25642      ) ; IERS Conventions 2003. pp 12
          (list "sphere"     6370997.0    6370997.0       nil          ) ; Normal Sphere (r=6370997)
          (list "equatorial" 6378137.0    6378137.0       nil          ) ; Sphere: using equatorial radius
          (list "polar"      6356752.3    6356752.3       nil          ) ; Sphere: using polar radius
          (list "maxradius"  6384400.0    6384400.0       nil          ) ; Sphere: using maximum radius (summit of Chimborazo)
          (list "minradius"  6352800.0    6352800.0       nil          ) ; Sphere: using minimum radius (Arctic Ocean floor)
          ))
        (ellps (string-downcase ellps)))
    (if ellps
        (apply #'mjr_geo_spheroid-parameter-normalize
               (rest (find-if (lambda (x) (string= (string-downcase (car x)) ellps)) ell-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_geod-mean-radius (&key a b rf (ellps "wgs84"))
  "Compute the mean radius via the International Union of Geodesy and Geophysics (IUGG) definition."
  (multiple-value-bind (a b rf) (if (or a b rf)
                                    (mjr_geo_spheroid-parameter-normalize a b rf)
                                    (mjr_geo_spheroid-string-to-parameter ellps))
    (declare (ignore rf))
    (if a
        (/ (+ (* 2 a) b) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_geod-quadratic-mean-radius (&key a b rf (ellps "wgs84"))
  "Compute the quadratic mean radius."
  (multiple-value-bind (a b rf) (if (or a b rf)
                                    (mjr_geo_spheroid-parameter-normalize a b rf)
                                    (mjr_geo_spheroid-string-to-parameter ellps))
    (declare (ignore rf))
    (if a
        (sqrt (/ (+ (* 3 a a) (* b b)) 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_geod-volumetric-radius (&key a b rf (ellps "wgs84"))
  "Compute the volumetric radius via the International Union of Geodesy and Geophysics (IUGG) definition.
This is the radius of the sphere with the same volume as the spheroid."
  (multiple-value-bind (a b rf) (if (or a b rf)
                                    (mjr_geo_spheroid-parameter-normalize a b rf)
                                    (mjr_geo_spheroid-string-to-parameter ellps))
    (declare (ignore rf))
    (if a
        (expt (* a a b) 1/3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_geod-authalic-mean-radius (&key a b rf (ellps "wgs84"))
  "Compute authalic mean radius via the International Union of Geodesy and Geophysics (IUGG) definition.
This is the radius of the sphere with the same surface area as the reference spheroid."
  (multiple-value-bind (a b rf) (if (or a b rf)
                                    (mjr_geo_spheroid-parameter-normalize a b rf)
                                    (mjr_geo_spheroid-string-to-parameter ellps))
    (declare (ignore rf))
    (if a
        (sqrt (/ (+ (* a a)
                    (* (/ (* a b b) (sqrt (- (* a a) (* b b))))
                       (log (/ (+ a (sqrt (- (* a a) (* b b)))) b))))
                 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_geo_geod-distance (lat1 lon1 lat2 lon2 &key a b rf (ellps "wgs84"))
  "Compute the distance between two points on Earth using the given ellipsoid modeling the surface of a spheroid, or NIL if error.
The two points are given by lat/lon in degrees."
  (let* ((rlat1   (mjr_a_d2r lat1))
         (rlon1   (mjr_a_d2r lon1))
         (rlat2   (mjr_a_d2r lat2))
         (rlon2   (mjr_a_d2r lon2)))
    (multiple-value-bind (a b rf) (if (or a b rf)
                                      (mjr_geo_spheroid-parameter-normalize a b rf)
                                      (mjr_geo_spheroid-string-to-parameter ellps))
      (mjr_geo_spheroid-distance rlat1 rlon1 rlat2 rlon2 a b rf))))
