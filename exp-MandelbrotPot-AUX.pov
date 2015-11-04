#version 3.7;

#include "colors.inc"
#include "textures.inc"
#include "woods.inc"
#include "metals.inc"
#include "skies.inc"

global_settings { assumed_gamma 2.0 }

// povray -W3840 -H2160  -Q11 -K0.17 +A +AM2  +R4 +J3 -P -D -Oexp-MandelbrotPot-ART-povray1.png -Iexp-MandelbrotPot-AUX.pov
// povray -W683  -H384 -Q5  -A -K0.17         +P +D -Oexp-MandelbrotPot-ART-povray1.png -Iexp-MandelbrotPot-AUX.pov

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Global Contenxt & Camera

#declare cx = 0;    // X Center of rotation
#declare cy = -0.2;    // Y Center of rotation
#declare h  = .5;    // Height of camera
#declare r  = .70;  // Distance form center of rotation
camera {
   location <cos(clock*2*pi)*r+cx,sin(clock*2*pi)*r+cy,h>
   look_at  <cx,cy,0>
   sky <0,0,1>
   up <0,0,1>
   right <0,16/9,0>
}

light_source { < 0,  0,30>  color 0.2*White }
light_source { < .7, -.7,.1>    color 0.2*White }
light_source { < -4, 1,.5>    color 0.2*White }
light_source { < 1, 1,.5>    color 0.2*White }


//sphere { < .7, -.7,-.15>, .1  finish { ambient .50 } pigment{ color Red }}

background { color Black }

height_field {
  tga "exp-MandelbrotPot-OUT.tga"
  smooth

 finish  {
      ambient .30
      diffuse 0.15
      reflection 0.0
      specular 0.7
      roughness 0.15
      phong 1 
      phong_size 100
    }

  pigment {
    gradient y
      color_map {
        [0.00     color Black   ]
        [0.50     color Red     ]
        [0.99     color Red     ]
        [0.999    color Green   ]
        [0.9995   color Yellow  ]
        [0.9999   color Magenta   ]
        [0.99995  color Red    ]
        [0.99999  color Black    ]
        [1.00     color Black   ]
      }
  }
  translate <-0.5, -0.75, -0.5>
  scale <1.1, 1, 1.0>
  rotate <90,0,0>
}



  difference {
  height_field {
  tga "exp-MandelbrotPot-OUT.tga"
  smooth       
  
 finish  {
      ambient 1.0
      diffuse 0.0
      reflection 0.0
      specular 0.0
      roughness 0.20
      phong 0
    }

  pigment {
    Black
  }
  translate <-0.5, -0.75, -0.5>
  scale <1.1, 1, 1.0>
  rotate <90,0,0>
}
    box { <-10, -10, -10>, <10, 10, .24998> }
  }
