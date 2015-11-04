#include "colors.inc"
#include "textures.inc"
#include "woods.inc"
#include "metals.inc"
#include "skies.inc"

// povray -W1366 -H768 -Q11 +A -K0.18 +R4 +J3 +P +D -Oexp-MandelbrotTGA3-ART-pov.png -Iexp-MandelbrotTGA3-AUX.pov

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Global Contenxt & Camera

#declare cx = 0;    // X Center of rotation
#declare cy = 0;    // Y Center of rotation
#declare h  = 3;    // Height of camera
#declare r  = 7.5;  // Distance form center of rotation
camera {
   location <cos(clock*2*pi)*r+cx,sin(clock*2*pi)*r+cy,h>
   look_at  <cx,cy,0>
   sky <0,0,1>
   up <0,0,1>
   right <0,1.7786458333333333,0>
}

light_source { <  0,  0,30>  color 0.2*White }
light_source { <-10, 10,20>  color 0.2*White }
light_source { < 10,-10,20>  color 0.2*White }
light_source { < 10, 10,20>  color 0.2*White }
light_source { <-10,-10,20>  color 0.2*White }

background { color Black }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Coordinate Axis

#declare axDiam=0.1;
#declare maxx=9;
#declare maxy=9;
#declare maxz=5;
#declare axOrig=<-0,-0,-0>;

#declare coordAxisTex=texture {
   pigment { color Blue } 
   finish  {
      ambient .50
      diffuse 0.05
      reflection 0.07
      specular 0.9
      roughness 0.03
      phong 1 
      phong_size 600
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content

height_field {
  tga "exp-MandelbrotTGA3-OUT-p.tga"
  smooth
  finish {
    ambient   .3
  }
  pigment {
    gradient y
      color_map {
        [0.00   color Red ]
        [0.002 color Magenta]
        [0.001   color Yellow ]
        [0.06   color Green ]
        [0.50   color White  ]
      }
  }
  translate <-0.5, -0.0, -0.5>
  scale <12, 6, 12>
  rotate <90,0,0>
}
