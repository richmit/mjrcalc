#include "colors.inc"
#include "textures.inc"
#include "woods.inc"
#include "metals.inc"
#include "skies.inc"

// Render high quality, and pause to display image when done.
// povray -W3840 -H2160 -Q11 +A +AM2 -K0.0 +R4 +J3 +P +D -HIexp-ODElorenzDEQ-AUX.pov -Iexp-ODElorenzDEQ-OUT.pov -Oexp-ODElorenzDEQ-ART.png

// Render low quality, and pause to display image when done.
// povray -W960  -H540  -Q0  +A +AM2 -K0.0 +R4 +J3 +P +D -HIexp-ODElorenzDEQ-AUX.pov -Iexp-ODElorenzDEQ-OUT.pov -Oexp-ODElorenzDEQ-ART.png

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Global Contenxt & Camera
camera {
   //orthographic
   location <14,20,50>
   look_at  <-10,-10,20>
   sky <1,0,0>
   up <1,0,0>
   right <0,1.7777778,0>            // Use for: ..........1920x1080...........1280x720.......................................................
}

light_source { <20, 15,30>  color 0.2*White }
light_source { < 15,  20, 50>  color 0.2*White }
light_source { < 5,  -20, 50>  color 0.2*White }
light_source { < 0, 0,30>  color 0.2*White }

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

//// X
//cylinder { axOrig, axOrig+<maxx,0,0> axDiam texture { coordAxisTex } }
//cone {axOrig+<2.5*axDiam+maxx,0,0>, 0.0, axOrig+<maxx,0,0>, 2.0*axDiam texture { coordAxisTex } }
//// Y
//cylinder { axOrig, axOrig+<0,maxy,0> axDiam texture { coordAxisTex } }
//cone {axOrig+<0,2.5*axDiam+maxy,0>, 0.0, axOrig+<0,maxy,0>, 2.0*axDiam texture { coordAxisTex } }
//// Z
//cylinder { axOrig, axOrig+<0,0,maxz> axDiam texture { coordAxisTex } }
//cone {axOrig+<0,0,2.5*axDiam+maxz>, 0.0, axOrig+<0,0,maxz>, 2.0*axDiam texture { coordAxisTex } }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Coordinate Planes

#declare coordPlnTex=texture {
         pigment { checker color Gray color Pink }
         //   finish  { ambient .20 }
}

// X
//plane { x, -11   texture { coordPlnTex } }
// Y
//plane { y, -11   texture { coordPlnTex } }
// Z
//plane { z, -2   texture { coordPlnTex } }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content

#declare lineTex=texture {
   pigment { color Red }
   finish  {
      ambient .10
      diffuse 0.05
      reflection 0.07
      specular 0.79
      roughness 0.05
      phong .51 
      phong_size 600
    }
}
#declare vertexTex=texture {
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
#declare triTex=texture {
    pigment { color Red }
    finish  {
      ambient 0.50
      diffuse 0.05
      reflection 0.07
      specular 1.9
      roughness 0.03
      phong 1 
      phong_size 600
    }
}
#declare vertexDiam=0.21;
#declare lineDiam=0.2;

