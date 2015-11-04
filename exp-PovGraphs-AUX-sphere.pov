#version 3.7;

#include "colors.inc"
#include "textures.inc"
#include "woods.inc"
#include "metals.inc"
#include "skies.inc"

global_settings { assumed_gamma 2.2 }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Global Contenxt & Camera

#declare cx = 0;    // X Center of rotation
#declare cy = 0;    // Y Center of rotation
#declare h  = 5;    // Height of camera
#declare r  = 7;   // Distance form center of rotation
camera {
   //orthographic
   location <cos(clock*2*pi)*r+cx,sin(clock*2*pi)*r+cy,h>
   look_at  <cx,cy,0>
   sky <0,0,1>
   up <0,0,1>
   right <0,16/9,0>
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

// X
cylinder { axOrig, axOrig+<maxx,0,0> axDiam texture { coordAxisTex } }
cone {axOrig+<2.5*axDiam+maxx,0,0>, 0.0, axOrig+<maxx,0,0>, 2.0*axDiam texture { coordAxisTex } }
// Y
cylinder { axOrig, axOrig+<0,maxy,0> axDiam texture { coordAxisTex } }
cone {axOrig+<0,2.5*axDiam+maxy,0>, 0.0, axOrig+<0,maxy,0>, 2.0*axDiam texture { coordAxisTex } }
// Z
cylinder { axOrig, axOrig+<0,0,maxz> axDiam texture { coordAxisTex } }
cone {axOrig+<0,0,2.5*axDiam+maxz>, 0.0, axOrig+<0,0,maxz>, 2.0*axDiam texture { coordAxisTex } }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Coordinate Planes

#declare coordPlnTex=texture {
         pigment { checker color Gray color Pink }
         //   finish  { ambient .20 }
}

// X
plane { x, -11   texture { coordPlnTex } }
// Y
plane { y, -11   texture { coordPlnTex } }
// Z
plane { z, -2   texture { coordPlnTex } }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Content

#declare lineTex=texture {
   pigment { color Green }
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
#declare vertexDiam=0.02;
#declare lineDiam=0.01;
