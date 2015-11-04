


;; Idea for a "uber math function -> thingy" function.  This would be one of the primary "ease of use" functions that allows me to replace all the duplicate code in VTK, IMG, PLOT, and POV.

;; Just an idea...  May not do it in the end.


(defun mjr_funout_r123-r123 (target ;; ......................... "foo.vtk" "foo.pov" "foo.tga" "plot"
                             ;; dquad args
                             func (func-lab "f")
                             xdat ydat zdat (arg-mode :arg-number) (xlab "x") (ylab "y") (zlab "z")
                             ax-color-meth  (ax-color-max-color  1) (ax-color-lab  "c") (ax-color-auto-scale  't) (ax-color-colorspace  :cs-rgb)
                             f-color-meth   (f-color-max-color   1) (f-color-lab   "c") (f-color-auto-scale   't) (f-color-colorspace   :cs-rgb)
                             all-color-meth (all-color-max-color 1) (all-color-lab "c") (all-color-auto-scale 't) (all-color-colorspace :cs-rgb)
                             ;; dsimp args
                             u-close v-close
                             ;; vtk args
                             file-note
                             ;; vtk dquad args
                             axes data-arrays
                             ;; vtk dsimp args
                             point-data simplices
                             ;; pov args
                             ;; img args
                             )
)




|--------------+-----------------+--------------------------------+-----------------------------+---------------------------------|
| Math object  | Function Types  | Alternate Geometry             | Object Under Field Geometry | Object Under Alternate Geometry |
|--------------+-----------------+--------------------------------+-----------------------------+---------------------------------|
| Curve        | R    ->R        | (u, f(u))                      | Scalar Field                | Rectilinear Curve               |
|              | R    ->RxR      | (f_x(u), f_y(u))               | 2D Vector Field in 1D       | Parametric Curve                |
|              | R    ->RxRxR    | (f_x(u), f_y(u), f_z(u))       | 3D Vector Field in 1D       | Parametric Curve                |
| Surface      | RxR  ->R        | (u, v, f(u,v))                 | Rectilinear Surface         |                                 |
|              | RxR  ->RxRxR    | (f_x(u,v), f_y(u,v), f_z(u,v)) | 3D Vector Field in 2D       | Parametric Surface              |
| Vector Field | RxR  ->RxR      |                                | 2D Vector Field in 2D       |                                 |
|              | RxRxR->RxRxR    |                                | 3D Vector Field in 3D       |                                 |
| Slope Field  | RxR  ->R+\infty |                                | Slope Field                 |                                 |
|              | RxRxR->R+\infty |                                | Slope Field                 |                                 |
| Scalar Field | RxR  ->R        |                                | Scalar Field                |                                 |
|              | RxRxR->R        |                                | Scalar Field                |                                 |
|--------------+-----------------+--------------------------------+-----------------------------+---------------------------------|
| Complex Map  | C    -> C       | (Re(z), Im(z), abs(f(z)))      |                             | Rectilinear Surface With Color  |
|--------------+-----------------+--------------------------------+-----------------------------+---------------------------------|

| 1D Scalar Field       | Rare       | 1D Image, Color Ribbon, Glyphs |
| 2D Scalar Field       | Common     | 2D Image, Glyphs               |
| 3D Scalar Field       | Ocassional | 3D Image/Voxel Image, Glyphs   |
| 2D Vector Field In 2D | Common     | Vector Field Plot              |
| 3D Vector Field In 3D | Common     | Vector Field Plot              |
| Slope Field In 2D     | Common     | Slope Field Plot               |
| Direction Field In 3D | Ocassional | Direction Field Plot           |
|                       |            |                                |


 
;   Various mathematical objects and graphical presentations   
;   -----------------------------------------------------------

||   Various Graph Primitives                                 
||--------------------------------------------------------    
||                                                            
||   C2  - Curve in 2D (points, segments)                     
||   C3  - Curve in 3D (points, segments)                     
||   S3  - Surface in 3D (points, segments, triangles)        
||   2V2 - 2D Vector field in 2D                              
||   3V3 - 3D Vector field in 3D                              
||   S2  - Slope field in 2D                                  
||   S2  - Slope field in 3D                                  
||   I2  - Pixel image in 2D                                  
||   I3  - Voxel image in 3D                                  
||   G2  - Glyph field in 2D                                  
||   G3  - Glyph field in 3D                                  






; Various function types along with implementation information:
;
;                   +--------+---+---+-----+-------+
;                   | in\out | R | C | RxR | RxRxR |       I    Implemented
;                   +--------+---+---+-----+-------+       2    Require 2D vector field plots
;                   |   R    | I | I |  I  |   I   |       3    Require 3D scalar field plots
;                   +--------+---+---+-----+-------+       4    Require 3D vector field plots
;                   |   C    | I | 2 |  2  |   5   |       5    Parametric surface in 3D (should be implemented)
;                   +--------+---+---+-----+-------+       6    Wacky object with no good representation
;                   |  RxR   | I | 2 |  2  |   5   |  
;                   +--------+---+---+-----+-------+  
;                   | RxRxR  | 3 | 6 |  6  |   4   |  
;                   +--------+---+---+-----+-------+  


