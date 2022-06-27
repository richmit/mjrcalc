;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      use-rptree.lisp
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     Data sets on binary quadtrees.@EOL
;; @std       Common Lisp
;; @see       tst-rptree.lisp
;; @copyright
;;  @parblock
;;  Copyright (c) 1995, 2013, 2015, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
;; @warning   This code is still in development.@EOL@EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :MJR_RPTREE
  (:USE :COMMON-LISP
        :MJR_VEC
        :MJR_CMP
        :MJR_CHK
        :MJR_COMBC
        :MJR_GEOM
        :MJR_UTIL)
  (:DOCUMENTATION "Brief: Rectilinear Point Tree (RPTree).")
  (:EXPORT #:mjr_rptree_help                         ;;
           ;; Structure Stuff
           #:mjr_rptree_new                          ;; mesh-power bbox-min bbox-max
           #:mjr_rptree_struct-bbox-min              ;; rptree-instance
           #:mjr_rptree_struct-data                  ;; rptree-instance
           #:mjr_rptree_struct-mesh-power            ;; rptree-instance
           #:mjr_rptree_struct-mesh-power            ;; rptree-instance
           #:mjr_rptree_struct-min-coord-real-delta  ;; rptree-instance
           #:mjr_rptree_struct-qdim                  ;; rptree-instance
           ;; Utility Functions
           #:mjr_rptree_level-quad-isize             ;; rptree-instance level
           #:mjr_rptree_level-quad-rsize             ;; rptree-instance level
           #:mjr_rptree_min-coord-real-delta         ;; rptree-instance
           #:mjr_rptree_coord-in-bounds              ;; rptree-instance coord
           ;; Integer coordinates
           #:mjr_rptree_make-lex-<                   ;; rptree-instance
           #:mjr_rptree_coord-2-real                 ;; rptree-instance coord
           #:mjr_rptree_coord-2-hash-key             ;; rptree-instance coord
           #:mjr_rptree_hash-key-2-coord             ;; rptree-instance hash-key
           ;; Real coords related to quads
           #:mjr_rptree_quad-near-real-point         ;; rptree-instance quad-coord real-coord
           #:mjr_rptree_coord-2-real-points          ;; rptree-instance list-of-coords
           ;; Integer coords related to quads
           #:mjr_rptree_quad-get-vertexes            ;; rptree-instance quad-coord
           #:mjr_rptree_quad-get-points              ;; rptree-instance quad-coord
           #:mjr_rptree_quad-get-boundary-points     ;; rptree-instance quad-coord depth
           #:mjr_rptree_quad-get-children            ;; rptree-instance quad-coord
           #:mjr_rptree_quad-get-neighbors           ;; rptree-instance quad-coord depth
           #:mjr_rptree_quad-get-parent              ;; rptree-instance quad-coord
           #:mjr_rptree_quad-get-grandparent         ;; rptree-instance quad-coord
           #:mjr_rptree_level-0-quad-center          ;; rptree-instance
           #:mjr_rptree_coord-centerp                ;; rptree-instance quad-coord
           #:mjr_rptree_make-2d-boundary-<           ;; rptree-instance quad-coord
           ;; Quad meta data
           #:mjr_rptree_quad-get-level               ;; rptree-instance quad-coord
           #:mjr_rptree_quad-get-isize               ;; rptree-instance quad-coord
           ;; Qtree data-hashes
           #:mjr_rptree_coord-get-value              ;; rptree-instance coord
           #:mjr_rptree_coord-has-value              ;; rptree-instance coord
           ;; High Level Operations
           #:mjr_rptree_print                        ;; rptree-instance
           #:mjr_rptree_print-meta                   ;; rptree-instance
           #:mjr_rptree_2d-make-dsimp                ;; rptree-instance depth
           ;; Function Sampling
           #:mjr_rptree_balance                      ;; rptree-instance depth
           #:mjr_rptree_uniform-fsamp                ;; rptree-instance func quad-coord depth
           #:mjr_rptree_uniform-fsamp-start          ;; rptree-instance func depth
           ))

(in-package :MJR_RPTREE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_help ()
  "Help for MJR_RPTREE: binary indexed Rectilinear Point Tree (RPTree)

       ############################################################################################################################################
       ############################################################################################################################################
       ==== Under Construction == Under Construction == Under Construction ==== Under Construction == Under Construction == Under Construction ====
       ============================================================================================================================================
       --------------------------------------------------------------------------------------------------------------------------------------------
       
                   #     #                                 #####                                                                                  
                   #     # #    # #####  ###### #####     #     #  ####  #    #  ####  ##### #####  #    #  ####  ##### #  ####  #    #           
                   #     # ##   # #    # #      #    #    #       #    # ##   # #        #   #    # #    # #    #   #   # #    # ##   #           
                   #     # # #  # #    # #####  #    #    #       #    # # #  #  ####    #   #    # #    # #        #   # #    # # #  #           
                   #     # #  # # #    # #      #####     #       #    # #  # #      #   #   #####  #    # #        #   # #    # #  # #           
                   #     # #   ## #    # #      #   #     #     # #    # #   ## #    #   #   #   #  #    # #    #   #   # #    # #   ##           
                    #####  #    # #####  ###### #    #     #####   ####  #    #  ####    #   #    #  ####   ####    #   #  ####  #    #           

       --------------------------------------------------------------------------------------------------------------------------------------------
       ============================================================================================================================================
       ==== Under Construction == Under Construction == Under Construction ==== Under Construction == Under Construction == Under Construction ====
       ############################################################################################################################################
       ############################################################################################################################################

Introduction

A quadtree is a nested 2D multi-resolution mesh which may be elegantly stored and manipulated via tree data structures.  We start with a rectangular region of
$\\mathbb{R}^2$ and split it up into four congruent sub-rectangles.  We then continue the process with each of these new rectangles.  This recursive structure
is a natural fit with tree data structures.  The tree data structure allows the mesh to be dynamic.  The octtree is the obvious generalization of the quadtree
to 3D -- start with a cube, and recursively break it up into 8 sub-cubes.  This is naturally stored in a tree data structure, but with eight pointers per node
instead of the four we use with a quadtree.

Both quadtrees and octtrees are examples of $2^n$-trees.  While the mathematical idea is quite simple to generalize and describe, generalizing computer codes
to dimensions other than 2 or 3 is difficult because the tree structure changes with each dimension.  Generally speaking octtrees are the only extension
beyond 2D commonly employed in the real world.  Not only are the tree data structures associated with $2^n$-trees difficult to generalize, they tend to be a
source of difficult algorithmic problems.  The source of this difficulty stems from the simple fact that most operations one wishes to preform on $2^n$-trees
are inherently driven by the geometry of the mesh, and thus we must translate geometric concepts like 'quad neighbors' into the terms tree manipulation.  This
translation from geometric ides to tree manipulation is non-trivial, and becomes complex when generalized.  In this library we overcome these limitations by
dispensing with the tree data structures entirely.

The key innovation is an enumeration scheme allowing us to uniquely identify quads and points via an integer which is intimately related to the geometric
structure of the grid.  This allows us to not only generalize the quadtree to arbitrary dimensions, but to more naturally translate geometric operations into
actions on the underlying data structures.

The trees in this packing are also called point trees because they store points (instead of regions, lines, etc...).  In particular, they store the points at
the corners of each quad/oct as well as the point at the center of each quad/oct -- so they implicitly define TWO sets of regular meshes (corners and centers)
over a rectangular region.  Over the years the term 'quadtree' has come to be used as a generic term for $2^n$-trees and related data structures.  In what
follows, we use the quadtree in this more general manner and the term quad to mean a quadrilateral, or hyper-rectangle, in any dimension.

Many implementations of point quadtree data structures choose to explicitly construct and store the mesh points; however, one may use an implicit approach by
noting that all grid points may be derived (at a small computation cost) from the corners of the original rectangle and the position of the point in the tree.
In all practical applications we are faced with a minimal floating point epsilon, and thus a practical lower limit to size of any quad/cube.  We take this
implicit approach a step further by exploiting these limits, and imposing a fixed maximal refinement depth when we construct the data structure.  This limit
then allows us to assign a fixed integer coordinate system on all potential grid points.  We call this limit on the maximum refinement depth the tree's
MESH-POWER (usually represented by the symbol $P$).

When a tree is created, $P$ is specified.  We refer to the original rectangle as the 'top quad' and say it is at level $0$ of the tree.  The children of this
quad are said to be at level $1$.  We continue the process until we get to the bottom most quads at level $P-1$.  Thus a tree with mesh-power $P$ may have at
most $P$ levels.  Each potential grid point in the tree is assigned integer coordinates with each axis taking values from $[0, 2^P]\\cup\\mathbb{Z}$.

This coordinate system has two important qualities with respect to quad center points:

   * The center point of a quad (at any level) will never the be center point of another quad (at any level)

   * The center point of a quad (at any level) will never be on the edge (including corers) of any other quad (at any level)

This means the set of corner points and center points are disjoint, and that each quad center point uniquely identifies a quad!  Given integer coordinates for
a quad center, it is a simple matter to compute the quad's level in O(p) time.  We may then compute the integer width of the quad.  From the width we may
compute the coordinates of the quad's corners, the centers of the quad's children, the centers of neighboring quads, and shared boundary points with
neighboring quads.  Not only may we directly compute the centers of the quad's children, but we may directly compute the centers of grandchildren too -- or
descendants at any depth.  In a similar way we may find neighbors and shared neighbor boundary points at any depth as well.  All directly and in O(depth)
time!!

These elegant relationships between the integer coordinates of a quad and geometrically significant elements of the grid allow for particularly elegant, and
simple, algorithms to operate on the grid.  What is necessary to implement these algorithms is a data structure that allows us random access to grid data
points via integer coordinates, and allows us to enumerate all data in terms of integer coordinates.

An integer coordinate tuple may be easily transformed into a single, unique integer by packing each coordinate value into a bit field.  The resulting integer
allows one to recover the original coordinate values from the integer.  We may thus store grid data in a hash using this integer as the hash key.  This is
precisely the data structure we need!!

Note that the observations regarding grid points and quads are not confined to any particular dimension -- this all works in any dimension.  What is left is
to generalize the geometric operations on the quadtree to arbitrary dimension, and this may be done by recasting quadtree concepts in terms of set theory and
cross products -- all on the integer coordinate system.

This all leads to the RPTrees implemented in this library.

$P$ -- The mesh power of the quad tree

$n$ -- The dimension of the quad tree

$\overline{\mathbf{c}}=[c_0, ..., c_n]\in\mathbb{Z}_{P+1}^n$ -- a quad coordinate (i.e. the integer tuple corresponding to the center of a quad)

When $n=1$ we use $c$ instead of $\overline{\mathbf{c}}$

$Q(\overline{\mathbf{c}})$ -- The set of all coordinates contained in the quad

$L(\overline{\mathbf{c}})$ -- Level of quad $\overline{\mathbf{c}}$

$E(\overline{\mathbf{c}})$ -- Endpoints/corners

$A(\overline{\mathbf{c}}, d)$ -- Ancestor of level $L(\overline{\mathbf{c}})+d$.  $A(\overline{\mathbf{c}}, 0)=\overline{\mathbf{c}}$. For $d=1$ returns the parent, $d=2$ the grandparent, etc...

$C(\overline{\mathbf{c}}, d)$ -- Children of level $L(\overline{\mathbf{c}})-d$.  $C(\overline{\mathbf{c}}, 0)=\overline{\mathbf{c}}$. For $d=1$ the children, for $d=2$ the grandchildren, etc...  i.e.

$N(\overline{\mathbf{c}}, d)$ -- Neighbors of level $L(\overline{\mathbf{c}})-d$.  For $d=0$ returns same sized neighbors.

$G(\overline{\mathbf{c}}, d)$ -- Grid Points.  The vertexes of $C(\overline{\mathbf{c}}, d)$

$B(\overline{\mathbf{c}}, d)$ -- Boundary Vertexes.  That is the vertexes of $N(\overline{\mathbf{c}}, d)$ on the boundary of $Q(\overline{\mathbf{c}})$ -- i.e. $B(\overline{\mathbf{c}}, d) = \partial Q(\overline{\mathbf{c}}) \cap N(\overline{\mathbf{c}}, d)$

$I(\overline{\mathbf{c}}, d)$ -- Grid Points.  The vertexes of $C(\overline{\mathbf{c}}, d)$ that are on the interior of $Q(\overline{\mathbf{c}})$

Relationships:

$G(\overline{\mathbf{c}}, d) = B(\overline{\mathbf{c}}, d) \overline{\mathbf{c}}up I(\overline{\mathbf{c}}, d)$

$\emptyset = B(\overline{\mathbf{c}}, d) \cap I(\overline{\mathbf{c}}, d)$

$B(\overline{\mathbf{c}}, d) = \partial Q(\overline{\mathbf{c}}) \cap G(\overline{\mathbf{c}}, d) = \partial Q(\overline{\mathbf{c}}) \cap N(\overline{\mathbf{c}}, d)$

$W(\overline{\mathbf{c}})=2^{P-L(\overline{\mathbf{c}})}$ -- Width of the quad.  The smallest quads (level $P-1$) have width $2$, and the buggest quad (level $0$) has width $2^P$

$$E(c)=\{ c-\frac{c-W(c)}{2}, c+\frac{c-W(c)}{2} \}$$

$$E(\overline{\mathbf{c}})=W(c_0) \times \cdots \times W(c_n)=\prod_{j=0}^{n-1}W(c_j)$$

$$Q(c)=\{ j\in\mathbb{Z}_{P+1} | E_0(c) \le c \le E_1(c) \}$$

$$Q(\overline{\mathbf{c}})=Q(c_0) \times \cdots \times Q(c_n)=\prod_{j=0}^{n-1}Q(c_j)$$

$Q(\overline{\mathbf{c}})$ -- The set of all coordinates contained in the quad

Examples
^^^^^^^^

1D level 5 tree
^^^^^^^^^^^^^^^
                                 4                   A family example
                                3|                 Lev Center Left Right           We say that P=5, and we number the levels 0 to 4
                                ||2                  0     10    0    20
                              1 |||                  1     18   10    20
                      0       | |||                  2     1C   18    20
                      |       | |||                  3     1A   18    1C
      o---------------o-------o-ooo---o              4     1B   1A    1C

      000000000000000011111111111111112    <- First digit (in Hex) of integer coordinate
      0123456789ABCDEF0123456789ABCDEF0    <- Second digit (in Hex) of integer coordinate


2D level 5 tree
^^^^^^^^^^^^^^^


      |4342434143424340434243414342434|
32 20 o---------------o-------o-------o --
31 1F |               |       |       | 4
30 1E |               |       |       | 3
29 1D |               |       |       | 4
28 1C |               |   o   |   o   | 2        A family example:
27 1B |               |       |       | 4      |                   |        | grand- | great       |       | grand- | great
26 1A |               | x   x |       | 3    L | CC    LL    UR    | Parent | parent | grandparent | child | child  | grandchild
25 19 |               |       |       | 4    0 | 10,10 00,00 20,20 |      1 |      2 |           3 |   N/A |    N/A |        N/A
24 18 |       o       o---o---o-------o 1    1 | 18,18 10,10 20,20 |      2 |      3 |           4 |     0 |    N/A |        N/A
23 17 |               |   |   |       | 4    2 | 14,14 10,10 18,18 |      3 |      4 |         N/A |     1 |      0 |        N/A
22 16 |             x | o | o | x     | 3    3 | 16,16 14,14 18,18 |      4 |    N/A |         N/A |     2 |      1 |          0
21 15 |               |   |   |       | 4    4 | 13,13 12,12 14,14 |    N/A |    N/A |         N/A |     3 |      2 |          1
20 14 |           x   o---o---o   o   | 2    5 | N/A for P=5       |      - |      - |           - |     - |      - |          -
19 13 |               |o|o|   |       | 4
18 12 |             x |-o-| o | x     | 3      | Count | Width   |   - A quad is identified by the coordinates of its center vertex.
17 11 |               |o|o|   |       | 4    L | 4^L   | 2^(P-L) |   - Number of possible level $L$ quads: $4^L$
16 10 o---------------o---o---o-------o 0    0 |     1 |      32 |   - Width of level $L$ quads: $2^{P-L}$
15 0F |               |               | 4    1 |     4 |      16 |     The width of level $L-1$ quads are always 2 -- not 1 as with some quadtree implementations.
14 0E |               | x   x         | 3    2 |    16 |       8 |   - Neighbors are quads that share a part of an edge
13 0D |               |               | 4    3 |    64 |       4 |   - A quad with no children is called a 'leaf quad'
12 0C |               |   x           | 2    4 |   256 |       2 |   - Our focus is sampling functions with 2D domains, each quad has 5 associated samples
11 0B |               |               | 4    5 |     0 |       - |     - Four vertex samples which may be associated with other quads
10 0A |               |               | 3                              - A single, unique center sample shared with no other quads
09 09 |               |               | 4                              - A quad is said to *exist* in the RPTree if its center coordinate has sample data
08 08 |       o       |       o       | 1
07 07 |               |               | 4
06 06 |               |               | 3
05 05 |               |               | 4
04 04 |               |               | 2
03 03 |               |               | 4
02 02 |               |               | 3
01 01 |               |               | 4
00 00 o---------------o---------------o --
      000000000000000011111111111111112
      0123456789ABCDEF0123456789ABCDEF0

      000000000011111111112222222222333
      012345678901234567890123456789012

Enumerating Points Related To A Quad
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                                                                                  |       |       |       |       |
                                                                                                --o-------o-------o-------o-------o--
                                                                                                  |       |       |       |       |
             NOTE: Points are always returned in lexicographical less than order!                 |       |       |       |       |
                                                                                                  |  Not  |  Nbr  |  Nbr  |  Not  |
                                                                                                  |   A   | Child | Child |   A   |
                                                           |               |                      |  Nbr  |   3   |   5   |  Nbr  |
                                                           |   Neighbor 2  |                      |       |       |       |       |
    Vertex 1 |  Side #(0 1)  | Vertex 3                    |               |                      |       |       |       |       |
           --o-------o-------o--                      -----o---------------o----                --o-------o-------o-------o-------o--
             |       |       |                             |               |                      |       |               |       |
             |       |       |                             |               |                      |       |               |       |
             |       |       |                             |               |                      |  Nbr  |               |  Nbr  |
             | Child | Child |                             |               |                      | Child |               | Child |
             |   1   |   3   |                             |               |                      |   1   |               |   7   |
             |       |       |                             |               |                      |       |               |       |
             |       |       |                             |               |                      |       |               |       |
Side #(-1 0) o-------o-------o Side #(1 0)     Neighbor 0  |       o       |  Neighbor 3        --o-------o               o-------o--
             |       |       |                             |     Center    |                      |       |               |       |
             |       |       |                             |               |                      |       |               |       |
             |       |       |                             |               |                      |  Nbr  |               |  Nbr  |
             | Child | Child |                             |               |                      | Child |               | Child |
             |   0   |   2   |                             |               |                      |   0   |               |   6   |
             |       |       |                             |               |                      |       |               |       |
             |       |       |                             |               |                      |       |               |       |
           --o-------o-------o--                      -----o---------------o----                --o-------o-------o-------o-------o--
    Vertex 0 |  Side #(0 -1) | Vertex 2                    |               |                      |       |       |       |       |
                                                           |   Neighbor 1  |                      |       |       |       |       |
                                                           |               |                      |  Not  |  Nbr  |  Nbr  |  Not  |
                                                                                                  |   A   | Child | Child |   A   |
                                                                                                  |  Nbr  |   2   |   4   |  Nbr  |
           The vertexes and center of a quad are collectively called the Quad's Points            |       |       |       |       |
           They are numbered 0-4: point 0 = vertex 0                                              |       |       |       |       |
                                  point 1 = vertex 1                                            --o-------o-------o-------o-------o--
                                  point 2 = center                                                |       |       |       |       |
                                  point 3 = vertex 2
                                  point 4 = vertex 3



   1                 3     2|       |       |7      4|   |   |   |   |15      8| | | | | | | | |31               Enumerating
    o---------------o     --o-------o-------o--    --o---o---o---o---o--     --o-o-o-o-o-o-o-o-o--             Boundary Vertexes
    |               |       |       4       |        |   6   8  10   |         |  12  16  20   |
    |               |       |               |        |               |       --o 7          30 o--    Boundary points are vertexes of
    |               |       |               |        |               |         |               |           neighboring quads.
    |               |       |               |      --o 3          14 o--     --o 6          29 o--
    |               |       |               |        |               |         |               |      Level 0 == vertexes
    |               |       |               |        |               |       --o 5          28 o--    Level 1 == neighbor child vertexes
    |    DEPTH      |       |     DEPTH     |        |     DEPTH     |         |     DEPTH     |                 on the boundary of quad
    |      0        |     --o 1     1     6 o--    --o 2     2    13 o--     --o 4     3    27 o--    etc...
    |               |       |               |        |               |         |               |
    |               |       |               |        |               |       --o 3          26 o--    Boundary vertex 0 is vertex 0
    |               |       |               |        |               |         |               |
    |               |       |               |      --o 1          12 o--     --o 2          25 o--    Last Boundary vertex is vertex 3
    |               |       |               |        |               |         |               |
    |               |       |               |        |               |       --o 1          24 o--
    |               |       |       3       |        |   5   7   9   |         | 9  13  17  21 |
    o---------------o     --o-------o-------o--    --o---o---o---o---o--     --o-o-o-o-o-o-o-o-o--
   0                 2     0|       |       |5      0|   |   |   |   |11      0| | | | | | | | |23


Triangulating Quads
^^^^^^^^^^^^^^^^^^^^

Quads are triangulated such that:

  * Every triangle has precisely one vertex at the center of the quad
  * Every triangle has precisely two vertices in the boundary of the quad
  * Every quad boundary point is a vertex of two triangles
  * Triangles only overlap on edges and vertices
  * A triangle vertex is a vertex of every triangle containing it
  * No triangles are degenerate (area zero or three collinear vertexes)

Two examples:

                  Balanced Factor 0                             Balanced Factor 1                             Balanced Factor 2
         o---------------o---------------o            o---------------o-------o-------o            o---------------o---o---o-------o
         |\             /|\             /|            |\             /|\     /|\     /|            |\             /|\ /|\ /|\     /|
         | \           / | \           / |            | \           / | \   / | \   / |            | \           / | o | o | \   / |
         |  \         /  |  \         /  |            |  \         /  |  \ /  |  \ /  |            |  \         /  |/ \|/ \|  \ /  |
         |   \       /   |   \       /   |            |   \       /   |   o   |   o   |            |   \       /   o---o---o---o   |
         |    \     /    |    \     /    |            |    \     /    |  / \  |  / \  |            |    \     /   /|\ /|\ /|  / \  |
         |     \   /     |     \   /     |            |     \   /     | /   \ | /   \ |            |     \   /  /  | o | o | /   \ |
         |      \ /      |      \ /      |            |      \ /      |/     \|/     \|            |      \ / /    |/ \|/ \|/     \|
         |       o       |       o       |            |       o-------o-------o-------o            |       o-------o---o---o-------o
         |      / \      |      / \      |            |      / \      |\     /|\     /|            |      / \      |\  |  /|\     /|
         |     /   \     |     /   \     |            |     /   \     | \   / | \   / |            |     /   \     | \ | / | \   / |
         |    /     \    |    /     \    |            |    /     \    |  \ /  |  \ /  |            |    /     \    |  \|/  |  \ /  |
         |   /       \   |   /       \   |            |   /       \   |   o   |   o   |            |   /       \   |   o   |   o   |
         |  /         \  |  /         \  |            |  /         \  |  / \  |  / \  |            |  /         \  |  / \  |  / \  |
         | /           \ | /           \ |            | /           \ | /   \ | /   \ |            | /           \ | /   \ | /   \ |
         |/             \|/             \|            |/             \|/     \|/     \|            |/             \|/     \|/     \|
         o---------------o---------------o            o---------------o-------o-------o            o---------------o-------o-------o

Balanced Quadtrees
^^^^^^^^^^^^^^^^^^^

Traditionally a quadtree is called 'balanced' if no neighboring leaf quads have edges differing in length by more than 2x.  The word 'balanced' is used to
describe this situation because if the quadtree is stored in a tree data structure, then this edge length requirement is a kind of balance condition on the
tree structure.

In this library we generalize the concept of balanced like so:

We say a tree is balanced at depth $D$ if and only if if no neighboring leaf quads have an edges differing in length by more than $2^D$.  See the three
example triangulations above -- each example represents a quadtree balanced at a different depth.

In what follows we start with a tree balanced at depth 2.  We then refine it to a tree of balanced depth 1.  Lastly we triangulate that balance 1 tree.  Note
that triangulations of balanced depth 1 trees have between four and eight triangles per leaf quad.  The lower the balance number, the fatter the triangles
making up our triangulations.  Depth 1 is generally considered a good target for meshes used for additional numerical computation; however, depth 2 or 3 are
fine for many visualization applications.

       Initial Quad Refinement (balance depth 2)                             ==> Added Quads (balance depth 1)                                     ==> Triangulation
       o-------------------------------o-------------------------------o ==> o---------------o---------------o-------o-------o---------------o ==> o---------------o---------------o-------o-------o---------------o
       |                               |                               | ==> |               |               |       |       |               | ==> |\             /|\             /|\     /|\     /|\             /|
       |                               |                               | ==> |               |               |       |       |               | ==> | \           / | \           / | \   / | \   / | \           / |
       |                               |                               | ==> |               |               |       |       |               | ==> |  \         /  |  \         /  |  \ /  |  \ /  |  \         /  |
       |                               |                               | ==> |               |               |   o   |   o   |               | ==> |   \       /   |   \       /   |   o   |   o   |   \       /   |
       |                               |                               | ==> |               |               |       |       |               | ==> |    \     /    |    \     /    |  / \  |  / \  |    \     /    |
       |                               |                               | ==> |               |               |       |       |               | ==> |     \   /     |     \   /     | /   \ | /   \ |     \   /     |
       |                               |                               | ==> |               |               |       |       |               | ==> |      \ /      |      \ /      |/     \|/     \|      \ /      |
       |                               |                               | ==> |       o       |       o       o-------o-------o       o       | ==> |       o       |       o-------o-------o-------o-------o       |
       |                               |                               | ==> |               |               |       |       |               | ==> |      / \      |      / \      |\     /|\     /|      /|\      |
       |                               |                               | ==> |               |               |       |       |               | ==> |     /   \     |     /   \     | \   / | \   / |     / | \     |
       |                               |                               | ==> |               |               |       |       |               | ==> |    /     \    |    /     \    |  \ /  |  \ /  |    /  |  \    |
       |                               |                               | ==> |               |               |   o   |   o   |               | ==> |   /       \   |   /       \   |   o   |   o   |   /   |   \   |
       |                               |                               | ==> |               |               |       |       |               | ==> |  /         \  |  /         \  |  / \  |  /|\  |  /    |    \  |
       |                               |                               | ==> |               |               |       |       |               | ==> | /           \ | /           \ | /   \ | / | \ | /     |     \ |
       |                               |                               | ==> |               |               |       |       |               | ==> |/             \|/             \|/     \|/  |  \|/      |      \|
       |               o               o-------o---o---o---------------o ==> o---------------o---------------o-------o---o---o-------o-------o ==> o---------------o---------------o-------o---o---o-------o-------o
       |                               |       |   |   |               | ==> |               |               |       |   |   |       |       | ==> |\             /|\             /|\     /|\ /|\ /|\     /|\     /|
       |                               |       | o | o |               | ==> |               |               |       | o | o |       |       | ==> | \           / | \           / | \   / | o | o | \   / | \   / |
       |                               |       |   |   |               | ==> |               |               |       |   |   |       |       | ==> |  \         /  |  \         /  |  \ /  |/ \|/ \|  \ /  |  \ /  |
       |                               |   o   o---o---o               | ==> |               |               |   o   o---o---o   o   |   o   | ==> |   \       /   |   \       /   |   o---o---o---o---o   |   o   |
       |                               |       |   |   |               | ==> |               |               |       |   |   |       |       | ==> |    \     /    |    \     /    |  / \  |\ /|\ /|  / \  |  / \  |
       |                               |       | o | o |               | ==> |               |               |       | o | o |       |       | ==> |     \   /     |     \   /     | /   \ | o | o | /   \ | /   \ |
       |                               |       |   |   |               | ==> |               |               |       |   |   |       |       | ==> |      \ /      |      \ /      |/     \|/ \|/ \|/     \|/     \|
       |                               o-------o---o---o       o       | ==> |       o       |       o       o-------o---o---o-------o-------o ==> |       o       |       o-------o-------o---o---o-------o-------o
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> |      / \      |      / \      |\     /|\  |  /|\     /|\     /|
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> |     /   \     |     /   \     | \   / | \ | / | \   / | \   / |
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> |    /     \    |    /     \    |  \ /  |  \|/  |  \ /  |  \ /  |
       |                               |   o   |   o   |               | ==> |               |               |   o   |   o   |   o   |   o   | ==> |   /       \   |   /       \   |   o   |   o   |   o   |   o   |
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> |  /         \  |  /         \  |  / \  |  / \  |  / \  |  / \  |
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> | /           \ | /           \ | /   \ | /   \ | /   \ | /   \ |
       |                               |       |       |               | ==> |               |               |       |       |       |       | ==> |/             \|/             \|/     \|/     \|/     \|/     \|
       o-------------------------------o-------o-------o---------------o ==> o---------------o---------------o-------o-------o-------o-------o ==> o---------------o---------------o-------o-------o-------o-------o
       |                               |                               | ==> |                               |               |               | ==> |\              |              /|\      |      /|\      |      /|
       |                               |                               | ==> |                               |               |               | ==> | \             |             / | \     |     / | \     |     / |
       |                               |                               | ==> |                               |               |               | ==> |  \            |            /  |  \    |    /  |  \    |    /  |
       |                               |                               | ==> |                               |               |               | ==> |   \           |           /   |   \   |   /   |   \   |   /   |
       |                               |                               | ==> |                               |               |               | ==> |    \          |          /    |    \  |  /    |    \  |  /    |
       |                               |                               | ==> |                               |               |               | ==> |     \         |         /     |     \ | /     |     \ | /     |
       |                               |                               | ==> |                               |               |               | ==> |      \        |        /      |      \|/      |      \|/      |
       |                               |                               | ==> |                               |       o       |       o       | ==> |       \       |       /       |       o       |       o       |
       |                               |                               | ==> |                               |               |               | ==> |        \      |      /        |      / \      |      / \      |
       |                               |                               | ==> |                               |               |               | ==> |         \     |     /         |     /   \     |     /   \     |
       |                               |                               | ==> |                               |               |               | ==> |          \    |    /          |    /     \    |    /     \    |
       |                               |                               | ==> |                               |               |               | ==> |           \   |   /           |   /       \   |   /       \   |
       |                               |                               | ==> |                               |               |               | ==> |            \  |  /            |  /         \  |  /         \  |
       |                               |                               | ==> |                               |               |               | ==> |             \ | /             | /           \ | /           \ |
       |                               |                               | ==> |                               |               |               | ==> |              \|/              |/             \|/             \|
       |               o               |               o               | ==> |               o               o---------------o---------------o ==> |               o---------------o---------------o---------------o
       |                               |                               | ==> |                               |               |               | ==> |              / \              |\             /|\             /|
       |                               |                               | ==> |                               |               |               | ==> |             /   \             | \           / | \           / |
       |                               |                               | ==> |                               |               |               | ==> |            /     \            |  \         /  |  \         /  |
       |                               |                               | ==> |                               |               |               | ==> |           /       \           |   \       /   |   \       /   |
       |                               |                               | ==> |                               |               |               | ==> |          /         \          |    \     /    |    \     /    |
       |                               |                               | ==> |                               |               |               | ==> |         /           \         |     \   /     |     \   /     |
       |                               |                               | ==> |                               |               |               | ==> |        /             \        |      \ /      |      \ /      |
       |                               |                               | ==> |                               |       o       |       o       | ==> |       /               \       |       o       |       o       |
       |                               |                               | ==> |                               |               |               | ==> |      /                 \      |      / \      |      / \      |
       |                               |                               | ==> |                               |               |               | ==> |     /                   \     |     /   \     |     /   \     |
       |                               |                               | ==> |                               |               |               | ==> |    /                     \    |    /     \    |    /     \    |
       |                               |                               | ==> |                               |               |               | ==> |   /                       \   |   /       \   |   /       \   |
       |                               |                               | ==> |                               |               |               | ==> |  /                         \  |  /         \  |  /         \  |
       |                               |                               | ==> |                               |               |               | ==> | /                           \ | /           \ | /           \ |
       |                               |                               | ==> |                               |               |               | ==> |/                             \|/             \|/             \|
       o-------------------------------o-------------------------------o ==> o-------------------------------o---------------o---------------o ==> o-------------------------------o---------------o---------------o

Regions and Region Boundries

We define a region, $D$, of $\mathbf{R}^n$ via two different methods:

Signed Distance Functions

Given a function $f:\mathbf{R}^n\rightarrow\mathbf{R}$ (our our signed distance function), then we can define
the region and it's boundary like so:

      The region is $D=f^{-1}((0, -\infty))$.

      The region boundary is $f^{-1}(0)$.

That is to say, the region is where the function is positive and the boundary is where the function is zero.

Example 1: Let $f=\vert{\vec{\mathbf{x}}}\vert-1$.  The region is the open unit ball centered on the origin, and the boundary is the unit sphere centered on the origin.

Example 2: Let $f=1-\vert{\vec{\mathbf{x}}}\vert$.  The boundary is the same as the previous case; however, the region is the compliment of the closed unit ball at the origin.

Exclusion Function

Given a function $f:\mathbf{R}^n\rightarrow\overline{\mathbf{R}}\cup\mathbf{R}$ (our exclusion function) we define the region by
$D=f^{-1}(\overline{\mathbf{R}})$.

That is to say we define the region to be the places where the function is not real.

Example 1: Let $f=\sqrt{\vert{\vec{\mathbf{x}}}\vert-1}$. The region is the the open unit ball

Example 2: Let $f=\frac{1}{\vert{\vec{\mathbf{x}}}\vert}$. The region is the single point at the origin.
"
  (documentation 'mjr_rptree_help 'function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct mjr_rptree_struct
  (mesh-power              0    :type fixnum   :read-only t)
  (qdim                    0    :type fixnum   :read-only t)
  (bbox-min                nil                 :read-only t)
  (bbox-max                nil                 :read-only t)
  (min-coord-real-delta    nil                 :read-only t)
  (data                    nil                 :read-only t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_new (mesh-power bbox-min bbox-max)
  ""
  (let ((qdim (if (vectorp bbox-min) (length bbox-min) 1)))
    (cond ((not (integerp mesh-power))     (error "mjr_rptree_new: MESH-POWER must be an integer!"))
          ((> 1 mesh-power)                (error "mjr_rptree_new: MESH-POWER must be greater than zero!")))
    (if (= 1 qdim)
        (cond
          ((not (realp bbox-min))           (error "mjr_rptree_new: BBOX-MIN must be a real number!"))
          ((not (realp bbox-max))           (error "mjr_rptree_new: BBOX-MAX must be a real number!"))
          ((>= bbox-min bbox-max)           (error "mjr_rptree_new: BBOX-MAX must be greater than BBOX-MIN!")))
        (cond
          ((not (vectorp bbox-min))         (error "mjr_rptree_new: BBOX-MIN must be vectors!"))
          ((not (vectorp bbox-max))         (error "mjr_rptree_new: BBOX-MAX must be vectors!"))
          ((not (= qdim (length bbox-min))) (error "mjr_rptree_new: BBOX-MIN must be of length ~d!" qdim))
          ((not (= qdim (length bbox-max))) (error "mjr_rptree_new: BBOX-MAX must be of length ~d!" qdim))
          ((not (every #'realp bbox-min))   (error "mjr_rptree_new: BBOX-MIN vectors must contain only real numbers!"))
          ((not (every #'realp bbox-max))   (error "mjr_rptree_new: BBOX-MAX vectors must contain only real numbers!"))
          ((some #'>= bbox-min bbox-max)    (error "mjr_rptree_new: BBOX-MAX must be greater than BBOX-MIN!"))))
    (make-mjr_rptree_struct :mesh-power             mesh-power
                            :qdim                   qdim
                            :bbox-min               (if (= 1 qdim)
                                                        (float bbox-min 1.0d0)
                                                        (map 'vector (lambda (x) (float x 1.0d0)) bbox-min))
                            :bbox-max               (if (= 1 qdim)
                                                        (float bbox-max 1.0d0)
                                                        (map 'vector (lambda (x) (float x 1.0d0)) bbox-max))
                            :min-coord-real-delta   (if (= qdim 1)
                                                        (float (/ (- bbox-max bbox-min) (ash 1 mesh-power)) 1.0d0)
                                                        (map 'vector
                                                             (lambda (x) (float x 1.0d0))
                                                             (mjr_vec_/ (mjr_vec_- bbox-max bbox-min) (ash 1 mesh-power))))
                            :data                   (make-hash-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_level-quad-isize (rptree-instance level)
  "Integer width/height (one number) of a LEVEL quad"
  (ash 1 (- (mjr_rptree_struct-mesh-power rptree-instance) level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_level-quad-rsize (rptree-instance level)
  "Real widths (width for 1D, width/height 2D, width/height/depth for 3D, etc...) a quad of the given LEVEL."
  (mjr_vec_/ (mjr_vec_- (mjr_rptree_struct-bbox-max rptree-instance) (mjr_rptree_struct-bbox-min rptree-instance))
             (ash 1 level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_make-lex-< (rptree-instance)
  "Return a comparison function that will, when used with SORT, order integer points lexicographicaly."
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      #'<
      (eval `(lambda (coord1 coord2)
               (loop for c1 across coord1
                     for c2 across coord2
                     when (not (= c1 c2))
                     do (return (< c1 c2))
                     finally (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_min-coord-real-delta (rptree-instance)
  "Compute the x and y real distance corresponding to a one unit change in integer coordinates."
  (if (numberp (mjr_rptree_struct-bbox-min rptree-instance))
      (/ (- (mjr_rptree_struct-bbox-max rptree-instance) (mjr_rptree_struct-bbox-min rptree-instance)) (ash 1 (mjr_rptree_struct-mesh-power rptree-instance)))
      (mjr_vec_/ (mjr_vec_- (mjr_rptree_struct-bbox-max rptree-instance) (mjr_rptree_struct-bbox-min rptree-instance)) (ash 1 (mjr_rptree_struct-mesh-power rptree-instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-2-real (rptree-instance coord)
  "Convert integer coordinate to a real coordinate."
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      (+ (mjr_rptree_struct-bbox-min rptree-instance) (* coord (mjr_rptree_struct-min-coord-real-delta rptree-instance)))
      (map 'vector #'+ (mjr_rptree_struct-bbox-min rptree-instance) (map 'vector #'* coord (mjr_rptree_struct-min-coord-real-delta rptree-instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-2-hash-key (rptree-instance coord)
  "Return a hash key from integer quad coordinates.
Hash keys are integersn integer "
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      coord
      (let ((ret-val 0))
        (loop for c across coord
              for s from 0 by (1+ (mjr_rptree_struct-mesh-power rptree-instance))
              do (setq ret-val (logior ret-val (ash c s)))
              finally (return ret-val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-get-value (rptree-instance coord-or-list-of-coords)
  "Get the data point in DATA-HASH corresponding to QUAD-COORD.  Returns NIL if QUAD-COORD is NIL."
  (if coord-or-list-of-coords
        (if (listp coord-or-list-of-coords)
            (mapcar (lambda (c) (mjr_rptree_coord-get-value rptree-instance c)) coord-or-list-of-coords)
            (gethash (mjr_rptree_coord-2-hash-key rptree-instance coord-or-list-of-coords) (mjr_rptree_struct-data rptree-instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-has-value (rptree-instance coord)
  "Get the data point in DATA-HASH corresponding to QUAD-COORD.  Returns NIL if QUAD-COORD is NIL."
  (if coord
      (multiple-value-bind (value exists) (gethash (mjr_rptree_coord-2-hash-key rptree-instance coord) (mjr_rptree_struct-data rptree-instance))
        (declare (ignore value))
        (and exists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_hash-key-2-coord (rptree-instance hash-key)
  "Return integer quad coordinates from integer hash key."
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      hash-key
      (let ((ret-val (make-array (list (mjr_rptree_struct-qdim rptree-instance)))))
        (loop with maskv = (1- (ash 1 (1+ (mjr_rptree_struct-mesh-power rptree-instance))))
              for i from (1- (mjr_rptree_struct-qdim rptree-instance)) downto 0
              for s from 0 by (1+ (mjr_rptree_struct-mesh-power rptree-instance))
              do (setf (aref ret-val i) (logand maskv (ash hash-key (- s))))
              finally (return ret-val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_level-0-quad-center (rptree-instance)
  "Return the center coordinates of the unique, level 0 quad -- the 'top quad'."
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      (ash 1 (- (mjr_rptree_struct-mesh-power rptree-instance) 1))
      (make-array (list (mjr_rptree_struct-qdim rptree-instance)) :initial-element (ash 1 (- (mjr_rptree_struct-mesh-power rptree-instance) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-level (rptree-instance quad-coord)
  "Return the level of the quad with center at QUAD-COORD, or NIL if QUAD-COORD isn't a valid quad center.
Note: The unique top level quad is level 0.  The smallest quads at the bottom of the tree are at level (1- (mjr_rptree_struct-mesh-power rptree-instance))."
  (let* ((qc0 (if (= 1 (mjr_rptree_struct-qdim rptree-instance)) quad-coord (aref quad-coord 0)))
         (lev (if (and (oddp qc0) (< 0 qc0) (> qc0 (ash 1 (mjr_rptree_struct-mesh-power rptree-instance))))
                  (1- (mjr_rptree_struct-mesh-power rptree-instance))
                  (loop for lev from 0 upto (- (mjr_rptree_struct-mesh-power rptree-instance) 1)
                        do (multiple-value-bind (q r) (truncate qc0 (ash 1 (- (mjr_rptree_struct-mesh-power rptree-instance) lev 1)))
                             (if (and (zerop r) (oddp q) (> q 0) (<= q (1+ (ash (1- (ash 1 lev)) 1))))
                                 (return lev)))))))
    (if lev
        (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
            lev
            (loop for j from 1 upto (1- (length quad-coord))
                  finally (return lev)
                  do (multiple-value-bind (q r) (truncate (aref quad-coord j) (ash 1 (- (mjr_rptree_struct-mesh-power rptree-instance) lev 1)))
                       (if (or (not (zerop r)) (evenp q) (<= q 0) (> q (1+ (ash (1- (ash 1 lev)) 1))))
                           (return nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-centerp (rptree-instance quad-coord)
  "Return non-NIL if QUAD-COORD is the center of a quad."
  (mjr_rptree_quad-get-level rptree-instance quad-coord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-isize (rptree-instance quad-coord)
  "Return integer width of quad with center at QUAD-COORD, or NIL if QUAD-COORD isn't a valid quad center."
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (mjr_rptree_level-quad-isize rptree-instance quad-level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-near-real-point (rptree-instance quad-coord real-coord &optional (how-near 1))
  "Return non-NIL if QUAD-COORD is a valid quad center and near REAL-COORD

Value of HOW-NEAR:
   0 ... REAL-COORD is inside the quad (<= & >= are used to compare real numbers)
   1 ... REAL-COORD is within approximately one MIN-COORD-REAL-DELTA of the quad defined by QUAD-COORD.
   2 ... REAL-COORD is within approximately one 1/2 width of the quad defined by QUAD-COORD"
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (let ((delta (case how-near
                       (0 (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2))
                       (1 (1+ (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2)))
                       (2 (mjr_rptree_level-quad-isize rptree-instance quad-level)))))
          (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
              (and (>= quad-coord (mjr_rptree_coord-2-real rptree-instance (- quad-coord delta)))
                   (<= quad-coord (mjr_rptree_coord-2-real rptree-instance (+ quad-coord delta))))
              (mjr_geom_point-inside-bounding-box-f? real-coord
                                                     (mjr_rptree_coord-2-real rptree-instance (map 'vector (lambda (x) (- x delta)) quad-coord))
                                                     (mjr_rptree_coord-2-real rptree-instance (map 'vector (lambda (x) (+ x delta)) quad-coord))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-vertexes (rptree-instance quad-coord)
  "Return list of quad vertexes (end points in 1D, corners in 2D, etc...), or NIL if QUAD-COORD isn't a valid quad center."
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (let ((qisize2 (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2)))
          (labels ((get-1d-ends (c) (list (+ c qisize2) (- c qisize2))))
            (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
                (reverse (get-1d-ends quad-coord))
                (mjr_combc_gen-all-cross-product (map 'list #'get-1d-ends quad-coord) :collect-value #'copy-seq)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-points (rptree-instance quad-coord)
  "Return list of quad vertexs, or NIL if QUAD-COORD isn't a valid quad center."
  (let ((vertexes (mjr_rptree_quad-get-vertexes rptree-instance quad-coord)))
    (if vertexes
        (append vertexes (list quad-coord)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-in-bounds (rptree-instance coord)
  "Return non-NIL if COORD is within bounds for a quadtree of (MJR_RPTREE_STRUCT-MESH-POWER RPTREE-INSTANCE)."
  (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
      (and (>= coord 0) (<= (ash 1 (mjr_rptree_struct-mesh-power rptree-instance))))
      (let ((maxc (ash 1 (mjr_rptree_struct-mesh-power rptree-instance))))
        (every (lambda (x) (and (<= 0 x) (>= maxc x))) coord))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-boundary-points (rptree-instance quad-coord depth &optional adjust-depth)
  "Return list of quad boundary points and the depth of the returned boundary points.

Return NIL if QUAD-COORD isn't a valid quad center or if DEPTH is negative.  Return NIL if DEPTH too large and and ADJUST-DEPTH is NIL.
If DEPTH is too large and ADJUST-DEPTH is non-NIL, then DEPTH will be adjusted to the maximal valid value."
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (let* ((bdepth (if adjust-depth (min depth (- (mjr_rptree_struct-mesh-power rptree-instance) quad-level 1)) depth))
               (blevel (+ quad-level bdepth)))
          (if (< blevel (mjr_rptree_struct-mesh-power rptree-instance))
              (let ((vertexes (mjr_rptree_quad-get-vertexes rptree-instance quad-coord)))
                (values (if (numberp quad-coord)
                            vertexes
                              (let* ((vs       (car vertexes))
                                     (ve       (car (last vertexes)))
                                     (qisize2 (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2))
                                     (nchild  (ash 1 bdepth))
                                     (bwid    (mjr_rptree_level-quad-isize rptree-instance blevel))
                                     (offsets (loop for i downfrom nchild to 0
                                                    collect (* i bwid))))
                                (mjr_combc_gen-all-cross-product (loop for c across quad-coord
                                                                       collect (mapcar (lambda (y) (+ y (- c qisize2))) offsets))
                                                                 :collect-if (lambda (c) (or (some #'= c vs) (some #'= c ve)))
                                                                 :collect-value #'copy-seq)))
                            bdepth)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-children (rptree-instance quad-coord depth &optional adjust-depth)
  "Return a list of quad children and the depth of the returned children.

Return NIL if QUAD-COORD isn't a valid quad center or if DEPTH is negative.  Return NIL if DEPTH too large and and ADJUST-DEPTH is NIL.
If DEPTH is too large and ADJUST-DEPTH is non-NIL, then DEPTH will be adjusted to the maximal valid value."
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (let ((cdepth (if adjust-depth (min depth (- (mjr_rptree_struct-mesh-power rptree-instance) quad-level 1)) depth)))
          (if (<= (+ quad-level cdepth) (1- (mjr_rptree_struct-mesh-power rptree-instance)))
              (let* ((nchild  (ash 1 cdepth))
                     (qisize2 (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2))
                     (cisize2 (/ qisize2 nchild))
                     (offsets (loop for i downfrom (1- nchild) to 0
                                    collect (* cisize2 (1+ (ash i 1))))))
                (values (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
                            (reverse (mapcar (lambda (y) (+ y (- quad-coord qisize2))) offsets))
                            (mjr_combc_gen-all-cross-product (map 'list
                                                                  (lambda (x) (mapcar (lambda (y) (+ y (- x qisize2))) offsets))
                                                                  quad-coord)
                                                             :collect-value #'copy-seq))
                        cdepth)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-neighbors (rptree-instance quad-coord depth &optional adjust-depth)
  "Return a list of quad neighbors and the depth of the returned neighbors.

Return NIL if QUAD-COORD isn't a valid quad center or if DEPTH is negative.  Return NIL if DEPTH too large and and ADJUST-DEPTH is NIL.
If DEPTH is too large and ADJUST-DEPTH is non-NIL, then DEPTH will be adjusted to the maximal valid value."
  (if (mjr_rptree_coord-in-bounds rptree-instance quad-coord)
      (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
        (if quad-level
            (let ((ndepth (if adjust-depth (min depth (- (mjr_rptree_struct-mesh-power rptree-instance) quad-level 1)) depth)))
              (if (<= (+ quad-level ndepth) (1- (mjr_rptree_struct-mesh-power rptree-instance)))
                  (let* ((nchild  (ash 1 ndepth))
                         (qisize2 (/ (mjr_rptree_level-quad-isize rptree-instance quad-level) 2))
                         (nisize2 (/ qisize2 nchild))
                         (cisize2 nisize2)
                         (offsets (loop for i from 0 upto (1- nchild)
                                        collect (* cisize2 (1+ (ash i 1))))))
                    (labels ((get-1d-neighbors (c) (list (+ c qisize2 nisize2) (- c qisize2 nisize2)))
                             (get-1d-chldren   (c) (mapcar (lambda (y) (+ y (- c qisize2))) offsets)))
                      (values (if (= 1 (mjr_rptree_struct-qdim rptree-instance))
                                  (reverse (get-1d-neighbors quad-coord))
                                  (let* ((nbrs nil))
                                    (loop for i from 0 upto (1- (mjr_rptree_struct-qdim rptree-instance))
                                          do (setq nbrs (merge 'list
                                                               nbrs
                                                               (mjr_combc_gen-all-cross-product (loop for j from 0 upto (1- (mjr_rptree_struct-qdim rptree-instance))
                                                                                                      for c across quad-coord
                                                                                                      collect (if (= i j) (get-1d-neighbors c) (get-1d-chldren c)))
                                                                                                :collect-value #'copy-seq)
                                                               (mjr_rptree_make-lex-< rptree-instance)))
                                          finally (return nbrs))))
                              ndepth)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_quad-get-parent (rptree-instance quad-coord)
  "Return quad's parent, or NIL if QUAD-COORD isn't a valid quad center."
  (let ((quad-level (mjr_rptree_quad-get-level rptree-instance quad-coord)))
    (if quad-level
        (loop for vertex in (mjr_rptree_quad-get-vertexes rptree-instance quad-coord)
              for vertex-level = (mjr_rptree_quad-get-level rptree-instance vertex)
              do (if (and (integerp vertex-level) (= (1- quad-level) vertex-level))
                     (return vertex))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_print (rptree-instance)
  (let* ((c-i-w (ceiling (log (expt 2 (- (mjr_rptree_struct-mesh-power rptree-instance) 2)))))
         (c-k-w (1+ (ceiling (log (expt 2 (+ (mjr_rptree_struct-mesh-power rptree-instance) 1))))))
         (fstr  (format nil "(~~~dd ~~~dd) ~~2a (~~15,4f ~~15,4f) ~~~dd ~a~%" c-i-w c-i-w c-k-w "~20,5f")))
    (loop for c-k being the hash-keys of (mjr_rptree_struct-data rptree-instance)
          for c-i = (mjr_rptree_hash-key-2-coord rptree-instance c-k)
          for c-r = (mjr_rptree_coord-2-real rptree-instance c-i)
          for cp = (mjr_rptree_coord-centerp rptree-instance c-i)
          for l  = (if cp (format nil "~2d" (mjr_rptree_quad-get-level rptree-instance c-i)) "--")
          for d = (gethash c-k (mjr_rptree_struct-data rptree-instance))
          for i from 0
          do (format 't fstr (aref c-i 0) (aref c-i 1) l (aref c-r 0) (aref c-r 1) c-k d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_print-meta (rptree-instance &key title)
  (if title
      (format 't "~a~%" title))
  (let* ((h-k (loop for c-k being the hash-keys of (mjr_rptree_struct-data rptree-instance) collect c-k))
         (h-v (loop for c-k being the hash-keys of (mjr_rptree_struct-data rptree-instance) collect (gethash c-k (mjr_rptree_struct-data rptree-instance)))))
    (multiple-value-bind (minp maxp) (apply #'mjr_geom_bounding-box (mapcar #'vector (remove-if-not #'realp h-v)))
    (format 't "Potential:  ~d~%" (expt (ash 1 (mjr_rptree_struct-mesh-power rptree-instance)) 2))
    (format 't "Points:     ~d~%" (length h-k))
    (format 't "NIL Points: ~d~%" (count nil h-v))
    (format 't "Min Value:  ~a~%" minp)
    (format 't "Max Value:  ~a~%" maxp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_uniform-fsamp (rptree-instance func arg-mode func-check quad-coord depth &optional adjust-depth)
  "Return a list of quad children and the depth of the returned children."
  (loop for c in (mjr_rptree_quad-get-children rptree-instance quad-coord depth adjust-depth)
        do (loop for c-i in (mjr_rptree_quad-get-points rptree-instance c)
                 for c-key = (mjr_rptree_coord-2-hash-key rptree-instance c-i)
                 do (if (not (multiple-value-bind (value exists) (gethash c-key (mjr_rptree_struct-data rptree-instance))
                               (declare (ignore value))
                               exists))
                        (let ((c-r (mjr_rptree_coord-2-real rptree-instance c-i)))
                          (setf (gethash c-key (mjr_rptree_struct-data rptree-instance)) (mjr_util_fun-adapt-hostile-eval-v func c-r arg-mode func-check)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_uniform-fsamp-start (rptree-instance func arg-mode func-check depth &optional adjust-depth)
  (mjr_rptree_uniform-fsamp rptree-instance
                            func
                            arg-mode
                            func-check
                            (mjr_rptree_level-0-quad-center rptree-instance)
                            depth
                            adjust-depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_make-2d-boundary-< (rptree-instance quad-coord)
  "Return a comparison function that will, when used with SORT, order integer boundary points counter clockwise around a 2D quad.  Returns NIL on error."
  (if (= 2 (mjr_rptree_struct-qdim rptree-instance))
      (destructuring-bind (coord-min qv1 qv2 coord-max) (mjr_rptree_quad-get-vertexes rptree-instance quad-coord)
        (if (and coord-min qv1 qv2 coord-max)
            (eval `(lambda (coord1 coord2)
                     (labels ((what-side (c) (cond ((= (aref ,coord-min 0) (aref c 0)) 0)
                                                   ((= (aref ,coord-min 1) (aref c 1)) 1)
                                                   ((= (aref ,coord-max 0) (aref c 0)) 2)
                                                   ((= (aref ,coord-max 1) (aref c 1)) 3))))
                       (let ((side1 (what-side coord1))
                             (side2 (what-side coord2)))
                         (if (= side1 side2)
                             (if (or (= 1 side1) (= 2 side1))
                                 (< (aref coord1 (if (evenp side1) 1 0)) (aref coord2 (if (evenp side1) 1 0)))
                                 (> (aref coord1 (if (evenp side1) 1 0)) (aref coord2 (if (evenp side1) 1 0))))
                             (< side1 side2))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_coord-2-real-points (rptree-instance list-of-coords)
  "Return list of sample points
Sample points are have n real coordinates for nD RPTrees followed by the function value, flattened into multiple values when it is a vector, for the point"
  (loop for c-i in list-of-coords
        for c-r = (mjr_rptree_coord-2-real rptree-instance c-i)
        for c-k = (mjr_rptree_coord-2-hash-key rptree-instance c-i)
        for fv  = (gethash c-k (mjr_rptree_struct-data rptree-instance))
        collect (if fv
                    (if (vectorp fv)
                        (concatenate 'vector c-r fv)
                        (vector (aref c-r 0) (aref c-r 1) fv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr_rptree_balance (rptree-instance func &key
                                                  (arg-mode          :arg-number)
                                                  (func-check        #'realp)
                                                  (depth             1)
                                                  (show-progress     nil))
  "Balance RPTree to given DEPTH refining (and sampling) as required"
  (labels ((balance-quad (quad-coord depth)
             (let ((children (mjr_rptree_quad-get-children rptree-instance quad-coord 1)))
               (if (and children (mjr_rptree_coord-has-value rptree-instance (first children)))
                   (loop for c in children
                         sum (balance-quad c depth))
                   (let* ((neighbors (mjr_rptree_quad-get-neighbors rptree-instance quad-coord (1+ depth))))
                     (if (some (lambda (c) (mjr_rptree_coord-has-value rptree-instance c)) neighbors)
                         (progn (mjr_rptree_uniform-fsamp rptree-instance func arg-mode func-check quad-coord 1)
                                1)
                         0))))))
    (loop for rc = (balance-quad (mjr_rptree_level-0-quad-center rptree-instance) depth)
          for p from 1
          do (if show-progress
                 (format 't "QTREE Balance: Pass ~5d: Refined: ~10d~%" p rc))
          until (zerop rc))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


