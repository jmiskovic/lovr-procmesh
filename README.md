# lovr-procmesh

This repository is collection of Lua libraries for creating meshes from scratch and for constructing more complex objects from primitives.

The intended use is for constructing low-polygon meshes that can be dynamically adapted during run time. This works well with Lua's interpreter to enable asset generation with live-coding.

Code is meant to be used within [LÖVR](https://github.com/bjornbytes/lovr) framework. With simple substitution of vector and mesh data structures the code could be used elsewhere. Libraries have no inter-dependencies so they can be used separately.

## solids.lua

![showcase of solids](media/solids.png?raw=true "Showcase of solids")

This library can generate and manipulate meshes of geometry primitives. The advantage over LÖVR built-in primitives is ability to manipulate the mesh before rendering.

Included solids are: `cube`, `cantellatedCube` (rhombicuboctahedron), `bipyramid`, `pyramid`, `cylinder`, `sphere` (icosphere).

```lua
solids = require('solids')
cube, sides = solids.cube()
  -- cube is mesh userdata that can be rendered with cube:draw()
  -- sides is map of cube sides (top, bottom, left, right, up, down), each side being list of vertex indices
```

Geometry primitives are procedurally generated with specified number of segments / subdivisions. Vertex positions are computed and vertices are organized as triangle triplets into index list in standard OpenGL fashion for triangle meshes. This data is packed into [mesh](https://lovr.org/docs/Mesh) userdata.

Geometry-generating structures also return `sides` table that specifies which vertices belong to which sides of primitive. For example, cylinder has bottom and top side.

Function `transform()` is used to displace, rotate or scale mesh vertices by applying Mat4 parameter to each. If `side` table is specified, only vertices with indices listed in table will be affected.

```lua
cube, sides = solids.cube()
-- double the cube size
solids.transform(cube, mat4(0,0,0,  2,2,2))
-- shrink the top side
truncated_pyramid =  solids.transform(cube, mat4(0,0,0,  0.5, 1, 0.5), sides.top)

function lovr.draw()
  truncated_pyramid:draw(0, 1, -2)
end
```

Function `updateNormals()` calculates normals for each triangle and stores them into vertices data. This is only needed for shaders which often use this per-vertex data to calculate surface lightning. This function should be called after all vertex manipulations are done.

```lua
cuboid, sides = solids.cube()
-- rotate top
solids.transform(cuboid, mat4(0,0,0, math.pi/6, 0,1,0), sides.top)
solids.updateNormals(cuboid)
lovr.graphics.setShader(lovr.graphics.newShader('standard'))

function lovr.draw()
  cuboid:draw(0, 2, -2)
end
```

Function `debugDraw()` renders the mesh in wireframe, with vertex normals and face normals visualized.

```lua
sphere = solids.sphere(2) -- be careful with subdivisions > 3 as geometry count explodes
solids.updateNormals(sphere)

function lovr.draw()
  solids.debugDraw(sphere, 0, 2, -2)
end
```

The `extract()` function creates table of vertices and table of indices from mesh. It can be used to pass onto physics engine to create trimesh collider.

```lua
pyramid = solids.pyramid(5)  -- pyramid needs number of sides (same for bipyramid and cylinder)

world = lovr.physics.newWorld()
collider = world:newMeshCollider(solids.extract(pyramid))
```

There are two function for creating a copy of mesh. The `copy()` creates a dynamic mesh (which is also what other functions use), while `toStatic()` creates a static mesh from input mesh (see [MeshUsage](https://lovr.org/docs/MeshUsage)).

```lua
cylinder = solids.cylinder(8)
anotherCylinder = solids.copy(cylinder)     -- now each can be manipulated independently
staticCylinder = solids.toStatic(cylinder)  -- also a copy, but uses 'static' mesh for performance
```

Note that vertices of adjacent faces are not shared. This allows vertices to have different normals, so cube can have hard edges when rendered with appropriate shader. Even for cylinder, the curved surface subdivided into segments has separate non-smoothed normal for each segment. This is in line with low-polygon aesthetics which is intended use of this library.

TODO:
* more solids (plane, torus)
* subdivision function
* UV coordinates for texturing
* function for vertex coloring
* optional vertex sharing for smooth surfaces
* computing center of mass

## csg.lua

![realtime demo of CSG operations](media/csg.gif "Realtime CSG")

The Constructive Solid Geometry is a technique of modeling complex shapes by adding, subtracting and intersecting meshes. The csg library is implementation of CSG algorithms using efficient binary space partitioning. 

The algorithm was originaly constructed by Evan Wallace as JS library and ported to Lua by Tobias Teleman. This codebase fixes and improves on Lua code and adapts it to LOVR's data structures by using `lovr.math` vectors and implementing Mesh import and export.


```lua
csg = require('csg')
csgA = csg.fromMesh(meshA)
csgB = csg.fromMesh(meshB)
csgU = csgA:union(csgB)       -- also works: csgU = csgA + csgB
csgI = csgA:intersect(csgB)               -- csgI = csgA * csgB
csgS = csgA:subtract(csgB)                -- csgS = csgA - csgB
meshU = csgU:toMesh()
meshI = csgU:toMesh()
meshS = csgU:toMesh()
```

The input [mesh](https://lovr.org/docs/Mesh) is LOVR's userdata object initialized from list of vertices and indices. The CSG library converts it to list of polygons which are split as needed. Operations don't modify input mesh or input CSG objects, they create and return new CSG object as result.

At initialization of CSG object, an additional table `shared` can be optionally given with custom information related to mesh. All polygons originating from this mesh will have reference to same custom information. This can be used to propagate colors or material information related to individual meshes that take part in CSG operations. To take advantage of this additional information, a custom `toMesh()` function should export it using the app-specific vertex data format.

TODO:
 * non-intersecting polygons still generate excess geometry 
 * pick better split heuristics than using the first polygon
 * all [issues from original repo](https://github.com/evanw/csg.js/issues) are retained in this implementation

## License

The code in repository is under MIT license.

The sphere generation uses [lovr-icosphere](https://github.com/bjornbytes/lovr-icosphere) code under MIT license.

The csg algorithm originates from [https://github.com/evanw/csg.js](https://github.com/evanw/csg.js) code under MIT license.