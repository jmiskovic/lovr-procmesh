This repository is collection of Lua libraries for creating meshes from scratch and for constructing more complex objects from primitives.

The intended use is for constructing low-polygon meshes that can be dynamically adapted during run time. This works well with Lua's interpreter to enable asset generation with live-coding.

Code is meant to be used within [LÖVR](https://github.com/bjornbytes/lovr) framework. With simple substitution of vector and mesh data structures the code could be used elsewhere. Libraries have no inter-dependencies so they can be used separately.

# solids

![showcase of solids](media/solids.png?raw=true "Showcase of solids")

The module can be used to manipulate the mesh data, for example to copy, merge or transform the mesh. The module also contains a selection of geometric primitives that are computed from scratch.

The advantage over LÖVR built-in primitives is ability to manipulate the mesh before rendering. The disadvantage is that UV maps are not computed, so textures and surface shader effects won't work with them.

Included primitive solids are:

* `empty()` a blank mesh object
* `quad(subdivisions)` a 2D rectangle with optional subdivision
* `cube()` has top, bottom, front, back, and left, right sides
* `cubetrunc(slant)`, AKA rhombicuboctahedron, can be parametrized with slant between 0 and 1
* `bipyramid(segments)` has top, bottom and ring sides 
* `pyramid(segments)`  is a special case of bipyramid with flat base
* `cylinder(segments)`, has bottom and top sides
* `sphere(subdivisions)`  an icosphere with configurable number of details; more than 3 subdivisions create enormous amount of data

Geometry primitives are procedurally generated with specified number of segments / subdivisions. Vertex positions are computed and vertices are organized as triangle triplets into index list in standard OpenGL fashion for triangle meshes. This data is packed into [mesh](https://lovr.org/docs/Mesh) userdata.

Geometry-generating structures also return `sides` table that lists indices of which vertices belong to which sides of primitive. For example, a cylinder has bottom and top side, and it can be useful to manipulate only vertices on the top side.

```lua
solids = require('solids')
cube, sides = solids.cube()
  -- cube is mesh userdata that can be rendered with cube:draw()
  -- sides is map of cube sides (top, bottom, left, right, up, down), each side being list of vertex indices

function lovr.draw()
  cube:draw(0, 1, -2)
end
```

Note that vertices of adjacent faces are not shared. This allows vertices to have different normals, so cube can have hard edges when rendered with appropriate shader. Even for cylinder, the curved surface subdivided into segments has separate non-smoothed normal for each segment. This is in line with low-polygon aesthetics which is intended use of this library.

### Mesh utility functions

#### transform
Function `transform(mesh, m, side)` is used to displace, rotate or scale mesh vertices by applying Mat4 parameter to each. If `side` table is specified, only vertices with indices listed in table will be affected.

```lua
cube, sides = solids.cube()
solids.transform(cube, mat4(0,0,0,  2,2,2)) -- double the cube size
solids.transform(cube, mat4(0,0,0,  0.5, 1, 0.5), sides.top) -- shrink the top side
```

#### map

Function `map(mesh, callback)` iterates over all vertices and calls the passed callback function to modify the mesh in place.

```lua
modified.quad = solids.quad(6)
solids.map(modified.quad,
  function(x, y, z) 
    z = (lovr.math.noise(x, y) - 0.5) * 2
    return x, y, z
  end)
```

#### subdivide
Function `subdivide(mesh)` creates a new mesh with 4x times more geometry than original mesh, while preserving the shape. Each triangle is subdivided into four triangles. The generated triangles don't share any vertices between them. This operation can be used before the mesh is further processed by `map` function, to increase the fidelity of result.

```lua
mesh = solids.bipyramid(3)     -- 18 vertices
mesh = solids.subdivide(mesh)  -- 72 vertices
```

#### updateNormals

Function `updateNormals()` calculates normals for each triangle and stores them into vertices data. This is only needed for shaders which often use this per-vertex data to calculate the surface lightning. This function should be called after all vertex manipulations are done.

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

#### draw

Function `draw()` renders the mesh in wireframe, with face normals visualized. This is only used for inspection of meshes during development; ordinarily the meshes themselves would be rendered directly. 

```lua
sphere = solids.sphere(2) -- be careful with subdivisions > 3 as geometry count explodes
solids.updateNormals(sphere)

function lovr.draw()
  solids.draw(sphere, 0, 2, -2)  -- draw at 0, 2, -2 coordinates
end
```

#### extract

The `extract()` function creates table of vertices and table of indices from mesh. It can be used to pass onto physics engine to create trimesh collider.

```lua
pyramid = solids.pyramid(5)  -- pyramid needs number of sides (same for bipyramid and cylinder)

world = lovr.physics.newWorld()
collider = world:newMeshCollider(solids.extract(pyramid))
```

#### merge

The merge function can be used to merge two or more meshes into a single mesh. By flattening large amount of geometry into a single (preferably static) mesh, it is possible to eliminate draw calls and thus improve performance.

```lua
-- merge two or more meshes
local merged = solids.merge(meshA, meshB)
-- merge together a list of meshes
local merged = solids.merge(solids.empty(), unpack(meshList))
```

#### copy and toStatic

There are two function for creating a copy of mesh. The `copy()` creates a dynamic mesh (which is also what other functions use), while `toStatic()` creates a static mesh from input mesh. Complex meshes that don't need further manipulations can be converted to static mode for performance, see [MeshUsage](https://lovr.org/docs/MeshUsage).

```lua
cylinder = solids.cylinder(8)
anotherCylinder = solids.copy(cylinder)     -- now each mesh instance can be manipulated independently
staticCylinder = solids.toStatic(cylinder)  -- also a copy, but uses 'static' mesh for performance
```

TODO:
* more solids
* subdivision function
* UV coordinates for texturing
* computing center of mass

# csg

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