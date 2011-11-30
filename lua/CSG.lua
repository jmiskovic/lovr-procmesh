-- CSG - Constructive Solid Geometry

function table.map(t, f)
  local res = {};
  for i,v in ipairs(t) do res[i] = f(v); end
  return res;
end

function table.append(t1, t2)
  local res = table.copy(t1)
  for i,v in ipairs(t2) do table.insert(res, v) end
  return res
end

function table.copy(t)
  local t2 = {}
  for k,v in pairs(t) do t2[k] = v end
  return t2
end

function table.reverse(t)
    local s,t2 = #t,{}
    for i,v in ipairs(t) do
        t2[s-i] = v
    end
    return t2
end

-- Constructive Solid Geometry (CSG) is a modeling technique that uses Boolean
-- operations like union and intersection to combine 3D solids. self.library
-- implements CSG operations on meshes elegantly and concisely using BSP trees,
-- and is meant to serve as an easily understandable implementation of the
-- algorithm. All edge cases involving overlapping coplanar polygons in both
-- solids are correctly handled.
-- 
-- Example usage:
-- 
--     local cube = CSG.cube()
--     local sphere = CSG.sphere({ radius: 1.3 })
--     local polygons = cube.subtract(sphere).toPolygons()
-- 
-- ## Implementation Details
-- 
-- All CSG operations are implemented in terms of two functions, `clipTo()` and
-- `invert()`, which remove parts of a BSP tree inside another BSP tree and swap
-- solid and empty space, respectively. To find the union of `a` and `b`, we
-- want to remove everything in `a` inside `b` and everything in `b` inside `a`,
-- then combine polygons from `a` and `b` into one solid:
-- 
--     a:clipTo(b)
--     b:clipTo(a)
--     a:build(b:allPolygons())
-- 
-- The only tricky part is handling overlapping coplanar polygons in both trees.
-- The code above keeps both copies, but we need to keep them in one tree and
-- remove them in the other tree. To remove them from `b` we can clip the
-- inverse of `b` against `a`. The code for union now looks like self.
-- 
--     a:clipTo(b)
--     b:clipTo(a)
--     b:invert()
--     b:clipTo(a)
--     b:invert()
--     a:build(b:allPolygons())
-- 
-- Subtraction and intersection naturally follow from set operations. If
-- union is `A | B`, subtraction is `A - B = ~(~A | B)` and intersection is
-- `A & B = ~(~A | ~B)` where `~` is the complement operator.
-- 
-- ## License
-- 
-- Copyright (c) 2011 Evan Wallace (http:--madebyevan.com/), under the MIT license.

-- # class CSG

-- Holds a binary space partition tree representing a 3D solid. Two solids can
-- be combined using the `union()`, `subtract()`, and `intersect()` methods.
CSG = class()

function CSG:init()
  self.polygons = {}
end

-- Construct a CSG solid from a list of `CSG.Polygon` instances.
function CSG.fromPolygons(polygons) 
  local csg = CSG()
  csg.polygons = polygons
  return csg
end

function CSG:clone()
  local csg = CSG()
  csg.polygons = table.map(self.polygons, function(p) return p:clone() end)
  return csg
end

function CSG:toPolygons()
  return self.polygons
end

function CSG:toMesh()
  local m = mesh()
  local vertices = {}
  for i,p in ipairs(self.polygons) do
      for j=3,#p.vertices do
        local v = p.vertices[1]
        table.insert(vertices, vec3(v.pos.x, v.pos.y, v.pos.z))
        local v = p.vertices[j-1]
        table.insert(vertices, vec3(v.pos.x, v.pos.y, v.pos.z))
        local v = p.vertices[j]
        table.insert(vertices, vec3(v.pos.x, v.pos.y, v.pos.z))
      end
  --  for j,v in ipairs(p.vertices) do
  --      table.insert(vertices, vec3(v.pos.x, v.pos.y, v.pos.z))
  --  end

  end
  m.vertices = vertices
  m:setColors(255,0,0,255)
  for i = 1,#vertices do
      m:color(i, 200-math.sin(i*4)*50,100+math.cos(i*2)*40,100,255)
  end
  return m
end
-- Return a CSG solid representing space in either self.solid or in the
-- solid `csg`. Neither self.solid nor the solid `csg` are modified.
-- 
--     A.union(B)
-- 
--     +-------+            +-------+
--     |       |            |       |
--     |   A   |            |       |
--     |    +--+----+   =   |       +----+
--     +----+--+    |       +----+       |
--          |   B   |            |       |
--          |       |            |       |
--          +-------+            +-------+
-- 
function CSG:union(csg) 
  local a = CSG.Node(self:clone().polygons)
  local b = CSG.Node(csg:clone().polygons)
  a:clipTo(b)
  b:clipTo(a)
  b:invert()
  b:clipTo(a)
  b:invert()
  a:build(b:allPolygons())
  return CSG.fromPolygons(a:allPolygons())
end

-- Return a CSG solid representing space in self.solid but not in the
-- solid `csg`. Neither self.solid nor the solid `csg` are modified.
-- 
--     A.subtract(B)
-- 
--     +-------+            +-------+
--     |       |            |       |
--     |   A   |            |       |
--     |    +--+----+   =   |    +--+
--     +----+--+    |       +----+
--          |   B   |
--          |       |
--          +-------+
-- 
function CSG:subtract(csg)
  local a = CSG.Node(self:clone().polygons)
  local b = CSG.Node(csg:clone().polygons)
  a:invert()
  a:clipTo(b)
  b:clipTo(a)
  b:invert()
  b:clipTo(a)
  b:invert()
  a:build(b:allPolygons())
  a:invert()
  return CSG.fromPolygons(a:allPolygons())
end

-- Return a CSG solid representing space both self.solid and in the
-- solid `csg`. Neither self.solid nor the solid `csg` are modified.
-- 
--     A.intersect(B)
-- 
--     +-------+
--     |       |
--     |   A   |
--     |    +--+----+   =   +--+
--     +----+--+    |       +--+
--          |   B   |
--          |       |
--          +-------+
-- 
function CSG:intersect(csg)
  local a = CSG.Node(self:clone().polygons)
  local b = CSG.Node(csg:clone().polygons)
  a:invert()
  b:clipTo(a)
  b:invert()
  a:clipTo(b)
  b:clipTo(a)
  a:build(b:allPolygons())
  a:invert()
  return CSG.fromPolygons(a:allPolygons())
end

-- Return a CSG solid with solid and empty space switched. self.solid is
-- not modified.
function CSG:inverse()
  local csg = self:clone()
  table.map(csg.polygons, function(p) p:flip() end)
  return csg
end

-- Construct an axis-aligned solid cuboid. Optional parameters are `center` and
-- `radius`, which default to `[0, 0, 0]` and `[1, 1, 1]`. The radius can be
-- specified using a single number or a list of three numbers, one for each axis.
-- 
-- Example code:
-- 
--     local cube = CSG.cube({
--       center: [0, 0, 0],
--       radius: 1
--     })

function CSG.cube(options)
  local vs = {
    {-1, -1, 1},
    {1, -1, 1},
    {1, 1, 1},
    {-1, 1, 1},
    {-1, -1, -1},
    {1, -1, -1},
    {1, 1, -1},
    {-1, 1, -1}
  };

  local cvs = {
    {0, 1, 2}, {0, 2, 3},
    {1, 5, 6}, {1, 6, 2},
    {5, 4, 7}, {5, 7, 6},
    {4, 0, 3}, {4, 3, 7},
    {3, 2, 6}, {3, 6, 7},
    {4, 5, 1}, {4, 1, 0}
  }

  options = options or {}
  local cen = CSG.Vector(options.center or {0, 0, 0})
  local r = options.radius or {1, 1, 1}
  if (type(r) == "number") then
    r = {r, r, r}
  end

  return CSG.fromPolygons(table.map(cvs, function (triangle)
    local a = CSG.Vector(vs[triangle[1]+1]):plus(cen):times(r[1])
    local b = CSG.Vector(vs[triangle[2]+1]):plus(cen):times(r[1])
    local c = CSG.Vector(vs[triangle[3]+1]):plus(cen):times(r[1])
    local p = CSG.Plane.fromPoints(a, b, c)
    return CSG.Polygon({
      CSG.Vertex(a, p.normal),
      CSG.Vertex(b, p.normal),
      CSG.Vertex(c, p.normal)
    })
  end))
end

-- Construct a solid sphere. Optional parameters are `center`, `radius`,
-- `slices`, and `stacks`, which default to `[0, 0, 0]`, `1`, `16`, and `8`.
-- The `slices` and `stacks` parameters control the tessellation along the
-- longitude and latitude directions.
-- 
-- Example usage:
-- 
--     local sphere = CSG.sphere({
--       center: [0, 0, 0],
--       radius: 1,
--       slices: 16,
--       stacks: 8
--     })

function CSG.sphere(options)
  options = options or {}
  local c = CSG.Vector(options.center or {0, 0, 0})
  local r = options.radius or 1
  local slices = options.slices or 16
  local stacks = options.stacks or 8
  local polygons = {}
  local vertices
  function vertex(theta, phi)
    theta = theta * math.pi * 2
    phi = phi * math.pi
    local dir = CSG.Vector(
      math.cos(theta) * math.sin(phi),
      math.cos(phi),
      math.sin(theta) * math.sin(phi)
    )
    table.insert(vertices, CSG.Vertex(c:plus(dir:times(r)), dir))
  end
  for i = 0,(slices-1) do 
    for j = 0,(stacks-1) do 
      if (j > 0) then
        vertices = {}
        vertex(i / slices, j / stacks)
        vertex((i + 1) / slices, j / stacks) 
        vertex(i / slices, (j + 1) / stacks)
        table.insert(polygons, CSG.Polygon(vertices))
      end
      if (j < stacks - 1) then
        vertices = {}
        vertex((i+1) / slices, j / stacks)
        vertex((i + 1) / slices, (j + 1) / stacks) 
        vertex(i / slices, (j + 1) / stacks)
        table.insert(polygons, CSG.Polygon(vertices))
      end
    end
  end
  return CSG.fromPolygons(polygons)
end

-- Construct a solid cylinder. Optional parameters are `start`, `end`,
-- `radius`, and `slices`, which default to `[0, -1, 0]`, `[0, 1, 0]`, `1`, and
-- `16`. The `slices` parameter controls the tessellation.
-- 
-- Example usage:
-- 
--     local cylinder = CSG.cylinder({
--       start: [0, -1, 0],
--       end: [0, 1, 0],
--       radius: 1,
--       slices: 16
--     })

function CSG.cylinder(options)
  options = options or {}
  local s = CSG.Vector(options.start or {0, -1, 0})
  local e = CSG.Vector(options.stop or {0, 1, 0})
  local ray = e:minus(s)
  local r = options.radius or 1
  local slices = options.slices or 16
  local axisZ = ray:unit()
  local axisX
  if (math.abs(axisZ.y) > 0.5) then
    axisX = CSG.Vector(1, 0, 0):cross(axisZ):unit()
  else
    axisX = CSG.Vector(0, 1, 0):cross(axisZ):unit()
  end
  local axisY = axisX:cross(axisZ):unit()
  local start = CSG.Vertex(s, axisZ:negated())
  local endv = CSG.Vertex(e, axisZ:unit())
  local polygons = {}
  function point(stack, slice, normalBlend)
    local angle = slice * math.pi * 2
    local out = axisX:times(math.cos(angle)):plus(axisY:times(math.sin(angle)))
    local pos = s:plus(ray:times(stack)):plus(out:times(r))
    local normal = out:times(1 - math.abs(normalBlend)):plus(axisZ:times(normalBlend))
    return CSG.Vertex(pos, normal)
  end
  for i = 0,(slices-1) do
    local t0 = i / slices
    local t1 = (i + 1) / slices
    table.insert(polygons, CSG.Polygon({start, point(0, t0, -1), point(0, t1, -1)}))
    table.insert(polygons, CSG.Polygon({point(0, t1, 0), point(0, t0, 0), point(1, t0, 0)}))
    table.insert(polygons, CSG.Polygon({point(0, t1, 0), point(1, t0, 0), point(1, t1, 0)}))
    table.insert(polygons, CSG.Polygon({endv, point(1, t1, 1), point(1, t0, 1)}))
  end
  return CSG.fromPolygons(polygons)
end

-- # class Vector

-- Represents a 3D vector.
-- 
-- Example usage:
-- 
--     CSG.Vector(1, 2, 3)
--     CSG.Vector([1, 2, 3])
--     CSG.Vector({ x: 1, y: 2, z: 3 })

CSG.Vector = class()
function CSG.Vector:init(x, y, z)
  if (z ~= nil) then
    self.x = x
    self.y = y
    self.z = z
  elseif (x.x ~= nil) then
    self.x = x.x
    self.y = x.y
    self.z = x.z
  else
    self.x = x[1]
    self.y = x[2]
    self.z = x[3]
  end
end

function CSG.Vector:clone()
  return CSG.Vector(self.x, self.y, self.z)
end

function CSG.Vector:negated()
  return CSG.Vector(-self.x, -self.y, -self.z)
end

function CSG.Vector:plus(a)
  return CSG.Vector(self.x + a.x, self.y + a.y, self.z + a.z)
end

function CSG.Vector:minus(a)
  return CSG.Vector(self.x - a.x, self.y - a.y, self.z - a.z)
end

function CSG.Vector:times(a)
  return CSG.Vector(self.x * a, self.y * a, self.z * a)
end

function CSG.Vector:dividedBy(a)
  return CSG.Vector(self.x / a, self.y / a, self.z / a)
end

function CSG.Vector:dot(a)
  return self.x * a.x + self.y * a.y + self.z * a.z
end

function CSG.Vector:lerp(a, t)
  return self:plus(a:minus(self):times(t))
end

function CSG.Vector:length()
  return math.sqrt(self:dot(self))
end

function CSG.Vector:unit()
  return self:dividedBy(self:length())
end

function CSG.Vector:cross(a)
  return CSG.Vector(
    self.y * a.z - self.z * a.y,
    self.z * a.x - self.x * a.z,
    self.x * a.y - self.y * a.x
  )
end


-- # class Vertex

-- Represents a vertex of a polygon. Use your own vertex class instead of this
-- one to provide additional features like texture coordinates and vertex
-- colors. Custom vertex classes need to provide a `pos` property and `clone()`,
-- `flip()`, and `interpolate()` methods that behave analogous to the ones
-- defined by `CSG.Vertex`. self.class provides `normal` so convenience
-- functions like `CSG.sphere()` can return a smooth vertex normal, but `normal`
-- is not used anywhere else.

CSG.Vertex = class()
function CSG.Vertex:init(pos, normal)
  self.pos = CSG.Vector(pos)
  self.normal = CSG.Vector(normal)
end

function CSG.Vertex:clone()
  return CSG.Vertex(self.pos:clone(), self.normal:clone())
end

-- Invert all orientation-specific data (e.g. vertex normal). Called when the
-- orientation of a polygon is flipped.
function CSG.Vertex:flip()
  self.normal = self.normal:negated()
end

-- Create a vertex between self.vertex and `other` by linearly
-- interpolating all properties using a parameter of `t`. Subclasses should
-- override self.to interpolate additional properties.
function CSG.Vertex:interpolate(other, t)
  return CSG.Vertex(
    self.pos:lerp(other.pos, t),
    self.normal:lerp(other.normal, t)
  )
end

-- # class Plane

-- Represents a plane in 3D space.

CSG.Plane = class()
function CSG.Plane:init(normal, w)
  self.normal = normal
  self.w = w
end

-- `CSG.Plane.EPSILON` is the tolerance used by `splitPolygon()` to decide if a
-- point is on the plane.
CSG.Plane.EPSILON = 1e-5

function CSG.Plane.fromPoints(a, b, c)
  local n = b:minus(a):cross(c:minus(a)):unit()
  return CSG.Plane(n, n:dot(a))
end


function CSG.Plane:clone()
    return CSG.Plane(self.normal:clone(), self.w)
end

function CSG.Plane:flip()
    self.normal = self.normal:negated()
    self.w = -self.w
end

function bit(p)
  return 2 ^ (p - 1)  -- 1-based indexing
end

-- Typical call:  if hasbit(x, bit(3)) then ...
function hasbit(x, p)
  return x % (p + p) >= p       
end

function setbit(x, p)
  return hasbit(x, p) and x or x + p
end

function clearbit(x, p)
  return hasbit(x, p) and x - p or x
        
end

function bitor(a, b) 
  local res = 0
  for i = 1,4 do
    if hasbit(a, bit(i)) or hasbit(b, bit(i)) then
      res = setbit(res, bit(i))
    end
  end
  return res
end

-- Split `polygon` by self.plane if needed, then put the polygon or polygon
-- fragments in the appropriate lists. Coplanar polygons go into either
-- `coplanarFront` or `coplanarBack` depending on their orientation with
-- respect to self.plane. Polygons in front or in back of self.plane go into
-- either `front` or `back`.
function CSG.Plane:splitPolygon(polygon, coplanarFront, coplanarBack, front, back)
    local COPLANAR = 0
    local FRONT = 1
    local BACK = 2
    local SPANNING = 3

  -- Classify each point as well as the entire polygon into one of the above
  -- four classes.
    local polygonType = 0
    local types = {}
    for i, v in ipairs(polygon.vertices) do
      local t = self.normal:dot(v.pos) - self.w
      local ptype = COPLANAR
      if (t < -CSG.Plane.EPSILON) then
        ptype = BACK
      elseif (t > CSG.Plane.EPSILON) then
        ptype = FRONT
      end
      if (ptype ~= 0) then
        polygonType = setbit(polygonType, bit(ptype))
      end
      table.insert(types, ptype)
    end

  -- Put the polygon in the correct list, splitting it when necessary.
    if(polygonType == COPLANAR) then
      if self.normal:dot(polygon.plane.normal) > 0 then
        table.insert(coplanarFront, polygon)
      else 
        table.insert(coplanarBack, polygon)
      end
    elseif(polygonType == FRONT) then
      table.insert(front, polygon)
    elseif(polygonType == BACK) then
      table.insert(back, polygon)
    elseif(polygonType == SPANNING) then
      local f = {}
      local b = {}
      for i, vi in ipairs(polygon.vertices) do
        local j = (i + 1) % (#polygon.vertices+1)
        if j == 0 then j = 1 end
        local ti = types[i]
        local tj = types[j]
        local vj = polygon.vertices[j]
        if (ti ~= BACK) then table.insert(f, vi) end
        if (ti ~= FRONT) then
          if (ti ~= BACK) then
            table.insert(b, vi:clone())
          else
            table.insert(b, vi)
          end
        end
        if (bitor(ti, tj) == SPANNING) then
          local t = (self.w - self.normal:dot(vi.pos)) / self.normal:dot(vj.pos:minus(vi.pos))
          local v = vi:interpolate(vj, t)
          table.insert(f, v)
          table.insert(b, v:clone())
        end
      end
      if (#f >= 3) then table.insert(front, CSG.Polygon(f, polygon.shared)) end
      if (#b >= 3) then table.insert(back, CSG.Polygon(b, polygon.shared)) end
    end
end

-- # class Polygon

-- Represents a convex polygon. The vertices used to initialize a polygon must
-- be coplanar and form a convex loop. They do not have to be `CSG.Vertex`
-- instances but they must behave similarly (duck typing can be used for
-- customization).
-- 
-- Each convex polygon has a `shared` property, which is shared between all
-- polygons that are clones of each other or were split from the same polygon.
-- self.can be used to define per-polygon properties (such as surface color).

CSG.Polygon = class()
function CSG.Polygon:init(vertices, shared)
  self.vertices = vertices
  self.shared = shared
  self.plane = CSG.Plane.fromPoints(vertices[1].pos, vertices[2].pos, vertices[3].pos)
end

function CSG.Polygon:clone()
  local vertices = table.map(self.vertices, function(v) return v:clone() end)
  return CSG.Polygon(vertices, self.shared)
end

function CSG.Polygon:flip()
  table.map(table.reverse(self.vertices), function(v) v:flip() end)
  self.plane:flip()
end

-- # class Node

-- Holds a node in a BSP tree. A BSP tree is built from a collection of polygons
-- by picking a polygon to split along. That polygon (and all other coplanar
-- polygons) are added directly to that node and the other polygons are added to
-- the front and/or back subtrees. self.is not a leafy BSP tree since there is
-- no distinction between internal and leaf nodes.

CSG.Node = class()
function CSG.Node:init(polygons)
  self.plane = nil
  self.front = nil
  self.back = nil
  self.polygons = {}
  if (polygons) then self:build(polygons) end
end

function CSG.Node:clone()
  local node = CSG.Node()
  node.plane = self.plane and self.plane:clone()
  node.front = self.front and self.front:clone()
  node.back = self.back and self.back:clone()
  node.polygons = table.map(self.polygons, function(p) return p:clone() end)
  return node
end

-- Convert solid space to empty space and empty space to solid space.
function CSG.Node:invert()
  for i, p in ipairs(self.polygons) do
    p:flip()
  end
  self.plane:flip()
  if (self.front) then self.front:invert() end
  if (self.back) then self.back:invert() end
  local temp = self.front
  self.front = self.back
  self.back = temp
end

-- Recursively remove all polygons in `polygons` that are inside self.BSP
-- tree.
function CSG.Node:clipPolygons(polygons)
    if (not self.plane) then return table.copy(polygons) end
    local front = {}
    local back = {}
    for i, p in ipairs(polygons) do
      self.plane:splitPolygon(p, front, back, front, back)
    end
    if (self.front) then front = self.front:clipPolygons(front) end
    if (self.back) then back = self.back:clipPolygons(back)
    else back = {} end
    return table.append(front, back)
end

-- Remove all polygons in self.BSP tree that are inside the other BSP tree
-- `bsp`.
function CSG.Node:clipTo(bsp)
    self.polygons = bsp:clipPolygons(self.polygons)
    if (self.front) then self.front:clipTo(bsp) end
    if (self.back) then self.back:clipTo(bsp) end
end

-- Return a list of all polygons in self.BSP tree.
function CSG.Node:allPolygons()
  local polygons = table.copy(self.polygons)
  if (self.front) then polygons = table.append(polygons, self.front:allPolygons()) end
  if (self.back) then polygons = table.append(polygons, self.back:allPolygons()) end
  return polygons
end

-- Build a BSP tree out of `polygons`. When called on an existing tree, the
-- polygons are filtered down to the bottom of the tree and become new
-- nodes there. Each set of polygons is partitioned using the first polygon
-- (no heuristic is used to pick a good split).
function CSG.Node:build(polygons)
  if (#polygons == 0) then return end
  if (not self.plane) then self.plane = polygons[1].plane:clone() end
  local front = {}
  local back = {}
  for i, p in ipairs(polygons) do
    self.plane:splitPolygon(p, self.polygons, self.polygons, front, back)
  end
  if (#front ~= 0) then
    if (not self.front) then self.front = CSG.Node() end
    self.front:build(front)
  end
  if (#back ~= 0) then
    if (not self.back) then self.back = CSG.Node() end
    self.back:build(back)
  end
end









