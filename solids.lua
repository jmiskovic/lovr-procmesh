--- Solids are used to create and manipulate triangle meshes
-- The "solid" here refers to a triangle mesh stored in a table:
--  {
--    vlist = { {0,0,0,_}, {1,2,3,_}, _} -- list of vertices storing data (positions, normals, colors...)
--    ilist = {1, 2, 3, _},              -- flat list of indices; triplets that from the triangles
--    sides = {top = {1, 2, 3, _}, _}    -- shape sides mapped to the list of indices
--    vbuffer = Buffer(),                -- vertex buffer object for rendering, regenerated as needed
--    ibuffer = Buffer(),                -- index buffer object for rendering, regenerated as needed
--  }, with metatable accessors to manipulating functions.

local m = {}
m.__index = m

m.vbuffer_format = {
  { 'VertexPosition', 'vec3' },
  { 'VertexNormal',   'vec3' },
}

local EPSILON = 1e-6

local function listappend(t1, t2) -- mutates t1 in place
  for _,v in ipairs(t2) do
    table.insert(t1, v)
  end
  return t1
end


local function shuffle(list) -- shuffles a copy of a table list
  local shuffled = listappend({}, list)
  for i = 1, #shuffled do
    local j = lovr.math.random(1, #shuffled)
    shuffled[i], shuffled[j] = shuffled[j], shuffled[i]
  end
  return shuffled
end


-- to use integer pairs/triples as table keys, first convert them to a string
local function listToHash(...)   -- listToHash(1,2,3) -> '1;2;3'
  return table.concat({...}, ',')
end


local function hashToList(s)     -- hashToList('1;2;3') -> 1, 2, 3
  local res = {}
  for str in string.gmatch(s, "([^,]+)") do
    table.insert(res, tonumber(str))
  end
  return unpack(res)
end


--- Sets future buffer format fields; best called upfront on init
-- existing solids need to be invalidated manually, for example by replacing them with :clone()
function m.setFormats(vertex_format, index_format)
  m.vbuffer_format = vertex_format or m.vbuffer_format
  m.ibuffer_format = index_format or m.ibuffer_format
end


--- Distance of a point vector from a line segment formed from two vectors
function m.linePointDistance(line, point)
  local segment = (line[2] - line[1]):normalize()
  return segment:mul(segment:dot(point - line[1])):distance(point)
end


--- Distance of a point vector from a triangle formed from three vectors
-- if positive, the point is in front of the triangle
-- if zero, point and triangle are coplanar
-- if negative, the point is behind the triangle
function m.trianglePointDistance(triangle, point)
  local ux = triangle[2][1] - triangle[1][1]
  local uy = triangle[2][2] - triangle[1][2]
  local uz = triangle[2][3] - triangle[1][3]
  local vx = triangle[3][1] - triangle[1][1]
  local vy = triangle[3][2] - triangle[1][2]
  local vz = triangle[3][3] - triangle[1][3]
  local cx = uy * vz - uz * vy
  local cy = uz * vx - ux * vz
  local cz = ux * vy - uy * vx
  local dx = point[1] - triangle[1][1]
  local dy = point[2] - triangle[1][2]
  local dz = point[3] - triangle[1][3]
  return cx * dx + cy * dy + cz * dz
--[[ simple variant with lovr vectors (taxes the vector pool too much):
  local n = (triangle[2] - triangle[1]):cross(triangle[3] - triangle[1])
  return (point - triangle[1]):dot(n)
--]]
end


function m.barycentric(triangle, point)
  local AB = {triangle[2][1] - triangle[1][1], triangle[2][2] - triangle[1][2], triangle[2][3] - triangle[1][3]}
  local AC = {triangle[3][1] - triangle[1][1], triangle[3][2] - triangle[1][2], triangle[3][3] - triangle[1][3]}
  local AP = {point[1] - triangle[1][1], point[2] - triangle[1][2], point[3] - triangle[1][3]}
  local dotABAB = AB[1]*AB[1] + AB[2]*AB[2] + AB[3]*AB[3]
  local dotACAC = AC[1]*AC[1] + AC[2]*AC[2] + AC[3]*AC[3]
  local dotABAC = AB[1]*AC[1] + AB[2]*AC[2] + AB[3]*AC[3]
  local dotABAP = AB[1]*AP[1] + AB[2]*AP[2] + AB[3]*AP[3]
  local dotACAP = AC[1]*AP[1] + AC[2]*AP[2] + AC[3]*AP[3]
  local denom = dotABAB * dotACAC - dotABAC * dotABAC
  if (denom == 0) then
      return nil -- degenerate triangle
  end
  local u = (dotACAC * dotABAP - dotABAC * dotACAP) / denom
  local v = (dotABAB * dotACAP - dotABAC * dotABAP) / denom
  local coords = {1 - u - v, u, v}
  local reconstructed = vec3(
    coords[1] * triangle[1][1] + coords[2] * triangle[2][1] + coords[3] * triangle[3][1],
    coords[1] * triangle[1][2] + coords[2] * triangle[2][2] + coords[3] * triangle[3][2],
    coords[1] * triangle[1][3] + coords[2] * triangle[2][3] + coords[3] * triangle[3][3])
  if reconstructed:distance(point) < EPSILON then
    return coords
  else
    return nil -- point is not on triangle plane
  end
end


--- Cast a ray and return list of intersection points with the solid
-- uses Möller-Trumbore algorithm
function m:raycast(start, stop, doublesided)
  if self.normals_dirty then self:updateNormals() end
  local intersections = {}
  local dir = stop - start
  for i = 1, #self.ilist, 3 do
    local i1, i2, i3 = self.ilist[i], self.ilist[i+1], self.ilist[i+2]
    local a, b, c = vec3(unpack(self.vlist[i1])), vec3(unpack(self.vlist[i2])), vec3(unpack(self.vlist[i3]))
    if doublesided or dir:dot(self.vlist[i1][4], self.vlist[i1][5], self.vlist[i1][6]) <= 0 then
      local edge1 = b - a
      local edge2 = c - a
      local h = vec3(dir):cross(edge2)
      local par = edge1:dot(h)
      if par <= -EPSILON or par >= EPSILON then -- check if the ray and triangle are parallel
        local f = 1 / par
        local s = start - a
        local u = f * s:dot(h)
        if u >= 0 and u <= 1 then -- else intersection falls outside the triangle
          local q = s:cross(edge1)
          local v = f * dir:dot(q)
          if v >= 0 and u + v <= 1 then -- else intersection falls outside the triangle
            local t = f * edge2:dot(q)
            if t > EPSILON and t < 1 then -- chech is intersection behind the start point
              -- intersection is inside triangle
              local point = start + dir * t
              table.insert(intersections, point)
            end
          end
        end
      end
    end
  end
  return intersections
end


function m:centerOfMass()
    local center = vec3(0, 0, 0)
    for i = 1, #self.vlist do
        center:add(self.vlist[i][1], self.vlist[i][2], self.vlist[i][3])
    end
    center = center / #self.vlist
    return center
end


function m:volume()
    local total_volume = 0
    local centroid = self:centerOfMass()
    for i = 1, #self.ilist, 3 do
        local v1 = vec3(self.vlist[self.ilist[i]][1], self.vlist[self.ilist[i]][2], self.vlist[self.ilist[i]][3]) - centroid
        local v2 = vec3(self.vlist[self.ilist[i + 1]][1], self.vlist[self.ilist[i + 1]][2], self.vlist[self.ilist[i + 1]][3]) - centroid
        local v3 = vec3(self.vlist[self.ilist[i + 2]][1], self.vlist[self.ilist[i + 2]][2], self.vlist[self.ilist[i + 2]][3]) - centroid
        -- Calculate the volume of the tetrahedron formed by the triangle and the reference point
        local volume = math.abs(v1:dot(v2:cross(v3)) / 6)
        total_volume = total_volume + volume
    end
    return total_volume
end


--- Get a string with basic info about solid
function m:info()
  return string.format('Solid: %d vertices and %d indices; volume: %.2f, center-of-mass: %s',
    #self.vlist,
    #self.ilist,
    self:volume(),
    self:centerOfMass())
end


--- Create solid from existing using a user fn to process each vertex.
-- Preserves all the non-modified vertex information (for example colors).
--    modified_solid = solid_obj:map(function(x,y,z, ...)
--        -- manipulate x,y,z as needed
--        return x, y, z
--      end)
function m:map(fn, side_filter)
  local other = m.new()
  if side_filter then
    for i, vertex in ipairs(self.vlist) do
      other.vlist[i] = {unpack(vertex)}
    end
    for _, i in ipairs(side_filter) do
      local retvals = { fn(unpack(self.vlist[i])) }
      for j, val in ipairs(retvals) do
        other.vlist[i][j] = val
      end
    end
  else
    for i, v in ipairs(self.vlist) do
      other.vlist[i] = { fn(unpack(v)) }
      listappend(other.vlist[i], {select(#other.vlist[i] + 1, unpack(v))})
    end
  end
  for i, index in ipairs(self.ilist) do
    other.ilist[i] = index
  end
  for side, ilist in pairs(self.sides) do
    other.sides[side] = {}
    for i, index in ipairs(ilist) do
      other.sides[side][i] = index
    end
  end
  return other
end


--- Create a solid by transforming each vertex by mat4.
-- Example for laying down an upright mesh:
--    solid:transform(mat4():rotate(pi/2, 1,0,0))
function m:transform(transform, side_filter)
  local tvec3 = vec3()
  return self:map(function(x, y, z)
      return transform:mul(tvec3:set(x, y, z)):unpack()
    end, side_filter)
end


--- Bakes the additional data into all the vertex
-- Assumes that the first 6 vertex data are position and normal
-- Any other vertex data (material information, baked lighting) would go after and can be modified
-- This function applies same data to all the vertices
function m:bakeFill(...)
  local vertex_attributes = {...}
  return self:map(function(...)
    local v = {...}
    for i, vertex_attribute in ipairs(vertex_attributes) do
      v[6 + i] = vertex_attribute
    end
    return unpack(v)
  end)
end


--- Create new solid identical to existing one.
function m:clone()
  local other = m.new()
  for i, vertex in ipairs(self.vlist) do
    other.vlist[i] = {unpack(vertex)}
  end
  for i, index in ipairs(self.ilist) do
    other.ilist[i] = index
  end
  for side, ilist in pairs(self.sides) do
    other.sides[side] = {}
    for i, index in ipairs(ilist) do
      other.sides[side][i] = index
    end
  end
  other.vbuffer_format = self.vbuffer_format
  other.ibuffer_format = self.ibuffer_format
  other.normals_dirty = self.normals_dirty
  return other
end


--- Create solid with no vertices shared between the two faces
-- If vertices are shared between two triangles, the normals get interpolated and the lighting will
-- look incorrect; neighbor triangles will blend into each other as if the surface is smooth.
--     2          2 5
--   / | \       / | \
--  1  |  4  => 1  |  4
--   \ | /       \ | /
--     3          3 6
function m:separateFaces()
  local other = m.fromVertices(self.vlist, self.ilist)
  local visited = {}
  for i, index in ipairs(other.ilist) do
    if visited[index] then
      local new_index = #other.vlist + 1
      other.vlist[new_index] = {unpack(self.vlist[index])}
      other.ilist[i] = new_index
    end
    visited[index] = true
  end
  return other
end


--- Create solid with flipped vertex order.
-- Reverses the face normals by changing the triangle winding.
function m:flipWinding()
  local other = self:clone()
  for i = 1, #other.ilist, 3 do
    other.ilist[i + 1], other.ilist[i + 2] = other.ilist[i + 2], other.ilist[i + 1]
  end
  return other
end


--- Create solid with x4 the geometry by subdividing each triangle.
-- ABC triangle generates 4 smaller triangles:
--       B---AB---A
--        \  /\  /
--         \/__\/
--        BC\  /CA
--           \/
--           C
function m:subdivide()
  local function halfway(p1, p2)
    return {(p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2, (p1[3] + p2[3]) / 2}
  end
  local other = self.new()
  local vertices = {}
  local indices = {}
  for i = 1, #self.ilist, 3 do
    local i1, i2, i3 = self.ilist[i + 0], self.ilist[i + 1], self.ilist[i + 2]
    local va, vb, vc = self.vlist[i1], self.vlist[i2], self.vlist[i3]
    local vab = halfway(va, vb)
    local vbc = halfway(vb, vc)
    local vca = halfway(vc, va)
    listappend(vertices, {va, vab, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vab, vb, vbc})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vbc, vc, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
    listappend(vertices, {vab, vbc, vca})
    listappend(indices, {#vertices - 2, #vertices - 1, #vertices})
  end
  other.vlist = vertices
  other.ilist = indices
  return other
end


--- Converts flat list of triangle indices to flat list of line indices.
-- Edges shared between triangles are not repeated.
--     eg. {1,2,3,  2,3,4} -> {1,2, 2,3, 3,1, 3,4, 4,2}
function m:triangleToLine()
  local line_pairs = {}
  for i = 1, #self.ilist, 3 do
    local i1, i2, i3 = self.ilist[i], self.ilist[i+1], self.ilist[i+2]
    line_pairs[listToHash(i1, i2)] = {i1, i2}
    line_pairs[listToHash(i2, i3)] = {i2, i3}
    line_pairs[listToHash(i3, i1)] = {i3, i1}
  end
  local line_indices = {}
  for _, line_pair in pairs(line_pairs) do
    table.insert(line_indices, line_pair[1])
    table.insert(line_indices, line_pair[2])
  end
  local other = self:clone()
  other.ilist = line_indices
  return other
end


--- Combine triangles from two or more solids into a merged solid.
-- Note that all geometry is preserved (even invisible insides),
-- for better results use the union operator from CSG module.
function m.merge(solidsList)
  local other = m.new()
  for _, another in ipairs(solidsList) do
    local offset = #other.vlist
    listappend(other.vlist, another.vlist)
    for _, index in ipairs(another.ilist) do
      table.insert(other.ilist, index + offset)
    end
    other.normals_dirty = other.normals_dirty or another.normals_dirty
  end
  return other
end


--- Retrieves a map of connections between vertex indices in a solid.
--     graph = solid:getConnections()
--  The `graph[1][2]` is true if vertices #1 and #2 are connected.
function m:getConnections()
  local graph = {}
  for i = 1, #self.ilist, 3 do
    local ia, ib, ic = self.ilist[i + 0], self.ilist[i + 1], self.ilist[i + 2]
    -- build connectivity graph of ABC triangle with bi-directional lines
    if graph[ia] then graph[ia][ib] = true else graph[ia] = { [ib] = true} end
    if graph[ia] then graph[ia][ic] = true else graph[ia] = { [ic] = true} end
    if graph[ib] then graph[ib][ia] = true else graph[ib] = { [ia] = true} end
    if graph[ib] then graph[ib][ic] = true else graph[ib] = { [ic] = true} end
    if graph[ic] then graph[ic][ia] = true else graph[ic] = { [ia] = true} end
    if graph[ic] then graph[ic][ib] = true else graph[ic] = { [ib] = true} end
  end
  return graph
end


--- Recompute all the triangle normals in a solid, in-place modification!
function m:updateNormals()
  if not self.ilist then
    self.ilist = {}
    for i = 1, #self.vlist do
      self.ilist[i] = i
    end
  end
  if #self.ilist < 3 then return end
  local normals = {} -- maps vertex index to list of normals of adjacent faces
  local v1, v2, v3 = vec3(), vec3(), vec3()
  for i = 1, #self.ilist, 3 do
    local vi1, vi2, vi3 = self.ilist[i], self.ilist[i + 1], self.ilist[i + 2]
    v1:set(unpack(self.vlist[vi1]))
    v2:set(unpack(self.vlist[vi2]))
    v3:set(unpack(self.vlist[vi3]))
    local fnormal = {v2:sub(v1):cross(v3:sub(v1)):normalize():unpack()}
    normals[vi1] = normals[vi1] or {}
    normals[vi2] = normals[vi2] or {}
    normals[vi3] = normals[vi3] or {}
    table.insert(normals[vi1], fnormal)
    table.insert(normals[vi2], fnormal)
    table.insert(normals[vi3], fnormal)
  end
  local vnormal, tvec3 = vec3(), vec3()
  for i = 1, #self.vlist do
    assert(normals[i], 'no triangle in index list contains vertex ' .. i)
    if normals[i] then
      vnormal:set(0,0,0)
      local c = 0
      for _, fnormal in ipairs(normals[i]) do
        vnormal:add(tvec3:set(unpack(fnormal)))
        c = c + 1
      end
      vnormal:mul(1 / c)
      local v = self.vlist[i]
      v[4], v[5], v[6] = vnormal:normalize():unpack()
    end
  end
  self.normals_dirty = false
  return self
end


--- Remove the internal buffer representation of solid to force the refresh
function m:invalidate()
  self.mesh = nil
end


function m:calculateBoundingBox()
  local minx = math.huge
  local maxx = -math.huge
  local miny = math.huge
  local maxy = -math.huge
  local minz = math.huge
  local maxz = -math.huge
  for i, v in ipairs(self.vlist) do
    local x, y, z = unpack(v)
    minx = math.min(minx, x)
    maxx = math.max(maxx, x)
    miny = math.min(miny, y)
    maxy = math.max(maxy, y)
    minz = math.min(minz, z)
    maxz = math.max(maxz, z)
  end
  self.mesh:setBoundingBox(minx, maxx, miny, maxy, minz, maxz)
end


--- Draw the solid mesh in the supplied pass.
function m:draw(pass, ...)
  if #self.ilist < 3 then return end
  if self.normals_dirty then self:updateNormals() end
  if not self.mesh then
    self.mesh = lovr.graphics.newMesh(self.vbuffer_format, self.vlist, 'gpu')
    self.mesh:setIndices(self.ilist)
    self:calculateBoundingBox()
  end
  pass:draw(self.mesh, ...)
end


--- Draw the wireframe of a solid and each face's normal.
function m:debugDraw(pass, ...)
  pass:push('state')
  pass:setShader()
  local pose = mat4(...)
  -- wireframe model
  pass:setWireframe(true)
  pass:setColor(0x90d59c)
  self:draw(pass, pose)
  -- annotated indices around vertices
  local randomGenerator = lovr.math.newRandomGenerator(0)
  pass:setWireframe(false)
  for i, v in ipairs(self.vlist) do
    local vc = vec3(pose:mul(unpack(v)))
    local ne = vec3(v[4], v[5], v[6])
    quat(pose):mul(ne)
    ne:mul(0.2)
    pass:setColor(0xcc7048)
    pass:line(vc, vc + ne)
    pass:setColor(0x68a8ab)
    pass:sphere(vc, 0.02)
    local ap = vc:add(randomGenerator:randomNormal(0.02, 0),
                      randomGenerator:randomNormal(0.02, 0),
                      randomGenerator:randomNormal(0.02, 0))
    pass:setColor(0xffffb2)
    pass:text(i, ap, 0.02)
  end
  pass:pop('state')
end

----- Constructors for solid mesh primitives -----

--- Create an empty solid primitive.
function m.new()
  local self = setmetatable({
    vlist = {}, -- vertices
    ilist = {}, -- indices
    sides = {}, -- maps side name to index list (eg. 'top' = {1,2,3,4})
    normals_dirty = true
  }, m)
  return self
end


--- Create the solid shape from CSG representation.
function m.fromCSG(csg)
  local self = m.new()
  for _, p in ipairs(csg.polygons) do
    for j=3,#p.vertices do
      local v = p.vertices[1]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      local v = p.vertices[j-1]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      local v = p.vertices[j]
      table.insert(self.vlist, {v.pos.x, v.pos.y, v.pos.z, v.normal.x, v.normal.y, v.normal.z})
      table.insert(self.ilist, #self.ilist + 1)
      table.insert(self.ilist, #self.ilist + 1)
      table.insert(self.ilist, #self.ilist + 1)
    end
  end
  return self
end


--- Construct a solid from list of vertices and optional indices.
function m.fromVertices(vertices, indices)
  local self = m.new()
  for i, vertex in ipairs(vertices) do
    if type(vertex) == 'table' then
      self.vlist[i] = {unpack(vertex)}
    elseif type(vertex) == 'userdata' then
      local x, y, z = vertex:unpack()
      self.vlist[i] = {x, y, z}
    else
      error('unsupported vertex type')
    end
  end
  if indices then
    for i, index in ipairs(indices) do
      self.ilist[i] = index
    end
  else
    local triangle_vertices = math.floor(#vertices / 3) * 3
    for i = 1, triangle_vertices do
      self.ilist[i] = i
    end
  end
  return self
end


--- Construct a solid from LOVR's Model object.
function m.fromModel(model)
  assert(type(model) == 'userdata' and model.getTriangles, 'unsupported input')
  local vlist_flat, ilist = model:getTriangles()
  local vlist = {}
  for i = 1, #vlist_flat, 3 do
    table.insert(vlist, {vlist_flat[i], vlist_flat[i+1], vlist_flat[i+2]})
  end
  return m.fromVertices(vlist, ilist)
end


function m.convexHull(point_cloud)
  if #point_cloud < 4 then
    local vertices = {}
    for i, point in ipairs(point_cloud) do
      vertices[i] = { point:unpack() }
    end
    return m.fromVertices(vertices)
  end
  local points = shuffle(point_cloud)
  local tetrahedron
  -- initial tetrahedron; take 4 randomized points (to help avoid coplanars)
  local attempts = 0
  while true do
    attempts = attempts + 1
    tetrahedron = {}
    for i = 1, 4 do
      table.insert(tetrahedron, points[i])
    end
    if math.abs(m.trianglePointDistance(tetrahedron, tetrahedron[4])) > EPSILON then
      break
    else
      if attempts > 5 or #points < 5 then
        local vertices = {}
        for i, point in ipairs(point_cloud) do
          vertices[i] = { point:unpack() }
        end
        return m.fromVertices(vertices)
      end
      for i = 1, 4 do -- take different set of 4 points for initial tetrahedron
        local rnd_index = lovr.math.random(5, #points)
        points[i], points[rnd_index] = points[rnd_index], points[i]
      end
    end
  end

  -- construct faces of intial convex hull with correct triangle windings
  local faces = {} -- list of triangles {{i1,i2,i3},} where i indexes input list of points
  local center = (tetrahedron[1] + tetrahedron[2] + tetrahedron[3] + tetrahedron[4]):mul(1 / 4)
  for i = 1, 4 do
    local face = { i,
                   1 + (i + 1) % 4,
                   1 + (i + 2) % 4 }
    local triangle = {tetrahedron[face[1]], tetrahedron[face[2]], tetrahedron[face[3]]}
    if m.trianglePointDistance(triangle, center) > 0 then
      face[2], face[3] = face[3], face[2]
    end
    table.insert(faces, face)
  end
  -- iterate points and add to convex_hull
  for i = 5, #points do
    local point = points[i]
    local conflicting_faces = {}
    for _, face in ipairs(faces) do
      local triangle = {points[face[1]], points[face[2]], points[face[3]]}
      if m.trianglePointDistance(triangle, point) > EPSILON then
        table.insert(conflicting_faces, face)
      end
    end
    if #conflicting_faces > 0 then -- add a conflict point to convex hull
      -- determine horizon, a ring of outer edges visible from the new pont
      local horizon = {} -- set of edges that belong to horizon
      local added_index = i
      for _, cface in ipairs(conflicting_faces) do
        for j = 1, 3 do
          local i1, i2 = cface[j], cface[1 + j % 3]
          local hash = listToHash(math.min(i1, i2), math.max(i1, i2))
          -- each internal edge is referenced by two triangles, horizon edges have just one
          if horizon[hash] then
            horizon[hash] = nil
          else
            horizon[hash] = true
          end
        end
        -- find & remove conflicting face
        for j, face in ipairs(faces) do
          if face[1] == cface[1] and
             face[2] == cface[2] and
             face[3] == cface[3] then
            table.remove(faces, j)
            break
          end
        end
      end
      -- add faces to convex hull that connect the horizon edges to the added conflict point
      for edge, _ in pairs(horizon) do
        local i1, i2 = hashToList(edge)
        local face = {i1, i2, added_index}
        local triangle = {points[face[1]], points[face[2]], points[face[3]]}
        if m.trianglePointDistance(triangle, center) > 0 then
          face[2], face[3] = face[3], face[2]
        end
        table.insert(faces, face)
      end
    end -- else the point is already inside the hull and therefore discarded
  end
  -- build vertices and indices from hull faces
  local vertices, indices = {}, {}
  local input_to_hull = {} -- maps indices of input points to indices of convex hull points
  for _, face in ipairs(faces) do
    for _, index in ipairs(face) do
      if not input_to_hull[index] then
        table.insert(vertices, {points[index]:unpack()})
        input_to_hull[index] = #vertices
      end
      table.insert(indices, input_to_hull[index])
    end
  end
  return m.fromVertices(vertices, indices)
end


--- Efficiently add a point to existing convex hull (modifies the solid in place)
function m:addToConvexHull(point)
  table.insert(self.vlist, {point:unpack()})
  if #self.vlist < 4 then
    return self
  end
  local center = (vec3(unpack(self.vlist[1])) +
                  vec3(unpack(self.vlist[2])) +
                  vec3(unpack(self.vlist[3])) +
                  vec3(unpack(self.vlist[4]))):mul(1 / 4)
  if #self.vlist == 4 then
    -- construct faces of intial convex hull with correct triangle windings
    for i = 1, 4 do
      local face = { i,
                     1 + (i + 1) % 4,
                     1 + (i + 2) % 4 }
      local triangle = { vec3(unpack(self.vlist[face[1]])),
                         vec3(unpack(self.vlist[face[2]])),
                         vec3(unpack(self.vlist[face[3]])) }
      if m.trianglePointDistance(triangle, center) > 0 then
        face[2], face[3] = face[3], face[2]
      end
      listappend(self.ilist, face)
    end
    return m.fromVertices(self.vlist, self.ilist):updateNormals()
  end
  local faces = {}
  for i = 1, #self.ilist, 3 do
    table.insert(faces, {self.ilist[i], self.ilist[i + 1], self.ilist[i + 2]})
  end
  local conflicting_faces = {}
  for _, face in ipairs(faces) do
    local triangle = {vec3(unpack(self.vlist[face[1]])),
                      vec3(unpack(self.vlist[face[2]])),
                      vec3(unpack(self.vlist[face[3]]))}
    if m.trianglePointDistance(triangle, point) > EPSILON then
      table.insert(conflicting_faces, face)
    end
  end
  if #conflicting_faces > 0 then -- add a conflict point to convex hull
    -- determine horizon, a ring of outer edges visible from the new pont
    local horizon = {} -- set of edges that belong to horizon
    local added_index = #self.vlist
    for _, cface in ipairs(conflicting_faces) do
      for j = 1, 3 do
        local i1, i2 = cface[j], cface[1 + j % 3]
        local hash = listToHash(math.min(i1, i2), math.max(i1, i2))
        if horizon[hash] then
          horizon[hash] = nil
        else
          horizon[hash] = true
        end
      end
      -- find & remove conflicting face
      for j, face in ipairs(faces) do
        if face[1] == cface[1] and
           face[2] == cface[2] and
           face[3] == cface[3] then
          table.remove(faces, j)
          break
        end
      end
    end
    -- add faces to convex hull that connect the horizon edges to the added conflict point
    for edge, _ in pairs(horizon) do
      local i1, i2 = hashToList(edge)
      local face = {i1, i2, added_index}
      local triangle = {vec3(unpack(self.vlist[face[1]])),
                        vec3(unpack(self.vlist[face[2]])),
                        vec3(unpack(self.vlist[face[3]]))}
      if m.trianglePointDistance(triangle, center) > 0 then
        face[2], face[3] = face[3], face[2]
      end
      table.insert(faces, face)
    end
  end -- else the point is already inside the hull and therefore discarded
  -- build vertices from used points, and indices
  local vertices, indices = {}, {}
  local input_to_hull = {} -- maps indices of input points to indices of convex hull points
  for _, face in ipairs(faces) do
    for _, index in ipairs(face) do
      if not input_to_hull[index] then
        table.insert(vertices, self.vlist[index])
        input_to_hull[index] = #vertices
      end
      table.insert(indices, input_to_hull[index])
    end
  end
  return m.fromVertices(vertices, indices):updateNormals()
end


function m.superellipsePoints(segments, ax, ay, az, e1, e2)
  local points = {}
  local function sgn(n)
    return (n >= 0) and 1 or -1
  end
  for theta = -math.pi / 2, math.pi / 2, math.pi / segments do
    for gamma = -math.pi, math.pi, math.pi / segments do
      points[#points + 1] = lovr.math.newVec3(
        ax * math.cos(theta)^e1 * math.abs(math.cos(gamma))^e2 * sgn(math.cos(gamma)),
        ay * math.cos(theta)^e1 * math.abs(math.sin(gamma))^e2 * sgn(math.sin(gamma)),
        az * math.abs(math.sin(theta))^e1 * sgn(math.sin(theta)))
    end
  end
  return points
end


function m.superellipse(segments, ax, ay, az, e1, e2)
  return m.convexHull(m.superellipsePoints(segments, ax, ay, az, e1, e2))
end


--- Single sided 1x1 plane facing down the -Z axis.
function m.quad(subdivisions)
  local size = 1 / math.floor(subdivisions or 1)
  local self = m.new()
  local epsilon = 1e-6
  for y = -0.5, 0.5 - epsilon, size do
    for x = -0.5, 0.5 - epsilon, size do
      table.insert(self.vlist, {x, y, 0})
      table.insert(self.vlist, {x, y + size, 0})
      table.insert(self.vlist, {x + size, y, 0})
      table.insert(self.vlist, {x + size, y + size, 0})
      listappend(self.ilist, {#self.vlist - 3, #self.vlist - 2, #self.vlist - 1})
      listappend(self.ilist, {#self.vlist - 2, #self.vlist - 0, #self.vlist - 1})
    end
  end
  return self
end


--- N-sided equilateral polygon.
function m.ngon(segments)
  segments = segments or 6
  local self = m.new()
  local vic = segments * 2 + 1
  for i = 0, segments - 1 do
    local theta, v1, v2, vi1, vi2
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta),  0.5 * math.sin(theta), 0}
    theta = (i + 1) * (2 * math.pi) / segments;
    v2 = {0.5 * math.cos(theta),  0.5 * math.sin(theta), 0}
    table.insert(self.vlist, v1)
    table.insert(self.vlist, v2)
    vi1, vi2 = #self.vlist - 1, #self.vlist
    listappend(self.ilist, {vic, vi2, vi1})
  end
  table.insert(self.vlist, {0,  0, 0})
  assert(vic, #self.vlist)
  return self
end


--- A cube.
function m.cube()
  local self = m.new()
  local s = 0.5
  self.vlist = {
    {-s, -s, -s}, {-s,  s, -s}, { s, -s, -s}, { s,  s, -s}, -- front
    { s,  s, -s}, { s,  s,  s}, { s, -s, -s}, { s, -s,  s}, -- right
    { s, -s,  s}, { s,  s,  s}, {-s, -s,  s}, {-s,  s,  s}, -- back
    {-s,  s,  s}, {-s,  s, -s}, {-s, -s,  s}, {-s, -s, -s}, -- left
    {-s, -s, -s}, { s, -s, -s}, {-s, -s,  s}, { s, -s,  s}, -- bottom
    {-s,  s, -s}, {-s,  s,  s}, { s,  s, -s}, { s,  s,  s}} -- top
  self.ilist = {
     1,  2,  3,  3,  2,  4, -- front
     5,  6,  7,  7,  6,  8, -- top
     9, 10, 11, 11, 10, 12, -- back
    13, 14, 15, 15, 14, 16, -- bottom
    17, 18, 19, 19, 18, 20, -- left
    21, 22, 23, 23, 22, 24} -- right
  self.sides = {
    right =  {3, 4,  5,  6,  7,  8,  9, 10, 18, 20, 23, 24},
    bottom = {1, 3,  7,  8,  9, 11, 15, 16, 17, 18, 19, 20},
    back =   {6, 8,  9, 10, 11, 12, 13, 15, 19, 20, 22, 24},
    top =    {2, 4,  5,  6, 10, 12, 13, 14, 21, 22, 23, 24},
    front =  {1, 2,  3,  4,  5,  7, 14, 16, 17, 18, 21, 23},
    left =   {1, 2, 11, 12, 13, 14, 15, 16, 17, 19, 21, 22}}
  return self
end


--- A truncated cube (rhombicuboctahedron) with variable slant cutoff.
function m.tcube(slant)
  local self = m.new()
  slant = slant or 0.8
  slant = math.min(math.max(slant, 0), 1)
  local s, l = slant * 0.5, 0.5
  self.vlist = {
    {-s, -l,  s}, {-s, -s,  l}, {-l, -s,  s}, {-s,  s,  l},
    {-s,  l,  s}, {-l,  s,  s}, {-s, -l, -s}, {-l, -s, -s},
    {-s, -s, -l}, {-s,  l, -s}, {-s,  s, -l}, {-l,  s, -s},
    { s, -l,  s}, { l, -s,  s}, { s, -s,  l}, { s,  l,  s},
    { s,  s,  l}, { l,  s,  s}, { s, -l, -s}, { s, -s, -l},
    { l, -s, -s}, { s,  l, -s}, { l,  s, -s}, { s,  s, -l},
  }
  self.ilist = {
    15,  4,  2,   21, 18, 14,   22,  5, 16,    3, 12,  8,
     9, 24, 20,    1,  2,  3,    4,  5,  6,    7,  8,  9,
    10, 11, 12,   13, 14, 15,   16, 17, 18,   19, 20, 21,
    22, 23, 24,    7,  3,  8,    2,  6,  3,    5, 12,  6,
    11,  8, 12,   19,  9, 20,   10, 24, 11,   23, 20, 24,
    13, 21, 14,   22, 18, 23,   17, 14, 18,    1, 15,  2,
    16,  4, 17,    7, 13,  1,   15, 17,  4,   21, 23, 18,
    22, 10,  5,    3,  6, 12,    9, 11, 24,    7,  1,  3,
     2,  4,  6,    5, 10, 12,   11,  9,  8,   19,  7,  9,
    10, 22, 24,   23, 21, 20,   13, 19, 21,   22, 16, 18,
    17, 15, 14,    1, 13, 15,   16,  5,  4,    7, 19, 13,
  }
  self.sides = {
    right =  {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24},
    bottom = { 1,  2,  3,  7,  8,  9, 13, 14, 15, 19, 20, 21},
    back =   { 1,  2,  3,  4,  5,  6, 13, 14, 15, 16, 17, 18},
    top =    { 4,  5,  6, 10, 11, 12, 16, 17, 18, 22, 23, 24},
    front =  { 7,  8,  9, 10, 11, 12, 19, 20, 21, 22, 23, 24},
    left =   { 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12},
  }
  return self
end


--- Bipyramid with variable number of sides (a diamond shape).
function m.bipyramid(segments)
  local self = m.new()
  segments = segments or 4
  self.sides = {top={}, bottom={}, ring={}}
  for i = 0, segments - 1 do
    -- top half
    table.insert(self.vlist,  {0, 0.5, 0})
    table.insert(self.sides.top, #self.vlist)
    local theta = i * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    theta = (i + 1) * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    listappend(self.ilist, {#self.vlist, #self.vlist - 1, #self.vlist - 2})
    -- bottom half
    table.insert(self.vlist,  {0, -0.5, 0})
    table.insert(self.sides.bottom, #self.vlist)
    theta = i * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    theta = (i + 1) * (2 * math.pi) / segments
    x = 0.5 * math.cos(theta)
    z = 0.5 * math.sin(theta)
    table.insert(self.vlist, {x, 0, z})
    table.insert(self.sides.ring, #self.vlist)
    listappend(self.ilist, {#self.vlist, #self.vlist - 2, #self.vlist - 1})
  end
  return self
end


--- A pyramid with variable number of sides.
function m.pyramid(segments)
  local self = m.bipyramid(segments)
  self = self:transform(mat4(0, -0.5, 0), self.sides.ring)
  listappend(self.sides.bottom, self.sides.ring)
  return self
end


--- A prism with variable number of sides.
function m.cylinder(segments)
  local self = m.new()
  segments = segments or 6
  self.sides = {top={}, bottom={}, ring={}}
  local vTop = segments * 8 + 1
  local vBottom = segments * 8 + 2
  for i = 0, segments - 1 do
    -- ring
    local theta, v1, v2, v3, v4, vi1, vi2, vi3, vi4
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v2 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    theta = (i + 1) * (2 * math.pi) / segments;
    v3 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v4 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    table.insert(self.vlist, v1)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v2)
    table.insert(self.sides.top, #self.vlist)
    table.insert(self.vlist, v3)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v4)
    table.insert(self.sides.top, #self.vlist)
    vi1, vi2, vi3, vi4 = #self.vlist-3, #self.vlist-2, #self.vlist-1, #self.vlist
    listappend(self.ilist, {vi1, vi2, vi4, vi1, vi4, vi3})
    -- top and bottom self.sides
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v2 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    theta = (i + 1) * (2 * math.pi) / segments;
    v3 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v4 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    table.insert(self.vlist, v1)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v2)
    table.insert(self.sides.top, #self.vlist)
    table.insert(self.vlist, v3)
    table.insert(self.sides.bottom, #self.vlist)
    table.insert(self.vlist, v4)
    table.insert(self.sides.top, #self.vlist)
    vi1, vi2, vi3, vi4 = #self.vlist-3, #self.vlist-2, #self.vlist-1, #self.vlist
    listappend(self.ilist, {vTop, vi4, vi2, vBottom, vi1, vi3})
  end
  table.insert(self.vlist, {0,  0.5, 0})
  table.insert(self.sides.top, #self.vlist)
  assert(vTop, #self.vlist)
  table.insert(self.vlist, {0, -0.5, 0})
  table.insert(self.sides.bottom, #self.vlist)
  assert(vBottom, #self.vlist)
  return self
end


--- Icosphere with customizable subdivision steps (each is x4 geometry)
-- https://github.com/bjornbytes/lovr-icosphere (MIT License)
function m.sphere(subdivisions)
  local self = m.new()
  subdivisions = subdivisions or 2
  local phi = (1 + math.sqrt(5)) / 2
  self.vlist = {
    { -1,  phi, 0 },
    {  1,  phi, 0 },
    { -1, -phi, 0 },
    {  1, -phi, 0 },

    { 0, -1,  phi },
    { 0,  1,  phi },
    { 0, -1, -phi },
    { 0,  1, -phi },

    {  phi, 0, -1 },
    {  phi, 0,  1 },
    { -phi, 0, -1 },
    { -phi, 0,  1 }
  }
  self.ilist = {
    1, 12, 6,  1, 6, 2,  1, 2, 8,  1, 8, 11,  1, 11, 12,
    2, 6, 10,  6, 12, 5,  12, 11, 3,  11, 8, 7,  8, 2, 9,
    4, 10, 5,  4, 5, 3,  4, 3, 7,  4, 7, 9,  4, 9, 10,
    5, 10, 6,  3, 5, 12,  7, 3, 11,  9, 7, 8,  10, 9, 2
  }
  -- Cache vertex splits to avoid duplicates
  local splits = {}
  -- Splits self.vlist i and j, creating a new vertex and returning the index
  local function split(i, j)
    local key = i < j and (i .. ',' .. j) or (j .. ',' .. i)

    if not splits[key] then
      local x = (self.vlist[i][1] + self.vlist[j][1]) / 2
      local y = (self.vlist[i][2] + self.vlist[j][2]) / 2
      local z = (self.vlist[i][3] + self.vlist[j][3]) / 2
      table.insert(self.vlist, { x, y, z })
      splits[key] = #self.vlist
    end

    return splits[key]
  end
  -- Subdivide
  for _ = 1, subdivisions do
    for i = #self.ilist, 1, -3 do
      local v1, v2, v3 = self.ilist[i - 2], self.ilist[i - 1], self.ilist[i - 0]
      local a = split(v1, v2)
      local b = split(v2, v3)
      local c = split(v3, v1)

      table.insert(self.ilist, v1)
      table.insert(self.ilist, a)
      table.insert(self.ilist, c)

      table.insert(self.ilist, v2)
      table.insert(self.ilist, b)
      table.insert(self.ilist, a)

      table.insert(self.ilist, v3)
      table.insert(self.ilist, c)
      table.insert(self.ilist, b)

      table.insert(self.ilist, a)
      table.insert(self.ilist, b)
      table.insert(self.ilist, c)

      table.remove(self.ilist, i - 0)
      table.remove(self.ilist, i - 1)
      table.remove(self.ilist, i - 2)
    end
  end
  -- Normalize
  for _, v in ipairs(self.vlist) do
    local x, y, z = unpack(v)
    local length = math.sqrt(x * x + y * y + z * z) * 2
    v[1], v[2], v[3] = x / length, y / length, z / length
  end
  return self
end


--[[ Octasphere is a geometric primitive in which a sphere is cut into 8
  separate octants which are stitched together with quads. By extruding
  and manipulating quad lengths it is possible to coerce the octasphere
  into various shapes:

    sphere: edges xyz = 0
    box: radii xyz = 0
    circle: radius y = 0, edges xyz = 0
    plane: radii xyz = 0, edge y = 0
    cylinder: radius y = 0, edges xz = 0
    rounded cuboid: radii x = y = z set to small value
    capsule: radii x = y = z, edges xz = 0

  At zero-subdivision level it is also possible to create octagonal and
  hexagonal prisms.

  The surface of octasphere can be divided into these parts:
    8 round "corners" (1/8 of a sphere)
    12 round "edges" (elongated arcs)
    6 flat faces

  A single corner is constructed and mirrored to obtain other seven corners. All eight
  corners are sewn together to form edges. Then the six faces are created on the sides.

  The created octasphere has edge length of 2 and radius of 1; can be resized afterwards.
  Concept and algorithm is based on https://prideout.net/blog/octasphere/ by Philip Rideout.
--]]


--- Create list of indices that sews together two lists of indices.
--          cA       nA               triangle I:  cA nB nA
--        ---*-------*---   edge A    triangle II: cA cB nB
--           | \   I | \
--           |   \   |               Ex. 1-2-3   will generate:
--         \ |II   \ |                   |\|\|    1 5 2, 1 4 5,
--        ---*-------*---   edge B       4-5-6    2 6 3, 2 5 6
--          cB       nB
--  Constructs triangles between edges A and B, defined as list of indices.
--  Similar to triangle strip, but for each triangle all 3 indices will be generated.
--  When supplied, constant offsets can be added to each index fetched from A and B edge.
function m.sewindices(edgeA, edgeB, offsetA, offsetB)
  assert(#edgeA == #edgeB)
  offsetA, offsetB = offsetA or 0, offsetB or 0
  local indices = {}
  for i = 1, #edgeA - 1 do
    local cA, cB = edgeA[i], edgeB[i]
    local nA, nB = edgeA[i + 1], edgeB[i + 1]
    table.insert(indices, cA + offsetA) -- triangle I
    table.insert(indices, nB + offsetB)
    table.insert(indices, nA + offsetA)
    table.insert(indices, cA + offsetA) -- triangle II
    table.insert(indices, cB + offsetB)
    table.insert(indices, nB + offsetB)
  end
  return indices
end


-- Construct a list of vertices inside a geodesic.
local function compute_geodesic(point_a, point_b, num_segments)
  -- add first and last point of geodesic, iterate along arc and compute points
  local tvec3, tquat = vec3(), quat()
  local angle_between_endpoints = math.acos( point_a:dot(point_b) )
  local rotation_axis = vec3(point_a):cross(point_b)
  local first = tvec3:set(point_a)
  local point_list = { { first:unpack() } }
  if num_segments == 0 then
    return point_list
  end
  local dtheta = angle_between_endpoints / num_segments
  for point_index = 1, num_segments - 1 do -- iterate horizontally along slice
    local theta = point_index * dtheta
    local q = tquat:set(theta, rotation_axis.x, rotation_axis.y, rotation_axis.z)
    q:mul(tvec3:set(point_a))
    local point = { tvec3:unpack() }
    table.insert(point_list, point)
  end
  local last = tvec3:set(point_b)
  table.insert(point_list, { last:unpack() })
  return point_list
end



--- Subdivide an octant into vertical slices and construct geodesic of each.
local function tessellate_octasphere_patch(subdivisions)
  local n = 2^subdivisions + 1
  local num_vertices = n * (n + 1) / 2
  local point_a, point_b = vec3(), vec3()
  local vertices = {}
  for i = 0, n - 1 do
    local theta = math.pi * 0.5 * i / (n - 1)
    point_a:set(0,               math.sin(theta), math.cos(theta))
    point_b:set(math.cos(theta), math.sin(theta), 0)
    local num_segments = n - 1 - i
    local geodesic_vertices = compute_geodesic(point_a, point_b, num_segments)
    for _, vertex in ipairs(geodesic_vertices) do
      table.insert(vertices, vertex)
    end
  end
  assert(#vertices == num_vertices)
  -- construct indices for single octasphere patch
  local indices = {}
  local f, j0 = 1, 1
  for col_index = 0, n-2 do
    local col_height = n - 1 - col_index
    local j1 = j0 + 1
    local j2 = j0 + col_height + 1
    local j3 = j0 + col_height + 2
    for row = 0, col_height - 2 do
      table.insert(indices, j0 + row) -- edges and 'odd' inside faces
      table.insert(indices, j1 + row)
      table.insert(indices, j2 + row)
      table.insert(indices, j2 + row) -- 'even' inside faces
      table.insert(indices, j1 + row)
      table.insert(indices, j3 + row)
      f = f + 2
    end
    local row = col_height - 1
    table.insert(indices, j0 + row) -- faces with z = 0
    table.insert(indices, j1 + row)
    table.insert(indices, j2 + row)
    f = f + 1
    j0 = j2
  end
  -- identify indices of vertices on edges
  local edges = {x = {}, y = {}, z = {}} -- edges on x=0, y=0 and z=0 planes
  local step = n
  local xi = 1
  local zi = n
  for i = 1, n do
    table.insert(edges.y, i)
    table.insert(edges.x, xi)
    table.insert(edges.z, zi)
    xi = xi + step
    step = step - 1
    zi = zi + step
  end
  return vertices, indices, edges
end


local function sign(n)
  return (n < 0 and -1) or 1
end


local function reshape_octasphere(solid, rx, ry, rz, ex, ey, ez, slant)
  slant = math.max(-1, math.min(1, slant or 0))
  return solid:map(
    function(x,y,z)
      -- deform the octosphere from initial form (edge = 2, radius = 1)
      x = sign(x) * ex / 2 + (x - sign(x)) / 2 * rx
      y = sign(y) * ey / 2 + (y - sign(y)) / 2 * ry
      z = sign(z) * ez / 2 + (z - sign(z)) / 2 * rz
      -- apply shape slant
      x = x * (1 + (z / (ez / 2 + rz)) * slant)
      y = y * (1 + (z / (ez / 2 + rz)) * slant)
      return x, y, z
    end)
end


function m.octasphere(subdivisions)
  local mergedvertices = {}
  local mergedindices  = {}
  -- create the corner patch (in +X+Y+Z octant) and insert into final shape
  local patchvertices, patchindices, patchedges = tessellate_octasphere_patch(subdivisions)
  -- prepare flipped version of corner patch, to use when mirroring results in wrong winding
  local patchindicesflipped = {}
  for i = 1, #patchindices, 3 do
    patchindicesflipped[i]     = patchindices[i]
    patchindicesflipped[i + 1] = patchindices[i + 2]
    patchindicesflipped[i + 2] = patchindices[i + 1]
  end
  -- rotate vertices of original patch to other 7 corners of octasphere
  local reflections = { -- x   y   z
                  [1] = {  1,  1,  1 },  -- +X+Y+Z (original patch)
                  [2] = {  1,  1, -1 },  -- +X+Y-Z  reflect along z axis
                  [3] = {  1, -1,  1 },  -- +X-Y+Z        6------2
                  [4] = {  1, -1, -1 },  -- +X-Y-Z      / |    / |
                  [5] = { -1,  1,  1 },  -- -X+Y+Z     5------1  |
                  [6] = { -1,  1, -1 },  -- -X+Y-Z     |  8---|--4
                  [7] = { -1, -1,  1 },  -- -X-Y+Z     | /    | /
                  [8] = { -1, -1, -1 }}  -- -X-Y-Z     7------3
  local patchcount = #patchvertices
  local tvec3 = vec3()
  local reflect = vec3()
  for ri, refl in ipairs(reflections) do
    reflect:set(unpack(refl))
    -- reflect/mirror the original patch, insert vertices into final shape
    for _,v in ipairs(patchvertices) do
      local rv = tvec3:set( unpack(v) )
      rv:add(1,1,1) -- spread apart 8 domes, so that no two points are on same coordinates
      rv:mul(reflect)
      local x, y, z = rv:unpack()
      local nv = tvec3:set( unpack(v) )
      nv:mul(reflect)
      local nx, ny, nz = nv:unpack()
      table.insert(mergedvertices, { x, y, z, nx, ny, nz })
    end
    -- create new indices from indices of original patch, with fixed offset
    local offset = (ri - 1) * patchcount

    local indices = refl[1] * refl[2] * refl[3] > 0 and patchindices or patchindicesflipped
    for _,i in ipairs(indices) do
      table.insert(mergedindices, i + offset)
    end
  end
  -- sew the 8 patches together to create rounded edges (will be collapsed for a sphere)
  -- each newly rotated part defines its stitches to existing patches
  -- ordering of elements is important; `rotationstiches` uses same order as `reflections`
  local rotationstiches = {
    {},                                               -- +X+Y+Z  first patch, no sewing needed yet
    {{ 1, 'z', true }},                               -- +X+Y-Z  stitch 2st patch with 1th along z plane, flipped
    {{ 1, 'y', true }},                               -- +X-Y+Z
    {{ 2, 'y' }, { 3, 'z' }},                         -- +X-Y-Z
    {{ 1, 'x' }},                                     -- -X+Y+Z
    {{ 2, 'x', true }, { 5, 'z' }},                   -- -X+Y-Z
    {{ 3, 'x', true }, { 5, 'y' }},                   -- -X-Y+Z
    {{ 4, 'x' }, { 6, 'y', true }, { 7, 'z', true }}} -- -X-Y-Z
  -- sew each new patch together with patches created before it
  for ri, stitchdefs in ipairs(rotationstiches) do
      for _, stitchdef in ipairs(stitchdefs) do
        local ti, axis, flip = unpack(stitchdef)
        local offsetA = (ri - 1) * patchcount
        local offsetB = (ti - 1) * patchcount
        if flip then -- reverse edge order to make the stitch with opposite triangle winding
          offsetA, offsetB = offsetB, offsetA
        end
        local stitch = m.sewindices(patchedges[axis], patchedges[axis], offsetA, offsetB)
        for _, index in ipairs(stitch) do
          table.insert(mergedindices, index)
        end
      end
  end
  -- create 6 faces needed for rounded cuboid (will be collapsed for a sphere and capsule)
  local faces = {  {{0, 2}, {4, 6}, patchedges.x[1]},
                   {{1, 3}, {0, 2}, patchedges.z[1]},
                   {{4, 6}, {5, 7}, patchedges.z[1]},
                   {{5, 7}, {1, 3}, patchedges.x[1]},
                   {{4, 5}, {0, 1}, patchedges.x[#patchedges.x]},
                   {{6, 2}, {7, 3}, patchedges.x[#patchedges.x]}}
  for _, face in ipairs(faces) do
    local edgeA = {face[1][1] * patchcount + face[3], face[1][2] * patchcount + face[3]}
    local edgeB = {face[2][1] * patchcount + face[3], face[2][2] * patchcount + face[3]}
    local stitch = m.sewindices(edgeA, edgeB)
    for _, index in ipairs(stitch) do
      table.insert(mergedindices, index)
    end
  end
  local solid = m.fromVertices(mergedvertices, mergedindices)
  solid.subdivisions = subdivisions
  solid.reshape = reshape_octasphere
  return solid
end


return m
