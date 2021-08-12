local m = {}

local meshFormat = {{'lovrPosition', 'float', 3},
                    {'lovrNormal',   'float', 3}}
-- note that texcoord (UV mapping) is missing, currently not implemented


local function listappend(t1, t2) -- mutates t1 in place
  for i,v in ipairs(t2) do table.insert(t1, v) end
  return t1
end


-- usage example for laying upright meshes down: 
--   solids.transform(mesh, mat4():rotate(pi/2, 1,0,0))
function m.transform(mesh, m, side)
  if side then
    for _, vi in ipairs(side) do
      local v = {mesh:getVertex(vi)}
      v[1], v[2], v[3] = m:mul(vec3(unpack(v))):unpack()
      mesh:setVertex(vi, v)
    end
  else
    for vi = 1, mesh:getVertexCount() do
      local v = {mesh:getVertex(vi)}
      v[1], v[2], v[3] = m:mul(vec3(unpack(v))):unpack()
      mesh:setVertex(vi, v)
    end
  end
  return mesh
end


function m.extract(mesh)
  local vertices = {}
  local indices = mesh:getVertexMap()
  for i = 1, mesh:getVertexCount() do
    table.insert(vertices, {mesh:getVertex(i)})
  end
  return vertices, indices
end


function m.copy(mesh)
  local vertices, indices = m.extract(mesh)
  local meshFormat = mesh:getVertexFormat()
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh
end


function m.toStatic(mesh)
  local vertices, indices = m.extract(mesh)
  local meshFormat = mesh:getVertexFormat()
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'static', false)
  mesh:setVertexMap(indices)
  return mesh
end


function m.debugDraw(mesh, ...)
  lovr.math.drain()
  local pose = mat4(...)
  local shader = lovr.graphics.getShader()
  lovr.graphics.setLineWidth(2)
  lovr.graphics.setShader()
  -- wireframe model
  lovr.graphics.setColor(0xd35c5c)
  lovr.graphics.setWireframe(true)
  mesh:draw(pose)
  lovr.graphics.setWireframe(false)
  -- vertex normals - direct representation
  lovr.graphics.setColor(0x606060)
  for i = 1, mesh:getVertexCount() do
    local v = {mesh:getVertex(i)}
    local position = pose:mul(vec3(v[1], v[2], v[3]))
    local normal = vec3(v[4], v[5], v[6])
    lovr.graphics.line(position, normal:mul(0.1):add(position))
  end
  -- face normals - calculated average
  lovr.graphics.setColor(0x8D1C1C)
  local indices = mesh:getVertexMap()
  local position, normal = vec3(), vec3()
  local temp = vec3()
  for i = 1, #indices, 3 do
    local vi1, vi2, vi3 = indices[i], indices[i + 1], indices[i + 2]
    local v1 = {mesh:getVertex(vi1)}
    local v2 = {mesh:getVertex(vi2)}
    local v3 = {mesh:getVertex(vi3)}
    position:set(         v1[1], v1[2], v1[3])
    position:add(temp:set(v2[1], v2[2], v2[3]))
    position:add(temp:set(v3[1], v3[2], v3[3]))
    position:mul(1/3)
    position = pose:mul(position)
    normal:set(         v1[4], v1[5], v1[6])
    normal:add(temp:set(v2[4], v2[5], v2[6]))
    normal:add(temp:set(v3[4], v3[5], v3[6]))
    normal:mul(1/3 * 0.1)
    normal = quat(pose):mul(normal):add(position)
    lovr.graphics.line(position, normal)
    lovr.graphics.plane('fill', mat4(position, vec3(0.05), quat(normal:sub(position):normalize())))
  end
  lovr.graphics.setShader(shader)
end


function m.updateNormals(mesh)
  local indices = mesh:getVertexMap()
  if not indices then return end
  local normals = {} -- maps vertex index to list of normals of adjacent faces
  lovr.math.drain()
  local v1, v2, v3 = vec3(), vec3(), vec3()
  for i = 1, #indices, 3 do
    local vi1, vi2, vi3 = indices[i], indices[i + 1], indices[i + 2]
    v1:set(mesh:getVertex(vi1))
    v2:set(mesh:getVertex(vi2))
    v3:set(mesh:getVertex(vi3))
    local fnormal = (v2 - v1):cross(v3 - v1):normalize()
    normals[vi1] = normals[vi1] or {}
    normals[vi2] = normals[vi2] or {}
    normals[vi3] = normals[vi3] or {}
    table.insert(normals[vi1], fnormal)
    table.insert(normals[vi2], fnormal)
    table.insert(normals[vi3], fnormal)
  end
  local vnormal = vec3()
  for i = 1, mesh:getVertexCount() do
    assert(normals[i], 'no triangle in index list contains vertex ' .. i)
    vnormal:set(0,0,0)
    local c = 0
    for _, fnormal in ipairs(normals[i]) do
      vnormal:add(fnormal)
      c = c + 1
    end
    vnormal:mul(1 / c)
    local v = {mesh:getVertex(i)}
    v[4], v[5], v[6] = vnormal:unpack()
    mesh:setVertex(i, v)
  end
  return mesh
end


function m.quad(subdivisions)
  local size = 1 / math.floor(subdivisions or 1)
  local vertices = {}
  local indices  = {}
  for y = -0.5, 0.5, size do
    for x = -0.5, 0.5, size do
      table.insert(vertices, {x, y, 0})
      table.insert(vertices, {x, y + size, 0})
      table.insert(vertices, {x + size, y, 0})
      table.insert(vertices, {x + size, y + size, 0})
      listappend(indices, {#vertices - 3, #vertices - 2, #vertices - 1})
      listappend(indices, {#vertices - 2, #vertices - 0, #vertices - 1})
    end
  end
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, "triangles", "dynamic", true)
  mesh:setVertexMap(indices)
  return mesh
end


function m.cube()
  local s = 0.5
  local vertices = {
    {-s, -s, -s}, {-s,  s, -s}, { s, -s, -s}, { s,  s, -s}, -- front
    { s,  s, -s}, { s,  s,  s}, { s, -s, -s}, { s, -s,  s}, -- right
    { s, -s,  s}, { s,  s,  s}, {-s, -s,  s}, {-s,  s,  s}, -- back
    {-s,  s,  s}, {-s,  s, -s}, {-s, -s,  s}, {-s, -s, -s}, -- left
    {-s, -s, -s}, { s, -s, -s}, {-s, -s,  s}, { s, -s,  s}, -- bottom
    {-s,  s, -s}, {-s,  s,  s}, { s,  s, -s}, { s,  s,  s}, -- top
  }
  local indices = {
     1,  2,  3,  3,  2,  4, -- front
     5,  6,  7,  7,  6,  8, -- top
     9, 10, 11, 11, 10, 12, -- back
    13, 14, 15, 15, 14, 16, -- bottom
    17, 18, 19, 19, 18, 20, -- left
    21, 22, 23, 23, 22, 24  -- right
  }
  local sides = {
    right =  {3, 4,  5,  6,  7,  8,  9, 10, 18, 20, 23, 24},
    bottom = {1, 3,  7,  8,  9, 11, 15, 16, 17, 18, 19, 20},
    back =   {6, 8,  9, 10, 11, 12, 13, 15, 19, 20, 22, 24},
    top =    {2, 4,  5,  6, 10, 12, 13, 14, 21, 22, 23, 24},
    front =  {1, 2,  3,  4,  5,  7, 14, 16, 17, 18, 21, 23},
    left =   {1, 2, 11, 12, 13, 14, 15, 16, 17, 19, 21, 22},
  }
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)

  return mesh, sides
end


function m.cantellatedCube() -- AKA rhombicuboctahedron
  local s, l = 0.5, 0.5 + 0.5 * math.sqrt(2)
  local vertices = {
    {-s, -l,  s}, {-s, -s,  l}, {-l, -s,  s}, {-s,  s,  l},
    {-s,  l,  s}, {-l,  s,  s}, {-s, -l, -s}, {-l, -s, -s},
    {-s, -s, -l}, {-s,  l, -s}, {-s,  s, -l}, {-l,  s, -s},
    { s, -l,  s}, { l, -s,  s}, { s, -s,  l}, { s,  l,  s},
    { s,  s,  l}, { l,  s,  s}, { s, -l, -s}, { s, -s, -l},
    { l, -s, -s}, { s,  l, -s}, { l,  s, -s}, { s,  s, -l},
  }
  local indices = {
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
  local sides = {
    right =  {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24},
    bottom = { 1,  2,  3,  7,  8,  9, 13, 14, 15, 19, 20, 21},
    back =   { 1,  2,  3,  4,  5,  6, 13, 14, 15, 16, 17, 18},
    top =    { 4,  5,  6, 10, 11, 12, 16, 17, 18, 22, 23, 24},
    front =  { 7,  8,  9, 10, 11, 12, 19, 20, 21, 22, 23, 24},
    left =   { 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12},
  }
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)

  return mesh, sides
end


function m.bipyramid(segments)
  segments = segments or 4
  local vertices = {}
  local indices = {}
  local sides = {top={}, bottom={}, ring={}}
  for i = 0, segments - 1 do
    -- top half
    table.insert(vertices,  {0, 0.5, 0})
    table.insert(sides.top, #vertices)
    local theta = i * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(vertices, {x, 0, z})
    table.insert(sides.ring, #vertices)
    local theta = (i + 1) * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(vertices, {x, 0, z})
    table.insert(sides.ring, #vertices)
    listappend(indices, {#vertices, #vertices - 1, #vertices - 2})
    -- bottom half
    table.insert(vertices,  {0, -0.5, 0})
    table.insert(sides.bottom, #vertices)
    local theta = i * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(vertices, {x, 0, z})
    table.insert(sides.ring, #vertices)
    local theta = (i + 1) * (2 * math.pi) / segments
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    table.insert(vertices, {x, 0, z})
    table.insert(sides.ring, #vertices)
    listappend(indices, {#vertices, #vertices - 2, #vertices - 1})
  end
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, sides
end


function m.pyramid(segments)
  local mesh, sides = m.bipyramid(segments)
  m.transform(mesh, mat4(0, -0.5, 0), sides.ring)
  listappend(sides.bottom, sides.ring)
  return mesh, sides
end


function m.cylinder(segments)
  segments = segments or 6
  local vertices = {}
  local indices = {}
  local sides = {top={}, bottom={}, ring={}}
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
    table.insert(vertices, v1)
    table.insert(sides.bottom, #vertices)
    table.insert(vertices, v2)
    table.insert(sides.top, #vertices)
    table.insert(vertices, v3)
    table.insert(sides.bottom, #vertices)
    table.insert(vertices, v4)
    table.insert(sides.top, #vertices)
    vi1, vi2, vi3, vi4 = #vertices-3, #vertices-2, #vertices-1, #vertices
    listappend(indices, {vi1, vi2, vi4, vi1, vi4, vi3})
    -- top and bottom sides
    theta = i * (2 * math.pi) / segments;
    v1 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v2 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    theta = (i + 1) * (2 * math.pi) / segments;
    v3 = {0.5 * math.cos(theta), -0.5, 0.5 * math.sin(theta)}
    v4 = {0.5 * math.cos(theta),  0.5, 0.5 * math.sin(theta)}
    table.insert(vertices, v1)
    table.insert(sides.bottom, #vertices)
    table.insert(vertices, v2)
    table.insert(sides.top, #vertices)
    table.insert(vertices, v3)
    table.insert(sides.bottom, #vertices)
    table.insert(vertices, v4)
    table.insert(sides.top, #vertices)
    vi1, vi2, vi3, vi4 = #vertices-3, #vertices-2, #vertices-1, #vertices
    listappend(indices, {vTop, vi4, vi2, vBottom, vi1, vi3})
  end
  table.insert(vertices, {0,  0.5, 0})
  table.insert(sides.top, #vertices)
  assert(vTop, #vertices)
  table.insert(vertices, {0, -0.5, 0})
  table.insert(sides.bottom, #vertices)
  assert(vBottom, #vertices)
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, sides
end


-- lovr-icosphere v0.0.1
-- https://github.com/bjornbytes/lovr-icosphere
-- MIT License
function m.sphere(subdivisions)
  subdivisions = subdivisions or 2
  local phi = (1 + math.sqrt(5)) / 2
  local vertices = {
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
  local indices = {
    1, 12, 6,  1, 6, 2,  1, 2, 8,  1, 8, 11,  1, 11, 12,
    2, 6, 10,  6, 12, 5,  12, 11, 3,  11, 8, 7,  8, 2, 9,
    4, 10, 5,  4, 5, 3,  4, 3, 7,  4, 7, 9,  4, 9, 10,
    5, 10, 6,  3, 5, 12,  7, 3, 11,  9, 7, 8,  10, 9, 2
  }
  -- Cache vertex splits to avoid duplicates
  local splits = {}
  -- Splits vertices i and j, creating a new vertex and returning the index
  local function split(i, j)
    local key = i < j and (i .. ',' .. j) or (j .. ',' .. i)

    if not splits[key] then
      local x = (vertices[i][1] + vertices[j][1]) / 2
      local y = (vertices[i][2] + vertices[j][2]) / 2
      local z = (vertices[i][3] + vertices[j][3]) / 2
      table.insert(vertices, { x, y, z })
      splits[key] = #vertices
    end

    return splits[key]
  end
  -- Subdivide
  for _ = 1, subdivisions do
    for i = #indices, 1, -3 do
      local v1, v2, v3 = indices[i - 2], indices[i - 1], indices[i - 0]
      local a = split(v1, v2)
      local b = split(v2, v3)
      local c = split(v3, v1)

      table.insert(indices, v1)
      table.insert(indices, a)
      table.insert(indices, c)

      table.insert(indices, v2)
      table.insert(indices, b)
      table.insert(indices, a)

      table.insert(indices, v3)
      table.insert(indices, c)
      table.insert(indices, b)

      table.insert(indices, a)
      table.insert(indices, b)
      table.insert(indices, c)

      table.remove(indices, i - 0)
      table.remove(indices, i - 1)
      table.remove(indices, i - 2)
    end
  end
  -- Normalize
  for i, v in ipairs(vertices) do
    local x, y, z = unpack(v)
    local length = math.sqrt(x * x + y * y + z * z) * 2
    v[1], v[2], v[3] = x / length, y / length, z / length
  end
  local mesh = lovr.graphics.newMesh(meshFormat, vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, {}
end


return m
