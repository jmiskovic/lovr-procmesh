local m = {}
m.__index = m

local function tappend(t1, t2) -- table append
  for i,v in ipairs(t2) do table.insert(t1, v) end
end


function m.transform(vertices, m)
  for _, v in ipairs(vertices) do
    v[1], v[2], v[3] = m:mul(vec3(unpack(v))):unpack()
  end
end


function m:updateMesh(updateNormals)
  if updateNormals then
    self:updateNormals()
  end
  self.mesh = lovr.graphics.newMesh(self.vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(self.indices)
  return mesh
end


function m:updateNormals()
  if not self.vertices or not self.indices then return end
  local normals = {} -- maps vertex index to list of normals of adjacent faces
  lovr.math.drain()
  local v1, v2, v3 = vec3(), vec3(), vec3()
  for i = 1, #indices, 3 do
    local vi1, vi2, vi3 = indices[i], indices[i + 1], indices[i + 2]
    v1:set(unpack(vertices[vi1]))
    v2:set(unpack(vertices[vi2]))
    v3:set(unpack(vertices[vi3]))
    local fnormal = (v2 - v1):cross(v3 - v1):normalize()
    normals[vi1] = normals[vi1] or {}
    normals[vi2] = normals[vi2] or {}
    normals[vi3] = normals[vi3] or {}
    table.insert(normals[vi1], fnormal)
    table.insert(normals[vi2], fnormal)
    table.insert(normals[vi3], fnormal)
  end
  local vnormal = vec3()
  for i = 1, #vertices do
    assert(normals[i], 'missing normals for index ' .. i)
    vnormal:set(0,0,0)
    local c = 0
    for _, fnormal in ipairs(normals[i]) do
      vnormal:add(fnormal)
      c = c + 1
    end
    vnormal:mul(1 / c)
    vertices[i][4], vertices[i][5], vertices[i][6] = vnormal:unpack()
  end
end


function m.cube()
  local vertices = {
    {-0.5, -0.5,  0.5},
    { 0.5, -0.5,  0.5},
    {-0.5,  0.5,  0.5},
    { 0.5,  0.5,  0.5},
    {-0.5,  0.5, -0.5},
    { 0.5,  0.5, -0.5},
    {-0.5, -0.5, -0.5},
    { 0.5, -0.5, -0.5}
  }
  vertices.front  = {vertices[1], vertices[2], vertices[3], vertices[4]}
  vertices.top    = {vertices[3], vertices[4], vertices[5], vertices[6]}
  vertices.back   = {vertices[5], vertices[6], vertices[7], vertices[8]}
  vertices.bottom = {vertices[1], vertices[2], vertices[7], vertices[8]}
  vertices.left   = {vertices[1], vertices[3], vertices[5], vertices[7]}
  vertices.right  = {vertices[2], vertices[4], vertices[6], vertices[8]}
  local indices = {
    1, 2, 3, -- front
    3, 2, 4,
    3, 4, 5, -- top
    5, 4, 6,
    5, 6, 7, -- back
    7, 6, 8,
    7, 8, 1, -- bottom
    1, 8, 2,
    7, 1, 5, -- left
    5, 1, 3,
    2, 8, 4, -- right
    4, 8, 6,
  }
  indices.front  = {1, 2, 3, 3, 2, 4,}
  indices.top    = {3, 4, 5, 5, 4, 6,}
  indices.back   = {5, 6, 7, 7, 6, 8,}
  indices.bottom = {7, 8, 1, 1, 8, 2,}
  indices.left   = {7, 1, 5, 5, 1, 3,}
  indices.right  = {2, 8, 4, 4, 8, 6,}
  local self = setmetatable({vertices=vertices, indices=indices}, m)
  return self
end

function m.pyramid(segments)
  local vertices = {top={}, bottom={}}
  local indices = {top={}, bottom={}}
  
  for i = 0, segments - 1 do
    local theta = i * (2 * math.pi) / segments;
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    local v = {x, -0.5, z}
    table.insert(vertices, v)
    table.insert(vertices.bottom, v)
  end
  local vtop = {0, 0.5, 0}
  table.insert(vertices,  vtop)
  table.insert(vertices.top, vtop)
  local topIndex = #vertices
  local vbottom = {0, -0.5, 0}
  table.insert(vertices,  vbottom)
  table.insert(vertices.bottom, vbottom)
  local bottomIndex = #vertices
  for i = 0, segments - 1 do
    local base = {bottomIndex, 1 + i, 1 + ((i + 1) % segments)}
    local side = {topIndex, 1 + i, 1 + ((i + 1) % segments)}
    tappend(indices, base)
    tappend(indices.bottom, base)
    tappend(indices, side)
    tappend(indices.top, base)
  end
  local self = setmetatable({vertices=vertices, indices=indices}, m)
  return self
end

function m.cylinder(segments)
  segments = segments or 24
  local vertices = {top={}, bottom={}, side={}}
  local indices = {top={}, bottom={}, side={}}
  -- side
  for i = 0, segments - 1 do
    local theta = i * (2 * math.pi) / segments;
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    local v1 = {x, -0.5, z}
    local v2 = {x,  0.5, z}
    table.insert(vertices, v1) -- odd indices for bottom
    table.insert(vertices, v2) -- even indices for top
    table.insert(vertices.bottom, v1)
    table.insert(vertices.top, v2)
    table.insert(vertices.side, v1)
    table.insert(vertices.side, v2)
    local tri1 = {1 + i * 2, 2 + i * 2, 1 + ((i + 1) % segments) * 2}
    local tri2 = {2 + ((i + 1) % segments) * 2, 1 + ((i + 1) % segments) * 2, 2 + i * 2}
    tappend(indices, tri1)
    tappend(indices, tri2)
    tappend(indices.side, tri1)
    tappend(indices.side, tri2)
  end
  -- bottom
  local v = {0, -0.5, 0}
  table.insert(vertices,  v)
  table.insert(vertices.bottom, v)
  local bottomIndex = #vertices
  for i = 0, segments - 1 do
    local tri = {bottomIndex, 1 + i * 2, 1 + ((i + 1) % segments) * 2}
    tappend(indices, tri)
    tappend(indices.bottom, tri)
  end
  -- top
  local v = {0, 0.5, 0}
  table.insert(vertices, v)
  table.insert(vertices.top, v)
  local topIndex = #vertices
  for i = 0, segments - 1 do
    local tri = {topIndex, 2 + ((i + 1) % segments) * 2, 2 + i * 2}
    tappend(indices, tri)
    tappend(indices.top, tri)
  end
  local self = setmetatable({vertices=vertices, indices=indices}, m)
  return self
end


-- lovr-icosphere v0.0.1
-- https://github.com/bjornbytes/lovr-icosphere
-- MIT License
function m.sphere(subdivisions)
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
    1, 12, 6,
    1, 6, 2,
    1, 2, 8,
    1, 8, 11,
    1, 11, 12,

    2, 6, 10,
    6, 12, 5,
    12, 11, 3,
    11, 8, 7,
    8, 2, 9,

    4, 10, 5,
    4, 5, 3,
    4, 3, 7,
    4, 7, 9,
    4, 9, 10,

    5, 10, 6,
    3, 5, 12,
    7, 3, 11,
    9, 7, 8,
    10, 9, 2
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
  for _ = 1, subdivisions or 0 do
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
    local length = math.sqrt(x * x + y * y + z * z)
    v[1], v[2], v[3] = x / length, y / length, z / length
  end
  local self = setmetatable({vertices=vertices, indices=indices}, m)
  return self
end


return m
