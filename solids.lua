local m = {}

local function tappend(t1, t2) -- table append
  for i,v in ipairs(t2) do table.insert(t1, v) end
end


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


function m.debugDraw(mesh, ...)
  local pose = mat4(...)
  local shader = lovr.graphics.getShader()
  lovr.graphics.setLineWidth(2)
  lovr.graphics.setShader()
  -- wireframe model
  lovr.graphics.setColor(0xd35c5c)
  lovr.graphics.setWireframe(true)
  mesh:draw(pose)
  lovr.graphics.setWireframe(false)
  -- face normals
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
    assert(normals[i], 'missing normals for index ' .. i)
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
  local sides = {
    front  = {1, 2, 3, 4},
    top    = {3, 4, 5, 6},
    back   = {5, 6, 7, 8},
    bottom = {1, 2, 7, 8},
    left   = {1, 3, 5, 7},
    right  = {2, 4, 6, 8},
  }
  local mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, sides
end

function m.pyramid(segments)
  segments = segments or 4
  local vertices = {}
  local indices = {}
  local sides = {top={}, bottom={}}
  
  for i = 0, segments - 1 do
    local theta = i * (2 * math.pi) / segments;
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    local v = {x, -0.5, z}
    table.insert(vertices, v)
    table.insert(sides.bottom, #vertices)
  end
  local vtop = {0, 0.5, 0}
  table.insert(vertices,  vtop)
  local topIndex = #vertices
  table.insert(sides.top, topIndex)
  local vbottom = {0, -0.5, 0}
  table.insert(vertices,  vbottom)
  local bottomIndex = #vertices
  table.insert(sides.bottom, bottomIndex )
  for i = 0, segments - 1 do
    local base = {bottomIndex, 1 + i, 1 + ((i + 1) % segments)}
    local side = {topIndex, 1 + i, 1 + ((i + 1) % segments)}
    tappend(indices, base)
    tappend(indices, side)
  end
  local mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, sides
end

function m.cylinder(segments)
  segments = segments or 24
  local vertices = {}
  local indices = {}
  local sides = {top={}, bottom={}, side={}}
  -- side
  for i = 0, segments - 1 do
    local theta = i * (2 * math.pi) / segments;
    local x = 0.5 * math.cos(theta)
    local z = 0.5 * math.sin(theta)
    local v1 = {x, -0.5, z}
    local v2 = {x,  0.5, z}
    table.insert(vertices, v1) -- odd indices for bottom
    table.insert(sides.bottom, #vertices)
    table.insert(vertices, v2) -- even indices for top
    table.insert(sides.top, #vertices)
    local tri1 = {1 + i * 2, 2 + i * 2, 1 + ((i + 1) % segments) * 2}
    local tri2 = {2 + ((i + 1) % segments) * 2, 1 + ((i + 1) % segments) * 2, 2 + i * 2}
    tappend(indices, tri1)
    tappend(indices, tri2)
  end
  -- bottom
  local v = {0, -0.5, 0}
  table.insert(vertices,  v)
  local bottomIndex = #vertices
  table.insert(sides.bottom, bottomIndex)
  for i = 0, segments - 1 do
    local tri = {bottomIndex, 1 + i * 2, 1 + ((i + 1) % segments) * 2}
    tappend(indices, tri)
  end
  -- top
  local v = {0, 0.5, 0}
  table.insert(vertices, v)
  local topIndex = #vertices
  table.insert(sides.top, topIndex)
  for i = 0, segments - 1 do
    local tri = {topIndex, 2 + ((i + 1) % segments) * 2, 2 + i * 2}
    tappend(indices, tri)
  end
  local mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, sides
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
  local mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  return mesh, {}
end


return m
