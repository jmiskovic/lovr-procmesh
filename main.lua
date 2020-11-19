local CSG = require('csg')

function updatenormals(mesh)
  lovr.math.drain()
  local indices = mesh:getVertexMap(indices)
  local normals = {} -- maps vertex index to list of normals of adjacent faces
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
    mesh:setVertexAttribute(i, 2, {vnormal:unpack()})
  end
end

local cube = CSG.cube()
local sphere = CSG.sphere({radius=1, stacks=15})
local cylinder = CSG.cylinder({radius=1, slices=15})
local complex
--complex = CSG.cube():intersect(CSG.cube({radius={2, 0.1, 0.1}}))
--complex = sphere:clone()
--complex = CSG.cube():subtract(CSG.cylinder({start={0,2,0}, stop={0,-2,0}, slices=15}))
--complex = complex:intersect(cube:clone():transform(mat4(0, -1, 0)))
complex = CSG.cube():subtract(CSG.cube({ radius= 0.1, center={0,0,1} }))
local vertices, indices = complex:toMeshVertices()
local mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
mesh:setVertexMap(indices)
updatenormals(mesh)

function lovr.update(dt)
  complex = CSG.cube():subtract(CSG.cube({ radius= 0.3 + 0.2 * math.sin(lovr.timer.getTime()), center={1,1,1} }))
  complex = complex:subtract(CSG.cube({ radius= 0.3 + 0.2 * math.sin(lovr.timer.getTime()), center={-1,1,1} }))
  local vertices, indices = complex:toMeshVertices()
  mesh = lovr.graphics.newMesh(vertices, 'triangles', 'dynamic', true)
  mesh:setVertexMap(indices)
  updatenormals(mesh)
end


shader = lovr.graphics.newShader('standard')
shader:send('lovrExposure', 2)
shader:send('lovrLightDirection', { -2, -0.5, -0.6 })
shader:send('lovrLightColor', { 1, 1, 1, 1.0 })
lovr.graphics.setShader(shader)
lovr.graphics.setBackgroundColor(0.1, 0.2, 0)
lovr.graphics.setCullingEnabled(not true)
lovr.headset.setClipDistance(0.1, 100000)


function lovr.draw()
  lovr.graphics.translate(0, 1, -3)
  lovr.graphics.setColor(0.5, 0.9, 0.5)
  mesh:draw()
  --[[ wireframe ]]
  lovr.graphics.setDepthTest('less', true)
  lovr.graphics.setColor(1,1,1)
  lovr.graphics.setWireframe(true)
  mesh:draw()
  lovr.graphics.setWireframe(not true)
end
  complex:debugDraw()