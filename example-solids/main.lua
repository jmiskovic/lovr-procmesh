local solids = require 'solids'

local primitives = {}
primitives.cube = solids.cube()
primitives.sphere = solids.sphere(1)
primitives.cylinder = solids.cylinder(12)
primitives.bipyramid = solids.bipyramid(6)
primitives.pyramid = solids.pyramid(6)

local modified = {}
modified.cube, sides = solids.cube()
solids.transform(modified.cube, mat4(0,0,0, 0.4, 1, 0.4), sides.top)
modified.sphere = solids.sphere(2)
solids.transform(modified.sphere, mat4(0,0,0, 1, 0.4, 1))
modified.cylinder, sides = solids.cylinder(12)
solids.transform(modified.cylinder, mat4(0,0,0, 0.3,1,0.3), sides.top)
modified.bipyramid, sides = solids.bipyramid(8)
solids.transform(modified.bipyramid, mat4(0,-0.3,0), sides.bottom)
modified.pyramid, sides = solids.pyramid()
solids.transform(modified.pyramid, mat4(0,0,0, 1,1,0.3), sides.bottom)
solids.transform(modified.pyramid, mat4(0,0,0, 1,1,1, math.pi/2,   1,0,0))

for k,v in pairs(primitives) do
  solids.updateNormals(v)
end

for k,v in pairs(modified) do
  solids.updateNormals(v)
end

function lovr.update(dt)
end

lovr.graphics.setBackgroundColor(0xf7eac8)
shader = lovr.graphics.newShader('standard')
lovr.graphics.setLineWidth(1)

function drawAt(mesh, wire, text, ...)
  lovr.graphics.push()
  lovr.graphics.transform(...)
  if #text > 0 then
    lovr.graphics.setColor(0x544532)
    lovr.graphics.print(text,  -0, 0.8, 0, 0.25)
  end
  lovr.graphics.rotate(lovr.timer.getTime()/4, 0,1,0)
  lovr.graphics.setWireframe(false)
  lovr.graphics.setColor(0x2b295f)
  if wire then
    lovr.graphics.setWireframe(true)
    lovr.graphics.setColor(0x544532)
    mesh:draw()
    lovr.graphics.setWireframe(false)
  else
    lovr.graphics.setColor(0xf0f0f0)
    lovr.graphics.setShader(shader)
    mesh:draw()
    lovr.graphics.setShader()
    lovr.graphics.setWireframe(true)
    --lovr.graphics.setColor(0f0f0f0)
    mesh:draw()
    lovr.graphics.setWireframe(false)
  end
  lovr.graphics.pop()
end

function lovr.draw()
  local x, y = -2.7, 1
  for name, mesh in pairs(primitives) do
    drawAt(mesh, false, name, x, y, -5)
    x = x + 1.3
  end
  local x, y = -2.7, -1
  for name, mesh in pairs(modified) do
    drawAt(mesh, false, '', x, y, -5)
    x = x + 1.3
  end
  lovr.graphics.setColor(0x544532)
  lovr.graphics.print('modified primitives', 0, -1.9, -5, 0.25)
end
