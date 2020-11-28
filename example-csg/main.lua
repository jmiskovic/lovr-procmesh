local solids = require 'solids'
local csg = require 'csg'

meshA = solids.pyramid(4)
solids.transform(meshA, mat4(0, 0, 0, 1,1,1, math.pi/12, 1,0,0))
csgA = csg.fromMesh(meshA)
meshB = solids.cube()
solids.transform(meshB, mat4(0, 0, 0, 0.7,0.7,0.7, math.pi/6, 1,1,1))
  solids.updateNormals(meshA)
  solids.updateNormals(meshB)


function lovr.update(dt)
  --solids.transform(meshA, mat4(0, 0, 0, dt/4, 0,1,0))
  solids.transform(meshB, mat4( math.cos(lovr.timer.getTime()) * 0.005, 0, math.sin(lovr.timer.getTime()) * 0.005))
  csgB = csg.fromMesh(meshB)
  csgU = csgA:union(csgB)
  csgI = csgA:intersect(csgB)
  csgS = csgA:subtract(csgB)
  meshU = csgU:toMesh()
  meshI = csgI:toMesh()
  meshS = csgS:toMesh()
  solids.updateNormals(meshU)
  solids.updateNormals(meshI)
  solids.updateNormals(meshS)
end

lovr.graphics.setBackgroundColor(0xf7eac8)
shader = lovr.graphics.newShader('standard')
lovr.graphics.setLineWidth(1)

function drawAt(mesh, wire, text, ...)
  lovr.graphics.push()
  lovr.graphics.transform(...)
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
  if #text > 0 then
    lovr.graphics.setColor(0x544532)
    lovr.graphics.print(text,  -0, 0.8, 0, 0.25)
  end
  lovr.graphics.pop()
end

function lovr.draw()
  drawAt(meshA, true, 'A',            -1.6, 0,   -5)
  drawAt(meshB, true, 'B',             1.6, 0,   -5)
  drawAt(meshU, false, 'union',        0,   0,   -5)
  --drawAt(meshB, true, '',              0,   0,   -5)
  drawAt(meshI, false, 'intersection', 0,   1.7, -5)
  --drawAt(meshB, true, '',              0,   1.5, -5)
  drawAt(meshS, false, 'subtraction',  0,  -1.7, -5)
  --drawAt(meshB, true, '',              0,  -1.5, -5)
end
