function setup()
    displayMode(FULLSCREEN)    
    a = CSG.cube()
    b = CSG.sphere({radius=1.35, stacks=8})
    c = CSG.cylinder({radius=0.4, start={-2,0,0}, stop={2,0,0}, slices=8})
    cs = a:intersect(b):subtract(c):toMesh()    
end

function draw()
    background(40, 40, 50)
    
    camera(0,0,800, 0,0,0)
    perspective(45, WIDTH/HEIGHT,-10,10)
    local d = math.sin(ElapsedTime*0.2)*360
    scale(100)
    rotate(d, 0,1,0)   
    cs:draw()
end

