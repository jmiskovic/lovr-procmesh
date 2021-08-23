--[[
Lua port of Raymond Hill's excellent Javascript implementation.
Subsequently adapted API to better fit procmesh library.

-------- Original notes ------------------

Author: Raymond Hill (rhill@raymondhill.net)
Contributor: Jesse Morgan (morgajel@gmail.com)
File: rhill-voronoi-core.js
Version: 0.98
Date: January 21, 2013
Description: This is my personal Javascript implementation of
Steven Fortune's algorithm to compute Voronoi diagrams.

Copyright (C) 2010,2011 Raymond Hill
https://github.com/gorhill/Javascript-Voronoi

Licensed under The MIT License
http://en.wikipedia.org/wiki/MIT_License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

More historical information available at Voronoi.lua in:
https://code.google.com/archive/p/demirogue/source/default/source

--]]

local Voronoi = {
  RBTree = {},
  Cell = {},
  Halfedge = {},
}

local RBTree = Voronoi.RBTree
local Cell = Voronoi.Cell
local Halfedge = Voronoi.Halfedge

local function push( array, value )
  array[#array+1] = value
end

local function pop( array )
  local result = array[#array]
  array[#array] = nil
  return result
end

function Voronoi:new()
  local result = {
    edges = nil,
    cells = nil,
    beachsectionJunkyard = {},
    circleEventJunkyard = {},
    beachline = nil,
    circleEvents = nil,
    firstCircleEvent = nil,
  }

  setmetatable(result, self)
  self.__index = self

  return result
end

function Voronoi:reset()
  if not self.beachline then
    self.beachline = RBTree:new()
  end
  -- Move leftover beachsections to the beachsection junkyard.
  if self.beachline.root then
    local beachsection = self.beachline:getFirst(self.beachline.root)
    while beachsection do
      push(self.beachsectionJunkyard, beachsection) -- mark for reuse
      beachsection = beachsection.rbNext
    end
  end
  self.beachline.root = nil
  
  if not self.circleEvents then
    self.circleEvents = RBTree:new()
  end

  self.circleEvents.root, self.firstCircleEvent = nil, nil
  self.edges = {}
  self.cells = {}
end


local sqrt = math.sqrt
local abs = math.abs
local EPSILON = 1e-9
local function equalWithEpsilon( a, b )
  return abs(a-b) < EPSILON
end
local function greaterThanWithEpsilon( a, b )
  return a-b > EPSILON
end
local function greaterThanOrEqualWithEpsilon( a, b )
  return b-a < EPSILON
end
local function lessThanWithEpsilon( a, b )
  return b-a > EPSILON
end
local function lessThanOrEqualWithEpsilon( a, b )
  return a-b < EPSILON
end


-- ---------------------------------------------------------------------------
-- Red-Black tree code (based on C version of "rbtree" by Franck Bui-Huu
-- https://github.com/fbuihuu/libtree/blob/master/rb.c

-- [DCS] Anything that can be an RBTree node will need the following fields.
--   rbPrevious
--   rbNext
--   rbRight
--   rbLeft
--   rbLeft
--   rbParent
--   rbRed

function Voronoi.RBTree:new()
  local result = {
    root = nil
  }

  setmetatable(result, self)
  self.__index = self

  return result
end

-- [DCS] The following is useful for debugging.
-- function Voronoi.RBTree:_invariant()
--  local root = self.root
--  
--  if not root then
--    return
--  end
--
--  assert(not root.rbRed)
--
--  local function aux( node )
--    -- Children of red nodes must be black.
--    if node.rbRed then
--      assert(not node.rbLeft or not node.rbLeft.rbRed)
--      assert(not node.rbRight or not node.rbRight.rbRed)
--    end
--
--    -- Opinions seemd to differ about whether this is a rule or not.
--    -- -- Leaves are the same colour as the root.
--    -- if not node.rbLeft and not node.rbRight then
--    --  assert(node.rbRed == root.rbRed)
--    -- end
--
--    if node.rbLeft then
--      aux(node.rbLeft)
--    end
--
--    if node.rbRight then
--      aux(node.rbRight)
--    end
--  end
--
--  aux(root)
-- end
--
-- local function rbPrint( root )
--  if not root then
--    print('empty')
--    print()
--    return
--  end
--
--  local function aux( node, indent )
--    local pad = string.rep(' ', indent)
--
--    print(string.format('%s%s', pad, node.rbRed and 'red' or 'black'))
--
--    if node.rbLeft then
--      aux(node.rbLeft, indent+2)
--    end
--
--    if node.rbRight then
--      aux(node.rbRight, indent+2)
--    end
--  end
--
--  aux(root, 0)
--  print()
-- end
--
-- local function wrap( f )
--  return
--    function ( self, ... )
--      print('_invariant', ...)
--      print('pre')
--      rbPrint(self.root)
--      self:_invariant()
--      local result = f(self, ...)
--      print('post')
--      rbPrint(self.root)
--      self:_invariant()
--      return result
--    end
-- end

function Voronoi.RBTree:rbInsertSuccessor( node, successor )
  local parent

  if node then
    -- >>> rhill 2011-05-27: Performance: cache previous/next nodes
    successor.rbPrevious = node
    successor.rbNext = node.rbNext
    if node.rbNext then
      node.rbNext.rbPrevious = successor
    end
    node.rbNext = successor
    -- <<<
    if node.rbRight then
      -- in-place expansion of node.rbRight.getFirst()
      node = node.rbRight
      while node.rbLeft do
        node = node.rbLeft
      end
      node.rbLeft = successor
    else
      node.rbRight = successor
    end
    parent = node
  -- rhill 2011-06-07: if node is null, successor must be inserted
  -- to the left-most part of the tree
  elseif self.root then
    node = self:getFirst(self.root)
    -- >>> Performance: cache previous/next nodes
    successor.rbPrevious = nil
    successor.rbNext = node
    node.rbPrevious = successor
    -- <<<
    node.rbLeft = successor
    parent = node
  else
    -- >>> Performance: cache previous/next nodes
    successor.rbPrevious, successor.rbNext = nil, nil
    -- <<<
    self.root = successor
    parent = nil
  end
  successor.rbLeft, successor.rbRight = nil, nil
  successor.rbParent = parent
  successor.rbRed = true
  -- Fixup the modified tree by recoloring nodes and performing
  -- rotations (2 at most) hence the red-black tree properties are
  -- preserved.
  local grandpa, uncle
  node = successor
  while parent and parent.rbRed do
    grandpa = parent.rbParent
    if parent == grandpa.rbLeft then
      uncle = grandpa.rbRight
      if uncle and uncle.rbRed then
        parent.rbRed, uncle.rbRed = false, false
        grandpa.rbRed = true
        node = grandpa
      else
        if node == parent.rbRight then
          self:rbRotateLeft(parent)
          node = parent
          parent = node.rbParent
        end
        parent.rbRed = false
        grandpa.rbRed = true
        self:rbRotateRight(grandpa)
      end
    else
      uncle = grandpa.rbLeft
      if uncle and uncle.rbRed then
        parent.rbRed, uncle.rbRed = false, false
        grandpa.rbRed = true
        node = grandpa
      else
        if node == parent.rbLeft then
          self:rbRotateRight(parent)
          node = parent
          parent = node.rbParent
        end
        parent.rbRed = false
        grandpa.rbRed = true
        self:rbRotateLeft(grandpa)
      end
    end
    parent = node.rbParent
  end
  
  self.root.rbRed = false
end

-- [DCS] Useful for debugging.
-- Voronoi.RBTree.rbInsertSuccessor = wrap(Voronoi.RBTree.rbInsertSuccessor)

function Voronoi.RBTree:rbRemoveNode( node )
  -- >>> rhill 2011-05-27: Performance: cache previous/next nodes
  if node.rbNext then
    node.rbNext.rbPrevious = node.rbPrevious
  end
  if node.rbPrevious then
    node.rbPrevious.rbNext = node.rbNext
  end
  node.rbNext, node.rbPrevious = nil, nil
  -- <<<
  local parent = node.rbParent
  local left = node.rbLeft
  local right = node.rbRight
  local next
  if not left then
    next = right
  elseif not right then
    next = left
  else
    next = self:getFirst(right)
  end
  if parent then
    if parent.rbLeft == node then
      parent.rbLeft = next
    else
      parent.rbRight = next
    end
  else
    self.root = next
  end
  -- enforce red-black rules
  local isRed
  if left and right then
    isRed = next.rbRed
    next.rbRed = node.rbRed
    next.rbLeft = left
    left.rbParent = next
    if next ~= right then
      parent = next.rbParent
      next.rbParent = node.rbParent
      node = next.rbRight
      parent.rbLeft = node
      next.rbRight = right
      right.rbParent = next
    else
      next.rbParent = parent
      parent = next
      node = next.rbRight
    end
  else
    isRed = node.rbRed
    node = next
  end
  -- 'node' is now the sole successor's child and 'parent' its
  -- new parent (since the successor can have been moved)
  if node then
    node.rbParent = parent
  end
  -- the 'easy' cases
  if isRed then
    return
  end
  if node and node.rbRed then
    node.rbRed = false
    return
  end
  -- the other cases
  local sibling
  repeat
    if node == self.root then
      break
    end
    if node == parent.rbLeft then
      sibling = parent.rbRight
      if sibling.rbRed then
        sibling.rbRed = false
        parent.rbRed = true
        self:rbRotateLeft(parent)
        sibling = parent.rbRight
      end
      if (sibling.rbLeft and sibling.rbLeft.rbRed) or (sibling.rbRight and sibling.rbRight.rbRed) then
        if not sibling.rbRight or not sibling.rbRight.rbRed then
          sibling.rbLeft.rbRed = false
          sibling.rbRed = true
          self:rbRotateRight(sibling)
          sibling = parent.rbRight
        end
        sibling.rbRed = parent.rbRed
        parent.rbRed, sibling.rbRight.rbRed = false, false
        self:rbRotateLeft(parent)
        node = self.root
        break
      end
    else
      sibling = parent.rbLeft
      if sibling.rbRed then
        sibling.rbRed = false
        parent.rbRed = true
        self:rbRotateRight(parent)
        sibling = parent.rbLeft
      end
      if (sibling.rbLeft and sibling.rbLeft.rbRed) or (sibling.rbRight and sibling.rbRight.rbRed) then
        if not sibling.rbLeft or not sibling.rbLeft.rbRed then
          sibling.rbRight.rbRed = false
          sibling.rbRed = true
          self:rbRotateLeft(sibling)
          sibling = parent.rbLeft
        end
        sibling.rbRed = parent.rbRed
        parent.rbRed, sibling.rbLeft.rbRed = false, false
        self:rbRotateRight(parent)
        node = self.root
        break
      end
    end
    sibling.rbRed = true
    node = parent
    parent = parent.rbParent
  until node.rbRed
  
  if node then
    node.rbRed = false
  end
end

-- [DCS] Useful for debugging.
-- Voronoi.RBTree.rbRemoveNode = wrap(Voronoi.RBTree.rbRemoveNode)

function Voronoi.RBTree:rbRotateLeft( node )
  local p = node
  local q = node.rbRight -- can't be nil
  local parent = p.rbParent
  if parent then
    if parent.rbLeft == p then
      parent.rbLeft = q
    else
      parent.rbRight = q
    end
  else
    self.root = q
  end
  q.rbParent = parent
  p.rbParent = q
  p.rbRight = q.rbLeft
  if p.rbRight then
    p.rbRight.rbParent = p
  end
  q.rbLeft = p
end

function Voronoi.RBTree:rbRotateRight( node )
  local p = node
  local q = node.rbLeft -- can't be nil
  local parent = p.rbParent
  if parent then
    if parent.rbLeft == p then
      parent.rbLeft = q
    else
      parent.rbRight = q
    end
  else
    self.root = q
  end
  q.rbParent = parent
  p.rbParent = q
  p.rbLeft = q.rbRight
  if p.rbLeft then
    p.rbLeft.rbParent = p
  end
  q.rbRight = p
end

function Voronoi.RBTree:getFirst( node )
  while node.rbLeft do
    node = node.rbLeft
  end
  return node
end

function Voronoi.RBTree:getLast( node )
  while node.rbRight do
    node = node.rbRight
  end
  return node
end

-- [DCS] Voronoi.Diagram is just a POD so I've removed it.

-- ---------------------------------------------------------------------------
-- Cell methods

function Voronoi.Cell:new( site )
  local result = {
    site = site,
    halfedges = {},
  }

  setmetatable(result, self)
  self.__index = self

  return result
end

function Voronoi.Cell:prepare()
  local halfedges = self.halfedges
  local numHalfedges = #halfedges
  -- get rid of unused halfedges
  -- rhill 2011-05-27: Keep it simple, no point here in trying
  -- to be fancy: dangling edges are a typically a minority.
  for index = numHalfedges, 1, -1 do
    local edge = halfedges[index].edge
    if not edge.vb or not edge.va then
      table.remove(halfedges, index)
    end
  end

  -- rhill 2011-05-26: I tried to use a binary search at insertion
  -- time to keep the array sorted on-the-fly (in Cell.addHalfedge()).
  -- There was no real benefits in doing so, performance on
  -- Firefox 3.6 was improved marginally, while performance on
  -- Opera 11 was penalized marginally.
  table.sort(halfedges, function( a, b ) return b.angle < a.angle end)
  return #halfedges
end


-- Return a list of the neighbor Ids
function Voronoi.Cell:getNeighborIds()
  local neighbors = {}
  local halfedges = self.halfedges
  for index = 1, #halfedges do
    local edge = halfedges[index].edge
    if edge.lSite ~= nil and edge.lSite.voronoiId ~= self.site.voronoiId then
      neighbors[#neighbors+1] = edge.lSite.voronoiId
    elseif edge.rSite ~= nil and edge.rSite.voronoiId ~= self.site.voronoiId then
      neighbors[#neighbors+1] = edge.rSite.voronoiId
    end
  end

  return neighbors
end

local function _edgeLength( edge )
  local p1 = edge.lSite
  local p2 = edge.rSite

  local s = {
    x = p2[1] - p1[1],
    y = p2[2] - p1[2]
  }

  return math.sqrt(s[1] * s[1] + s[2] * s[2])
end

function Voronoi.Cell:getNeighborIdAndEdgeLengths()
  local neighbors = {}
  local halfedges = self.halfedges
  for index = 1, #halfedges do
    local edge = halfedges[index].edge
    if edge.lSite ~= nil and edge.lSite.voronoiId ~= self.site.voronoiId then
      neighbors[#neighbors+1] = {
        voronoiId = edge.lSite.voronoiId,
        edgeLength = _edgeLength(edge),
      }
    elseif edge.rSite ~= nil and edge.rSite.voronoiId ~= self.site.voronoiId then
      neighbors[#neighbors+1] = {
        voronoiId = edge.rSite.voronoiId,
        edgeLength = _edgeLength(edge),
      }
    end
  end

  return neighbors
end


-- Compute bounding box
--
function Voronoi.Cell:getBbox()
  local halfedges = self.halfedges
  local xmin = math.huge
  local ymin = math.huge
  local xmax = -math.huge
  local ymax = -math.huge
  for index = 1, #halfedges do
    local v = halfedges[index]:getStartpoint()
    local vx = v[1]
    local vy = v[2]
    if vx < xmin then xmin = vx end
    if vy < ymin then ymin = vy end
    if vx > xmax then xmax = vx end
    if vy > ymax then ymax = vy end
    -- we dont need to take into account end point,
    -- since each end point matches a start point
  end
  return {
    x = xmin,
    y = ymin,
    width = xmax-xmin,
    height = ymax-ymin,
  }
end

-- Return whether a point is inside, on, or outside the cell:
--   -1: point is outside the perimeter of the cell
--  0: point is on the perimeter of the cell
--  1: point is inside the perimeter of the cell
--
function Voronoi.Cell:pointIntersection( x, y )
  -- Check if point in polygon. Since all polygons of a Voronoi
  -- diagram are convex, then:
  -- http://paulbourke.net/geometry/polygonmesh/
  -- Solution 3 (2D):
  --   "If the polygon is convex then one can consider the polygon
  --   "as a 'path' from the first vertex. A point is on the interior
  --   "of this polygons if it is always on the same side of all the
  --   "line segments making up the path. ...
  --   "(y - y0) (x1 - x0) - (x - x0) (y1 - y0)
  --   "if it is less than 0 then P is to the right of the line segment,
  --   "if greater than 0 it is to the left, if equal to 0 then it lies
  --   "on the line segment"
  local halfedges = self.halfedges
  for index = 1, #halfedges do
    local halfedge = halfedges[index]
    local p0 = halfedge:getStartpoint()
    local p1 = halfedge:getEndpoint()
    local r = (y-p0[2])*(p1[1]-p0[1])-(x-p0[1])*(p1[2]-p0[2])
    if r == 0 then
      return 0
    end
    if r > 0 then
      return -1
    end
  end
  return 1
end

-- ---------------------------------------------------------------------------
-- Edge methods
--

-- [DCS] Both Vertex and Edge are PoD but I've kept them to make porting the
--     rest of the code more straightforward.

local function Vertex( x, y )
  return {
    x,
    y,
  }
end

local function Edge( lSite, rSite )
  return {
    lSite = lSite,
    rSite = rSite,
    va = nil,
    vb = nil,
  }
end

function Voronoi.Halfedge:new( edge, lSite, rSite )
  local result = {
    site = lSite,
    edge = edge,
    angle = nil,
  }

  -- 'angle' is a value to be used for properly sorting the
  -- halfsegments counterclockwise. By convention, we will
  -- use the angle of the line defined by the 'site to the left'
  -- to the 'site to the right'.
  -- However, border edges have no 'site to the right': thus we
  -- use the angle of line perpendicular to the halfsegment (the
  -- edge should have both end points defined in such case.)
  if rSite then
    result.angle = math.atan2(rSite[2]-lSite[2], rSite[1]-lSite[1])
  else
    local va = edge.va
    local vb = edge.vb
    -- rhill 2011-05-31: used to call getStartpoint()/getEndpoint(),
    -- but for performance purpose, these are expanded in place here.
    if edge.lSite == lSite then
      result.angle = math.atan2(vb[1]-va[1], va[2]-vb[2])
    else
      result.angle = math.atan2(va[1]-vb[1], vb[2]-va[2])
    end
  end

  setmetatable(result, self)
  self.__index = self

  return result
end

function Voronoi.Halfedge:getStartpoint()
  return self.edge.lSite == self.site and self.edge.va or self.edge.vb
end

function Voronoi.Halfedge:getEndpoint()
  return self.edge.lSite == self.site and self.edge.vb or self.edge.va
end

function Voronoi.Halfedge:length()
  local p1 = self:getStartpoint()
  local p2 = self:getEndpoint()

  local s = {
    x = p2[1] - p1[1],
    y = p2[2] - p1[2]
  }

  return math.sqrt(s[1] * s[1] + s[2] * s[2])
end


-- this create and add an edge to internal collection, and also create
-- two halfedges which are added to each site's counterclockwise array
-- of halfedges.
function Voronoi:createEdge( lSite, rSite, va, vb )
  assert(lSite, rSite)
  local edge = Edge(lSite, rSite)
  local edges = self.edges
  edges[#edges+1] = edge
  if va then
    self:setEdgeStartpoint(edge, lSite, rSite, va)
  end
  if vb then
    self:setEdgeEndpoint(edge, lSite, rSite, vb)
  end

  local cells = self.cells
  local lhalfedges = cells[lSite.voronoiId].halfedges
  local rhalfedges = cells[rSite.voronoiId].halfedges

  lhalfedges[#lhalfedges+1] = Halfedge:new(edge, lSite, rSite)
  rhalfedges[#rhalfedges+1] = Halfedge:new(edge, rSite, lSite)

  return edge
end

function Voronoi:createBorderEdge( lSite, va, vb )
  local edge = Edge(lSite, nil)
  edge.va = va
  edge.vb = vb
  local edges = self.edges
  edges[#edges+1] = edge
  return edge
end

function Voronoi:setEdgeStartpoint( edge, lSite, rSite, vertex )
  if not edge.va and not edge.vb then
    edge.va = vertex
    edge.lSite = lSite
    edge.rSite = rSite
  elseif edge.lSite == rSite then
    edge.vb = vertex
  else
    edge.va = vertex
  end
end

function Voronoi:setEdgeEndpoint( edge, lSite, rSite, vertex )
  self:setEdgeStartpoint(edge, rSite, lSite, vertex)
end

-- ---------------------------------------------------------------------------
-- Beachline methods

-- rhill 2011-06-07: For some reasons, performance suffers significantly
-- when instanciating a literal object instead of an empty ctor
local function Beachsection()
  return {
    site = nil,
    edge = nil,

    -- [DCS] Used as an RBTree node so declare the fields here so the
    --     memory is allocated for them.
    rbPrevious = nil,
    rbNext = nil,
    rbRight = nil,
    rbLeft = nil,
    rbLeft = nil,
    rbParent = nil,
    rbRed = nil,
  }
end

-- rhill 2011-06-02: A lot of Beachsection instanciations
-- occur during the computation of the Voronoi diagram,
-- somewhere between the number of sites and twice the
-- number of sites, while the number of Beachsections on the
-- beachline at any given time is comparatively low. For this
-- reason, we reuse already created Beachsections, in order
-- to avoid new memory allocation. This resulted in a measurable
-- performance gain.
function Voronoi:createBeachsection( site )
  assert(site)
  local beachsection = pop(self.beachsectionJunkyard)
  if not beachsection then
    beachsection = Beachsection()
  end
  beachsection.site = site
  return beachsection
end

-- calculate the left break point of a particular beach section,
-- given a particular sweep line
function Voronoi:leftBreakPoint( arc, directrix )
  -- http://en.wikipedia.org/wiki/Parabola
  -- http://en.wikipedia.org/wiki/Quadratic_equation
  -- h1 = x1,
  -- k1 = (y1+directrix)/2,
  -- h2 = x2,
  -- k2 = (y2+directrix)/2,
  -- p1 = k1-directrix,
  -- a1 = 1/(4*p1),
  -- b1 = -h1/(2*p1),
  -- c1 = h1*h1/(4*p1)+k1,
  -- p2 = k2-directrix,
  -- a2 = 1/(4*p2),
  -- b2 = -h2/(2*p2),
  -- c2 = h2*h2/(4*p2)+k2,
  -- x = (-(b2-b1) + Math.sqrt((b2-b1)*(b2-b1) - 4*(a2-a1)*(c2-c1))) / (2*(a2-a1))
  -- When x1 become the x-origin:
  -- h1 = 0,
  -- k1 = (y1+directrix)/2,
  -- h2 = x2-x1,
  -- k2 = (y2+directrix)/2,
  -- p1 = k1-directrix,
  -- a1 = 1/(4*p1),
  -- b1 = 0,
  -- c1 = k1,
  -- p2 = k2-directrix,
  -- a2 = 1/(4*p2),
  -- b2 = -h2/(2*p2),
  -- c2 = h2*h2/(4*p2)+k2,
  -- x = (-b2 + Math.sqrt(b2*b2 - 4*(a2-a1)*(c2-k1))) / (2*(a2-a1)) + x1

  -- change code below at your own risk: care has been taken to
  -- reduce errors due to computers' finite arithmetic precision.
  -- Maybe can still be improved, will see if any more of this
  -- kind of errors pop up again.
  local site = arc.site
  local rfocx = site[1]
  local rfocy = site[2]
  local pby2 = rfocy-directrix
  -- parabola in degenerate case where focus is on directrix
  if pby2 == 0 then
    return rfocx
  end
  local lArc = arc.rbPrevious
  if not lArc then
    return -math.huge
  end
  site = lArc.site
  local lfocx = site[1]
  local lfocy = site[2]
  local plby2 = lfocy-directrix
  -- parabola in degenerate case where focus is on directrix
  if plby2 == 0 then
    return lfocx
  end
  local hl = lfocx-rfocx
  local aby2 = 1/pby2-1/plby2
  local b = hl/plby2
  if aby2 ~= 0 then
    return (-b+math.sqrt(b*b-2*aby2*(hl*hl/(-2*plby2)-lfocy+plby2/2+rfocy-pby2/2)))/aby2+rfocx
  end
  -- both parabolas have same distance to directrix, thus break point is midway
  return (rfocx+lfocx)/2
end

-- calculate the right break point of a particular beach section,
-- given a particular directrix
function Voronoi:rightBreakPoint( arc, directrix )
  local rArc = arc.rbNext
  if rArc then
    return self:leftBreakPoint(rArc, directrix)
  end
  local site = arc.site
  return (site[2] == directrix) and site[1] or math.huge
end

function Voronoi:detachBeachsection( beachsection )
  self:detachCircleEvent(beachsection) -- detach potentially attached circle event
  self.beachline:rbRemoveNode(beachsection) -- remove from RB-tree
  push(self.beachsectionJunkyard, beachsection) -- mark for reuse

  -- [DCS] nil fields to ensure nothing dangles.
  -- beachsection.site = nil
  -- beachsection.edge = nil
  -- beachsection.rbPrevious = nil
  -- beachsection.rbNext = nil
  -- beachsection.rbRight = nil
  -- beachsection.rbLeft = nil
  -- beachsection.rbLeft = nil
  -- beachsection.rbParent = nil
  -- beachsection.rbRed = nil
end

function Voronoi:removeBeachsection( beachsection )
  local circle = beachsection.circleEvent
  local x = circle[1]
  local y = circle.ycenter
  local vertex = Vertex(x, y)
  local previous = beachsection.rbPrevious
  local next = beachsection.rbNext
  local disappearingTransitions = { beachsection }
  local abs_fn = math.abs

  -- remove collapsed beachsection from beachline
  self:detachBeachsection(beachsection)

  -- there could be more than one empty arc at the deletion point, this
  -- happens when more than two edges are linked by the same vertex,
  -- so we will collect all those edges by looking up both sides of
  -- the deletion point.
  -- by the way, there is *always* a predecessor/successor to any collapsed
  -- beach section, it's just impossible to have a collapsing first/last
  -- beach sections on the beachline, since they obviously are unconstrained
  -- on their left/right side.

  -- look left
  local lArc = previous
  while lArc.circleEvent and abs_fn(x-lArc.circleEvent[1])<1e-9 and abs_fn(y-lArc.circleEvent.ycenter)<1e-9 do
    previous = lArc.rbPrevious
    table.insert(disappearingTransitions, 1, lArc)
    self:detachBeachsection(lArc) -- mark for reuse
    lArc = previous
  end
  -- even though it is not disappearing, I will also add the beach section
  -- immediately to the left of the left-most collapsed beach section, for
  -- convenience, since we need to refer to it later as this beach section
  -- is the 'left' site of an edge for which a start point is set.
  table.insert(disappearingTransitions, 1, lArc)
  self:detachCircleEvent(lArc)

  -- look right
  local rArc = next
  while rArc.circleEvent and abs_fn(x-rArc.circleEvent[1])<1e-9 and abs_fn(y-rArc.circleEvent.ycenter)<1e-9 do
    next = rArc.rbNext
    push(disappearingTransitions, rArc)
    self:detachBeachsection(rArc) -- mark for reuse
    rArc = next
  end
  -- we also have to add the beach section immediately to the right of the
  -- right-most collapsed beach section, since there is also a disappearing
  -- transition representing an edge's start point on its left.
  push(disappearingTransitions, rArc)
  self:detachCircleEvent(rArc)

  -- walk through all the disappearing transitions between beach sections and
  -- set the start point of their (implied) edge.
  for iArc = 2, #disappearingTransitions do
    rArc = disappearingTransitions[iArc]
    lArc = disappearingTransitions[iArc-1]

    self:setEdgeStartpoint(rArc.edge, lArc.site, rArc.site, vertex)
  end

  -- create a new edge as we have now a new transition between
  -- two beach sections which were previously not adjacent.
  -- since this edge appears as a new vertex is defined, the vertex
  -- actually define an end point of the edge (relative to the site
  -- on the left)
  lArc = disappearingTransitions[1]
  rArc = disappearingTransitions[#disappearingTransitions]
  rArc.edge = self:createEdge(lArc.site, rArc.site, nil, vertex)

  -- create circle events if any for beach sections left in the beachline
  -- adjacent to collapsed sections
  self:attachCircleEvent(lArc)
  self:attachCircleEvent(rArc)
end

function Voronoi:addBeachsection( site )
  local x = site[1]
  local directrix = site[2]

  -- find the left and right beach sections which will surround the newly
  -- created beach section.
  -- rhill 2011-06-01: This loop is one of the most often executed,
  -- hence we expand in-place the comparison-against-epsilon calls.
  local lArc, rArc
  local dxl, dxr
  local node = self.beachline.root

  while node do
    dxl = self:leftBreakPoint(node,directrix)-x
    -- x lessThanWithEpsilon xl => falls somewhere before the left edge of the beachsection
    if dxl > 1e-9 then
      -- this case should never happen
      -- if (!node.rbLeft) {
      --  rArc = node.rbLeft;
      --  break;
      --  }
      node = node.rbLeft
    else
      dxr = x-self:rightBreakPoint(node,directrix)
      -- x greaterThanWithEpsilon xr => falls somewhere after the right edge of the beachsection
      if dxr > 1e-9 then
        if not node.rbRight then
          lArc = node
          break
        end
        node = node.rbRight
      else
        -- x equalWithEpsilon xl => falls exactly on the left edge of the beachsection
        if dxl > -1e-9 then
          lArc = node.rbPrevious
          rArc = node
        -- x equalWithEpsilon xr => falls exactly on the right edge of the beachsection
        elseif dxr > -1e-9 then
          lArc = node
          rArc = node.rbNext
        -- falls exactly somewhere in the middle of the beachsection
        else
          lArc, rArc = node, node
        end
        break
      end
    end
  end
  -- at this point, keep in mind that lArc and/or rArc could be
  -- undefined or null. [DCS] in lua undefined or nil means nil.

  -- create a new beach section object for the site and add it to RB-tree
  local newArc = self:createBeachsection(site)
  self.beachline:rbInsertSuccessor(lArc, newArc)

  -- cases:
  --

  -- [null,null]
  -- least likely case: new beach section is the first beach section on the
  -- beachline.
  -- This case means:
  --   no new transition appears
  --   no collapsing beach section
  --   new beachsection become root of the RB-tree
  if not lArc and not rArc then
    return
  end

  -- [lArc,rArc] where lArc == rArc
  -- most likely case: new beach section split an existing beach
  -- section.
  -- This case means:
  --   one new transition appears
  --   the left and right beach section might be collapsing as a result
  --   two new nodes added to the RB-tree
  if lArc == rArc then
    -- invalidate circle event of split beach section
    self:detachCircleEvent(lArc)

    -- split the beach section into two separate beach sections
    rArc = self:createBeachsection(lArc.site)
    self.beachline:rbInsertSuccessor(newArc, rArc)

    -- since we have a new transition between two beach sections,
    -- a new edge is born
    local edge = self:createEdge(lArc.site, newArc.site)
    newArc.edge, rArc.edge = edge, edge

    -- check whether the left and right beach sections are collapsing
    -- and if so create circle events, to be notified when the point of
    -- collapse is reached.
    self:attachCircleEvent(lArc)
    self:attachCircleEvent(rArc)
    return
  end

  -- [lArc,null]
  -- even less likely case: new beach section is the *last* beach section
  -- on the beachline -- this can happen *only* if *all* the previous beach
  -- sections currently on the beachline share the same y value as
  -- the new beach section.
  -- This case means:
  --   one new transition appears
  --   no collapsing beach section as a result
  --   new beach section become right-most node of the RB-tree
  if lArc and not rArc then
    newArc.edge = self:createEdge(lArc.site,newArc.site)
    return
  end

  -- [null,rArc]
  -- impossible case: because sites are strictly processed from top to bottom,
  -- and left to right, which guarantees that there will always be a beach section
  -- on the left -- except of course when there are no beach section at all on
  -- the beach line, which case was handled above.
  -- rhill 2011-06-02: No point testing in non-debug version
  --if (!lArc && rArc) {
  --  throw "Voronoi.addBeachsection(): What is this I don't even";
  --  }

  -- [lArc,rArc] where lArc != rArc
  -- somewhat less likely case: new beach section falls *exactly* in between two
  -- existing beach sections
  -- This case means:
  --   one transition disappears
  --   two new transitions appear
  --   the left and right beach section might be collapsing as a result
  --   only one new node added to the RB-tree
  if lArc ~= rArc then
    -- invalidate circle events of left and right sites
    self:detachCircleEvent(lArc)
    self:detachCircleEvent(rArc)

    -- an existing transition disappears, meaning a vertex is defined at
    -- the disappearance point.
    -- since the disappearance is caused by the new beachsection, the
    -- vertex is at the center of the circumscribed circle of the left,
    -- new and right beachsections.
    -- http://mathforum.org/library/drmath/view/55002.html
    -- Except that I bring the origin at A to simplify
    -- calculation
    local lSite = lArc.site
    local ax = lSite[1]
    local ay = lSite[2]
    local bx=site[1]-ax
    local by=site[2]-ay
    local rSite = rArc.site
    local cx=rSite[1]-ax
    local cy=rSite[2]-ay
    local d=2*(bx*cy-by*cx)
    local hb=bx*bx+by*by
    local hc=cx*cx+cy*cy
    local vertex = Vertex((cy*hb-by*hc)/d+ax, (bx*hc-cx*hb)/d+ay)

    -- one transition disappear
    self:setEdgeStartpoint(rArc.edge, lSite, rSite, vertex)

    -- two new transitions appear at the new vertex location
    newArc.edge = self:createEdge(lSite, site, nil, vertex)
    rArc.edge = self:createEdge(site, rSite, nil, vertex)

    -- check whether the left and right beach sections are collapsing
    -- and if so create circle events, to handle the point of collapse.
    self:attachCircleEvent(lArc)
    self:attachCircleEvent(rArc)
    return
  end
end

-- local old = Voronoi.addBeachsection

-- function checkEdge( node )
--  assert(node.edge)

--  if node.rbLeft then
--    checkEdge(node.rbLeft)
--  end

--  if node.rbRight then
--    checkEdge(node.rbRight)
--  end
-- end

-- Voronoi.addBeachsection = function ( self, site )
--  local root = self.beachline.root
--  if root then
--    checkEdge(root)
--  end

--  old(self, site)

--  if root then
--    checkEdge(root)
--  end
-- end



-- ---------------------------------------------------------------------------
-- Circle event methods

-- rhill 2011-06-07: For some reasons, performance suffers significantly
-- when instanciating a literal object instead of an empty ctor
-- [DCS] It's because of garbage collector load, something that is present in
--     Lua as well so keep the optimisation.
function CircleEvent()
  return {
    arc = nil,
    site = nil,
    x = nil,
    y = nil,
    ycenter = nil,

    -- [DCS] Used as an RBTree node so declare the fields here so the
    --     memory is allocated for them.
    rbPrevious = nil,
    rbNext = nil,
    rbRight = nil,
    rbLeft = nil,
    rbLeft = nil,
    rbParent = nil,
    rbRed = nil,
  }
end

function Voronoi:attachCircleEvent( arc )
  local lArc = arc.rbPrevious
  local rArc = arc.rbNext
  if not lArc or not rArc then
    return -- does that ever happen?
  end
  local lSite = lArc.site
  local cSite = arc.site
  local rSite = rArc.site

  -- If site of left beachsection is same as site of
  -- right beachsection, there can't be convergence
  if lSite == rSite then
    return
  end

  -- Find the circumscribed circle for the three sites associated
  -- with the beachsection triplet.
  -- rhill 2011-05-26: It is more efficient to calculate in-place
  -- rather than getting the resulting circumscribed circle from an
  -- object returned by calling Voronoi.circumcircle()
  -- http://mathforum.org/library/drmath/view/55002.html
  -- Except that I bring the origin at cSite to simplify calculations.
  -- The bottom-most part of the circumcircle is our Fortune 'circle
  -- event', and its center is a vertex potentially part of the final
  -- Voronoi diagram.
  local bx = cSite[1]
  local by = cSite[2]
  local ax = lSite[1]-bx
  local ay = lSite[2]-by
  local cx = rSite[1]-bx
  local cy = rSite[2]-by

  -- If points l->c->r are clockwise, then center beach section does not
  -- collapse, hence it can't end up as a vertex (we reuse 'd' here, which
  -- sign is reverse of the orientation, hence we reverse the test.
  -- http://en.wikipedia.org/wiki/Curve_orientation#Orientation_of_a_simple_polygon
  -- rhill 2011-05-21: Nasty finite precision error which caused circumcircle() to
  -- return infinites: 1e-12 seems to fix the problem.
  local d = 2*(ax*cy-ay*cx)
  if d >= -2e-12 then
    return
  end

  local ha = ax*ax+ay*ay
  local hc = cx*cx+cy*cy
  local x = (cy*ha-ay*hc)/d
  local y = (ax*hc-cx*ha)/d
  local ycenter = y+by

  -- Important: ybottom should always be under or at sweep, so no need
  -- to waste CPU cycles by checking

  -- recycle circle event object if possible
  local circleEvent = pop(self.circleEventJunkyard)
  if not circleEvent then
    circleEvent = CircleEvent()
  end
  circleEvent.arc = arc
  circleEvent.site = cSite
  circleEvent[1] = x+bx
  circleEvent[2] = ycenter+math.sqrt(x*x+y*y) -- y bottom
  circleEvent.ycenter = ycenter
  arc.circleEvent = circleEvent

  -- find insertion point in RB-tree: circle events are ordered from
  -- smallest to largest
  local predecessor = nil
  local node = self.circleEvents.root
  while node do
    if circleEvent[2] < node[2] or (circleEvent[2] == node[2] and circleEvent[1] <= node[1]) then
      if node.rbLeft then
        node = node.rbLeft
      else
        predecessor = node.rbPrevious
        break
      end
    else
      if node.rbRight then
        node = node.rbRight
      else
        predecessor = node
        break
      end
    end
  end
  self.circleEvents:rbInsertSuccessor(predecessor, circleEvent)
  if not predecessor then
    self.firstCircleEvent = circleEvent
  end
end

function Voronoi:detachCircleEvent( arc )
  local circle = arc.circleEvent
  if circle then
    if not circle.rbPrevious then
      self.firstCircleEvent = circle.rbNext
    end
    self.circleEvents:rbRemoveNode(circle) -- remove from RB-tree
    push(self.circleEventJunkyard, circle)
    arc.circleEvent = nil
  end
end


-- ---------------------------------------------------------------------------
-- Diagram completion methods

-- connect dangling edges (not if a cursory test tells us
-- it is not going to be visible.
-- return value:
--   false: the dangling endpoint couldn't be connected
--   true: the dangling endpoint could be connected
function Voronoi:connectEdge( edge, bbox )
  -- skip if end point already connected
  local vb = edge.vb
  if vb then
    return true
  end

  -- make local copy for performance purpose
  local va = edge.va
  local xl = bbox.xl
  local xr = bbox.xr
  local yt = bbox.yt
  local yb = bbox.yb
  local lSite = edge.lSite
  local rSite = edge.rSite

  local lx = lSite[1]
  local ly = lSite[2]
  local rx = rSite[1]
  local ry = rSite[2]
  local fx = (lx+rx)/2
  local fy = (ly+ry)/2
  local fm, fb

  -- get the line equation of the bisector if line is not vertical
  if ry ~= ly then
    fm = (lx-rx)/(ry-ly)
    fb = fy-fm*fx
  end

  -- remember, direction of line (relative to left site):
  -- upward: left[1] < right[1]
  -- downward: left[1] > right[1]
  -- horizontal: left[1] == right[1]
  -- upward: left[1] < right[1]
  -- rightward: left[2] < right[2]
  -- leftward: left[2] > right[2]
  -- vertical: left[2] == right[2]

  -- depending on the direction, find the best side of the
  -- bounding box to use to determine a reasonable start point

  -- special case: vertical line
  if fm == nil then
    -- doesn't intersect with viewport
    if fx < xl or fx >= xr then
      return false
    end
    -- downward
    if lx > rx then
      if not va then
        va = Vertex(fx, yt)
      elseif va[2] >= yb then
        return false
      end
      vb = Vertex(fx, yb)
    -- upward
    else
      if not va then
        va = Vertex(fx, yb)
      elseif va[2] < yt then
        return false
      end
      vb = Vertex(fx, yt)
    end
  -- closer to vertical than horizontal, connect start point to the
  -- top or bottom side of the bounding box
  elseif fm < -1 or fm > 1 then
    -- downward
    if lx > rx then
      if not va then
        va = Vertex((yt-fb)/fm, yt)
      elseif va[2] >= yb then
        return false
      end
      vb = Vertex((yb-fb)/fm, yb)
    -- upward
    else
      if not va then
        va = Vertex((yb-fb)/fm, yb)
      elseif va[2] < yt then
        return false
      end
      vb = Vertex((yt-fb)/fm, yt)
    end
  -- closer to horizontal than vertical, connect start point to the
  -- left or right side of the bounding box
  else
    -- rightward
    if ly < ry then
      if not va then
        va = Vertex(xl, fm*xl+fb)
      elseif va[1] >= xr then
        return false
      end
      vb = Vertex(xr, fm*xr+fb)
    -- leftward
    else
      if not va then
        va = Vertex(xr, fm*xr+fb)
      elseif va[1] < xl then
        return false
      end
      vb = Vertex(xl, fm*xl+fb)
    end
  end
  edge.va = va
  edge.vb = vb
  return true
end

-- line-clipping code taken from:
--   Liang-Barsky function by Daniel White
--   http://www.skytopia.com/project/articles/compsci/clipping.html
-- Thanks!
-- A bit modified to minimize code paths
function Voronoi:clipEdge( edge, bbox )
  local ax = edge.va[1]
  local ay = edge.va[2]
  local bx = edge.vb[1]
  local by = edge.vb[2]
  local t0 = 0
  local t1 = 1
  local dx = bx-ax
  local dy = by-ay
  -- left
  local q = ax-bbox.xl
  if dx == 0 and q<0 then
    return false
  end
  local r = -q/dx
  if dx<0 then
    if r<t0 then
      return false
    elseif r<t1 then
      t1=r
    end
  elseif dx>0 then
    if r>t1 then
      return false
    elseif r>t0 then
      t0=r
    end
  end
  -- right
  q = bbox.xr-ax
  if dx == 0 and q<0 then
    return false
  end
  r = q/dx
  if dx<0 then
    if r>t1 then
      return false
    elseif r>t0 then
      t0=r
    end
  elseif dx>0 then
    if r<t0 then
      return false
    elseif r<t1 then
      t1=r
    end
  end
  -- top
  q = ay-bbox.yt
  if dy == 0 and q<0 then
    return false
  end
  r = -q/dy
  if dy<0 then
    if r<t0 then
      return false
    elseif r<t1 then
      t1=r
    end
  elseif dy>0 then
    if r>t1 then
      return false
    elseif r>t0 then
      t0=r
    end
  end
  -- bottom     
  q = bbox.yb-ay
  if dy == 0 and q<0 then
    return false
  end
  r = q/dy
  if dy<0 then
    if r>t1 then
      return false
    elseif r>t0 then
      t0=r
    end
  elseif dy>0 then
    if r<t0 then
      return false
    elseif r<t1 then
      t1=r
    end
  end

  -- if we reach this point, Voronoi edge is within bbox

  -- if t0 > 0, va needs to change
  -- rhill 2011-06-03: we need to create a new vertex rather
  -- than modifying the existing one, since the existing
  -- one is likely shared with at least another edge
  if t0 > 0 then
    edge.va = Vertex(ax+t0*dx, ay+t0*dy)
  end

  -- if t1 < 1, vb needs to change
  -- rhill 2011-06-03: we need to create a new vertex rather
  -- than modifying the existing one, since the existing
  -- one is likely shared with at least another edge
  if t1 < 1 then
    edge.vb = Vertex(ax+t1*dx, ay+t1*dy)
  end

  return true
end

-- Connect/cut edges at bounding box
function Voronoi:clipEdges( bbox )
  -- connect all dangling edges to bounding box
  -- or get rid of them if it can't be done
  local edges = self.edges
  local abs_fn = math.abs

  -- iterate backward so we can splice safely
  for iEdge = #edges, 1, -1 do
    local edge = edges[iEdge]
    -- edge is removed if:
    --   it is wholly outside the bounding box
    --   it is actually a point rather than a line
    if not self:connectEdge(edge, bbox) or
      not self:clipEdge(edge, bbox) or
      (abs_fn(edge.va[1]-edge.vb[1])<1e-9 and abs_fn(edge.va[2]-edge.vb[2])<1e-9) then 
      edge.va, edge.vb = nil
      table.remove(edges, iEdge)
    end
  end
end

-- Close the cells.
-- The cells are bound by the supplied bounding box.
-- Each cell refers to its associated site, and a list
-- of halfedges ordered counterclockwise.
function Voronoi:closeCells( bbox )
  -- prune, order halfedges, then add missing ones
  -- required to close cells
  local xl = bbox.xl
  local xr = bbox.xr
  local yt = bbox.yt
  local yb = bbox.yb
  local cells = self.cells
  local abs_fn = math.abs

  for iCell = 1, #cells do
    local cell = cells[iCell]
    -- trim non fully-defined halfedges and sort them counterclockwise
    if cell:prepare() then
      -- close open cells
      -- step 1: find first 'unclosed' point, if any.
      -- an 'unclosed' point will be the end point of a halfedge which
      -- does not match the start point of the following halfedge
      local halfedges = cell.halfedges
      local numHalfedges = #halfedges
      -- special case: only one site, in which case, the viewport is the cell
      -- ...
      -- all other cases
      local iLeft = 1
      while iLeft <= numHalfedges do
        local iRight = iLeft+1
        if iRight > numHalfedges then
          iRight = 1
        end
        local endpoint = halfedges[iLeft]:getEndpoint()
        local startpoint = halfedges[iRight]:getStartpoint()
        -- if end point is not equal to start point, we need to add the missing
        -- halfedge(s) to close the cell
        if abs_fn(endpoint[1]-startpoint[1])>=1e-9 or abs_fn(endpoint[2]-startpoint[2])>=1e-9 then
          -- if we reach this point, cell needs to be closed by walking
          -- counterclockwise along the bounding box until it connects
          -- to next halfedge in the list
          local va = endpoint
          local vb
          -- walk downward along left side
          if equalWithEpsilon(endpoint[1],xl) and lessThanWithEpsilon(endpoint[2],yb) then
            vb = Vertex(xl, equalWithEpsilon(startpoint[1],xl) and startpoint[2] or yb)
          -- walk rightward along bottom side
          elseif equalWithEpsilon(endpoint[2],yb) and lessThanWithEpsilon(endpoint[1],xr) then
            vb = Vertex(equalWithEpsilon(startpoint[2],yb) and startpoint[1] or xr, yb)
          -- walk upward along right side
          elseif equalWithEpsilon(endpoint[1],xr) and greaterThanWithEpsilon(endpoint[2],yt) then
            vb = Vertex(xr, equalWithEpsilon(startpoint[1],xr) and startpoint[2] or yt)
          -- walk leftward along top side
          elseif equalWithEpsilon(endpoint[2],yt) and greaterThanWithEpsilon(endpoint[1],xl) then
            vb = Vertex(equalWithEpsilon(startpoint[2],yt) and startpoint[1] or xl, yt)
          end
          local edge = self:createBorderEdge(cell.site, va, vb)
          table.insert(halfedges, iLeft+1, Halfedge:new(edge, cell.site, nil))
          numHalfedges = numHalfedges + 1
        end
        iLeft = iLeft + 1
      end
    end
  end
end
-- DONE

-- ---------------------------------------------------------------------------
-- Top-level Fortune loop

-- rhill 2011-05-19:
--   Voronoi sites are kept client-side now, to allow
--   user to freely modify content. At compute time,
--   *references* to sites are copied locally.
function Voronoi:recompute(sites, width, height)
		width = width or 1
  height = height or width
  local bbox = {
    xl = -width / 2,
    xr = width / 2, 
    yt = -height / 2 - 1, 
    yb =  height / 2 - 1
  }

  -- to measure execution time
  -- [DCS] Lua doesn't have standard high accuracy timer :^(
  -- var startTime = new Date();

  -- init internal state
  self:reset()

  -- Initialize site event queue
  local siteEvents = {}
  for i = 1, #sites do
    siteEvents[i] = {sites[i][1], sites[i][2]}
  end

  -- [DCS] the sort is the opposite of what you'd think because the site
  --     events are processed from the end.
  table.sort(siteEvents,
    function( a, b )
      if a[2] == b[2] then
        return a[1] > b[1]
      end
      return a[2] > b[2]
    end)


  -- process queue
  local site = pop(siteEvents)
  local siteid = 1
  local xsitex = -math.huge -- to avoid duplicate sites
  local xsitey = -math.huge
  local cells = self.cells

  -- main loop
  while true do
    -- we need to figure whether we handle a site or circle event
    -- for this we find out if there is a site event and it is
    -- 'earlier' than the circle event
    local circle = self.firstCircleEvent

    -- add beach section
    if site and (not circle or site[2] < circle[2] or (site[2] == circle[2] and site[1] < circle[1])) then
      -- only if site is not a duplicate
      if site[1] ~= xsitex or site[2] ~= xsitey then
        -- first create cell for new site
        cells[siteid] = Cell:new(site)
        site.voronoiId = siteid
        siteid = siteid + 1
        -- then create a beachsection for that site
        self:addBeachsection(site)
        -- remember last site coords to detect duplicate
        xsitey = site[2]
        xsitex = site[1]
      end
      site = pop(siteEvents)
    -- remove beach section
    elseif circle then
      self:removeBeachsection(circle.arc)
    -- all done, quit
    else
      break
    end
  end

  -- wrapping-up:
  --   connect dangling edges to bounding box
  --   cut edges as per bounding box
  --   discard edges completely outside bounding box
  --   discard edges which are point-like
  self:clipEdges(bbox)

  --   add missing edges in order to close opened cells
  self:closeCells(bbox)

  -- to measure execution time
  -- [DCS] Lua doesn't have standard high accuracy timer :^(
  -- var stopTime = new Date();

  -- prepare return values
  local diagram = {
    cells = self.cells,
    edges = self.edges,
    -- [DCS] Lua doesn't have standard high accuracy timer :^(
    -- diagram.execTime = stopTime.getTime()-startTime.getTime()
  }

  -- clean up
  self:reset()
  self.cells = diagram.cells
  self.edges = diagram.edges
  return diagram
end

function Voronoi.compute(sites, width, height)
  return Voronoi:new():recompute(sites, width, height)
end
  
return Voronoi
