-- HSProcessUsage
-- Copyright (C) 2010  Josh Stone
--
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.
--
--
-- Josh's program to visualize the activity of his CPUs.  I was
-- frustrated with top, htop, and Gnome's system-monitor.  For various
-- reasons, I just wanted a very simple, efficient, graphical
-- representation of my CPU activity.  The "top" variants are too busy
-- on the screen.  Gnome's system-monitor shows a lot more than just
-- CPUs, and it's not configurable enough.
--
-- This program simply parses /proc/stat every once in awhile and
-- shows a bar graph showing the percent utilization of each CPU
-- installed on the system.
--
-- In the future, I may want to break out the bar graph to show
-- different types of utilization (user, nice, system, etc.), but for
-- now it's just a simple visual that takes less than 2% of a CPU.
--
-- Author:  Josh Stone
-- Contact: josh@josho.org
-- Date:    2013-05-03
--
--

-- If you need the libraries: cabal install opengl glut

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.List
import Control.Concurrent
import Control.Monad

-- A simple data type that describes a CPU's state.  This may expand
-- in the future to break out the different fields.  I found that it
-- was much simpler to keep this data structure simplistic, though,
-- for now.

data CPU = CPU {
  cpuName   :: String, 
  cpuFields :: [Int]
  } deriving (Show, Eq)
                  
data CPUHistory = CPUHistory {
  cpuHist :: [[Rational]],
  cpuLen  :: Int
  } deriving (Show, Eq)
             
--
-- Maintain a history
--

resolveHistory (CPUHistory [] n) = CPUHistory [] n
resolveHistory (CPUHistory cs n)
  | length cs > n = CPUHistory (take n cs) n
  | otherwise     = CPUHistory cs n

pushHistory cpu (CPUHistory cs n) = 
  resolveHistory $ CPUHistory (cpu : cs) n
  
histLength (CPUHistory cs _) = length cs

-- The string for a CPU's information in /proc/stat looks like this:
--
--   cpu3 19613 106883 9903 9564061 3436 0 102 0 0 0
--

str2cpu s = CPU cpu fields
  where (cpu:fs) = words s
        fields   = map (\x -> read x :: Int) fs

-- The current jiffy counts for each relevant CPU field can be found
-- by reading in the following pseudo-file in /proc.  There's lots of
-- other information as well, but none is particularly relevant for my
-- current needs.

cpustat = do
  stat <- readFile "/proc/stat"
  let ls   = lines stat
      cpus = filter (isPrefixOf "cpu") ls
  return $ map str2cpu cpus

-- To get the utilization for a slice of time, we need to subtract the
-- values from two snapshots of the /proc/stat output.  This function
-- accomplishes this subtraction.

cpuSub a b = CPU (cpuName a) deltas
  where deltas = map (\(x,y) -> y - x) $ zip (cpuFields a) (cpuFields b)

-- Given two snapshots, we want to compare the movement in the jiffy
-- counts to arrive at a ratio representing the CPU's utilization.

cpuUse a b = 
  if total == 0 then 0 else (fromIntegral use) / (fromIntegral total)
  where delta = cpuSub a b 
        total = sum (map ((cpuFields delta) !!) [0,1,2,3])
        use   = sum (map ((cpuFields delta) !!) [0,1,2])

-- The effective "framerate" of the program is governed by the delay
-- in this function.  We take two snapshots of the /proc/stat output
-- and calculate the CPU utilization for each CPU installed in the
-- system.

cpuVal = do
  a <- cpustat
  seq a $ threadDelay 75000
  b <- cpustat
  let ps = map (\(x,y) -> cpuUse x y) $ zip a b
  return ps

-- When we're building a bar graph, we need to calculate the
-- x-coordinates for each of the bars.  Given the index of the current
-- bar and the total count of the bars, this function calculates these
-- two values.  I found it easier to break this out into a separate
-- function because of the ugliness of converting types.  *sigh*
-- Haskell is *usually* cool.

xCoords :: Int -> Int -> (GLfloat, GLfloat)
xCoords i c = (realToFrac x1 + 0.01, realToFrac x2 - 0.01)
  where i' = fromIntegral i :: Float
        c' = fromIntegral c :: Float
        dx = 1.0 / c'
        x1 = i' * dx
        x2 = i' * dx + dx

blueBar = (Color4 (0.3::GLfloat) 0.3 1 1.0,
           Color4 (0.3::GLfloat) 0.3 1 0.1)

greenBar per = (Color4 (0.3::GLfloat) 1 0.3 (0.25 * per),
                Color4 (0.3::GLfloat) 1 0.3 0.0)

-- This function builds the graph.  The coordinate system is measured
-- from (0, 1.0) from left to right and top to bottom, so we just draw
-- a series of rectangles.  It looks pretty when we do alpha blending,
-- etc.

processorGraph (top, bottom) ry = do
  let cpus   = reverse $ sort $ map (\x -> fromRational x :: GLfloat) $ tail ry
      count  = length cpus
      colors = [bottom, bottom, top, top]
  forM_ [0..count-1] $ \i -> do
    renderPrimitive Quads $ do
      let (x1,x2) = xCoords i count
          y       = cpus !! i
          pts     = [(x1,0), (x2,0), (x2,y), (x1,y)] :: [(GLfloat, GLfloat)]
      forM_ (zip colors pts) $ \(c,(a,b)) -> do
        color c
        vertex $ Vertex2 a b
        
-- The graph is much spiffier if we have a grid for comparing the bars
-- and a rectangular outline.  There might be some better primitives
-- for this in OpenGL, but I didn't want to spend extra time fiddling.

graphLines = do
  let linecolor = Color3 (0::GLfloat) 1 0
      zero = -0.001 :: GLfloat
      one  = 1.001  :: GLfloat      
      pts = [(zero, zero), (one,  zero),
             (zero, zero), (zero, one ),
             (one,  zero), (one,  one ),
             (zero, one),  (one,  one )]
  renderPrimitive Lines $ do
    color linecolor
    mapM_ (\(x,y) -> vertex $ Vertex2 x y) pts
    forM_ [1..4] $ \i -> do
      let i' = (realToFrac i) * 0.2
      vertex $ Vertex2 zero i'
      vertex $ Vertex2 one  i'

display hist = do
  clear [ ColorBuffer ]
  graphLines
  let frames = cpuHist hist
      flen   = length frames
      combo  = zip frames (reverse [1..flen])
  mapM_ (\(x,y) -> processorGraph (greenBar ((fromIntegral y) / (fromIntegral flen))) x) combo
  ry <- cpuVal
  let hist' = pushHistory ry hist
  processorGraph blueBar (head $ cpuHist hist')
  flush
  postRedisplay Nothing
  displayCallback $= display hist'

main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Process Usage"
  ortho (-0.1) 1.1 (-0.1) 1.1 (-1) 10
  let hist = CPUHistory [] 32
  displayCallback    $= display hist
  initialDisplayMode $= [RGBAMode]
  blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
  blend              $= Enabled
  clearColor         $= Color4 (0::GLfloat) 0 0 0 
  mainLoop

