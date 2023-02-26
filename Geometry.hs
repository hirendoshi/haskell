module Geometry
( sphereVolume
, sphereArea
--, cubeVolume
--, cubeArea
--, cuboidArea
--, cuboidVolume)
)where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4.0 * pi * (r ^ 2)
