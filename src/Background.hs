module Background where

import Graphics.Gloss

import World

drawBackground :: World -> Picture -> Picture -> [Picture]
drawBackground gs a1 a2 = [translate x y (qualAgua gs a1 a2) | x <- [-512, -456 .. 512], y <- [-384, -327 .. 384]]

qualAgua :: World -> Picture -> Picture -> Picture
qualAgua gs p1 p2 = if even (floor (timer gs)) then p1 else p2