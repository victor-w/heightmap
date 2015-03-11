module Terrain
  (default, initialize, get, set, morph, Transform (..), size, World) where

import Trampoline (..)

import Array as A

type World = W (A.Array (A.Array Float))

type alias MorphType = Float -> Float -> Float -> Float -> Float

type Transform = Gauss | Acc | Exact

-- Initialize with n = 20
default : World
default = initialize 20

-- Initialize to nxn flat terrain at elevation 0
initialize : Int -> World
initialize n =
  A.repeat n 0 |> A.repeat n |> W

-- Assumes world is square
size : World -> Int
size (W a) =
  A.length a

get : World -> (Int,Int) -> Float
get (W a) (i,j) =
  case A.get i a of
    Just a' -> case A.get j a' of
                 Just x -> x
                 Nothing -> 0
    Nothing -> 0

set : World -> (Int,Int) -> Float -> World
set (W a) (i,j) v =
  case A.get i a of
    Just a' -> A.set i (A.set j v a') a |> W
    Nothing -> W a

{-
-- Isocontours
-- This function returns a list of line segments
contours : World -> Float -> Float -> Int -> ((Float, Float), (Float, Float)) List
contours world start end num =
  

contour_tr : World -> Float -> ((Float, Float), (Float, Float)) List
contour_tr world val =
-}

-- Morph functions take a world, i,j world coordinates,
-- a "rad", and a value to determine magnitude
-- Also a morph function
morph : Transform -> (Int,Int) -> (Float, Float) -> World -> World
morph t (i,j) (rad,mag) w =
  let
    mf = case t of
      Gauss -> morphGauss
      Acc   -> morphAcc
      Exact -> morphExact
    l = size w
  in
  if i < 0 || i >= l || j < 0 || j >= l then w
  else morph_tr w (max (i - ceiling rad) 0, max (j - ceiling rad) 0) (i,j) rad mag mf
    |> trampoline

morph_tr : World -> (Int,Int) -> (Int, Int) -> Float -> Float -> MorphType
           -> Trampoline World
morph_tr w (i,j) (ci,cj) rad mag mf =
  let dist = (toFloat i - toFloat ci)^2 + (toFloat j - toFloat cj)^2 |> sqrt
      l = size w
      w' = set w (i,j) <| mf (get w (i,j)) dist rad mag
  in
  if | j < min (cj + ceiling rad) (l-1) ->
         Continue (\() -> morph_tr w' (i,j+1) (ci,cj) rad mag mf)
     | i < min (ci + ceiling rad) (l-1) ->
         Continue (\() -> morph_tr w' (i+1, max (cj - ceiling rad) 0) (ci,cj) rad mag mf)
     | otherwise -> Done w'

morphGauss : MorphType
morphGauss orig dist' rad mag =
  let dist = abs dist' in
  if dist < rad then orig + mag * e ^ (-(dist^2)/(2*rad^2/5)) else orig

morphAcc : MorphType
morphAcc orig dist rad mag =
  if dist < rad then mag + orig else orig

morphExact : MorphType
morphExact orig dist rad mag =
  if dist < rad then mag else orig
