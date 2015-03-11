module TDisplay where

import List ((::))
import List
import Signal (Signal, (<~), (~))
import Signal
import Color
import Window
import Text
import String
import Mouse
import Graphics.Input (button, checkbox)
import Graphics.Input.Field as F
import Graphics.Element as E
import Trampoline (..)
import Time (..)

import Math.Vector2 (..)
import Math.Vector3 (..)
import WebGL (..)

import Terrain as T

type Msg = Reset Int | TransType T.Transform | TransParams (Float,Float) | Smooth Bool

type Event = B Msg | M (Int,Int)

--Morph transform, morph (radius,magnitude), world
type alias State = { mType : T.Transform
                   , mParams : (Float, Float)
                   , cache : List (Triangle Vertex)
                   , world : T.World
                   , smooth : Bool }

{- We could store as Vec3 RGB values, but this way
   we can use the built-in colors -}
type alias ColorMap = List (Float, Color.Color)

type alias Vertex =
  { color : Vec3
  , position : Vec2
  }

ch : Signal.Channel Msg
ch = Signal.channel (Reset initSize)

radiusField : Signal.Channel F.Content
radiusField = Signal.channel F.noContent

magField : Signal.Channel F.Content
magField = Signal.channel F.noContent

sizeField : Signal.Channel F.Content
sizeField = Signal.channel F.noContent

check : Signal.Channel Bool
check = Signal.channel False

initSize = 50
initState = { mType = T.Gauss
            , mParams = (8,3)
            , cache = cells initWorld False
            , world = initWorld
            , smooth = False }

initWorld = T.initialize initSize

initCMap = [(0, Color.blue), (1, Color.lightBrown),
            (15, Color.darkGreen), (30, Color.lightGreen), (40, Color.white)]

cellDist : (Int,Int) -> T.World -> Float
cellDist (w,h) world = toFloat (min w h) / toFloat (T.size world)

-- Assumes that our color map is sorted in increasing data order
getColor : ColorMap -> Float -> Vec3
getColor cmap data =
  let colorVec c =
    let cc = Color.toRgb c
    in vec3 (toFloat cc.red / 255) (toFloat cc.green / 255) (toFloat cc.blue / 255)
  in
  case cmap of
    [] -> vec3 0 0 0 --Return black so we don't have to deal with option values
    [(_,c)] -> colorVec c
    (d1,c1) :: (d2,c2) :: cs ->
      if | data <= d1 -> colorVec c1
         | data > d1 && data < d2 -> cLerp data (d1,c1) (d2,c2)
         | otherwise -> getColor ((d2,c2) :: cs) data

-- Linear interpolation between colors (yay scivis!)
cLerp : Float -> (Float, Color.Color) -> (Float, Color.Color) -> Vec3
cLerp data (d1,c1) (d2,c2) =
  let rgb1 = Color.toRgb c1
      rgb2 = Color.toRgb c2
      r = lerp data (d1, toFloat rgb1.red) (d2, toFloat rgb2.red)
      g = lerp data (d1, toFloat rgb1.green) (d2, toFloat rgb2.green)
      b = lerp data (d1, toFloat rgb1.blue) (d2, toFloat rgb2.blue)
  in vec3 (r / 255) (g / 255) (b / 255)

lerp : Float -> (Float, Float) -> (Float, Float) -> Float
lerp x (x0,y0) (x1,y1) =
  y0 + (y1 - y0) * (x - x0) / (x1 - x0)

-- Convert x,y click position to i,j index positions
-- Takes w,h dimensions and a world object (for length)
mouseToWorld : (Int,Int) -> (Int,Int) -> T.World -> (Int,Int)
mouseToWorld (w,h) (x,y) world =
  let x' = (toFloat x - toFloat w / 2) / (cellDist (w,h) world)
      y' = (toFloat (h-y) - toFloat h / 2) / (cellDist (w,h) world)
      l = T.size world
  in (round <| x' + toFloat l / 2, round <| y' + toFloat l / 2)

cells : T.World -> Bool -> List (Triangle Vertex)
cells world smooth =
  cells_tr world (0,0) [] smooth |> trampoline

cells_tr : T.World -> (Int,Int) -> List (List (Triangle Vertex)) -> Bool
           -> Trampoline (List (Triangle Vertex))
cells_tr world (i,j) acc smooth =
  let cf = if smooth then cellInterp else cell
      c = cf world (i,j)
      l = T.size world
  in
  if | j < l - 1 -> Continue (\() -> cells_tr world (i,j+1) (c::acc) smooth)
     | i < l - 1 -> Continue (\() -> cells_tr world (i+1,0) (c::acc) smooth)
     | otherwise -> Done (List.concat <| c::acc)

-- A cell is the smallest unit of data in our world (a single point of data,
-- represented by a square of pixels - i.e. two triangles)
cell : T.World -> (Int,Int) -> List (Triangle Vertex)
cell world (i,j) =
  let color = T.get world (i,j) |> getColor initCMap
      l = T.size world
      vertex position = Vertex color position
      shift = toFloat l / 2
      x = toFloat i - shift
      y = toFloat j - shift
      a = vec2 x y
      b = vec2 (x+1) y
      c = vec2 (x+1) (y+1)
      d = vec2 x (y+1)
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]

getIf : T.World -> (Int,Int) -> (Int,Int) -> Float
getIf world def (chki,chkj) =
  let l = T.size world
  in if chki < 0 || chki >= l || chkj < 0 || chkj >= l
     then T.get world def
     else T.get world (chki,chkj)

cellTriangles : T.World -> (Int,Int) -> List (Int,Int) -> (Int,Int)
                -> List (Triangle Vertex)
cellTriangles world (ci,cj) coords fst =
  let vertex position =
        Vertex (getIf world (ci,cj) position |> getColor initCMap)
               (toWorld position)
      l = T.size world
      fci = toFloat ci
      fcj = toFloat cj
      toWorld (i,j) = vec2 (0.5+toFloat i - toFloat l / 2)
                           (0.5+toFloat j - toFloat l / 2)
  in case coords of
    [] -> []
    p1::p2::rest -> (vertex p1, vertex p2, vertex (ci,cj))
                      :: cellTriangles world (ci,cj) (p2::rest) fst
    [p] -> [(vertex p, vertex fst, vertex (ci,cj))]

cellInterp : T.World -> (Int,Int) -> List (Triangle Vertex)
cellInterp world (i,j) =
  let coords = [(i-1,j+1),(i,j+1),(i+1,j+1)
               ,          (i+1,j)
               ,(i+1,j-1),(i,j-1),(i-1,j-1)
               ,          (i-1,j)
               ]
  in cellTriangles world (i,j) coords (List.head coords)

scene : (Int,Int) -> State -> List Entity
scene (w,h) state =
  let uniforms = { dimensions = vec2 (toFloat w) (toFloat h)
                 , size = T.size state.world
                 }
  in [ entity vertexShader fragmentShader state.cache uniforms ]

sceneDims : (Int,Int) -> (Int,Int)
sceneDims (w,h) =
  let sideMin = min (toFloat w) (toFloat h)
      side = min sideMin (toFloat w * 0.9) |> floor
  in (side, side)

view : (Int,Int) -> State -> (F.Content, F.Content) -> F.Content -> E.Element
view (w,h) state (rfc, mfc) sfc =
  let (w',h') = sceneDims (w,h)
      (mpr,mpm) = state.mParams
  in [webgl (w',h') (scene (w',h') state)
     , E.spacer 10 1
     , List.intersperse (E.spacer 1 3)
       [ E.spacer 1 10
       , button (Signal.send ch (Reset
                                (getIntSize (T.size state.world) sfc.string))) "Reset"
       , F.field F.defaultStyle (Signal.send sizeField) "World Size" sfc
       , E.spacer 1 50
       , button (Signal.send ch (TransType T.Gauss)) "Gaussian Brush"
       , button (Signal.send ch (TransType T.Acc)) "Additive Brush"
       , button (Signal.send ch (TransType T.Exact)) "Exact Brush"
       , E.spacer 1 50
       , button (Signal.send ch (TransParams
                                (getFloatParam mpr rfc.string,
                                 getFloatParam mpm mfc.string))) "Update Brush"
       , F.field F.defaultStyle (Signal.send radiusField) "Radius" rfc
       , F.field F.defaultStyle (Signal.send magField) "Magnitude" mfc
       , E.spacer 1 50
       , [ checkbox (\b -> Signal.send ch (Smooth b)) state.smooth
         , Text.fromString " Smoothing" |> Text.leftAligned
         ] |> E.flow E.right
       , E.spacer 1 50
       , Text.fromString (settings state) |> Text.leftAligned
       ] |> E.flow E.down ]
     |> E.flow E.right

settings : State -> String
settings state =
  let mtStr = case state.mType of
        T.Gauss -> "Gaussian Brush\n"
        T.Acc   -> "Additive Brush\n"
        T.Exact -> "Exact Brush\n"
      (mpr,mpm) = state.mParams
      mpStr = "Radius: " ++ (toString mpr) ++ "\n" ++
              "Magnitude: " ++ (toString mpm) ++ "\n"
  in "Brush Settings:\n\n" ++ mtStr ++ mpStr

getIntSize : Int -> String -> Int
getIntSize def str =
  case String.toInt str of
    Ok i -> i
    Err _ -> def

getFloatParam : Float -> String -> Float
getFloatParam def str =
  case String.toFloat str of
    Ok f -> f
    Err _ -> def

upstate : ((Int,Int), Event) -> State -> State
upstate ((w,h), event) state =
  case event of
  B (Reset s) ->
    let world = T.initialize s
    in { state | world <- world
               , cache <- cells world state.smooth }
  B (TransType t) -> { state | mType <- t }
  B (TransParams mp) -> { state | mParams <- mp }
  B (Smooth b) -> { state | smooth <- b
                          , cache <- cells state.world b }
  M (x,y) ->
    let dims = sceneDims (w,h)
        (x',y') = mouseToWorld dims (x,y) state.world
        world = T.morph (state.mType) (x',y') (state.mParams) (state.world)
    in
      { state | world <- world
              , cache <- cells world state.smooth }
  _ -> state

state : Signal State
state = Signal.foldp upstate initState
          (Signal.merge
            (Signal.sampleOn
              (Signal.keepWhen Mouse.isDown 0 (every (second/10)))
              (Signal.map2 (\x y -> (x, M y)) Window.dimensions Mouse.position))
            (Signal.map2 (\x y -> (x, B y)) Window.dimensions (Signal.subscribe ch)))

main =
  view <~ Window.dimensions
        ~ state
        ~ Signal.map2 (\x y -> (x, y))
            (Signal.subscribe radiusField) (Signal.subscribe magField)
        ~ Signal.subscribe sizeField

-- SHADERS

vertexShader : Shader { attr | position: Vec2, color: Vec3 }
                      { unif | dimensions: Vec2, size: Int }
                      { vcolor: Vec3, vposition: Vec2 }
vertexShader = [glsl|

attribute vec2 position;
attribute vec3 color;
uniform vec2 dimensions;
uniform int size;
varying vec3 vcolor;
varying vec2 vposition;

void main() {
  gl_Position = vec4(position[0] * 2.0 * (dimensions[1]/dimensions[0]) / float(size),
                     position[1] * 2.0 / float(size),
                     0.0,
                     1.0);
  vcolor = color;
  vposition = position;
}

|]

fragmentShader : Shader {} { unif | dimensions: Vec2 } { vcolor: Vec3, vposition: Vec2 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;
varying vec2 vposition;

void main() {
  gl_FragColor = vec4(vcolor, 1.0);
}

|]
