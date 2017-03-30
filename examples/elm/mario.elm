-- http://elm-lang.org:1234/examples/mario

import Color exposing (..)
import MacCASOutreach.GraphicSVG exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) mario =
  mario
    |> gravity dt
    |> jump keys
    |> walk keys
    |> pipeCollider
    |> physics dt


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 && mario.vy == 0 then
      { mario | vy = 6.0 }

  else
      mario


gravity : Float -> Model -> Model
gravity dt mario =
  let
    xPipe = (between (-pipeWidth/2) (pipeWidth/2) (mario.x - 100)) 
    yPipe = (between -8 -2 (mario.y - pipeHeight)) 
  in
  { mario |
      vy = if xPipe && yPipe then 0 else if mario.y > 0 then max (mario.vy - dt/4) -3 else 0
  }



physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y = max 0 (mario.y + dt * mario.vy)
  }


between min max x = (x >= min && x <= max)

pipeWidth = 35
pipeHeight = 35

pipeCollider : Model -> Model
pipeCollider mario = let
    leftPipe = mario.y < (pipeHeight - 10) && (between (-pipeWidth/2) 0 (mario.x - 100))
    rightPipe = mario.y < (pipeHeight - 10) && (between 0 (pipeWidth/2) (mario.x - 100)) 
  in 
    if leftPipe then { mario | x = 100-(pipeWidth/2) } 
    else if rightPipe then { mario | x = 100 + (pipeWidth/2) }
    else mario

walk : Keys -> Model -> Model
walk keys mario =
  { mario |
      vx = toFloat keys.x,
      dir =
        if keys.x < 0 then
            Left

        else if keys.x > 0 then
            Right

        else
            mario.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if mario.y > 0 then
          "jump"

      else if mario.vx /= 0 then
          "walk"

      else
          "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "/imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"
   
    pipeImage = 
      image 35 35 "http://vignette1.wikia.nocookie.net/fantendo/images/b/b0/A_green_cartoon_pipe.png/revision/latest?cb=20150710175714"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , pipeImage
          |> toForm
          |> move (100, 65 - h/2)
      , marioImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)