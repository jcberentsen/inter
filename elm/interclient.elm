import Keyboard
import Window
import Debug
import Math.Vector2 as V
import Math.Vector2 (Vec2, vec2, add, scale)

-- MODEL

tau = 2*pi

type Rotor = Float
rotor : Float -> Rotor
rotor r = r

dir : Rotor -> Vec2
dir r = vec2 (0-sin r) (cos r)

type Ship =
    { acceleration : Vec2
    , velocity : Vec2
    , position : Vec2

    , alpha : Rotor -- angular acceleration
    , omega : Rotor
    , orientation : Rotor
    }

data Direction = Left | Right

type Keys = { x:Int, y:Int }

ship : Ship
ship =
    { acceleration = vec2 0 0
    , velocity = vec2 0 0
    , position = vec2 0 0

    , alpha = rotor 0 -- angular accel
    , omega = rotor 0 -- angular vel
    , orientation = rotor 0 -- angular pos
    }


-- UPDATE

step : (Float, Keys) -> Ship -> Ship
step (dt, keys) ship =
    ship
        |> gravity dt
        |> thrust keys
        |> impulse keys
        |> physics dt
        |> Debug.watch "ship"

thrust : Keys -> Ship -> Ship
thrust keys ship =
    if keys.y > 0 then { ship | acceleration <- V.scale 0.1 (dir ship.orientation) } else { ship | acceleration <- vec2 0.0 0.0 }

gravity : Float -> Ship -> Ship
gravity dt ship = ship

physics : Float -> Ship -> Ship
physics dt ship =
    { ship |
        velocity <- V.add ship.velocity (V.scale dt ship.acceleration),
        position <- V.add ship.position (V.scale dt ship.velocity),
        omega <- ship.omega + dt * ship.alpha,
        orientation <- ship.orientation + dt * ship.omega
    }

impulse : Keys -> Ship -> Ship
impulse keys ship =
    { ship |
        alpha <- (-0.001) * toFloat keys.x
    }

-- DISPLAY

display : (Int, Int) -> Ship -> Element
display (w',h') ship =
  let (w,h) = (toFloat w', toFloat h')
      src  = "images/placeholdership.jpeg"
      shipImage = image 35 35 src
  in
      collage w' h'
          [ rect w h
              |> filled background
          , shipImage
              |> toForm
              |> Debug.trace "ship"
              |> rotate (ship.orientation)
              |> move (V.getX ship.position, V.getY ship.position)
          ]

background = white
spaceblack = rgb 10 10 20

-- SIGNALS

main : Signal Element
main = lift2 display Window.dimensions (foldp step ship input)

input : Signal (Float, Keys)
input =
  let delta = lift (\t -> t/20) (fps 25)
      deltaArrows =
          lift2 (,) delta (Debug.watch "arrows" <~ Keyboard.arrows)
  in
      sampleOn delta deltaArrows
