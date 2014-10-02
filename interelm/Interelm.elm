import Keyboard
import Window
import Debug
import Math.Vector2 as V
import Math.Vector2 (Vec2, vec2, add, scale)

-- MODEL

type Rotor = Float
rotor : Float -> Rotor
rotor r = r

dir : Rotor -> Vec2
dir r = vec2 (0-sin r) (cos r)

type Physical =
    { mass : Float
    , acc : Vec2
    , vel : Vec2
    , pos : Vec2
    , alpha : Rotor
    , omega : Rotor
    , orient : Rotor
    }

type Ship =
    { body : Physical
    }

data AccretionClass = PlanetClass | StellarClass | GalacticClass | ClusterClass

type Accretion =
    { body : Physical
    , radius : Float -- meter
    , classification : AccretionClass
    }

data Direction = Left | Right

type Keys = { x:Int, y:Int }

escape_velocity : Float -> Float -> Float
escape_velocity mass r = sqrt (2 * gravitational_constant * mass / r)

ship : Ship
ship =
    { body =
        { mass = 1E6 -- kg
        , acc = vec2 0 0
        , vel = vec2 0 (0.5 * escape_velocity earth_mass 12E6)
        , pos = vec2 12000000 0

        , alpha = rotor 0 -- angular accel
        , omega = rotor 0 -- angular vel
        , orient = rotor 0 -- angular pos
        }
    }

-- Mass og earth = 1M+
earth_mass = 5.97219E24 -- kg
gravitational_constant = 6.67E-11 --N(m/kg)^2

earth : Accretion
earth =
    { body =
        { mass = earth_mass
        , pos = vec2 0 0
        , acc = vec2 0 0
        , vel = vec2 0 0
        , alpha = rotor 0 -- angular accel
        , omega = rotor 0 -- angular vel
        , orient = rotor 0 -- angular pos
        }
    , radius = 6378100 -- meter
    , classification = PlanetClass
    }

-- UPDATE

timescale = 1 * 10
solscale = 1E5 -- meter

step : (Float, Keys) -> Ship -> Ship
step (dt, keys) ship =
    { ship | body <- ship.body
        |> thrust keys
        |> impulse keys
        |> gravity (timescale*dt)
        |> physics (timescale*dt)
        |> Debug.watch "ship"
    }

thrust : Keys -> Physical -> Physical
thrust keys body =
    if keys.y > 0 then
        { body | acc <- V.scale 0.1 (dir body.orient) }
    else
        { body | acc <- vec2 0.0 0.0 }

gravity : Float -> Physical -> Physical
gravity dt body =
    let dist = body.pos
        dir = V.normalize dist
        r2 = V.lengthSquared body.pos
        f = 0 - earth_mass * body.mass * gravitational_constant / r2
    in
        { body | acc <- V.scale (f/body.mass) dir }

physics : Float -> Physical -> Physical
physics dt body =
    { body |
        vel <- V.add body.vel (V.scale dt body.acc),
        pos <- V.add body.pos (V.scale dt body.vel),
        omega <- body.omega + dt * body.alpha,
        orient <- body.orient + dt * body.omega
    }

impulse : Keys -> Physical -> Physical
impulse keys body =
    { body |
        alpha <- (-0.001) * toFloat keys.x
    }

-- DISPLAY

movePos pos = move ((V.getX pos) / solscale, (V.getY pos) / solscale)

display : (Int, Int) -> Ship -> Element
display (w',h') ship =
  let (w,h) = (toFloat w', toFloat h')
      src  = "images/placeholdership.jpeg"
      shipImage = image 35 35 src
      earthImage = image 100 100 "images/earth.jpg"
  in
      collage w' h'
          [ rect w h
              |> filled background
          , earthImage
              |> toForm
              |> movePos earth.body.pos
          , shipImage
              |> toForm
              |> Debug.trace "ship"
              |> rotate ship.body.orient
              |> movePos ship.body.pos
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
