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
    { mass : Float
    , acceleration : Vec2
    , velocity : Vec2
    , position : Vec2

    , alpha : Rotor -- angular acceleration
    , omega : Rotor
    , orientation : Rotor
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }

data AccretionClass = PlanetClass | StellarClass | GalacticClass | ClusterClass

type Accretion =
    { position : Vec2
    , mass : Float -- kg
    , radius : Float -- meter
    , class : AccretionClass
    }

data Direction = Left | Right

type Keys = { x:Int, y:Int }

escape_velocity : Float -> Float -> Float
escape_velocity mass r = sqrt (2*gravitational_constant * mass / r)

ship : Ship
ship =
    { mass = 1E6 -- kg
    , acceleration = vec2 0 0
    , velocity = vec2 0 (0.5 * escape_velocity earth_mass 12E6)
    , position = vec2 12000000 0

    , alpha = rotor 0 -- angular accel
    , omega = rotor 0 -- angular vel
    , orientation = rotor 0 -- angular pos
    , x = 0
    , y = 0
    , vx = 0
    , vy = 0
    }

-- Mass og earth = 1M+
earth_mass = 5.97219E24 -- kg
gravitational_constant = 6.67E-11 --N(m/kg)^2

earth : Accretion
earth =
    { position = vec2 0 0
    , mass = earth_mass
    , radius = 6378100 -- meter
    , class = PlanetClass
    }

-- UPDATE

timescale = 1 * 10
solscale = 1E5 -- meter

step : (Float, Keys) -> Ship -> Ship
step (dt, keys) ship =
    ship
        |> thrust keys
        |> impulse keys
        |> gravity (timescale*dt)
        |> physics (timescale*dt)
        |> Debug.watch "ship"

thrust : Keys -> Ship -> Ship
thrust keys ship =
    if keys.y > 0 then
        { ship | acceleration <- V.scale 0.1 (dir ship.orientation) }
    else { ship | acceleration <- vec2 0.0 0.0 }

gravity : Float -> Ship -> Ship
gravity dt ship =
    let dist = ship.position
        dir = V.normalize dist
        r2 = V.lengthSquared ship.position
        f = 0 - earth_mass*ship.mass*gravitational_constant/r2
    in
        { ship | acceleration <- V.scale (f/ship.mass) dir }

physics : Float -> Ship -> Ship
physics dt ship =
    { ship |
        velocity <- V.add ship.velocity (V.scale dt ship.acceleration),
        position <- V.add ship.position (V.scale dt ship.velocity),
        omega <- ship.omega + dt * ship.alpha,
        orientation <- ship.orientation + dt * ship.omega,
        x <- V.getX ship.position,
        y <- V.getY ship.position,
        vx <- V.getX ship.velocity,
        vy <- V.getY ship.velocity
    }

impulse : Keys -> Ship -> Ship
impulse keys ship =
    { ship |
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
              |> move (V.getX earth.position, V.getY earth.position)
          , shipImage
              |> toForm
              |> Debug.trace "ship"
              |> rotate ship.orientation
              |> movePos ship.position
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
