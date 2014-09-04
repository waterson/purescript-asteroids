{- -*- Mode: Haskell -*- -}

module Asteroids.Main where

import Math
import Debug.Trace
import Control.Monad
import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST
import Graphics.Canvas

import Data.Array
import Data.Tuple
import Data.Foldable
import Data.Maybe.Unsafe
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

import Asteroids.Types
import Asteroids.Sounds

--main :: forall eff. (Eff eff Unit)
main = do
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow

  let ship = defaultShip w h
      controls = { thrust: 0, left: false, right: false }

  asteroids <- replicateM 10 (randomAsteroid w h)

  sounds <- initSounds

  st <- newSTRef { w: w, h: h
                 , phase: GameOver
                 , nships: 3
                 , score: 0
                 , asteroids: asteroids
                 , missiles: [ ]
                 , controls: controls
                 , sounds: sounds }

  resize st
  addUIEventListener ResizeEvent (resize0 st) globalWindow

  addKeyboardEventListener KeydownEvent (keydown st) globalWindow
  addKeyboardEventListener KeyupEvent (keyup st) globalWindow
  addKeyboardEventListener KeypressEvent (keypress st) globalWindow

  setInterval globalWindow 33 $ tick st
  startSounds st
  return unit

-- |Returns a new ship in the middle of the canvas.
defaultShip :: Number -> Number -> Ship
defaultShip w h = { x: w / 2, y: w / 2, dir: 0, dx: 0, dy: 0, r: 10 }

-- |The maximum speed we'll allow for a ship.
maxSpeed = 6.0

twoPi = 2 * pi

-- |Creates a randomly located asteroid somewhere on the canvas.
randomAsteroid :: forall e. Number -> Number -> Eff ( random :: Random | e ) Asteroid
randomAsteroid w h = do
  x  <- randomRange 0 w
  y  <- randomRange 0 h
  dx <- (*) <$> (randomRange 1 2) <*> randomSign
  dy <- (*) <$> (randomRange 1 2) <*> randomSign
  path <- replicateM 12 (randomRange 0.7 1.1)
  spin <- randomRange (-0.1) 0.1
  return { x: x, y: y, dx: dx, dy: dy, r: 50, path: path, spin: spin, dir: 0 }
      where randomRange lo hi = (\n -> lo + n * (hi - lo)) <$> random
            randomSign = (\n -> if n < 0.5 then (-1) else 1) <$> random

-- |Thunk to attach resize to the ResizeEvent
-- XXX WTF? Why do I need this?
resize0 :: forall s e. STRef s State -> DOMEvent -> (Eff (st :: ST s, dom :: DOM, canvas :: Canvas | e) Unit)
resize0 st _ = resize st

-- |Handles window resizing by stretching the canvas to fit the
-- viewport and updating our notion of how large the canvas is.
resize :: forall s e. STRef s State -> (Eff (st :: ST s, dom :: DOM, canvas :: Canvas | e) Unit)
resize st = do
  state <- readSTRef st
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  canvas <- getCanvasElementById "canvas"
  setCanvasWidth w canvas
  setCanvasHeight h canvas
  modifySTRef st $ (\state -> state { w = w, h = h })
  return unit

-- |Handles key presses for keys that can be held down.
keydown :: forall s e. STRef s State -> DOMEvent -> Eff (st :: ST s, dom :: DOM | e) Unit
keydown st event = do
  code <- keyCode event
  modifySTRef st $ \state ->
      let controls = case code of
                       65  -> state.controls { left = true }
                       83  -> state.controls { right = true }
                       75  -> state.controls { thrust = 0.5 }
                       _   -> state.controls
      in state { controls = controls }

  return unit

-- |Handles key releases for keys that can be held down.
keyup :: forall s e. STRef s State -> DOMEvent -> Eff (st :: ST s, dom :: DOM | e) Unit
keyup st event = do
  code <- keyCode event
  modifySTRef st $ \state ->
      let controls = case code of
                       65  -> state.controls { left = false }
                       83  -> state.controls { right = false }
                       75  -> state.controls { thrust = 0.0 }
                       _   -> state.controls
      in state { controls = controls }

  return unit

-- |Handles single key presses.
keypress :: forall s e. STRef s State -> DOMEvent -> Eff (st :: ST s, dom :: DOM, random :: Random | e) Unit
keypress st event = do
  state <- readSTRef st
  k <- key event
  case state.phase of
    Playing ship | (k == "l" || k == "L") -> do
      let x = ship.x
          y = ship.y
          dx = ship.dx + 8 * cos (ship.dir - pi/2)
          dy = ship.dy + 8 * sin (ship.dir - pi/2)
          missile = { x: x, y: y, dx: dx, dy: dy, r: 1, fuse: 50 }

      writeSTRef st $ state { missiles = (missile : state.missiles) }
      return unit

    GameOver | (k == " ") -> do
      asteroids <- replicateM 10 (randomAsteroid state.w state.h)
      writeSTRef st $ state { phase     = Respawning 11
                            , nships    = 3
                            , score     = 0
                            , asteroids = asteroids
                            }
      return unit

    _ -> return unit

-- |Handles a single animation frame.
tick :: forall s e. STRef s State -> Eff (st :: ST s, random :: Random, canvas :: Canvas | e ) Unit
tick st = do
  state <- readSTRef st
  state' <- update state
  writeSTRef st state'
  render state

-- |Updates the world state.
update :: forall s e. State -> Eff (st :: ST s, random :: Random | e) State
update state = do
  let -- These asteroids haven't been hit.
      asteroids = do
        a <- state.asteroids
        guard $ not (any (flip hittest $ a) state.missiles)
        [move state.w state.h $ spin a]

      -- These asteroids have been hit: split 'em.
      asteroids' = do
        a <- state.asteroids
        case filter (flip hittest $ a) state.missiles of
          (m : ms) ->
              if a.r >= 20
              then let dx = m.dx / 10
                       dy = m.dy / 10
                   in [ a { dx = (dx + 1.5 * a.dy), dy = (dy + 1.5 * a.dx), r = a.r / 2 },
                        a { dx = (dx - 1.5 * a.dy), dy = (dy - 1.5 * a.dx), r = a.r / 2 } ]
              else [ ]

          _ -> [ ]

      asteroids'' = asteroids ++ asteroids'

      -- Remove any missiles that hit something.
      missiles = do
        m <- state.missiles
        guard $ not (any (hittest m) state.asteroids)
        let m' = burn m
        guard $ m'.fuse > 0
        [move state.w state.h m']

  case state.phase of
    Playing ship | length asteroids'' > 0 -> do
      let c = state.controls

          -- Update the ship based on any controls.
          dir = ship.dir + (if c.right then (pi / 16) else 0.0) + (if c.left then (-pi / 16) else 0.0)
          dx = clamp (-maxSpeed) maxSpeed (ship.dx + c.thrust * cos (dir - pi/2))
          dy = clamp (-maxSpeed) maxSpeed (ship.dy + c.thrust * sin (dir - pi/2))
          x = (state.w + ship.x + ship.dx) % state.w
          y = (state.h + ship.y + ship.dy) % state.h
          ship' = ship { x = x, y = y, dx = dx, dy = dy, dir = dir }

          -- Accumulate points for any hit asteroids
          points = sum $ do
            a <- state.asteroids
            guard $ any (flip hittest $ a) state.missiles
            [if a.r >= 50 then 20 else if a.r >= 25 then 50 else 100]

          -- Did the ship crash?
          crash = any (hittest ship') asteroids

      return $ state { phase     = if crash then Crashing ship' 0 else Playing ship'
                     , nships    = if crash then state.nships - 1 else state.nships
                     , missiles  = missiles
                     , asteroids = asteroids''
                     , score     = state.score + points
                     }

    Playing ship -> do
      asteroids <- replicateM 10 (randomAsteroid state.w state.h)
      return $ state { phase     = Respawning 33
                     , asteroids = asteroids
                     , missiles  = [ ] }

    Crashing ship step -> do
      let ship' = ship { x = (state.w + ship.x + ship.dx) % state.w,
                         y = (state.h + ship.y + ship.dy) % state.h }
          phase = if step > 33 then
                      if state.nships > 0 then Respawning 33 else GameOver
                  else Crashing ship' (step + 1)

      return $ state { phase     = phase
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    Respawning n | n > 0 -> do
      return $ state { phase = Respawning (n-1)
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    Respawning n -> do
      let center = (defaultShip state.w state.h) { r = 40 }
          phase = if any (hittest center) state.asteroids
                  then Respawning n
                  else Playing (defaultShip state.w state.h)

      return $ state { phase     = phase
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    GameOver -> do
      return $ state { missiles  = missiles
                     , asteroids = asteroids'' }

-- |Ticks down the fuse on a missile.
burn :: forall m. { fuse :: Number | m } -> { fuse :: Number | m }
burn missile = missile { fuse = missile.fuse - 1.0 }

-- |Checks to see if two Moveable's have collided.
hittest :: forall m n. Moveable m -> Moveable n -> Boolean
hittest a b =
    let dx = a.x - b.x
        dy = a.y - b.y
    in sqrt ((dx * dx) + (dy * dy)) <= (a.r + b.r)

-- |Clamps the value of `x` between `lo` and `hi`.
clamp :: Number -> Number -> Number -> Number
clamp lo hi x = max lo (min hi x)

spin :: Asteroid -> Asteroid
spin a = let spin = a.dir + a.spin in a { dir = if spin > twoPi then spin - twoPi else spin }

-- |Moves a Moveable by updating it's `x` and `y` coordinates by `dx`
-- and `dy`, respectively.
move :: forall m. Number -> Number -> Moveable m -> Moveable m
move w h obj = obj { x = (w + obj.x + obj.dx) % w
                   , y = (h + obj.y + obj.dy) % h }

-- |Renders the current state.
render :: forall e. State -> Eff ( canvas :: Canvas | e ) Unit
render state = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle "#000000" ctx
  fillPath ctx $ rect ctx { x: 0, y: 0, w: state.w, h: state.h }

  -- Draw the score
  save ctx
  setFillStyle "#ffffff" ctx
  setFont "16px Hyperspace" ctx
  fillText ctx (show state.score) 10 20
  restore ctx

  -- Draw the remaining ships
  forE 1 state.nships $ \i -> do
      let ship = (defaultShip 0 0) { x = 20 * i, y = 40 }
      renderShip ctx ship "#ffffff" false
      return unit

  case state.phase of
    Playing ship    -> renderShip ctx ship "#ffffff" (state.controls.thrust /= 0)
    Respawning _    -> renderShip ctx (defaultShip state.w state.h) "#555555" false
    Crashing ship i -> renderCrash ctx ship i
    GameOver        -> renderGameOver ctx state.w state.h

  foldM (renderAsteroid ctx) unit state.asteroids
  foldM (renderMissile ctx) unit state.missiles
  return unit

-- |Renders a ship.
renderShip :: forall e. Context2D -> Ship -> String -> Boolean -> Eff ( canvas :: Canvas | e ) Unit
renderShip ctx ship color engines = do
  save ctx
  translate { translateX: ship.x, translateY: ship.y } ctx
  rotate ship.dir ctx
  setLineWidth 1 ctx
  setStrokeStyle color ctx
  beginPath ctx
  moveTo ctx 0 (-10)
  lineTo ctx (-7) 10
  lineTo ctx 0 8
  lineTo ctx 7 10
  lineTo ctx 0 (-10)
  stroke ctx

  when engines $ do
    moveTo ctx (-4) 11
    lineTo ctx 0 15
    lineTo ctx 4 11
    stroke ctx
    return unit

  restore ctx
  return unit

-- |Renders an exploding ship.
renderCrash :: forall e. Context2D -> Ship -> Number -> Eff ( canvas :: Canvas | e ) Unit
renderCrash ctx ship i = do
  save ctx
  translate { translateX: ship.x, translateY: ship.y } ctx
  rotate ship.dir ctx
  setLineWidth 1 ctx
  setStrokeStyle "#ffffff" ctx

  save ctx
  translate { translateX: -i, translateY: -i } ctx
  rotate (i * pi / 60) ctx
  beginPath ctx
  moveTo ctx 0 (-10)
  lineTo ctx (-7) 10
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: i, translateY: -i } ctx
  rotate ((-i) * pi / 30) ctx
  beginPath ctx
  moveTo ctx 0 (-10)
  lineTo ctx 7 10
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: -i, translateY: i } ctx
  rotate ((-i) * pi / 20) ctx
  beginPath ctx
  moveTo ctx (-7) 10
  lineTo ctx 0 8
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: i, translateY: i } ctx
  rotate (i * pi / 15) ctx
  beginPath ctx
  moveTo ctx 0 8
  lineTo ctx 7 10
  stroke ctx
  restore ctx

  restore ctx

  return unit

-- |Renders an asteroid.
renderAsteroid :: forall e. Context2D -> Unit -> Asteroid -> Eff ( canvas :: Canvas | e ) Unit
renderAsteroid ctx _ asteroid = do
  save ctx

  setLineWidth 1 ctx
  translate { translateX: asteroid.x, translateY: asteroid.y } ctx
  rotate asteroid.dir ctx

  setStrokeStyle "#ffffff" ctx
  beginPath ctx

  let n = length asteroid.path
      steps = map ((*) (twoPi / n)) (1 .. n)
      theta = fromJust $ last steps
      off = fromJust $ last asteroid.path

  moveTo ctx (asteroid.r * off * cos theta) (asteroid.r * off * sin theta)

  for_ (zip asteroid.path steps) $ \(Tuple off theta) -> do
                                  let x = asteroid.r * off * cos theta
                                      y = asteroid.r * off * sin theta
                                  lineTo ctx x y


  stroke ctx

  restore ctx
  return unit

-- |Renders a missile.
renderMissile :: forall e. Context2D -> Unit -> Missile -> Eff ( canvas :: Canvas | e ) Unit
renderMissile ctx _ missile = do
  save ctx
  setFillStyle "#ffffff" ctx
  fillPath ctx $ arc ctx { x: missile.x, y: missile.y, r: 2, start: 0, end: twoPi }
  restore ctx
  return unit

-- |Renders the GAME OVER screen.
renderGameOver :: forall e. Context2D -> Number -> Number -> Eff ( canvas :: Canvas | e ) Unit
renderGameOver ctx w h = do
  save ctx
  setFillStyle "#ffffff" ctx

  centerText "Game Over" 64 (-40)
  centerText "Controls" 20 0
  centerText "A = Rotate Left, S = Rotate Right" 14 20
  centerText "K = Thrust, L = Fire" 14 40
  centerText "Press the space bar to start" 16 120

  restore ctx
  return unit
      where centerText text pixels y = do
              setFont ((show pixels) ++ "px Hyperspace") ctx
              metrics <- measureText ctx text
              fillText ctx text (w / 2 - metrics.width / 2) (y + h / 2)
