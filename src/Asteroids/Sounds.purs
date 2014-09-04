module Asteroids.Sounds where

import Control.Bind
import Control.Monad.Eff
import Control.Monad.ST

import Audio.WebAudio.Types
import Audio.WebAudio.AudioContext
import Audio.WebAudio.DestinationNode
import Audio.WebAudio.OscillatorNode
import Audio.WebAudio.GainNode
import Audio.WebAudio.AudioParam

import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Asteroids.Types

initSounds :: forall eff. (Eff (wau :: WebAudio | eff) Sounds)
initSounds = do
  context <- makeAudioContext

  beepOscillator <- createOscillator context
  setOscillatorType Square beepOscillator
  startOscillator 0.0 beepOscillator

  beepGain <- createGain context
  setValue 0.0 =<< gain beepGain

  masterGain <- createGain context
  setValue 0.5 =<< gain masterGain

  connect beepOscillator beepGain
  connect beepGain masterGain
  connect masterGain =<< destination context

  return { context:        context
         , beepOscillator: beepOscillator
         , beepGain:       beepGain
         , masterGain:     masterGain
         }

beep :: forall eff s. STRef s State
        -> (Eff (st :: ST s, wau :: WebAudio, dom :: DOM | eff) Unit)
beep st = do
  state <- readSTRef st
  let s = state.sounds

  freqParam <- frequency s.beepOscillator
  f <- getValue freqParam
  setValue (if f == 55 then 53 else 55) freqParam

  case state.phase of
    Playing _ -> do
        t <- currentTime s.context
        gainParam <- gain s.beepGain
        setValueAtTime 1.000 t gainParam
        setValueAtTime 0.001 (t + 0.2) gainParam
        return unit

    _ -> do return unit

  setTimeout globalWindow 1000 $ beep st
  return unit

startSounds = beep
