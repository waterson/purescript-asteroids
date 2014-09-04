module Asteroids.Sounds where

import Control.Bind
import Control.Monad.Eff
import Control.Monad.ST

import Data.Maybe

import Audio.WebAudio.Types
import Audio.WebAudio.AudioContext
import Audio.WebAudio.DestinationNode
import Audio.WebAudio.OscillatorNode
import Audio.WebAudio.GainNode
import Audio.WebAudio.AudioParam
import Audio.WebAudio.AudioBufferSourceNode

import Data.DOM.Simple.Types
import Data.DOM.Simple.Ajax
import Data.DOM.Simple.Events
import Data.DOM.Simple.Window

import Asteroids.Types

makeSounds :: forall eff. (Eff (wau :: WebAudio, dom :: DOM | eff) Sounds)
makeSounds = do
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

  return { context:         context
         , beepOscillator:  beepOscillator
         , beepGain:        beepGain
         , shootBuffer:     (Nothing :: Maybe AudioBuffer)
         , explosionBuffer: (Nothing :: Maybe AudioBuffer)
         , masterGain:      masterGain
         }

playBeep :: forall eff s. (STRef s State)
         -> (Eff (st :: ST s, wau :: WebAudio, dom :: DOM | eff) Unit)
playBeep st = do
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

  setTimeout globalWindow 1000 $ playBeep st
  return unit

startSounds :: forall eff s. (STRef s State)
            -> (Eff (st :: ST s, wau :: WebAudio, dom :: DOM | eff) Unit)

startSounds st = do
  state <- readSTRef st

  loadAudioBuffer "shoot.wav" state.sounds.context (\buf -> do
    modifySTRef st (\state -> state { sounds = state.sounds { shootBuffer = Just buf } })
    return unit)

  loadAudioBuffer "explosion.wav" state.sounds.context (\buf -> do
    modifySTRef st (\state -> state { sounds = state.sounds { explosionBuffer = Just buf } })
    return unit)

  playBeep st

loadAudioBuffer :: forall e f. String
                -> AudioContext
                -> (AudioBuffer -> (Eff (wau :: WebAudio | f) Unit))
                -> (Eff (wau :: WebAudio, dom :: DOM | e) Unit)
loadAudioBuffer url ctx cont = do
  req <- makeXMLHttpRequest
  open "GET" url req
  setResponseType "arraybuffer" req
  addProgressEventListener ProgressLoadEvent (decodeInto ctx req cont) req
  send req

decodeInto :: forall e f. AudioContext
           -> XMLHttpRequest
           -> (AudioBuffer -> (Eff (wau :: WebAudio | f) Unit))
           -> DOMEvent
           -> (Eff (wau :: WebAudio, dom :: DOM | e) Unit)

decodeInto ctx req cont ev = do
  audioData <- response req
  decodeAudioData ctx audioData $ \res ->
      case res of
        Just buf -> cont buf
        _        -> return unit

playBufferedSound :: forall eff. Sounds -> Maybe AudioBuffer -> (Eff (wau :: WebAudio | eff) Unit)

playBufferedSound sounds (Just buf) = do
  node <- createBufferSource sounds.context
  connect node sounds.masterGain
  setBuffer buf node
  startBufferSource 0 node

playBufferedSound _ _ = return unit
