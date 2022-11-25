{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Main (main) where

import Vivid

playSong :: VividAction m => m ()
playSong = do
   fork $ do
      s0 <- synth theSound (36 ::I "note")
      wait 1
      free s0
   s1 <- synth theSound (60 ::I "note")
   forM_ [62,66,64] $ \note -> do
      wait (1/4)
      set s1 (note ::I "note")
   wait (1/4)
   free s1

test1 :: Scheduled ()
test1 = do
   s1 <- synth theSound (60 ::I "note")
   wait 1
   s2 <- synth theSound (76 ::I "note")
   wait 1
   free s1
   wait 1
   free s2

note :: I "note" -> Int -> Scheduled ()
note i t = do
   s <- synth theSound i
   wait t
   free s

theSound :: SynthDef '["note"]
theSound = sd (0 ::I "note") $ do
   wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
   s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
   out 0 [s,s]

main :: IO ()
main = do
   let portTODO = "57110"
       hostnameTODO = "127.0.0.1"
   serverState <- makeEmptySCServerState
   let connectConfig = defaultConnectConfig { _scConnectConfig_port = portTODO, _scConnectConfig_hostName = hostnameTODO }
   socket <- createSCServerConnection' serverState connectConfig
   doScheduledInWith serverState 5.0 (note (60 :: I "note") 3)
   -- putStrLn "Simplest:"
   -- playSong

   -- putStrLn "With precise timing:"
   -- doScheduledIn 0.1 playSong
   -- wait 1

   -- putStrLn "Written to a file, non-realtime synthesis:"
   -- putStrLn "(Need to quit the running server for NRT)"
   -- quitSCServer
   -- writeNRT "song.wav" playSong
