{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Main (main) where

import Vivid
import System.Console.Haskeline
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, modify)
import Control.Monad.Trans.Reader (ReaderT(..))
import Network.Socket (Socket)

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
   env <- setupServerConnection hostnameTODO portTODO
   runMusicEngine env defaultState musicEngineLoop

setupServerConnection :: String -> String -> IO MusicEngineEnv
setupServerConnection hostname port = do
   serverState <- makeEmptySCServerState
   let connectConfig =
         defaultConnectConfig
            { _scConnectConfig_port = port
            , _scConnectConfig_hostName = hostname
            }
   createSCServerConnection' serverState connectConfig
   return MusicEngineEnv {connectConfig, serverState}

data MusicEngineState = MusicEngineState
   { currentInstrument :: SynthDef '["note"]
   }

data MusicEngineEnv = MusicEngineEnv
   { serverState :: SCServerState
   , connectConfig :: SCConnectConfig
   }

defaultState :: MusicEngineState
defaultState =
   MusicEngineState
      { currentInstrument = theSound
      }

type MusicEngineT m = InputT (StateT MusicEngineState (ReaderT MusicEngineEnv m)) 

runMusicEngine :: MusicEngineEnv -> MusicEngineState -> MusicEngineT IO () -> IO ()
runMusicEngine env state action =
   runReaderT (evalStateT (runInputT defaultSettings action) state) env 

musicEngineLoop :: MusicEngineT IO ()
musicEngineLoop = do
   minput <- getInputLine "> "
   case minput of
      Nothing -> return ()
      Just "exit" -> return ()
      Just input -> do
         evaluateCommand input
         musicEngineLoop

evaluateCommand :: String -> MusicEngineT IO ()
evaluateCommand str
   | str == "demo" = do
      outputStrLn "Playing demo song..."
      liftIO playSong
   | otherwise =
      outputStrLn "Command not recognized."
