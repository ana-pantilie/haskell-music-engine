{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Main (main) where

import Vivid
import System.Console.Haskeline
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Options.Applicative

playSong :: VividAction m => SynthDef '["note"] -> m ()
playSong currentInstrument = do
   fork $ do
      s0 <- synth currentInstrument (36 ::I "note")
      wait 1
      free s0
   s1 <- synth currentInstrument (60 ::I "note")
   forM_ [62,66,64,60,60,62,60,90,80] $ \note -> do
      wait (1/4)
      set s1 (note ::I "note")
   wait (1/4)
   free s1

halloween :: SynthDef '["note"]
halloween = sd (0 ::I "note") $ do
   wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
   s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
   out 0 [s,s]

basicSaw :: SynthDef '["note"]
basicSaw = sd (0 ::I "note") $ do
   s <- 0.1 ~* saw (freq_ $ midiCPS (V::V "note"))
   out 0 [s, s] 

main :: IO ()
main = do
   CLOptions {supercolliderAddress, port} <- execParser clParser
   env <- setupServerConnection supercolliderAddress port
   putStrLn "Welcome to the Haskell Music Engine!"
   runMusicEngine
      env
      defaultState
      (runInputT defaultSettings musicEngineLoop)
  where
   clParser =
      info
         (clOptionsParser <**> helper)
         parserInfoModifiers
   parserInfoModifiers =
      fullDesc
         <> header "Haskell Music Engine - a work in progress..."

data CLOptions = CLOptions
    { supercolliderAddress :: String
    , port :: String
    }

clOptionsParser :: Parser CLOptions
clOptionsParser =
    CLOptions
        <$> option
            str
            ( metavar "SERVER_ADDRESS"
               <> long "server-address"
               <> value "127.0.0.1"
               <> help "Address of a running SuperCollider server."
               <> showDefault
            )
        <*> option
            str
            ( metavar "SERVER_PORT"
                <> long "server-port"
                <> value "57110"
                <> help "Port to use for accessing SuperCollider server."
                <> showDefault
            )

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
      { currentInstrument = basicSaw
      }

newtype MusicEngineT m a = MusicEngineT
   { runMusicEngineT :: StateT MusicEngineState (ReaderT MusicEngineEnv m) a
   } 
   deriving newtype (Functor, Applicative, Monad)
   deriving newtype (MonadIO, MonadState MusicEngineState, MonadReader MusicEngineEnv)
   deriving newtype (MonadThrow, MonadCatch, MonadMask)

runMusicEngine :: MusicEngineEnv -> MusicEngineState -> MusicEngineT IO () -> IO ()
runMusicEngine env state (MusicEngineT action) =
   runReaderT (evalStateT action state) env 

musicEngineLoop :: InputT (MusicEngineT IO) ()
musicEngineLoop = do
   minput <- getInputLine "> "
   case minput of
      Nothing -> return ()
      Just "exit" -> return ()
      Just input -> do
         evaluateCommand input
         musicEngineLoop

evaluateCommand :: String -> InputT (MusicEngineT IO) ()
evaluateCommand str
   | str == "demo" = do
      MusicEngineEnv {serverState} <- lift ask
      MusicEngineState {currentInstrument} <- lift get
      outputStrLn "Playing demo song..."
      liftIO $ doScheduledInWith serverState 0.1 (playSong currentInstrument)
   | str == "set halloween" = do
      engineState <- lift get
      lift $ put engineState {currentInstrument = halloween}
      outputStrLn "Set the current instrument to 'halloween'."
   | str == "set basic-synth" = do
      engineState <- lift get
      lift $ put engineState {currentInstrument = basicSaw}
      outputStrLn "Set the current instrument to 'basic-synth'."
   | otherwise =
      outputStrLn "Command not recognized."
