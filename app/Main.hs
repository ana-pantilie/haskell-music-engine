{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Vivid
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Options.Applicative
    ( fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      showDefault,
      str,
      value,
      execParser,
      helper,
      Parser )

-- Slightly modified from the Vivid package examples.
playSong :: VividAction m => [I "note"] -> SynthDef '["note"] -> m ()
playSong song currentInstrument = do 
   _ <- traverse (playNote currentInstrument) song
   return ()

playNote :: VividAction m => SynthDef '["note"] -> I "note" -> m ()
playNote instr note = do
   s1 <- synth instr (note ::I "note")
   wait 1
   free s1

basicSaw :: SynthDef '["note"]
basicSaw = sd (0 ::I "note") $ do
   s <-
      0.1
      ~* saw (freq_ $ midiCPS (V::V "note"))
      ~* adsrGen 0.2 0.5 0.01 0.01 Curve_Linear (peakLevel_ 10)
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
   _ <- createSCServerConnection' serverState connectConfig
   return MusicEngineEnv {connectConfig, serverState}

data MusicEngineState = MusicEngineState
   { currentInstrument :: SynthDef '["note"]
   , currentSong :: [I "note"]
   }

data MusicEngineEnv = MusicEngineEnv
   { serverState :: SCServerState
   , connectConfig :: SCConnectConfig
   }

defaultState :: MusicEngineState
defaultState =
   MusicEngineState
      { currentInstrument = basicSaw
      , currentSong = []
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
   | str == "play" = do
      MusicEngineEnv {serverState} <- lift ask
      MusicEngineState {currentInstrument, currentSong} <- lift get
      outputStrLn "Playing song..."
      liftIO $ doScheduledInWith serverState 0.1 (playSong currentSong currentInstrument)
   | str == "set basic-saw" = do
      engineState <- lift get
      lift $ put engineState {currentInstrument = basicSaw}
      outputStrLn "Set the current instrument to 'basic-synth'."
   | otherwise =
      case words str of
         ["add", "note", note] -> do
            engineState <- lift get
            lift $ put engineState {currentSong = currentSong engineState ++ [toI (read note)]}
            outputStrLn "Added note to song."
         _ -> outputStrLn "Command not recognized."

type Semitones = Integer

newtype Interval = Interval Semitones -- no of semitones

unison :: Interval
unison = Interval 0

minorSecond :: Interval
minorSecond = Interval 1

minorThird :: Interval
minorThird = Interval 3

majorThird :: Interval
majorThird = Interval 4

perfectFourth :: Interval
perfectFourth = Interval 5

newtype Chord = Chord [Interval]

majorTriad :: Chord 
majorTriad = Chord [majorThird, minorThird]

minorTriad :: Chord
minorTriad = Chord [minorThird, majorThird]

minorTriad2 :: Chord
minorTriad2 = Chord [majorThird, perfectFourth]

minorTriad3 :: Chord
minorTriad3 = Chord [perfectFourth, minorThird]

-- mod 12

newtype Scale = Scale [Interval]


