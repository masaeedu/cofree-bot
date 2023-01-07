{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

--------------------------------------------------------------------------------

import CofreeBot
import CofreeBot.Bot.Serialization qualified as S
import Control.Monad
import Control.Monad.Except
  ( ExceptT,
    runExceptT,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import GHC.Conc (threadDelay)
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed

--------------------------------------------------------------------------------

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"

  case command of
    LoginCmd cred -> do
      session <- login cred
      matrixMain session xdgCache
    TokenCmd TokenCredentials {..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      matrixMain session xdgCache
    CLI -> cliMain xdgCache

--------------------------------------------------------------------------------

bot' process =
  helloBot @_ @() -- <----- polymorphic states need to get asserted to a monoid
    /+\ updogBot @_ @()
    /+\ coinFlipBot
    /+\ magic8BallBot
    /+\ jitsiBot
    /+\ ghciBot process
    /+\ sessionize mempty calculatorBot

serializer' =
  helloBotSerializer
    S./+\ updogSerializer
    S./+\ coinFlipSerializer
    S./+\ magic8BallSerializer
    S./+\ jitsiSerializer
    S./+\ ghciSerializer
    S./+\ sessionSerializer calculatorSerializer

bot process = S.translate (bot' process) serializer'

--------------------------------------------------------------------------------

cliMain :: FilePath -> IO ()
cliMain xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ bot process
  void $ loop $ annihilate repl fixedBot

unsafeCrashInIO :: Show e => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> FilePath -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ embedTextBot $ hoistBot liftIO $ bot process
  unsafeCrashInIO $ loop $ annihilate (matrix session xdgCache) $ batch $ fixedBot
