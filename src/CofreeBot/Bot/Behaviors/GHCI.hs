{-# LANGUAGE NumDecimals #-}

module CofreeBot.Bot.Behaviors.GHCI
  ( -- * Bot
    ghciBot,
    ghciMatrixBot,
    ghciConfig,
    hGetOutput,

    -- * Serializer
    ghciSerializer,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot (Bot (..), embedTextBot)
import CofreeBot.Bot.Serialization (TextSerializer)
import CofreeBot.Bot.Serialization qualified as S
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM)
import Data.Attoparsec.Text as A
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (threadDelay)
import Network.Matrix.Client (Event, RoomID)
import System.IO
import System.Process.Typed

--------------------------------------------------------------------------------

ghciBot :: Process Handle Handle () -> Bot IO () Text Text
ghciBot p = Bot $
  \s i -> do
    o <- liftIO $ do
      hPutStrLn (getStdin p) $ Text.unpack i
      hFlush (getStdin p)
      void $ threadDelay 5e5
      hGetOutput (getStdout p)
    pure (Text.pack o, s)

ghciMatrixBot :: Process Handle Handle () -> Bot IO () (RoomID, Event) (RoomID, Event)
ghciMatrixBot handle = embedTextBot $ S.simplifyBot (ghciBot handle) ghciSerializer

--------------------------------------------------------------------------------

ghciSerializer :: TextSerializer Text Text
ghciSerializer = S.Serializer {parser, printer = id}

parser :: Text -> Maybe Text
parser = either (const Nothing) Just . parseOnly ("ghci:" *> takeText)

--------------------------------------------------------------------------------

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

-- ghciBot :: Process Handle Handle () -> Bot IO () Text Text
-- ghciBot p =
--   dimap (distinguish (/= "ghci: :q")) indistinct $
--     pureStatelessBot (const $ "I'm Sorry Dave")
--       \/ ghciBot' p

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig =
  setStdin createPipe $
    setStdout createPipe $
      shell
        "docker run -i --rm haskell 2>&1"
