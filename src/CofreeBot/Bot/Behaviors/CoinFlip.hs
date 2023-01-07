module CofreeBot.Bot.Behaviors.CoinFlip
  ( -- * Bot
    coinFlipBot,
    coinFlipMatrixBot,

    -- * Serializer
    coinFlipSerializer,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot (Bot, embedTextBot)
import CofreeBot.Bot.Serialization (TextSerializer)
import CofreeBot.Bot.Serialization qualified as S
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client (Event, RoomID)
import System.Random (randomIO)

--------------------------------------------------------------------------------

coinFlipBot :: Bot IO () () Bool
coinFlipBot = randomIO

coinFlipMatrixBot :: Bot IO () (RoomID, Event) (RoomID, Event)
coinFlipMatrixBot = embedTextBot $ S.translate coinFlipBot coinFlipSerializer

--------------------------------------------------------------------------------

coinFlipSerializer :: TextSerializer Bool ()
coinFlipSerializer = S.Serializer {parser, printer}

parser :: Text -> Maybe ()
parser = either (const Nothing) Just . parseOnly ("flip a coin" *> pure ())

printer :: Bool -> Text
printer x = "Coin Flip Result: " <> Text.pack (show x)
