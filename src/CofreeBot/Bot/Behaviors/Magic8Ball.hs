module CofreeBot.Bot.Behaviors.Magic8Ball
  ( -- * Bot
    magic8BallBot,
    magic8BallMatrixBot,

    -- * Serializer
    magic8BallSerializer,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Bot.Serialization (TextSerializer)
import CofreeBot.Bot.Serialization qualified as S
import Data.Attoparsec.Text
import Data.Text (Text)
import Network.Matrix.Client (Event, RoomID)
import System.Random

--------------------------------------------------------------------------------

magic8BallBot :: Bot IO () () Int
magic8BallBot = do
  randomRIO (1, 20)

magic8BallMatrixBot :: Bot IO () (RoomID, Event) (RoomID, Event)
magic8BallMatrixBot = embedTextBot $ S.simplifyBot magic8BallBot magic8BallSerializer

--------------------------------------------------------------------------------

magic8BallSerializer :: TextSerializer Int ()
magic8BallSerializer = S.Serializer {parser, printer}

parser :: Text -> Maybe ()
parser = either (const Nothing) Just . parseOnly ("8 ball" *> pure ())

printer :: Int -> Text
printer i = case i `mod` 20 of
  1 -> "It is certain."
  2 -> "It is decidedly so."
  3 -> "Without a doubt."
  4 -> "Yes definitely."
  5 -> "You may rely on it."
  6 -> "As I see it, yes."
  7 -> "Most likely."
  8 -> "Outlook good."
  9 -> "Yes."
  10 -> "Signs point to yes."
  11 -> "Reply hazy, try again."
  12 -> "Ask again later."
  13 -> "Better not tell you now."
  14 -> "Cannot predict now."
  15 -> "Concentrate and ask again."
  16 -> "Don't count on it."
  17 -> "My reply is no."
  18 -> "My sources say no."
  19 -> "Outlook not so good."
  _ -> "Very doubtful."
