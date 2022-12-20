module CofreeBot.Bot.Behaviors.Jitsi
  ( -- * Bot
    jitsiBot,
    jitsiMatrixBot,

    -- * Serializer,
    jitsiSerializer,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot (Bot, embedTextBot, liftEffect)
import CofreeBot.Bot.Behaviors.Jitsi.Dictionary (adjectives, adverbs, pluralNouns, verbs)
import CofreeBot.Bot.Serialization (TextSerializer)
import CofreeBot.Bot.Serialization qualified as S
import Data.Text (Text)
import Data.Vector qualified as V
import Network.Matrix.Client (Event, RoomID)
import System.Random (randomRIO)

--------------------------------------------------------------------------------

jitsiBot :: Bot IO () () Text
jitsiBot = liftEffect jitsiUrl

jitsiMatrixBot :: Bot IO () (RoomID, Event) (RoomID, Event)
jitsiMatrixBot = embedTextBot $ S.simplifyBot jitsiBot jitsiSerializer

--------------------------------------------------------------------------------

jitsiSerializer :: TextSerializer Text ()
jitsiSerializer = S.Serializer {parser, printer = id}

parser :: Text -> Maybe ()
parser i = if (i == "🍐" || i == "pair" || i == "pair") then Just () else Nothing

--------------------------------------------------------------------------------

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiUrl :: IO Text
jitsiUrl = do
  adjective <- pickRandomElement adjectives
  noun <- pickRandomElement pluralNouns
  verb <- pickRandomElement verbs
  adverb <- pickRandomElement adverbs
  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ url
