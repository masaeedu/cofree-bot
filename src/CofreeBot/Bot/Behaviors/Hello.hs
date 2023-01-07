-- | The Simplest Bot. This module serves as an introductory example
-- for bot construction.
module CofreeBot.Bot.Behaviors.Hello
  ( -- * Bot
    helloBot,
    helloMatrixBot,

    -- * Serializer
    helloBotSerializer,
    helloBotParser,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Bot.Serialization
import CofreeBot.Bot.Serialization qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client (Event, RoomID)

--------------------------------------------------------------------------------

-- | A pure, stateless bot which simply takes a 'Text' input and
-- produces a 'Text' output from it.
helloBot :: Monad m => Bot m s () Text
helloBot = Bot $ \s () -> pure ("Are you talking to me, punk?", s)

-- | We can then embed our bot in the Matrix API using
-- 'liftSimpleBot'.
helloMatrixBot :: Monad m => Bot m () (RoomID, Event) (RoomID, Event)
helloMatrixBot = embedTextBot $ S.translate helloBot helloBotSerializer

--------------------------------------------------------------------------------

helloBotParser :: Text -> Maybe ()
helloBotParser msg =
  let name = "cofree-bot"
   in if name `Text.isInfixOf` msg
        then Just ()
        else Nothing

helloBotSerializer :: TextSerializer Text ()
helloBotSerializer = Serializer helloBotParser id
