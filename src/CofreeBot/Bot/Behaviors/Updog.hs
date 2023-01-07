module CofreeBot.Bot.Behaviors.Updog
  ( -- * Bot
    updogBot,
    updogMatrixBot,

    -- * Serializer
    Updog (..),
    updogBotParser,
    updogSerializer,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Bot.Serialization
import CofreeBot.Bot.Serialization qualified as S
import CofreeBot.Utils.ListT (toListT)
import Control.Applicative (liftA2)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client (Event, RoomID)

--------------------------------------------------------------------------------

updogBot :: Monad m => Bot m s Updog Text
updogBot = Bot $ \s -> \case
  Updog -> toListT [("nothin much whats up with you dog", s), ("HAH GOTTEM", s)]
  Snakesay -> toListT [("Hissss, hisssss", s), ("HAH GOTTEM", s)]
  OPP -> toListT [("yo, you know me!", s), ("HAH GOTTEM", s)]

updogMatrixBot :: Monad m => Bot m () (RoomID, Event) (RoomID, Event)
updogMatrixBot = embedTextBot $ S.translate updogBot updogSerializer

--------------------------------------------------------------------------------

data Updog = Updog | Snakesay | OPP
  deriving (Show, Read)

updogBotParser :: Text -> Maybe Updog
updogBotParser msg
  | runMatcher (what <> "updog") msg = Just Updog
  | runMatcher (what <> "snakesay") msg = Just Snakesay
  | runMatcher (what <> "OPP") msg = Just OPP
  | otherwise = Nothing

updogSerializer :: TextSerializer Text Updog
updogSerializer = Serializer updogBotParser id

--------------------------------------------------------------------------------

newtype Matcher = Matcher
  { runMatcher :: Text -> Bool
  }

instance IsString Matcher where
  fromString = Matcher . Text.isInfixOf . Text.pack

instance Semigroup Matcher where
  Matcher p <> Matcher f = Matcher (liftA2 (&&) p f)

instance Monoid Matcher where
  mempty = Matcher $ const True

(|||) :: Matcher -> Matcher -> Matcher
Matcher p ||| Matcher f = Matcher $ liftA2 (||) p f

what :: Matcher
what = "what" ||| "What" ||| "WHAT"
