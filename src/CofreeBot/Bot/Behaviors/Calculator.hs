module CofreeBot.Bot.Behaviors.Calculator
  ( -- * Bot
    calculatorBot,
    calculatorBot',
    calculatorMatrixBot,
    printer,

    -- * Serializer
    calculatorSerializer,

    -- * Language
    module Language,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot (Bot, embedTextBot)
import CofreeBot.Bot.Behaviors.Calculator.Language as Language
import CofreeBot.Bot.Serialization (TextSerializer)
import CofreeBot.Bot.Serialization qualified as S
import CofreeBot.Utils (type (\/))
import Control.Monad.Reader (ask)
import Control.Monad.State (state)
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client (Event, RoomID)

--------------------------------------------------------------------------------

calculatorBot :: Bot IO CalcState Statement (CalcError \/ CalcResp)
calculatorBot = ask >>= state . execCalculator

calculatorBot' :: Bot IO CalcState Text Text
calculatorBot' = S.simplifyBot calculatorBot calculatorSerializer

calculatorMatrixBot :: Bot IO CalcState (RoomID, Event) (RoomID, Event)
calculatorMatrixBot = embedTextBot calculatorBot'

--------------------------------------------------------------------------------

calculatorSerializer :: TextSerializer (CalcError \/ CalcResp) Statement
calculatorSerializer = S.Serializer {parser, printer}

parser :: Text -> Maybe Statement
parser = either (const Nothing) Just . parseOnly statementP

printer :: Either CalcError CalcResp -> Text
printer = \case
  Left err -> Text.pack $ show err
  Right Ack -> "*variable saved*"
  Right (Log e n) -> Text.pack $ show e <> " = " <> show n
