module CofreeBot.Bot.Simple where

import Control.Lens (view)
import CofreeBot.Bot ( BotAction(..), Bot(..) )
--import CofreeBot.Bot.Matrix ( MatrixBot )
import Data.Foldable ( traverse_ )
import Data.Profunctor ( dimap, second' )
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import System.IO ( stdout, hFlush )

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type SimpleBot m s = Bot m s T.Text [T.Text]

-- | Lift a 'SimpleBot' into a 'MatrixBot'
--liftSimpleBot :: Functor m => ((RoomID, Event) -> T.Text) -> (T.Text -> (RoomID, Event)) -> SimpleBot m s -> MatrixBot m s
--liftSimpleBot to from = dimap to _ -- (fmap from)

-- | An evaluator for running 'SimpleBots' in 'IO'
runSimpleBot :: forall s. SimpleBot IO s -> s -> IO ()
runSimpleBot bot = go
  where
  go :: s -> IO ()
  go state = do
    putStr "<<< "
    hFlush stdout
    input <- getLine
    BotAction {..} <- runBot bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState

liftSimpleBot :: Functor m => SimpleBot m s -> Bot m s (RoomID, Event) (RoomID, [Event])
liftSimpleBot = second' . dimap to from
  where
    viewBody :: Event -> T.Text
    viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

    to :: Event -> T.Text
    to = viewBody

    from :: [T.Text] -> [Event]
    from = fmap (EventRoomMessage . mkMsg)

    mkMsg :: T.Text -> RoomMessage
    mkMsg msg = RoomMessageText $ MessageText msg TextType Nothing Nothing
