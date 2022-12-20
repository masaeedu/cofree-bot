module CofreeBot.Bot.Serialization where

--------------------------------------------------------------------------------

import Control.Applicative
import CofreeBot.Utils.ListT (emptyListT)
import CofreeBot.Bot (Bot (..))
import CofreeBot.Utils
import Data.Text (Text)
import Data.These

--------------------------------------------------------------------------------

simplifyBot ::
  Monad m =>
  Bot m s bi bo ->
  Serializer so si bo bi ->
  Bot m s so si
simplifyBot (Bot bot) (Serializer parser printer) = Bot $ \s i ->
  case parser i of
    Nothing -> emptyListT
    Just i' -> do
      (o, s') <- bot s i'
      pure (printer o, s')

--------------------------------------------------------------------------------

-- | Bidirectional serializer from 'Server' I/O to 'Bot' I/O.
data Serializer so si bo bi = Serializer
  {parser :: so -> Maybe bi, printer :: bo -> si}

-- | A 'Serializer' whose 'Server' I/O has been specialized to 'Text'.
type TextSerializer = Serializer Text Text

-- | P
prefix :: Text -> TextSerializer x y -> TextSerializer x y
prefix prefix Serializer {..} =
  Serializer
    { parser = \so -> parser (prefix <> ": " <> so),
      printer = \bo -> prefix <> ":" <> printer bo
    }

infixr 6 /+\

(/+\) :: TextSerializer o i -> TextSerializer o' i' -> TextSerializer (o /+\ o') (i /+\ i')
(/+\) (Serializer par1 pri1) (Serializer par2 pri2) = Serializer (par1 *|* par2) (pri1 +|+ pri2)

-- | Parse and tensor the components @a@ and @b@ of a 'These'.
infixr 6 *|*

(*|*) :: (Text -> Maybe a) -> (Text -> Maybe b) -> Text -> Maybe (a /+\ b)
(*|*) p1 p2 = these (fmap This) (fmap That) can . liftA2 These p1 p2

-- | Print and combine the components @a@ and @b@ of a 'These'.
infixr 6 +|+

(+|+) :: (a -> Text) -> (b -> Text) -> a /+\ b -> Text
(+|+) p1 p2 = these p1 p2 $ \a b -> p1 a <> "\n" <> p2 b