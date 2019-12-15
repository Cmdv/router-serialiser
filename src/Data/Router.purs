module Data.Router
  ( Router(..)
  , as
  , end
  , int
  , path
  , prefix
  , print
  , segment
  , string
  , suffix
  , rest
  , root
  )where

import Prelude

import Data.Foldable (foldMap, foldr)
import Data.Functor.Contravariant (class Contravariant)
import Data.Router.Print (RouterPrint)
import Data.Router.Print as Printer
import Data.String (Pattern(..))
import Data.String as String

data Router a = Router (a -> RouterPrint)

instance contravariantRouter :: Contravariant Router where
  cmap f (Router g) = Router (\b -> g $ f b)

-- | create a codec
as :: forall s a. (a -> s) -> Router s -> Router a
as f (Router enc) = Router (enc <<< f)

end :: forall a. Router a -> Router a
end (Router enc) = Router enc

int :: Router String -> Router Int
int = as show

-- | adding the '/' at the beginning of path,
path :: forall a. String -> Router a -> Router a
path = flip (foldr prefix) <<< String.split (Pattern "/")

-- | adds a given String prefix
prefix :: forall a. String -> Router a -> Router a
prefix s (Router enc) = Router (\a -> Printer.put s <> enc a)

-- | Renders a value of type `a` into a String representation of URI path,
print :: forall a. Router a -> a -> String
print (Router enc) = Printer.run <<< enc

segment :: Router String
segment = Router Printer.put

string :: Router String -> Router String
string = identity

-- | adds a given String suffix
suffix :: forall a. Router a -> String -> Router a
suffix (Router enc) s = Router (\a -> enc a <> Printer.put s)

-- | Consumes or prints all the remaining segments.
-- |
-- | print rest ["a", "b"] == "a/b"
-- |
rest :: Router (Array String)
rest = Router (foldMap Printer.put)

-- | "/" segment
root :: forall a. Router a -> Router a
root = path ""

class RouterParams (r1 :: # Type) (r2 :: # Type) | r1 -> r2 where
  params :: { | r1 } -> Router { | r2 }
