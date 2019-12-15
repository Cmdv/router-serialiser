module Data.Router.Print
  ( RouterPrint(..)
  , put
  , run
  , printPath
  ) where

import Prelude

import Data.Array as Array
import Data.Function (applyFlipped)
import Data.Newtype (class Newtype, unwrap)
import Data.Router.Types (RouterState, emptyRouterState)
import Data.String (joinWith)
import Global.Unsafe (unsafeEncodeURIComponent)

newtype RouterPrint = RouterPrint (RouterState -> RouterState)

derive instance newtypeRouterPrint :: Newtype RouterPrint _

instance semigroupRoutePrint :: Semigroup RouterPrint where
  append (RouterPrint f) (RouterPrint g) = RouterPrint (f >>> g)

instance monoideRouterPrint :: Monoid RouterPrint where
  mempty = RouterPrint identity

-- | append a string to the end of segments array
put :: String -> RouterPrint
put str = RouterPrint \state -> state { segments = Array.snoc state.segments str }

run :: RouterPrint -> String
run = printPath <<< applyFlipped emptyRouterState <<< unwrap
      --            populate emptyRouterState      |  unwrap RouterPrint Newtype
      -- let x = unwrap r :: (RouterState -> RouterState)
      --     y = applyFlipped emptyRouterState x :: RouterState
      -- in printPath y

printPath :: RouterState -> String
printPath { segments, hash: hash' } =
  printSegments segments
  where
    printSegments = case _ of
      [""] -> "/"
      xs -> joinWith "/" $ map unsafeEncodeURIComponent xs
