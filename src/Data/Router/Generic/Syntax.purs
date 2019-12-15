module Data.Router.Generic.Syntax where

import Prelude

import Data.Generic.Rep (Argument, Product)
import Data.Router (Router(..), prefix, suffix)
import Data.Router.Generic (class GenRouterCtr, gRouterCtr, product)

class GenSep a b c | a b -> c where
  gensep :: a -> b -> Router c

instance gsepStringString ::
  GenSep String String Unit where
  gensep a b = prefix a $ prefix b $ Router mempty
else
instance gsepStringRouter ::
  GenRouterCtr a b =>
  GenSep String (Router a) b where
  gensep a = prefix a <<< gRouterCtr
else
instance gsepRouteString ::
  GenRouterCtr a b =>
  GenSep (Router a) String b where
  gensep = suffix <<< gRouterCtr
else
instance gsepProduct ::
  GenRouterCtr b c =>
  GenSep (Router a) (Router b) (Product (Argument a) c) where
  gensep = product

infixr 1 gensep as //
