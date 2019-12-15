module Data.Router.Generic
  ( class GenRouter
  , class GenRouterCtr
  , gRouterCtr
  , genRouter
  , product
  , sum
  , noArgs
  )where

import Prelude

import Data.Functor.Contravariant (cmap)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product(..), Sum(..), from)
import Data.Router (Router(..), end)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Record as Record

sum :: forall a rep r.
  Generic a rep =>
  GenRouter rep r =>
  { | r } ->
  Router a
sum = cmap from <<< genRouter

class GenRouter rep (r :: # Type) | rep -> r where
  genRouter :: { | r } -> Router rep

-- | Sum instance of GenRouter
instance gRouterSum ::
  ( GenRouter a r
  , GenRouter b r
  ) =>
  GenRouter (Sum a b) r where
  genRouter r = Router enc
    where
    Router encl = genRouter r
    Router encr = genRouter r
    enc = case _ of
      Inl a -> encl a
      Inr b -> encr b

instance gRouterConstructor ::
  ( IsSymbol sym
  , Row.Cons sym (Router a) rx r
  , GenRouterCtr a b
  ) =>
  GenRouter (Constructor sym b) r where
  genRouter r = Router enc
    where
      Router enc' =
        end
          $ (gRouterCtr :: Router a -> Router b)
          $ Record.get (SProxy :: SProxy sym) r
      enc (Constructor a) = enc' a


class GenRouterCtr a b | a -> b where
  gRouterCtr :: Router a -> Router b

instance gRouterProduct ::
  GenRouterCtr (Product a b) (Product a b) where
  gRouterCtr = identity
else
instance gRouteNoArguments ::
  GenRouterCtr NoArguments NoArguments where
  gRouterCtr = identity
else
instance gRouteArgument ::
  GenRouterCtr (Argument a) (Argument a) where
  gRouterCtr = identity
else
instance gRouteAll ::
  GenRouterCtr a (Argument a) where
  gRouterCtr (Router enc) =
    Router (\(Argument a) -> enc a)

product :: forall a b c.
  GenRouterCtr b c =>
  Router a ->
  Router b ->
  Router (Product (Argument a) c)
product (Router encl) l = Router enc
  where
  Router encr = gRouterCtr l
  enc (Product (Argument a) b) = encl a <> encr b

noArgs:: Router NoArguments
noArgs = Router mempty
