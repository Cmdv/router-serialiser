module Data.Router.Types
  ( RouterState(..)
  , emptyRouterState
  ) where

type RouterState =
  { segments :: Array String
  , hash :: String
  }

emptyRouterState :: RouterState
emptyRouterState =
  { segments: []
  , hash: ""
  }
