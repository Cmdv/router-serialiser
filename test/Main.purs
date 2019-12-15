module Test.Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Router (Router, int, path, print, rest, root, segment)
import Data.Router.Generic (noArgs)
import Data.Router.Generic as G
import Data.Router.Generic.Syntax ((//))
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual { actual: print rest ["a", "b"], expected: "a/b" }

data API
  = Root
  | Foo String Int String
  | Bar String
  | Baz String (Array String)

derive instance eqTestAPI :: Eq API
derive instance genericAPI :: Generic API _
instance showTestAPI :: Show API where show = genericShow

route :: Router API
route =
  root $ path "api" $ G.sum
    { "Root": noArgs
    , "Foo": segment // int segment // segment
    , "Bar": path "bar" segment
    , "Baz": segment // rest
    }
