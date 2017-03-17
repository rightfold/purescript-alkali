module Main
  ( main
  ) where

import Color (Color)
import Color.Scheme.X11 (aqua)
import Control.Monad.Eff (Eff)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (T11, (/\))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Alkali (class ToComponent, QueryGeneric, genericToComponent, toComponent)
import Halogen.VDom.Driver (runUI)
import Prelude
import Type.Proxy (Proxy(..))

data Pair = Pair String String

derive instance genericPair :: Generic Pair _

instance toComponentPair :: ToComponent Pair (QueryGeneric Pair) where
  toComponent = genericToComponent

newtype Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }

derive instance genericPerson :: Generic Person _

instance toComponentPerson :: ToComponent Person (QueryGeneric Person) where
  toComponent = genericToComponent

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui init
  where
  ui = toComponent (Proxy :: Proxy (T11 Unit Boolean Int Char String (Array String) Ordering (Maybe Ordering) Color Pair Person))
  init = unit /\ false /\ 0 /\ 'A' /\ "" /\ [] /\ LT /\ Nothing /\ aqua /\ Pair "" "" /\ Person { firstName: "", lastName: "", age: 0 }
