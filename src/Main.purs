module Main
  ( main
  ) where

import Color (Color)
import Color.Scheme.X11 (aqua)
import Control.Monad.Eff (Eff)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (T10, (/\))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Alkali (class ToComponent, QueryGeneric, genericToComponent, toComponent)
import Halogen.VDom.Driver (runUI)
import Prelude
import Type.Proxy (Proxy(..))

data Person = Person String String Int

derive instance genericPerson :: Generic Person _

instance toComponentPerson :: ToComponent Person (QueryGeneric Person) where
  toComponent = genericToComponent

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui init
  where
  ui = toComponent (Proxy :: Proxy (T10 Unit Boolean Int Char String (Array String) Ordering (Maybe Ordering) Color Person))
  init = unit /\ false /\ 0 /\ 'A' /\ "" /\ [] /\ LT /\ Nothing /\ aqua /\ Person "" "" 0
