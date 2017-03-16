module Main
  ( main
  ) where

import Color (Color)
import Color.Scheme.X11 (aqua)
import Control.Monad.Eff (Eff)
import Data.Tuple.Nested (T7, (/\))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Alkali (toComponent)
import Halogen.VDom.Driver (runUI)
import Prelude
import Type.Proxy (Proxy(..))

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui init
  where
  ui = toComponent (Proxy :: Proxy (T7 Unit Boolean Int Char String Ordering Color))
  init = unit /\ false /\ 0 /\ 'A' /\ "" /\ LT /\ aqua
