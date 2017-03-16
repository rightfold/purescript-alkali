module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Data.Tuple.Nested (T4, (/\))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Alkali (toComponent)
import Halogen.VDom.Driver (runUI)
import Prelude
import Type.Proxy (Proxy(..))

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui init
  where
  ui = toComponent (Proxy :: Proxy (T4 Unit Boolean Int String))
  init = unit /\ false /\ 0 /\ ""
