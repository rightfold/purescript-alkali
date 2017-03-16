module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(..))
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.Alkali (toComponent)
import Halogen.VDom.Driver (runUI)
import Prelude
import Type.Proxy (Proxy(..))

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff $ awaitBody >>= runUI ui init
  where
  ui = toComponent (Proxy :: Proxy (Tuple Boolean (Tuple String Unit)))
  init = Tuple false (Tuple "" unit)
