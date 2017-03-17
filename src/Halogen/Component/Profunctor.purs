{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Halogen.Component.Profunctor where

import Prelude
import Control.Applicative.Free (hoistFreeAp)
import Control.Monad.Free (hoistFree)
import Data.Newtype (class Newtype, over)
import Data.Profunctor (class Profunctor, lmap)
import Halogen.Component as HC
import Halogen.Query.HalogenM as HM
import Halogen.Query.ForkF as FF

newtype ProComponent h f m i o = ProComponent (HC.Component h f i o m)

derive instance newtypeProComponent ∷ Newtype (ProComponent h f m i o) _

instance profunctorProComponent ∷ Profunctor (ProComponent h f m) where
  dimap = dimapProComponent

dimapProComponent ∷ ∀ h f m i i' o o'. (i' → i) → (o → o') → ProComponent h f m i o → ProComponent h f m i' o'
dimapProComponent f g (ProComponent c) = ProComponent (HC.unComponent go c)
  where
  go ∷ ∀ s g p. HC.Component' h s f g p i o m → HC.Component h f i' o' m
  go comp = HC.mkComponent $ comp
    { initialState = lmap f comp.initialState
    , receiver = lmap f comp.receiver
    , eval = mapOutput g <$> comp.eval
    }

mapOutput ∷ ∀ s f g p o o' m. (o → o') → HM.HalogenM s f g p o m ~> HM.HalogenM s f g p o' m
mapOutput f (HM.HalogenM h) = HM.HalogenM (hoistFree go h)
  where
  go ∷ HM.HalogenF s f g p o m ~> HM.HalogenF s f g p o' m
  go = case _ of
    HM.State s → HM.State s
    HM.Subscribe es next -> HM.Subscribe es next
    HM.Lift q → HM.Lift q
    HM.Halt msg → HM.Halt msg
    HM.GetSlots k → HM.GetSlots k
    HM.CheckSlot p k → HM.CheckSlot p k
    HM.ChildQuery p cq → HM.ChildQuery p cq
    HM.Raise o a → HM.Raise (f o) a
    HM.Par p → HM.Par (over HM.HalogenAp (hoistFreeAp (mapOutput f)) p)
    HM.Fork p → HM.Fork (FF.hoistFork (mapOutput f) p)
    HM.GetRef p k → HM.GetRef p k
