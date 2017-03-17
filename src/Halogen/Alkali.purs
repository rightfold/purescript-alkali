module Halogen.Alkali
  ( class Initial
  , initial

  , class ToComponent
  , toComponent

  , QueryVoid

  , QueryUnit

  , QueryBoolean

  , QueryInt

  , QueryChar

  , QueryString

  , QueryArray

  , QueryOrdering

  , QueryTuple

  , QueryMaybe

  , QueryColor

  , QueryGeneric
  , genericToComponent
  , class GenericToComponent
  , genericToComponent'
  , class GenericToComponentArgument
  , genericToComponentArgument'
  ) where

import Color (Color)
import Color as Color
import Control.Monad.State.Class as State
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic.Rep as G
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Halogen.Component (Component, ComponentDSL, ComponentHTML, ParentDSL, ParentHTML, component, parentComponent)
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (action, raise)
import Prelude
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

class Initial a where
  initial :: a

instance initialUnit     :: Initial Unit      where initial = unit
instance initialBoolean  :: Initial Boolean   where initial = false
instance initialInt      :: Initial Int       where initial = 0
instance initialString   :: Initial String    where initial = ""
instance initialArray    :: Initial (Array a) where initial = []
instance initialOrdering :: Initial Ordering  where initial = EQ

--------------------------------------------------------------------------------

class ToComponent a q | a -> q where
  toComponent :: ∀ m. Proxy a -> Component HTML q a a m

--------------------------------------------------------------------------------

newtype QueryVoid a = QueryVoid Void

instance toComponentVoid :: ToComponent Void QueryVoid where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Void -> Void
    initialState = absurd

    render :: Void -> ComponentHTML QueryVoid
    render = absurd

    eval :: ∀ m. QueryVoid ~> ComponentDSL Void QueryVoid Void m
    eval (QueryVoid void) = absurd void

    receiver :: Void -> Maybe (QueryVoid Unit)
    receiver = const Nothing

--------------------------------------------------------------------------------

newtype QueryUnit a = QueryUnit Void

instance toComponentUnit :: ToComponent Unit QueryUnit where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Unit -> Unit
    initialState = id

    render :: Unit -> ComponentHTML QueryUnit
    render = const $ H.span [] []

    eval :: ∀ m. QueryUnit ~> ComponentDSL Unit QueryUnit Unit m
    eval (QueryUnit void) = absurd void

    receiver :: Unit -> Maybe (QueryUnit Unit)
    receiver = const Nothing

--------------------------------------------------------------------------------

data QueryBoolean a
  = ReceiveBoolean Boolean a
  | ChangeBoolean Boolean a

instance toComponentBoolean :: ToComponent Boolean QueryBoolean where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Boolean -> Boolean
    initialState = id

    render :: Boolean -> ComponentHTML QueryBoolean
    render value =
      H.input [ P.type_ P.InputCheckbox
              , P.checked value
              , E.onChecked (E.input ChangeBoolean)
              ]

    eval :: ∀ m. QueryBoolean ~> ComponentDSL Boolean QueryBoolean Boolean m
    eval (ReceiveBoolean value next) = next <$ State.put value
    eval (ChangeBoolean value next) = do
      State.put value
      raise value
      pure next

    receiver :: Boolean -> Maybe (QueryBoolean Unit)
    receiver = E.input ReceiveBoolean

--------------------------------------------------------------------------------

data QueryInt a
  = ReceiveInt Int a
  | ChangeInt String a

instance toComponentInt :: ToComponent Int QueryInt where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Int -> Int
    initialState = id

    render :: Int -> ComponentHTML QueryInt
    render value =
      H.input [ P.type_ P.InputNumber
              , P.value (show value)
              , E.onValueChange (E.input ChangeInt)
              ]

    eval :: ∀ m. QueryInt ~> ComponentDSL Int QueryInt Int m
    eval (ReceiveInt value next) = next <$ State.put value
    eval (ChangeInt valueS next) = do
      case Int.fromString valueS of
        Just value -> do
          State.put value
          raise value
        Nothing -> pure unit
      pure next

    receiver :: Int -> Maybe (QueryInt Unit)
    receiver = E.input ReceiveInt

--------------------------------------------------------------------------------

data QueryChar a
  = ReceiveChar Char a
  | ChangeChar String a

instance toComponentChar :: ToComponent Char QueryChar where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Char -> Char
    initialState = id

    render :: Char -> ComponentHTML QueryChar
    render value =
      H.input [ P.type_ P.InputText
              , P.value (String.singleton value)
              , E.onValueChange (E.input ChangeChar)
              ]

    eval :: ∀ m. QueryChar ~> ComponentDSL Char QueryChar Char m
    eval (ReceiveChar value next) = next <$ State.put value
    eval (ChangeChar valueS next) = do
      case String.charAt 0 valueS of
        Just value -> do
          State.put value
          raise value
        Nothing -> pure unit
      pure next

    receiver :: Char -> Maybe (QueryChar Unit)
    receiver = E.input ReceiveChar

--------------------------------------------------------------------------------

data QueryString a
  = ReceiveString String a
  | ChangeString String a

instance toComponentString :: ToComponent String QueryString where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: String -> String
    initialState = id

    render :: String -> ComponentHTML QueryString
    render value =
      H.input [ P.type_ P.InputText
              , P.value value
              , E.onValueChange (E.input ChangeString)
              ]

    eval :: ∀ m. QueryString ~> ComponentDSL String QueryString String m
    eval (ReceiveString value next) = next <$ State.put value
    eval (ChangeString value next) = do
      State.put value
      raise value
      pure next

    receiver :: String -> Maybe (QueryString Unit)
    receiver = E.input ReceiveString

--------------------------------------------------------------------------------

data QueryArray a n
  = ReceiveArray (Array a) n
  | ChangeArray Int a n
  | AddArray n
  | DeleteArray Int n

type SlotArray = Int

instance toComponentArray :: (Initial a, ToComponent a aq) => ToComponent (Array a) (QueryArray a) where
  toComponent _ = parentComponent { initialState, render, eval, receiver }
    where
    initialState :: Array a -> Array a
    initialState = id

    render :: ∀ m. Array a -> ParentHTML (QueryArray a) aq SlotArray m
    render value =
      H.div []
        [ H.ul [] (Array.mapWithIndex slot value)
        , H.button [E.onClick (E.input_ AddArray)] [H.text "+"]
        ]
      where
      slot i x =
        H.li []
          [ H.slot i (toComponent (Proxy :: Proxy a)) x (handle i)
          , H.button [E.onClick (E.input_ (DeleteArray i))] [H.text "−"]
          ]
      handle i = Just <<< action <<< ChangeArray i

    eval :: ∀ m. QueryArray a ~> ParentDSL (Array a) (QueryArray a) aq SlotArray (Array a) m
    eval (ReceiveArray value next) = next <$ State.put value
    eval (ChangeArray i value next) =
      next <$ modify \a -> Array.updateAt i value a # fromMaybe a
    eval (AddArray next) = next <$ modify (_ <> [initial])
    eval (DeleteArray i next) =
      next <$ modify \a -> Array.deleteAt i a # fromMaybe a

    modify :: ∀ m. (Array a -> Array a) -> ParentDSL (Array a) (QueryArray a) aq SlotArray (Array a) m Unit
    modify f = State.modify f *> State.get >>= raise

    receiver :: Array a -> Maybe (QueryArray a Unit)
    receiver = E.input ReceiveArray

--------------------------------------------------------------------------------

data QueryOrdering a
  = ReceiveOrdering Ordering a
  | ChangeOrdering String a

instance toComponentOrdering :: ToComponent Ordering QueryOrdering where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Ordering -> Ordering
    initialState = id

    render :: Ordering -> ComponentHTML QueryOrdering
    render value =
      H.select [E.onValueChange (E.input ChangeOrdering)]
        [option LT "<", option EQ "=", option GT ">"]
      where option o t =
              H.option [ P.value (show o)
                       , P.selected (o == value)
                       ]
                [H.text t]

    eval :: ∀ m. QueryOrdering ~> ComponentDSL Ordering QueryOrdering Ordering m
    eval (ReceiveOrdering value next) = next <$ State.put value
    eval (ChangeOrdering valueS next) = do
      let value = case valueS of
            "LT" -> LT
            "EQ" -> EQ
            _    -> GT
      State.put value
      raise value
      pure next

    receiver :: Ordering -> Maybe (QueryOrdering Unit)
    receiver = E.input ReceiveOrdering

--------------------------------------------------------------------------------

data QueryTuple f s a
  = ReceiveTuple (Tuple f s) a
  | ChangeTuple (Tuple f s -> Tuple f s) a

type ChildQueryTuple fq sq = Coproduct fq (Coproduct sq (Const Void))

type SlotTuple = Either Unit (Either Unit Void)

instance toComponentTuple :: (ToComponent f fq, ToComponent s sq)
                          => ToComponent (Tuple f s) (QueryTuple f s) where
  toComponent _ = parentComponent { initialState, render, eval, receiver }
    where
    initialState :: Tuple f s -> Tuple f s
    initialState = id

    render :: ∀ m. Tuple f s -> ParentHTML (QueryTuple f s) (ChildQueryTuple fq sq) SlotTuple m
    render value =
      H.div []
        [ H.slot' cp1 unit (toComponent (Proxy :: Proxy f)) (fst value) handleFst
        , H.slot' cp2 unit (toComponent (Proxy :: Proxy s)) (snd value) handleSnd
        ]
      where
      handleFst f = Just <<< action $ ChangeTuple \(Tuple _ s) -> Tuple f s
      handleSnd s = Just <<< action $ ChangeTuple \(Tuple f _) -> Tuple f s

    eval :: ∀ m. QueryTuple f s ~> ParentDSL (Tuple f s) (QueryTuple f s) (ChildQueryTuple fq sq) SlotTuple (Tuple f s) m
    eval (ReceiveTuple value next) = next <$ State.put value
    eval (ChangeTuple transform next) = do
      old <- State.get
      let new = transform old
      State.put new
      raise new
      pure next

    receiver :: Tuple f s -> Maybe (QueryTuple f s Unit)
    receiver = E.input ReceiveTuple

--------------------------------------------------------------------------------

data QueryMaybe a n
  = ReceiveMaybe (Maybe a) n
  | ChangeMaybe (Maybe a) n
  | ToggleMaybe Boolean n

type SlotMaybe = Unit

instance toComponentMaybe :: (Initial a, ToComponent a aq) => ToComponent (Maybe a) (QueryMaybe a) where
  toComponent _ = parentComponent { initialState, render, eval, receiver }
    where
    initialState :: Maybe a -> Maybe a
    initialState = id

    render :: ∀ m. Maybe a -> ParentHTML (QueryMaybe a) aq SlotMaybe m
    render value =
      H.div [] $
        [ H.input [ P.type_ P.InputCheckbox
                  , P.checked (isJust value)
                  , E.onChecked (E.input ToggleMaybe)
                  ]
        ] <> slot
      where
      slot = case value of
        Just v -> [H.slot unit (toComponent (Proxy :: Proxy a)) v handle]
        Nothing -> []
      handle = Just <<< action <<< ChangeMaybe <<< Just

    eval :: ∀ m. QueryMaybe a ~> ParentDSL (Maybe a) (QueryMaybe a) aq SlotMaybe (Maybe a) m
    eval (ReceiveMaybe value next) = next <$ State.put value
    eval (ChangeMaybe value next) = do
      State.put value
      raise value
      pure next
    eval (ToggleMaybe value next) =
      if value
        then eval $ ChangeMaybe (Just initial) next
        else eval $ ChangeMaybe Nothing next

    receiver :: Maybe a -> Maybe (QueryMaybe a Unit)
    receiver = E.input ReceiveMaybe

--------------------------------------------------------------------------------

data QueryColor a
  = ReceiveColor Color a
  | ChangeColor String a

instance toComponentColor :: ToComponent Color QueryColor where
  toComponent _ = component { initialState, render, eval, receiver }
    where
    initialState :: Color -> Color
    initialState = id

    render :: Color -> ComponentHTML QueryColor
    render value =
      H.input [ P.type_ P.InputColor
              , P.value (Color.toHexString value)
              , E.onValueChange (E.input ChangeColor)
              ]

    eval :: ∀ m. QueryColor ~> ComponentDSL Color QueryColor Color m
    eval (ReceiveColor value next) = next <$ State.put value
    eval (ChangeColor valueS next) = do
      case Color.fromHexString valueS of
        Just value -> do
          State.put value
          raise value
        Nothing -> pure unit
      pure next

    receiver :: Color -> Maybe (QueryColor Unit)
    receiver = E.input ReceiveColor

--------------------------------------------------------------------------------

data QueryGeneric a n
  = ReceiveGeneric a n
  | ChangeGeneric a n

genericToComponent
  :: ∀ a r m
   . ( G.Generic a r
     , GenericToComponent a r
     )
  => Proxy a
  -> Component HTML (QueryGeneric a) a a m
genericToComponent _ = genericToComponent' (Proxy :: Proxy r)

class GenericToComponent a r where
  genericToComponent' :: ∀ m. Proxy r -> Component HTML (QueryGeneric a) a a m

class GenericToComponentArgument r where
  genericToComponentArgument' :: ∀ m. Proxy r -> Component HTML (QueryGeneric r) r r m

instance genericToComponentArgumentArgument :: (ToComponent a aq)
                                            => GenericToComponentArgument (G.Argument a) where
  genericToComponentArgument' _ =
    parentComponent { initialState, render, eval, receiver }
    where
    initialState :: G.Argument a -> G.Argument a
    initialState = id

    render :: ∀ m. G.Argument a -> ParentHTML (QueryGeneric (G.Argument a)) aq Unit m
    render (G.Argument value) =
      H.slot unit (toComponent (Proxy :: Proxy a)) value handle
      where handle = E.input $ ChangeGeneric <<< G.Argument

    eval :: ∀ m. QueryGeneric (G.Argument a) ~> ParentDSL (G.Argument a) (QueryGeneric (G.Argument a)) aq Unit (G.Argument a) m
    eval (ReceiveGeneric value next) = next <$ State.put value
    eval (ChangeGeneric value next) = do
      State.put value
      raise value
      pure next

    receiver :: G.Argument a -> Maybe (QueryGeneric (G.Argument a) Unit)
    receiver = E.input ReceiveGeneric
