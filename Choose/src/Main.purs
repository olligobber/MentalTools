module Main where

import Prelude (Unit, pure, unit, bind, const, (==), ($), (>>=), (-))
import Data.Array (snoc, mapWithIndex, deleteAt)
import Data.Foldable (length)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State =
    { items :: Array String
    , new :: String
    , selected :: Maybe Int
    }

initialState :: State
initialState =
    { items : []
    , new : ""
    , selected: Nothing
    }

data Action
    = AddNew
    | ChangeNew String
    | Delete Int
    | Choose
    | None

renderItem :: forall m. Maybe Int -> Int -> String -> H.ComponentHTML Action () m
renderItem selected index item = HH.div
    [ HP.classes $
        if selected == Just index then
            [HH.ClassName "item", HH.ClassName "selected"]
        else
            [HH.ClassName "item"]
    ]
    [ HH.text item
    , HH.button
        [ HE.onClick $ const $ Delete index
        , HP.class_ $ HH.ClassName "delete"
        ] [ HH.text "🗑️" ]
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [ HP.id "main" ]
    [ HH.div [ HP.id "itemlist" ] $
        mapWithIndex (renderItem state.selected) state.items
    , HH.input
        [ HE.onValueChange $ ChangeNew
        , HP.value state.new
        , HP.id "newitem"
        ]
    , HH.button
        [ HE.onClick $ const AddNew
        , HP.id "addnew"
        ] [ HH.text "+" ]
    , HH.button
        [ HE.onClick $ const Choose
        , HP.id "choose"
        ] [ HH.text "Choose!" ]
    ]

handleAction :: forall q m. MonadEffect m =>
    Action -> H.HalogenM State Action () q m Unit
handleAction AddNew = H.modify_ $ \state -> state
    { items = state.items `snoc` state.new
    , new = ""
    }
handleAction (ChangeNew s) = H.modify_ $ _ { new = s }
handleAction (Delete i) = H.modify_ $ \state -> state
    { items = fromMaybe state.items $ deleteAt i state.items
    }
handleAction Choose = do
    state <- H.get
    i <- liftEffect $ randomInt 0 (length state.items - 1)
    H.modify_ $ _ { selected = Just i }
handleAction None = pure unit

component :: forall q m. MonadEffect m => H.Component q State Action m
component = H.mkComponent
    { initialState: pure initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component initialState
