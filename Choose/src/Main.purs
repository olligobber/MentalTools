module Main where

import Prelude
    ( Unit
    , pure, unit, bind, const, compare
    , (==), ($), (>>=), (-), (<$>)
    )
import Data.Array (snoc, mapWithIndex, deleteAt, (!!), modifyAt)
import Data.Foldable (length)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Ordering (Ordering(EQ, LT, GT))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type List =
    { name :: String
    , items :: Array String
    }

type State =
    { lists :: Array List
    , selectedList :: Maybe Int
    , newItem :: String
    , newName :: Maybe String
    , selectedItem :: Maybe Int
    }

newList :: List
newList =
    { name : "My List"
    , items : []
    }

initialState :: State
initialState =
    { lists : [ newList ]
    , selectedList : Just 0
    , newItem : ""
    , newName : Nothing
    , selectedItem : Nothing
    }

data Action
    = None
    | AddItem
    | ChangeNew String
    | DeleteItem Int
    | Choose
    | SwitchList Int
    | NewList
    | OpenRename
    | ChangeName String
    | ConfirmRename
    | CancelRename
    | DeleteList Int

renderList :: forall m. Int -> String -> H.ComponentHTML Action () m
renderList index name = HH.div
    [ HP.class_ $ HH.ClassName "list" ]
    [ HH.button
        [ HE.onClick $ const $ SwitchList index
        , HP.class_ $ HH.ClassName "changelist"
        ] [ HH.text name ]
    , HH.button
        [ HE.onClick $ const $ DeleteList index
        , HP.class_ $ HH.ClassName "delete"
        ] [ HH.text "🗑️" ]
    ]

renderLists :: forall m. Array List -> H.ComponentHTML Action () m
renderLists lists = HH.div
    [ HP.class_ $ HH.ClassName "lists" ] $
        (mapWithIndex renderList $ _.name <$> lists) `snoc`
        ( HH.button
            [ HE.onClick $ const $ NewList
            , HP.class_ $ HH.ClassName "newlist"
            ] [ HH.text "Add List" ]
        )

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
        [ HE.onClick $ const $ DeleteItem index
        , HP.class_ $ HH.ClassName "delete"
        ] [ HH.text "🗑️" ]
    ]

renderItems :: forall m. State -> H.ComponentHTML Action () m
renderItems state = case mlist of
    Nothing -> HH.div
        [ HP.class_ $ HH.ClassName "nolist" ]
        [ HH.text "No open list" ]
    Just list -> HH.div [ HP.class_ $ HH.ClassName "openlist" ]
        [ case state.newName of
            Nothing -> HH.div [ HP.id "listname" ]
                [ HH.text list.name
                , HH.button
                    [ HE.onClick $ const OpenRename
                    , HP.id "openeditname"
                    ] [ HH.text "✏️" ]
                ]
            Just new -> HH.div [ HP.id "listname" ]
                [ HH.input
                    [ HE.onValueChange ChangeName
                    , HP.value new
                    , HP.id "listname"
                    ]
                , HH.button
                    [ HE.onClick $ const ConfirmRename
                    , HP.id "confirmrename"
                    ] [ HH.text "Done" ]
                , HH.button
                    [ HE.onClick $ const CancelRename
                    , HP.id "cancelrename"
                    ] [ HH.text "Cancel" ]
                ]
        , HH.div [ HP.id "itemlist" ] $
            mapWithIndex (renderItem state.selectedItem) list.items
        , HH.input
            [ HE.onValueChange ChangeNew
            , HP.value state.newItem
            , HP.id "newitem"
            ]
        , HH.button
            [ HE.onClick $ const AddItem
            , HP.id "additem"
            ] [ HH.text "+" ]
        , HH.button
            [ HE.onClick $ const Choose
            , HP.id "choose"
            ] [ HH.text "Choose!" ]
        ]
    where
        mlist = do
            index <- state.selectedList
            state.lists !! index

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [ HP.id "main" ]
    [ renderLists state.lists
    , renderItems state
    ]

handleAction :: forall q m. MonadEffect m =>
    Action -> H.HalogenM State Action () q m Unit
handleAction None = pure unit
handleAction AddItem = H.modify_ $ \state -> state
    { lists = fromMaybe state.lists $ do
        index <- state.selectedList
        modifyAt index
            (\list -> list { items = list.items `snoc` state.newItem })
            state.lists
    , newItem = ""
    }
handleAction (ChangeNew s) = H.modify_ $ _ { newItem = s }
handleAction (DeleteItem itemIndex) = H.modify_ $ \state -> state
    { lists = fromMaybe state.lists $ do
        listIndex <- state.selectedList
        oldList <- state.lists !! listIndex
        newItems <- deleteAt itemIndex oldList.items
        modifyAt listIndex (_ { items = newItems }) state.lists
    }
handleAction Choose = do
    state <- H.get
    let
        listLen = do
            index <- state.selectedList
            list <- state.lists !! index
            if length list.items == 0 then Nothing else Just (length list.items)
    case listLen of
        Nothing -> pure unit
        Just n -> do
            i <- liftEffect $ randomInt 0 (n - 1)
            H.modify_ $ _ { selectedItem = Just i }
handleAction (SwitchList index) = H.modify_ $ _
    { selectedList = Just index
    , selectedItem = Nothing
    , newName = Nothing
    }
handleAction NewList = H.modify_ $ \state -> state
    { lists = state.lists `snoc` newList
    , selectedList = Just $ length state.lists
    , newItem = ""
    , newName = Nothing
    , selectedItem = Nothing
    }
handleAction OpenRename = H.modify_ $ \state -> state
    { newName = do
        index <- state.selectedList
        list <- state.lists !! index
        pure list.name
    }
handleAction (ChangeName new) = H.modify_ $ _ { newName = Just new }
handleAction ConfirmRename = H.modify_ $ \state -> state
    { lists = fromMaybe state.lists $ do
        index <- state.selectedList
        newName <- state.newName
        modifyAt index (_ { name = newName }) state.lists
    , newName = Nothing
    }
handleAction CancelRename = H.modify_ $ _ { newName = Nothing }
handleAction (DeleteList index) = H.modify_ $ \state -> state
    { lists = fromMaybe state.lists $ deleteAt index state.lists
    , selectedList = case compare index <$> state.selectedList of
        Nothing -> Nothing
        Just EQ -> Nothing
        Just LT -> (_ - 1) <$> state.selectedList
        Just GT -> state.selectedList
    }


component :: forall q m. MonadEffect m => H.Component q State Action m
component = H.mkComponent
    { initialState: pure initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component initialState
