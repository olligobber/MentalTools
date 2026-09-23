module Main where

import Prelude
    ( Unit
    , pure, unit, bind, const, compare, discard
    , (==), ($), (>>=), (-), (<$>), (<>), (<<<), (>=)
    )
import Data.Array (snoc, mapWithIndex, deleteAt, (!!), modifyAt)
import Data.Either (Either(Left, Right), note)
import Data.Foldable (length, oneOfMap)
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
    , latestError :: Maybe String
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
    , latestError : Nothing
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
    | DismissError

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

renderError :: forall m. String -> H.ComponentHTML Action () m
renderError e = HH.div
    [ HP.id "error"
    , HE.onClick $ const DismissError
    ] [ HH.text $ "Error: " <> e ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [ HP.id "main" ] $
    [ renderLists state.lists
    , renderItems state
    ] <> oneOfMap (pure <<< renderError) state.latestError

handleAction :: forall q m. MonadEffect m =>
    Action -> H.HalogenM State Action () q m Unit
handleAction None = pure unit
handleAction AddItem = H.modify_ $ \state ->
    case do
        index <- note "Cannot add item, no list selected" state.selectedList
        note "Cannot add item, selected list index out of range" $
            modifyAt
                index
                (\list -> list { items = list.items `snoc` state.newItem })
                state.lists
    of
        Left e -> state { latestError = Just e }
        Right newLists -> state
            { lists = newLists
            , newItem = ""
            , latestError = Nothing
            }
handleAction (ChangeNew s) = H.modify_ $ _ { newItem = s }
handleAction (DeleteItem itemIndex) = H.modify_ $ \state ->
    case do
        listIndex <- note "Cannot delete item, no list selected"
            state.selectedList
        oldList <- note "Cannot delete item, selected list index out of range" $
            state.lists !! listIndex
        newItems <- note "Cannot delete item, selected item index out of range" $
            deleteAt itemIndex oldList.items
        note "Cannot delete item, selected list index out of range" $
            modifyAt listIndex (_ { items = newItems }) state.lists
    of
        Left e -> state { latestError = Just e }
        Right newLists -> state
            { lists = newLists
            , latestError = Nothing
            }
handleAction Choose = do
    state <- H.get
    case do
        index <- note "Cannot choose item, no list selected" state.selectedList
        list <- note "Cannot choose item, selected list index out of range" $
            state.lists !! index
        if length list.items == 0 then
            Left "Cannot choose item, list is empty"
        else
            pure $ length list.items
    of
        Left e -> H.modify_ $ _ { latestError = Just e }
        Right n -> do
            i <- liftEffect $ randomInt 0 (n - 1)
            H.modify_ $ _
                { selectedItem = Just i
                , latestError = Nothing
                }
handleAction (SwitchList index) = H.modify_ $ \state ->
    if index >= length state.lists then
        state
            { latestError = Just "Could not switch to list, index out of range" }
    else
        state
            { selectedList = Just index
            , selectedItem = Nothing
            , newName = Nothing
            , latestError = Nothing
            }
handleAction NewList = H.modify_ $ \state -> state
    { lists = state.lists `snoc` newList
    , selectedList = Just $ length state.lists
    , newItem = ""
    , newName = Nothing
    , selectedItem = Nothing
    , latestError = Nothing
    }
handleAction OpenRename = H.modify_ $ \state ->
    case do
        case state.newName of
            Just _ -> Left "Cannot begin rename, rename already in progress!"
            Nothing -> Right unit
        index <- note "Cannot begin rename, no list selected" state.selectedList
        list <- note "Cannot begin rename, selected list index out of range" $
            state.lists !! index
        pure list.name
    of
        Left e -> state { latestError = Just e }
        Right name -> state
            { newName = Just name
            , latestError = Nothing
            }
handleAction (ChangeName new) = H.modify_ $ \state ->
    case state.newName of
        Nothing -> state
            { latestError = Just "Cannot change name, rename has not begun" }
        Just _ -> state { newName = Just new }
handleAction ConfirmRename = H.modify_ $ \state ->
    case do
        index <- note "Cannot confirm rename, no list selected"
            state.selectedList
        newName <- note "Cannot confirm rename, rename not started"
            state.newName
        note "Cannot confirm rename, selected list index out of range" $
            modifyAt index (_ { name = newName }) state.lists
    of
        Left e -> state { latestError = Just e }
        Right newLists -> state
            { lists = newLists
            , newName = Nothing
            , latestError = Nothing
            }
handleAction CancelRename = H.modify_ $ \state ->
    case state.newName of
        Nothing -> state
            { latestError = Just "Cannot cancel rename, rename has not begun " }
        Just _ -> state
            { newName = Nothing
            , latestError = Nothing
            }
handleAction (DeleteList index) = H.modify_ $ \state ->
    case
        note "Cannot delete list, index out of range" $
            deleteAt index state.lists
    of
        Left e -> state { latestError = Just e }
        Right newLists -> state
            { lists = newLists
            , selectedList = case compare index <$> state.selectedList of
                Nothing -> Nothing
                Just EQ -> Nothing
                Just LT -> (_ - 1) <$> state.selectedList
                Just GT -> state.selectedList
            , latestError = Nothing
            }
handleAction DismissError = H.modify_ $ _ { latestError = Nothing }


component :: forall q m. MonadEffect m => H.Component q State Action m
component = H.mkComponent
    { initialState: pure initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component initialState
