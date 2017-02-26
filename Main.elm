port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListExtra
import Regex as R
import Tuple exposing (first, second)
import Markdown
import Task
import Dom


main : Program (Maybe Model) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port dirtyToJS : Bool -> Cmd msg
port execCommand : String -> Cmd msg




-- MODEL

type alias Model =
  { data : List Card
  , visible : List String
  , editing : Maybe EditState
  , dirty : Bool
  }


type alias EditState =
  { title : String
  , fieldTitle : String
  , fieldBody : String
  }


type alias Card =
  { title : String
  , body : String
  }


init : Maybe Model -> (Model, Cmd Msg)
init savedState =
  case savedState of
    Just model ->
      model ! []

    Nothing ->
      ( { data = [Card "test1" "test content", Card "test2" "test content 2"]
        , visible = ["test1", "test2"]
        , editing = Nothing
        , dirty = False
        }
      , Cmd.none
      )




-- UPDATE


type Msg
  = NoOp
  -- === Card Creation  ===
  | NewCard
  | AddCard String
  | LinkClicked String
  -- === Card Edit/Delete  ===
  | SaveCard
  | EditCard String
  | UpdateFieldTitle String
  | UpdateFieldBody String
  | DeleteCard String
  -- === Card Visibility  ===
  | OpenCard String
  | CloseCard String
  -- === Ports ===
  | HandleKey String
  | Dirty Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []
    
    NewCard ->
      update (AddCard "") model

    AddCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data
      in
      case (card_, model.editing) of
        (Nothing, Nothing) ->
          { model
            | visible = "" :: model.visible
            , editing = Just (EditState "" title "")
          }
            ! [ dirtyToJS True
              , focus 
                ( if title == "" then "card-title-edit-"
                  else "card-body-edit-"
                )
              , execCommand "selectAll"
              ]

        _ ->
          model ! []

    LinkClicked title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        cardVisible_ =
          ListExtra.find (\v -> v == title) model.visible
      in
      case (card_, cardVisible_) of
        ( Just card, Nothing ) ->
          update (OpenCard title) model

        ( Nothing, _ ) ->
          update (AddCard title) model

        _ ->
          model ! []

    SaveCard ->
      case model.editing of
        Nothing ->
          model ! []

        Just {title, fieldTitle, fieldBody} ->
          case (title, fieldTitle) of
            -- empty titles not allowed
            ( _,  "" ) ->
              model ! []

            -- new card saved from draft
            ( "", _ ) ->
              { model
                | data = Card (fieldTitle |> String.trim) fieldBody :: model.data
                , visible = model.visible
                    |> List.map (\v -> if v == title then fieldTitle else v)
                , editing = Nothing
              }
                ! [ dirtyToJS True ]

            -- existing card modified
            ( _, _ ) ->
              { model
                | data = model.data
                    |> List.map (\c -> if c.title == title then Card fieldTitle fieldBody else c)
                , visible = model.visible
                    |> List.map (\v -> if v == title then fieldTitle else v)
                , editing = Nothing
              }
                ! [ dirtyToJS True ]

    EditCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data
      in
      case (card_, model.editing) of
        (Just card, Nothing) ->
          { model
            | editing = Just (EditState title card.title card.body)
          }
            ! [ focus ("card-body-edit-" ++ title)
              ]

        _ ->
          model ! []

    UpdateFieldTitle fieldTitleNew ->
      case model.editing of
        Nothing ->
          model ! []

        Just editState ->
          { model
            | editing = Just {editState | fieldTitle = fieldTitleNew}
          }
            ! []

    UpdateFieldBody fieldBodyNew ->
      case model.editing of
        Nothing ->
          model ! []

        Just editState ->
          { model
            | editing = Just {editState | fieldBody = fieldBodyNew}
          }
            ! []

    DeleteCard title ->
      let
        newEditState =
          case model.editing of
            Just editState ->
              if editState.title == title then 
                Nothing 
              else 
                Just editState

            Nothing -> Nothing
      in
      { model
        | data = List.filter (\c -> c.title /= title) model.data
        , visible = List.filter (\v -> v /= title) model.visible
        , editing = newEditState
      }
        ! []

    OpenCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        cardVisible_ =
          ListExtra.find (\v -> v == title) model.visible
      in
      case (card_, cardVisible_) of
        (Just card, Nothing) ->
          { model
            | visible = card.title :: model.visible
          }
            ! []

        _ ->
          model ! []

    CloseCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        cardVisible_ =
          ListExtra.find (\v -> v == title) model.visible
      in
      case (card_, cardVisible_) of
        (Just card, Just cardVisible) ->
          { model
            | visible = List.filter (\v -> v /= title) model.visible
          }
            ! []

        _ ->
          model ! []

    HandleKey str ->
      case str of
        "mod+enter" ->
          case model.editing of
            Just editState ->
              update SaveCard model

            Nothing ->
              model ! []

        "mod+option+n" ->
          normalMode NewCard model

        "mod+x" ->
          (Debug.log "model" model) ! []

        _ ->
          model ! []

    Dirty bool ->
      { model
        | dirty = bool
      }
        ! []



-- SUBSCRIPTIONS


port linkClicked : (String -> msg) -> Sub msg
port shortcut : (String -> msg) -> Sub msg
port dirty : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ linkClicked LinkClicked
    , shortcut HandleKey
    , dirty Dirty
    ]




-- VIEW


view : Model -> Html Msg
view model =
  div
    [ id "main" ]
    [ div
       [ id "app"]
       [ viewContents model.data
       , viewStory model.editing (getStory model.data model.visible)
       ]
    , div [][text (if model.dirty then "dirty!" else "")]
    , viewModel model
    ]


-- View: Story (visible cards)

viewStory : Maybe EditState -> List Card -> Html Msg
viewStory editState_ visibleCards =
  div
    [ id "story"
    ]
    ( List.map (viewCard editState_) visibleCards )



viewCard : Maybe EditState -> Card -> Html Msg
viewCard editState_ card =
  let
    viewTemplate =
      div 
        [ id ("card-" ++ card.title)
        , classList
            [ ("card", True)
            , ("editing", False)
            ]
        , onDoubleClick (EditCard card.title)
        ] 
        [ h2 [][ text card.title ]
        , viewBody card.body 
        , br [][]
        , button [onClick (CloseCard card.title)][text "close"]
        , button [onClick (EditCard card.title)][text "edit"]
        , button [onClick (DeleteCard card.title)][text "delete"]
        ]

    editTemplate fieldTitle =
      div 
        [ id ("card-" ++ card.title)
        , classList
            [ ("card", True)
            , ("editing", True)
            ]
        ] 
        [ input 
            [ id ("card-title-edit-" ++ card.title)
            , classList [ ("mousetrap", True) 
                        ]
            , defaultValue fieldTitle
            , onInput UpdateFieldTitle
            ]
            []
        , br [][]
        , textarea 
            [ id ("card-body-edit-" ++ card.title)
            , classList [ ("mousetrap", True) 
                        ]
            , defaultValue card.body
            , onInput UpdateFieldBody
            ]
            []
        , button
            [ onClick SaveCard ]
            [ text "save"]
        ]
  in
  case editState_ of
    Just {title, fieldTitle, fieldBody} ->
      if card.title == title then
        editTemplate fieldTitle
      else
        viewTemplate

    Nothing ->
      viewTemplate


viewBody : String -> Html Msg
viewBody str =
  let
    matchToLink {match} =
      let m = match |> String.dropLeft 2 |> String.dropRight 2 |> String.trim in
      String.join "" 
        [ "<a href=\"javascript:linkClicked('" , m , "')\">", m, "</a>" ]

    parsedWikiLinks =
      R.replace R.All
        (R.regex "\\[\\[([^\\]]+)\\]\\]")
        matchToLink
        str
  in
  Markdown.toHtml [] parsedWikiLinks



-- View: Contents List

viewContents : List Card -> Html Msg
viewContents cards =
  div
    [ id "content" ]
    [ button [onClick NewCard] [text "+"]
    , ul
        [ ]
        ( List.map viewCardItem cards )
    ]


viewCardItem : Card -> Html Msg
viewCardItem card =
  li 
    [ id ("card-item-" ++ card.title)
    , classList [ ("card-item", True)
                ]
    , onClick (OpenCard card.title)
    ] 
    [ text card.title ]


-- View: Model Data 

viewModel : Model -> Html Msg
viewModel model =
  div
    [ id "model"
    ]
    [ div [ id "model-data" ] 
        (viewListCardData model.data)
    , div [ id "model-visible" ]
        (List.map (\v -> div [ attribute "title" v ][]) model.visible)
    ]


viewListCardData : List Card -> List (Html Msg)
viewListCardData cards =
  let
    viewCardData card =
      div 
        [ attribute "title" card.title ] 
        [ pre [][text card.body] ]
  in
  List.map viewCardData cards




-- HELPERS

getStory : List Card -> List String -> List Card
getStory cards visible =
  let
    mapFn v =
      if v == "" then
        Just (Card "" "")
      else
        ListExtra.find (\c -> c.title == v) cards
  in


  visible
    |> List.filterMap mapFn


relativeGetCard : Int -> List Card -> String -> Maybe String
relativeGetCard delta cards title =
  let
    currIndex_ =
      ListExtra.findIndex (\c -> c.title == title) cards
  in
  case currIndex_ of
    Just currIndex ->
      ListExtra.getAt (currIndex + delta) cards
        |> Maybe.map (\c -> c.title)

    Nothing ->
      Nothing
      

getNext : List Card -> String -> Maybe String
getNext cards title =
  relativeGetCard 1 cards title


getPrev : List Card -> String -> Maybe String
getPrev cards title =
  relativeGetCard (-1) cards title


normalMode : Msg -> Model -> (Model, Cmd Msg)
normalMode msg model =
  case model.editing of
    Just title ->
      model ! []

    Nothing ->
      update msg model


focus : String -> Cmd Msg
focus elemId =
  Task.attempt (\_ -> NoOp) (Dom.focus elemId)
