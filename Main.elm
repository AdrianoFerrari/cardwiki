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


port dirty : Bool -> Cmd msg
port execCommand : String -> Cmd msg




-- MODEL

type alias Model =
  { data : List Card
  , visible : List String
  , editing : Maybe EditState
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
        }
      , Cmd.none
      )




-- UPDATE


type Msg
  = NoOp
  -- === Card Creation  ===
  | NewCard
  | AddCard String
  -- === Card Editing  ===
  | SaveCard
  | EditCard String
  | UpdateFieldTitle String
  | UpdateFieldBody String
  -- === Card Visibility  ===
  | OpenCard String
  | CloseCard String
  -- === Ports ===
  | HandleKey String
  -- === Later? ===
  | LinkClicked String
  | DeleteCard String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []
    
    NewCard ->
      update (AddCard "") model

    AddCard title ->
      case model.editing of
        Just _ ->
          model ! []

        Nothing ->
          { model
            | visible = "" :: model.visible
            , editing = Just (EditState "" "" "")
          }
            ! []

    SaveCard ->
      case model.editing of
        Nothing ->
          model ! []

        Just {title, fieldTitle, fieldBody} ->
          case (title, fieldTitle) of
            ( _,  "" ) ->
              model ! []

            ( "", _ ) ->
              { model
                | data = Card fieldTitle fieldBody :: model.data
                , visible = model.visible
                    |> List.map (\v -> if v == title then fieldTitle else v)
                , editing = Nothing
              }
                ! []

            ( _, _ ) ->
              { model
                | data = model.data
                    |> List.map (\c -> if c.title == title then Card fieldTitle fieldBody else c)
                , visible = model.visible
                    |> List.map (\v -> if v == title then fieldTitle else v)
                , editing = Nothing
              }
                ! []

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
            ! []

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

    _ ->
      model ! []




-- SUBSCRIPTIONS


port linkClicked : (String -> msg) -> Sub msg
port shortcut : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ linkClicked LinkClicked
    , shortcut HandleKey
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
    , viewModel model
    ]


-- View: Story (visible cards)

viewStory : Maybe EditState -> List Card -> Html Msg
viewStory editState_ visibleCards =
  let
    isEditing c =
      case editState_ of
        Just {title, fieldTitle, fieldBody} ->
          title == c.title
        Nothing ->
          False

    viewFn c =
      viewCard (isEditing c) c
  in
  div
    [ id "story"
    ]
    ( List.map viewFn visibleCards )



viewCard : Bool -> Card -> Html Msg
viewCard isEditing card =
  case isEditing of
    True ->
      div 
        [ id ("card-" ++ card.title)
        , classList
            [ ("card", True)
            , ("editing", isEditing)
            ]
        ] 
        [ input 
            [ id ("card-title-edit-" ++ card.title)
            , classList [ ("mousetrap", True) 
                        ]
            , defaultValue card.title
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

    False ->
      div 
        [ id ("card-" ++ card.title)
        , classList
            [ ("card", True)
            , ("editing", isEditing)
            ]
        , onDoubleClick (EditCard card.title)
        ] 
        [ text card.title
        , br [][]
        , viewBody card.body 
        , br [][]
        , button [onClick (CloseCard card.title)][text "close"]
        , button [onClick (EditCard card.title)][text "edit"]
        , button [onClick (DeleteCard card.title)][text "delete"]
        ]


viewBody : String -> Html Msg
viewBody str =
  let
    matchToLink {match} =
      let m = match |> String.dropLeft 2 |> String.dropRight 2 in
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
