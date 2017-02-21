port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListExtra
import Regex as R
import Tuple exposing (first, second)
import Markdown


main : Program (Maybe Model) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- MODEL

type alias Model =
  { data : List Card
  , story : List Card
  , fieldTitle : String
  , fieldBody : String
  , editing: Maybe String
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
        , story = [Card "test1" "test content", Card "test2" "test content 2"]
        , fieldTitle = ""
        , fieldBody = ""
        , editing = Nothing
        }
      , Cmd.none
      )




-- UPDATE


type Msg
  = NoOp
  | LinkClicked String
  | OpenCard String
  | CloseCard String
  | EditCard String
  | UpdateCard String
  | DeleteCard String
  | UpdateFieldTitle String
  | UpdateFieldBody String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    LinkClicked title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data
      in
      case card_ of
        Nothing ->
          { model 
            | story = Card title "" :: model.story 
            , fieldTitle = title
            , fieldBody = ""
            , editing = Just title
          }
          ! []

        Just card ->
          model ! []

    OpenCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        cardVisible_ =
          ListExtra.find (\c -> c.title == title) model.story
      in
      case (card_, cardVisible_) of
        (Just card, Just cardVisible) ->
          model ! [] -- activate

        (Just card, Nothing) ->
          { model
            | story = card :: model.story
          }
            ! []

        (Nothing, Just cardVisible) ->
          model ! [] -- focus/edit

        (Nothing, Nothing) ->
          model ! []

    CloseCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        cardVisible_ =
          ListExtra.find (\c -> c.title == title) model.story
      in
      case (card_, cardVisible_) of
        (Just card, Just cardVisible) ->
          { model
            | story = List.filter (\c -> c.title /= title) model.story
          } 
            ! []

        (Just card, Nothing) ->
          model ! []

        (Nothing, Just cardVisible) ->
          model ! []

        (Nothing, Nothing) ->
          model ! []


    EditCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data
      in
      case card_ of
        Nothing ->
          model ! []

        Just card ->
          { model
            | fieldBody = card.body
            , fieldTitle = title
            , editing = Just title
          }
            ! []

    UpdateCard title ->
      let
        card_ =
          ListExtra.find (\c -> c.title == title) model.data

        updateCard c =
          if c.title == title then
            { c 
              | title = model.fieldTitle
              , body = model.fieldBody 
            }
          else
            c
      in
      case card_ of
        Nothing ->
          { model 
            | data = Card model.fieldTitle model.fieldBody :: model.data
            , story = List.map updateCard model.story
            , fieldTitle = ""
            , fieldBody = ""
            , editing = Nothing
          }
            ! []

        Just card ->
          { model
            | data = List.map updateCard model.data
            , story = List.map updateCard model.story
            , fieldTitle = ""
            , fieldBody = ""
            , editing = Nothing
          }
            ! []

    DeleteCard title ->
      let
        filterFn c =
          c.title /= title
      in
      { model
        | data = List.filter filterFn model.data
        , story = List.filter filterFn model.story
      } 
        ! []

    UpdateFieldTitle title ->
      { model
        | fieldTitle = title
      }
        ! []

    UpdateFieldBody body ->
      { model
        | fieldBody = body
      }
        ! []


-- SUBSCRIPTIONS


port linkClicked : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ linkClicked LinkClicked
    ]




-- VIEW


view : Model -> Html Msg
view model =
  div
    [ id "main" ]
    [ div
       [ id "app"]
       [ viewContents model.data
       , viewStory model.editing model.story
       ]
    , viewModel model
    ]


-- View: Story (visible cards)

viewStory : Maybe String -> List Card -> Html Msg
viewStory editingId_ visibleCards =
  let
    viewFn c =
      viewCard (editingId_ == Just c.title) c
  in
  div
    [ id "story"
    ]
    ( List.map viewFn visibleCards )



viewCard : Bool -> Card -> Html Msg
viewCard isEditing card =
  if isEditing then
    div 
      [ id ("card-" ++ card.title)
      , classList
          [ ("card", True)
          , ("editing", isEditing)
          ]
      ] 
      [ input 
          [ id ("card-title-edit-" ++ card.title)
          , defaultValue card.title
          , onInput UpdateFieldTitle
          ]
          []
      , br [][]
      , textarea 
          [ id ("card-body-edit-" ++ card.title)
          , defaultValue card.body
          , onInput UpdateFieldBody
          ]
          []
      , button
          [ onClick (UpdateCard card.title) ]
          [ text "save"]
      ]
  else
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
      , button [onClick (CloseCard card.title)][text "X"]
      , button [onClick (DeleteCard card.title)][text "Delete"]
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
    [ id "content"]
    [ button [onClick (LinkClicked "")] [text "+"]
    , ul
        [ ]
        ( List.map viewCardItem cards )
    ]


viewCardItem : Card -> Html Msg
viewCardItem card =
  li 
    [ id ("card-item-" ++ card.title)
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
    , div [ id "model-story" ] 
        (viewListCardData model.story)
    , div [ id "model-fieldTitle" ] 
        [pre [][text model.fieldTitle]]
    , div [ id "model-fieldBody" ] 
        [pre [][text model.fieldBody]]
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
