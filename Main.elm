port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListExtra


main : Program Never Model Msg
main =
  program
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


init : (Model, Cmd Msg)
init =
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
  | OpenCard String
  | UpdateCard String
  | UpdateFieldTitle String
  | UpdateFieldBody String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    OpenCard title ->
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
        updateCard c =
          if c.title == title then
            { c 
              | title = model.fieldTitle
              , body = model.fieldBody 
            }
          else
            c
      in
      { model
        | data = List.map updateCard model.data
        , story = List.map updateCard model.story
        , fieldTitle = ""
        , fieldBody = ""
        , editing = Nothing
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


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ div
       [ id "app"]
       [ viewContents model.data
       , viewStory model.editing model.story
       ]
    , viewData model.data
    ]

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


viewContents : List Card -> Html Msg
viewContents cards =
  ul
    [ id "contents"
    ]
    ( List.map viewCardItem cards )



viewData : List Card -> Html Msg
viewData allCards =
  div
    [ id "data"
    ]
    ( List.map viewCardData allCards )


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
      , onDoubleClick (OpenCard card.title)
      ] 
      [ text card.title
      , br [][]
      , text card.body 
      ]


viewCardItem : Card -> Html Msg
viewCardItem card =
  li 
    [ id ("card-item-" ++ card.title)] 
    [ text card.title ]


viewCardData : Card -> Html Msg
viewCardData card =
  div 
    [ id ("card-data-" ++ card.title)] 
    [ text card.body ]
