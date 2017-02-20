port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
  | UpdateCard String String
  | UpdateFieldTitle String
  | UpdateFieldBody String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    OpenCard title ->
      { model
        | editing = Just title
      }
        ! []

    UpdateCard title body ->
      let
        updateCard c =
          if c.title == title then
            { c | body = body }
          else
            c
      in
      { model
        | data = List.map updateCard model.data
        , story = List.map updateCard model.story
        , editing = Nothing
      }
        ! []

    UpdateFieldTitle title ->
      model ! []

    UpdateFieldBody body ->
      model ! []


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
  div 
    [ id ("card-" ++ card.title)
    , classList
        [ ("card", True)
        , ("editing", isEditing)
        ]
    , onDoubleClick (OpenCard card.title)
    ] 
    [ text card.body ]


viewCardItem : Card -> Html Msg
viewCardItem card =
  li 
    [ id ("card-item-" ++ card.title)] 
    [ text card.body ]


viewCardData : Card -> Html Msg
viewCardData card =
  div 
    [ id ("card-data-" ++ card.title)] 
    [ text card.body ]
