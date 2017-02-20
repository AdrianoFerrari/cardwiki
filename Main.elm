port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)


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
  }


type alias Card =
  { title : String
  , content : String
  }


init : (Model, Cmd Msg)
init =
  ( { data = [Card "test1" "test content", Card "test2" "test content 2"]
    , story = [Card "test1" "test content", Card "test2" "test content 2"]
    }
  , Cmd.none
  )




-- UPDATE


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
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
       , viewStory model.story
       ]
    , viewData model.data
    ]

viewStory : List Card -> Html Msg
viewStory visibleCards =
  div
    [ id "story"
    ]
    ( List.map viewCard visibleCards )


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


viewCard : Card -> Html Msg
viewCard card =
  div 
    [ id ("card-" ++ card.title)] 
    [ text card.content ]


viewCardItem : Card -> Html Msg
viewCardItem card =
  li 
    [ id ("card-item-" ++ card.title)] 
    [ text card.content ]


viewCardData : Card -> Html Msg
viewCardData card =
  div 
    [ id ("card-data-" ++ card.title)] 
    [ text card.content ]
