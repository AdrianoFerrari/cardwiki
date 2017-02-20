port module Main exposing (..)


import Html exposing (..)


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
  ( { data = []
    , story = []
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
  div [] [text "test"]




