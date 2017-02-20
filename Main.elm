port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListExtra
import Regex
import Tuple exposing (first, second)


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
  | LinkClicked String
  | OpenCard String
  | UpdateCard String
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

    UpdateFieldTitle title ->
      { model
        | fieldTitle = title
            |> Debug.log "UpdateFieldTitle"
      }
        ! []

    UpdateFieldBody body ->
      { model
        | fieldBody = body
            |> Debug.log "UpdateFieldBody"
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
      , viewBody card.body 
      ]


viewBody : String -> Html Msg
viewBody str =
  let
    matches =
      Regex.find Regex.All (Regex.regex "\\[\\[(.*?)\\]\\]") str

    matchIndices =
      matches
        |> List.map getIndices
  in
  case matches of
    [] ->
      text str

    [a] ->
      let
        indices =
          getIndices a
      in
      div 
        []
        ( parseLinks
          [ (String.slice 0 a.index str, False)
          , (String.slice (first indices) (second indices) str, True)
          , (String.slice (second indices) (String.length str) str, False)
          ]
        )

    a :: _ ->
      let
        zippedIndices =
          ListExtra.zip matchIndices (List.drop 1 matchIndices)
            |> Debug.log "zippedIndices"
            |> getSplitIndices
            |> Debug.log "splitIndices"
            |> List.append [((0, a.index), False)]
            |> Debug.log "appended"
            |> List.map 
                (\t -> 
                  (String.slice (t |> first |> first) (t |> first |> second) str
                  , second t
                  )
                )
      in
      div 
        []
        (parseLinks zippedIndices)



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




-- HELPERS


getIndices : Regex.Match -> (Int, Int)
getIndices match =
  (match.index, match.index + (match.match |> String.length))


getSplitIndices : List ((Int, Int), (Int, Int)) -> List ((Int, Int), Bool)
getSplitIndices matchPairs =
  let
    indexIntersperse : ((Int, Int), (Int, Int)) -> List ((Int, Int), Bool)
    indexIntersperse matchPair =
      [ (matchPair |> first, True)
      , ((matchPair |> first |> second, matchPair |> second |> first), False)
      , (matchPair |> second, True)
      ]
  in
  List.concatMap indexIntersperse matchPairs


parseLinks : List (String, Bool) -> List (Html Msg)
parseLinks strings =
  let
    parseToken (s, b) =
      if b then
        let
          inner =
            s |> String.dropLeft 2 |> String.dropRight 2
        in
        a 
          [ href ("#" ++ inner)
          , onClick (LinkClicked inner)
          ]
          [ text inner]
       else
         text s
  in
  List.map parseToken strings
