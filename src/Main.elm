module Main exposing (..)

import Browser exposing (sandbox)
import Array exposing (Array)
import Html exposing (Html, button, div, form, input, label, li, text, ul)
import Html.Attributes exposing (autofocus, checked, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MAIN


main =
  sandbox {init = init, update = update, view = view}



-- MODEL

type alias Item =
  { text : String
  , completed : Bool
  }

type alias Model =
  { nextItem : String
  , items : Array Item
  }


init : Model
init =
  { nextItem = ""
  , items = Array.empty
  }



-- UPDATE


type Msg
  = AddItem
  | NextItem String
  | ToggleCompleted Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddItem ->
      if model.nextItem == "" then
        model

      else
        { model
        | nextItem = ""
        , items = Array.push (Item model.nextItem False) model.items
        }

    NextItem item -> { model | nextItem = item }

    ToggleCompleted index ->
      let
        item = Array.get index model.items
      in
        case item of
          Nothing -> model

          Just oldItem ->
            let
              newItem = { oldItem | completed = not oldItem.completed }
            in
              { model | items = Array.set index newItem model.items }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ nextItemForm model.nextItem
    , todolist model.items
    ]


nextItemForm : String -> Html Msg
nextItemForm nextItem =
  form [ onSubmit AddItem ]
    [ label []
        [ text "Next Item: "
        , input [ value nextItem, onInput NextItem, autofocus True ] []
        ]
    , text " "
    , button [ type_ "submit" ] [ text "Add" ]
    ]


todolist : Array Item -> Html Msg
todolist items =
  ul [] (List.map toLi (Array.toIndexedList items))


toLi : (Int, Item) -> Html Msg
toLi (index, item) =
  li [ onClick (ToggleCompleted index) ]
    [ input [ type_ "checkbox", checked item.completed ] []
    , text " "
    , text item.text
    ]
