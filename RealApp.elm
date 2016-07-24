module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length)

-- first part MODEL
type alias Model =
    { calories : Int
    , inputValue : String
    }

initModel : Model
initModel = 
    { calories = 0
    , inputValue = ""
    }

-- second part UPDATE
type Msg
    = AddCalorie Int
    | Clear
    | ChangedInput String

update : Msg -> Model -> Model
update msg model = 
    case msg of
        AddCalorie val ->
            { model | calories = model.calories + val }

        Clear ->
            initModel
        
        ChangedInput val ->
            { model | inputValue = val }

-- third part VIEW
view : Model -> Html Msg
view model = 
    let 
        (helperString, disabledButton, val) =
            case (String.toInt model.inputValue, length model.inputValue > 0) of
                (Ok val, True) ->
                    (toString val, False, val)
                
                (Err val, True) ->
                    ("It's not a number.", True, 0)

                (Ok val, False) ->
                    ("", False, 0)
                
                (Err val, False) ->
                    ("", False, 0)
    in  
        div []
            [ h3
                []
                [ text ("Total calories : " ++ toString model.calories) ]
            , input 
                [ onInput ChangedInput ]
                []
            , button
                [ type' "button"
                , disabled disabledButton
                , onClick (AddCalorie val)
                ]
                [ text "Add" ]
            , button
                [ type' "button"
                , onClick Clear 
                ]
                [ text "Reset" ]
            , h1
                []
                [ text (helperString) ]
            ]

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


