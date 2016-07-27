module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)

-- first part MODEL
type alias Model =
    { calories : Int
    , inputValue : Int
    , error : Maybe String
    }

initModel : Model
initModel = 
    { calories = 0
    , inputValue = 0
    , error = Nothing
    }

-- second part UPDATE
type Msg
    = AddCalorie
    | Clear
    | Input String

update : Msg -> Model -> Model
update msg model = 
    case msg of
        AddCalorie ->
            { model | calories = model.calories + model.inputValue
                    , inputValue = 0
                    , error = Nothing
            }

        Clear ->
            initModel
        
        Input val ->
            case toInt val of 
                Ok num ->
                    { model 
                        | inputValue = num
                        , error = Nothing 
                    }

                Err mess ->
                    { model 
                        | error = Just "It's not a number" 
                    }

valueInInput : Int -> String
valueInInput inputValue = 
    if inputValue == 0 then
        ""
    else
        toString inputValue

-- third part VIEW
view : Model -> Html Msg
view model = 
    let 
        (helperString, disabledButton, myVal) =
            case model.error of 
                Just mess ->
                    (mess
                    , True
                    , valueInInput model.inputValue
                    )

                Nothing ->
                    ("You want to add " ++ toString model.inputValue
                    , False
                    , valueInInput model.inputValue
                    )
    in  
        div []
            [ h3
                []
                [ text ("Total calories : " ++ toString model.calories) ]
            , input 
                [ onInput Input
                , value myVal
                ]
                []
            , button
                [ type' "button"
                , disabled disabledButton
                , onClick AddCalorie
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


