module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)
import Array exposing (Array, fromList, push, get, set, indexedMap)
import List 

-- first part MODEL

type alias Player = 
    { name : String
    , points : Int 
    }

type alias Model =
    { players : Array Player
    , input : String
    }

initModel : Model
initModel = 
    { players = fromList []
    , input = ""
    }

updatePlayerScore : Array Player -> Int -> Int -> Array Player
updatePlayerScore players index points =
    case get index players of 
        Just player ->
            set
                index
                { player | points = player.points + points }
                players
        Nothing ->
            players

-- second part UPDATE
type Msg
    = AddPlayer
    | Add3Points Int
    | Add2Points Int
    | Input String



update : Msg -> Model -> Model
update msg model = 
    case msg of
        AddPlayer ->
            { model
                | players = 
                    push 
                        { name = model.input
                        , points = 0
                        } 
                        model.players
            }
        
        Add2Points index ->
            { model
                | players =
                    updatePlayerScore model.players index 2
            }
        
        Add3Points index -> 
            { model
                | players =
                    updatePlayerScore model.players index 3
            }

        Input name ->
            { model
                | input = name
            }




renderPlayer : Player -> Int -> Html Msg
renderPlayer player index = 
    div []
        [ h2
            []
            [ text player.name ]
        , button
            [ type' "button"
            , onClick (Add3Points index)
            ]
            [ text "Add 3" ]
        , button
            [ type' "button"
            , onClick (Add2Points index)
            ]
            [ text "Add 2" ]
        ]

renderPlayers : Array Player -> Html Msg
renderPlayers players = 
    div []
        ( players
            |> indexedMap (\ index elm -> renderPlayer elm index)
            |> Array.toList
        )
            
        

view : Model -> Html Msg
view model = 
    div []
        [ h3
            []
            [ text "List Of Players : " ]
        , renderPlayers model.players
        , input 
            [ onInput Input
            , value model.input
            ]
            []
        , button
            [ type' "button"
            , onClick AddPlayer
            ]
            [ text "Add Player" ]
        ]

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


