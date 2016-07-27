module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)
import Array exposing (Array, fromList, push, get, set, indexedMap, map)
import List 

-- first part MODEL

type alias Player = 
    { name : String
    , points : Int 
    }

type alias Play = 
    { name : String
    , points : Int
    , playerIndex : Int
    }

type alias Model =
    { players : Array Player
    , input : String
    , plays : Array Play
    }

initModel : Model
initModel = 
    { players = fromList []
    , input = ""
    , plays = fromList []
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

type alias Index
    = Int

type alias Point
    = Int

type Msg
    = AddPlayer
    | Add3Points Int
    | Add2Points Int
    | Input String
    | DeletePlay Index Point

updatePlays : Array Player -> Array Play -> Int -> Int -> Array Play
updatePlays players plays index points = 
    -- get name
    -- make str
    -- push this str
    -- return 
    let 
        name = case get index players of
                Just player ->
                    player.name
                    
                Nothing ->
                    "Error with name"
    in
        push
            (Play name points index)
            plays 
    



update : Msg -> Model -> Model
update msg model = 
    case msg of

        DeletePlay index points ->
            { model
                | players =
                    updatePlayerScore 
                        model.players 
                        index 
                        -points
            }

        AddPlayer ->
            { model
                | players = 
                    push 
                        { name = model.input
                        , points = 0
                        } 
                        model.players
                , input = initModel.input
            }

        
        Add2Points index ->
            { model
                | players =
                    updatePlayerScore 
                        model.players 
                        index 
                        2
                , plays =
                    updatePlays
                        model.players
                        model.plays
                        index
                        2
            }
        
        Add3Points index -> 
            { model
                | players =
                    updatePlayerScore 
                        model.players 
                        index 
                        3
                , plays =
                    updatePlays
                        model.players
                        model.plays
                        index
                        3
            }

        Input name ->
            { model
                | input = name
            }

renderPlay : Play -> Html Msg
renderPlay play =
    div []
        [ text 
            ("Name : " ++ play.name ++ " , Points : " ++ toString play.points ++ " index: " ++ toString play.playerIndex)
        , button 
            [ type' "button"
            , onClick (DeletePlay play.playerIndex play.points)    
            ]
            [ text "Delete Play"]
        ]

renderPlays : Array Play -> Html Msg
renderPlays plays = 
    div []
        ( plays
            |> map (\ elm -> renderPlay elm)
            |> Array.toList
        )


renderPlayer : Player -> Int -> Html Msg
renderPlayer player index = 
    div []
        [ h2
            []
            [ text ("Name : " ++ player.name ++ " , Points : " ++ toString player.points) ]
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
        , renderPlays model.plays
        ]

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


