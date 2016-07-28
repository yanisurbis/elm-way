module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)
import Array exposing (Array, fromList, push, get, set, indexedMap, map, slice, append, foldl)
import List 

-- first part MODEL

type alias Name =
    String

type alias Points = 
    Int

type alias Index =
    Int

type alias Players =
    Array.Array Player

type alias Plays = 
    Array.Array Play

type alias Player = 
    { name : Name
    , points : Points
    }

type alias Play = 
    { name : Name
    , points : Points
    -- index of player (in the array of players) who got points
    , playerIndex : Index
    }

type alias Model =
    { players : Players
    , input : Name
    , plays : Plays
    -- if we want to change player name we use this variable
    , indexPlayerToChange : Index
    }

initModel : Model
initModel = 
    { players = fromList []
    , input = ""
    , plays = fromList []
    , indexPlayerToChange = -1
    }

updatePlayerScore : Players -> Index -> Points -> Players
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
    = SavePlayer
    | UpdatePlayer Name Index
    | Cancel
    | Add3Points Index
    | Add2Points Index
    | Input Name
    | DeletePlay Index Points Index

updatePlays : Players -> Plays -> Index -> Points -> Plays
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
    
updatePlayerName : Name -> Index -> Players -> Players
updatePlayerName newName index players =
    case get index players of 
        Just player ->
            set
                index
                { player | name = newName }
                players

        Nothing ->
            players



update : Msg -> Model -> Model
update msg model = 
    case msg of

        UpdatePlayer name index ->
            { model
                | input = name
                , indexPlayerToChange = index
            }


        DeletePlay index points indexForDeletion ->
            { model
                | players =
                    updatePlayerScore 
                        model.players 
                        index 
                        -points
                , plays =
                    append
                        (slice 0 (indexForDeletion) model.plays)
                        (slice (indexForDeletion + 1) (Array.length model.plays) model.plays)

            }

        SavePlayer ->
            if model.indexPlayerToChange == -1 then
                { model
                    | players = 
                        push 
                            { name = model.input
                            , points = 0
                            } 
                            model.players
                    , input = initModel.input
                }
            else
                { model
                    | indexPlayerToChange = -1
                    , players = 
                        updatePlayerName
                            model.input
                            model.indexPlayerToChange
                            model.players
                    , input = initModel.input
                }

        Cancel ->
            { model
                | input = initModel.input
                , indexPlayerToChange = -1
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

renderPlay : Play -> Index -> Html Msg
renderPlay play index =
    div []
        [ text 
            ("Name : " ++ play.name ++ " , Points : " ++ toString play.points ++ " index: " ++ toString play.playerIndex)
        , button 
            [ type' "button"
            , onClick (DeletePlay play.playerIndex play.points index)    
            ]
            [ text "Delete Play"]
        ]

renderPlays : Plays -> Html Msg
renderPlays plays = 
    div []
        ( plays
            |> indexedMap (\ index elm -> renderPlay elm index)
            |> Array.toList
        )


renderPlayer : Player -> Index -> Html Msg
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
        , button
            [ type' "button"
            , onClick (UpdatePlayer player.name index)
            ]
            [ text "Change Name" ]
        ]

renderPlayers : Players -> Html Msg
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
        , h3
            []
            [ text ("Total :" ++ toString (foldl 
                                            (\ a b -> a.points + b) 
                                            0
                                            model.players
                                        )
                    )
            ]
        , input 
            [ onInput Input
            , value model.input
            ]
            []
        , button
            [ type' "button"
            , onClick SavePlayer
            ]
            [ text "Save" ]
        , button
            [ type' "button"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        , renderPlays model.plays
        ]

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


