module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)



type alias Round = 
    Array.Array (Maybe String)

type alias Model =
    Array.Array Round

init : ( Model, Cmd Msg )
init =
    ( fromList [ fromList [ Just "hedgehogs", Just "pandas", Just "disciples", Just "fivers" 
        , Just "blues", Just "fighting", Just "aardvark", Just "alans"
        , Just "keyboards", Just "paddles", Just "mice", Just "knights" 
        , Just "aldeans", Just "spartans", Just "scooters", Just "squirrels"  ]
      , fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , fromList [ Nothing, Nothing, Nothing, Nothing ]
      , fromList [ Nothing, Nothing ]
      , fromList [ Nothing ]
      ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Selection


type alias Selection =
    { roundIdx : Int
    , teamIdx : Int
    , team : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Select selected ->
            let
                    
                newRoundIdx =
                    selected.roundIdx + 1

                bootedTeam =
                    getBooted (selected.teamIdx // 2) (get newRoundIdx model)

                newModel =
                    Array.indexedMap
                        (\roundIdx roundy ->
                            (Array.indexedMap
                                (\teamIdx team ->
                                    let 
                                        roundDiff =
                                            roundIdx - selected.roundIdx
                                        currentRoundsSelectedTeamsPath =
                                            determineTeamIdxPath roundDiff selected.teamIdx
                                    in 
                                        if roundIdx == newRoundIdx && teamIdx == currentRoundsSelectedTeamsPath then
                                            Just selected.team
                                        else if roundIdx > newRoundIdx && teamIdx == currentRoundsSelectedTeamsPath then
                                            case bootedTeam of
                                                Just boot ->
                                                    case team of
                                                        Just teamyy ->
                                                            if boot == teamyy && teamyy /= selected.team then
                                                                Nothing
                                                            else
                                                                Just teamyy
                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    team
                                        else
                                            team
                                )
                                roundy
                            )
                        )
                        model
                    
            in
                ( newModel, Cmd.none )

getBooted : Int -> Maybe Round -> Maybe String
getBooted idxToGrab roundy =
    case roundy of
        Just roundyVal ->
            case get idxToGrab roundyVal of
                Just teamy ->
                    teamy
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

determineTeamIdxPath : Int -> Int -> Int
determineTeamIdxPath roundDiff selectedTeamIdx =
    if roundDiff <= 0 then
        selectedTeamIdx
    else 
        determineTeamIdxPath (roundDiff - 1) (selectedTeamIdx // 2)


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        (Array.toList (Array.indexedMap
            (\roundIdx roundy ->
                div [ class "round" ]
                    (Array.toList (viewTeam roundIdx roundy))
            )
            model
        ))


viewTeam : Int -> Array (Maybe String) -> Array (Html Msg)
viewTeam roundIdx roundy =
    Array.indexedMap
        (\teamIdx team ->
            case team of
                Just teamy ->
                    div
                        [ class "team"
                        , onClick
                            (Select
                                ({ roundIdx = roundIdx
                                , teamIdx = teamIdx
                                , team = teamy
                                }
                                )
                            )
                        ]
                        [ text (getThing team) ]
                Nothing ->
                    div [class "team"] [text "(- - -)"]
        )
        roundy


getThing : Maybe String -> String
getThing val =
    case val of
        Just sumtin ->
            sumtin

        Nothing ->
            "(blank)"



{-
   div [className "container"] [
       div [className "round"] [
           div [className "team"] [text "hedgehogs"]
           , div [className "team"] [text "pandas"]
           , div [className "team"] [text "disciples"]
           , div [className "team"] [text "fivers"]
       ]
       , div [className "round"] [
           div [className "team"] [text "hedgehogs"]
           , div [className "team"] [text "disciples"]
       ]
       , div [className "round"] [
           div [className "team"] [text "disciples"]
       ]
   ]
-}
