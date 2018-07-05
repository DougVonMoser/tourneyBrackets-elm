module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)


type alias Round =
    Array.Array (Cell)


type alias Model =
    Array.Array Round


type alias Team =
    { 
        name : String
    }


-- played cells wont have onlicks; will have static scores, grayed out styles
type alias PlayedCell =
    { team : Team
    , score : String
    }


type Cell
    = Blank
    | Active Team
    | Played PlayedCell


init : ( Model, Cmd Msg )
init =
    ( fromList
        [ fromList
            [ Active { name = "hedgehogs"}
            , Active { name = "pandas"}
            , Active { name = "disciples"}
            , Active { name = "fivers"}
            , Active { name = "blues"}
            , Active { name = "fighting"}
            , Active { name = "aardvark"}
            , Active { name = "alans"}
            , Active { name = "keyboards"}
            , Active { name = "paddles"}
            , Active { name = "mice"}
            , Active { name = "knights"}
            , Active { name = "aldeans"}
            , Active { name = "spartans"}
            , Active { name = "scooters"}
            , Active { name = "squirrels"}
            ]
        , fromList [ Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank ]
        , fromList [ Blank, Blank, Blank, Blank ]
        , fromList [ Blank, Blank ]
        , fromList [ Blank ]
        ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Selection


type alias Selection =
    { roundIdx : Int
    , teamIdx : Int
    , team : Team
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Select selected ->
            let
                newRoundIdx =
                    selected.roundIdx + 1

                bootedCell =
                    getBooted (selected.teamIdx // 2) (get newRoundIdx model)

                newModel =
                    Array.indexedMap
                        (\roundIdx roundy ->
                            (Array.indexedMap
                                -- this anonymous function returns a Cell
                                (\teamIdx existingCell ->
                                    let
                                        roundDiff =
                                            roundIdx - selected.roundIdx

                                        currentRoundsSelectedTeamsPath =
                                            determineTeamIdxPath roundDiff selected.teamIdx
                                    in
                                        if teamIdx /= currentRoundsSelectedTeamsPath || roundIdx < newRoundIdx then
                                            existingCell
                                        else if roundIdx == newRoundIdx then
                                            Active selected.team
                                        else    -- roundIdx must be > newRoundIdx
                                            case (bootedCell, existingCell) of
                                                (Active bootedTeam, Active existingTeam) ->
                                                    if bootedTeam.name == existingTeam.name && existingTeam.name /= selected.team.name then
                                                        Blank
                                                    else
                                                        existingCell

                                                _ ->
                                                    existingCell
                                )
                                roundy
                            )
                        )
                        model
            in
                ( newModel, Cmd.none )


getBooted : Int -> Maybe Round -> Cell
getBooted idxToGrab roundy =
    case roundy of
        Just roundyVal ->
            case get idxToGrab roundyVal of
                Just teamy ->
                    teamy

                _ ->
                    Blank

        _ ->
            Blank


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
        (Array.toList
            (Array.indexedMap
                (\roundIdx roundy ->
                    div [ class "round" ]
                        (Array.toList (viewTeam roundIdx roundy))
                )
                model
            )
        )


viewTeam : Int -> Round -> Array (Html Msg)
viewTeam roundIdx roundy =
    Array.indexedMap
        (\teamIdx cell ->
            case cell of
                Active team ->
                    div
                        [ class "team"
                        , onClick
                            (Select
                                ({ roundIdx = roundIdx
                                 , teamIdx = teamIdx
                                 , team = team
                                 }
                                )
                            )
                        ]
                        [ text team.name ]

                _ ->
                    div [ class "team" ] [ text "(- - -)" ]
        )
        roundy

