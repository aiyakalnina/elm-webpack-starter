port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { board : List Cell
    , currentPlayer : Player
    , status : GameStatus
    }


type Cell
    = Circle
    | Cross
    | Empty


type Player
    = CirclePlayer
    | CrossPlayer


type Result
    = CirclePlayerWon
    | CrossPlayerWon
    | Draw


type GameStatus
    = Ongoing
    | Finished Result


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { board = List.repeat 9 Empty
      , currentPlayer = CirclePlayer
      , status = Ongoing
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Move Int



-- filter : (a -> Bool) -> List a -> List a
-- filter f list =
--     case list of
--         [] ->
--             []
--         h :: t ->
--             case f h of
--                 True ->
--                     h :: filter f t
--                 False ->
--                     filter f t


checkPosition : Int -> Cell -> Int -> Cell -> Cell
checkPosition newPosition newCell currentPosition currentCell =
    if newPosition == currentPosition then
        newCell

    else
        currentCell


winningCombos : List ( Int, Int, Int )
winningCombos =
    [ ( 0, 1, 2 )
    , ( 0, 3, 6 )
    , ( 0, 4, 8 )
    , ( 3, 4, 5 )
    , ( 1, 4, 7 )
    , ( 6, 4, 2 )
    , ( 6, 7, 8 )
    , ( 2, 5, 8 )
    ]


cellToString : Cell -> String
cellToString c =
    case c of
        Circle ->
            "O"

        Cross ->
            "X"

        Empty ->
            ""


getCell : Int -> List Cell -> Cell
getCell index board =
    Maybe.withDefault Empty (List.drop index board |> List.head)


checkCombo : ( Int, Int, Int ) -> List Cell -> GameStatus
checkCombo indexes board =
    let
        ( index1, index2, index3 ) =
            indexes

        firstCell =
            getCell index1 board

        secondCell =
            getCell index2 board

        thirdCell =
            getCell index3 board
    in
    if firstCell == secondCell && secondCell == thirdCell then
        case firstCell of
            Circle ->
                Finished CirclePlayerWon

            Cross ->
                Finished CrossPlayerWon

            _ ->
                Ongoing

    else
        Ongoing


checkStatus : List Cell -> GameStatus
checkStatus board =
    -- checkCombo 0 1 2 board
    let
        statusList =
            List.map (\combo -> checkCombo combo board) winningCombos

        listFilterFunc : GameStatus -> Bool
        listFilterFunc gameStatus =
            case gameStatus of
                Finished _ ->
                    True

                Ongoing ->
                    False

        winningStatusList =
            List.filter listFilterFunc statusList

        emptyCellsLeftFilter : Cell -> Bool
        emptyCellsLeftFilter boardCell =
            case boardCell of
                Empty ->
                    True

                _ ->
                    False

        emptyCellsLeft =
            List.filter emptyCellsLeftFilter board
    in
    case List.head winningStatusList of
        Just gameStatus ->
            gameStatus

        Nothing ->
            if List.length emptyCellsLeft == 0 then
                Finished Draw

            else
                Ongoing



-- checkStatus : List GameStatus -> GameStatus
-- checkStatus
-- checkCombo [ 0, 1, 2 ] board
-- case board of
--     [ Circle, Circle, Circle, Empty, Cross, Cross, Empty, Empty, Empty ] ->
--         Finished CirclePlayerWon
--     _ ->
--         Ongoing


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Move position ->
            let
                newCell =
                    case model.currentPlayer of
                        CirclePlayer ->
                            Circle

                        CrossPlayer ->
                            Cross

                -- update board
                newBoard =
                    List.indexedMap (checkPosition position newCell) model.board

                -- switch player
                newPlayer =
                    case model.currentPlayer of
                        CirclePlayer ->
                            CrossPlayer

                        CrossPlayer ->
                            CirclePlayer

                -- newStatus =
                --     checkStatus model.board
            in
            ( { model
                | board = newBoard
                , currentPlayer = newPlayer
                , status = checkStatus newBoard
              }
            , Cmd.none
            )



-- ---------------------------
-- VIEW
-- ---------------------------


viewCell : Int -> Cell -> Html Msg
viewCell position cell =
    div [ class "grid__item", onClick (Move position) ]
        [ case cell of
            Circle ->
                text "o"

            Cross ->
                text "x"

            _ ->
                text ""
        ]


viewFinishedMessage : GameStatus -> Html Msg
viewFinishedMessage status =
    case status of
        Finished CirclePlayerWon ->
            text "Circle won!"

        Finished CrossPlayerWon ->
            text "Cross won"

        Finished Draw ->
            text "It's a draw!"

        Ongoing ->
            text ""



-- indexedMap : (Int -> a -> b) -> List a -> List b
-- indexedMap f l =
--     indexedHelper 0 f l
-- indexedHelper : Int -> (Int -> a -> b) -> List a -> List b
-- indexedHelper index function list =
--     case list of
--       [] -> []
--       (h::t) -> (function index h)::(indexedHelper (index + 1) function t)
-- [1,2,3] == 1::2::3::[]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "message" ]
            [ viewFinishedMessage model.status ]
        , button [ class "btn-play" ]
            [ text "Play again" ]
        , div [ class "board" ]
            [ div [ class "grid" ]
                (List.indexedMap viewCell model.board)
            , div [ class "board__line-1" ]
                []
            , div [ class "board__line-2" ]
                []
            , div [ class "board__line-3" ]
                []
            , div [ class "board__line-4" ]
                []
            ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Tic Tac Toe"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
