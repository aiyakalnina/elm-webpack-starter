port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
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
    }


type Cell
    = Circle
    | Cross
    | Empty


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { board = [ Empty, Circle, Cross ] }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Move


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        _ ->
            ( model, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


viewCell : Cell -> Html Msg
viewCell cell =
    div [ class "grid__item", attribute "data-field" "0" ]
        [ case cell of
            Circle ->
                text "o"

            Cross ->
                text "x"

            _ ->
                text ""
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "message" ]
            []
        , button [ class "btn-play" ]
            [ text "Play again" ]
        , div [ class "board" ]
            [ div [ class "grid" ]
                (List.map viewCell model.board)

            -- [ div [ class "grid__item", attribute "data-field" "0" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "1" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "2" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "3" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "4" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "5" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "6" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "7" ]
            --     []
            -- , div [ class "grid__item", attribute "data-field" "8" ]
            --     []
            -- ]
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
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
