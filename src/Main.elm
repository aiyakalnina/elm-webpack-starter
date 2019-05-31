port module Main exposing (Model, Msg(..), add1, init, main, toJs, update, view)

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
    { counter : Int
    , serverMessage : String
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { counter = flags, serverMessage = "" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Inc
    | Set Int
    | TestServer
    | OnServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, toJs "Hello Js" )

        Set m ->
            ( { model | counter = m }, toJs "Hello Js" )

        TestServer ->
            let
                expect =
                    Http.expectJson OnServerResponse (Decode.field "result" Decode.string)
            in
            ( model
            , Http.get { url = "/test", expect = expect }
            )

        OnServerResponse res ->
            case res of
                Ok r ->
                    ( { model | serverMessage = r }, Cmd.none )

                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl _ ->
            "BadUrl"

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s


{-| increments the counter

    add1 5 --> 6

-}
add1 : Model -> Model
add1 model =
    { model | counter = model.counter + 1 }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div []
        [ div [ class "message" ]
            []
        , button [ class "btn-play" ]
            [ text "Play again" ]
        , div [ class "board" ]
            [ div [ class "grid" ]
                [ div [ class "grid__item", attribute "data-field" "0" ]
                    []
                , div [ class "grid__item", attribute "data-field" "1" ]
                    []
                , div [ class "grid__item", attribute "data-field" "2" ]
                    []
                , div [ class "grid__item", attribute "data-field" "3" ]
                    []
                , div [ class "grid__item", attribute "data-field" "4" ]
                    []
                , div [ class "grid__item", attribute "data-field" "5" ]
                    []
                , div [ class "grid__item", attribute "data-field" "6" ]
                    []
                , div [ class "grid__item", attribute "data-field" "7" ]
                    []
                , div [ class "grid__item", attribute "data-field" "8" ]
                    []
                ]
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
