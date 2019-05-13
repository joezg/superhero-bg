module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Tile exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = NoState


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoState
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Here be tiles"
        , drawTile
            (Tile.createTile ()
                |> addSide 0 Tiny NoColor
                |> addLimb 1
                |> addLimb 2
                |> addSide 3 Small Red
                |> addLimb 5
                |> addSide 6 Medium Green
                |> addLimb 9
                |> addLimb 10
                |> addSide 11 Tiny (Custom "aa23b7")
            )
            (Offset 0 100)
        ]
