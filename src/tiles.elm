module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Semantic as UI exposing (..)
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


type alias Model =
    { allCombinations : List Tile
    , after : Int
    , show : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { allCombinations =
            getTilePermutations 0 <| Tile.createTile ()
      , after = 0
      , show = 20
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Next
    | Previous


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( { model | after = model.after + 20 }, Cmd.none )

        Previous ->
            ( { model | after = model.after - 20 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        toShow =
            model.allCombinations |> List.drop model.after |> List.take model.show
    in
    container
        [ menu
        , segment
            [ List.length model.allCombinations |> String.fromInt |> (++) "Total generated tiles: " |> text
            , List.map (span [ style "margin" "10px" ] << List.singleton << drawTile Small) toShow
                |> div []
            , button [ onClick Next ] [ text "Next" ]
            , button [ onClick Previous ] [ text "Previous" ]
            ]
        ]


menu : Html msg
menu =
    ui <| inverted <| UI.menu []


container : List (Html msg) -> Html msg
container children =
    ui <| UI.container children


segment : List (Html msg) -> Html msg
segment children =
    ui <| UI.segment children


getTilePermutations : Position -> Tile -> List Tile
getTilePermutations position tile =
    case tile of
        Invalid _ ->
            []

        Complete _ ->
            [ tile ]

        Incomplete _ ->
            let
                l =
                    getTilePermutations (position + 1) (addLimb position tile)

                s1 =
                    getTilePermutations (position + 1) (addSide position Tiny Tile.Red tile)

                s2 =
                    getTilePermutations (position + 2) (addSide position Small Tile.Yellow tile)

                s3 =
                    getTilePermutations (position + 3) (addSide position Medium Tile.Green tile)

                s4 =
                    getTilePermutations (position + 4) (addSide position Large Tile.Blue tile)
            in
            l ++ s1 ++ s2 ++ s3 ++ s4
