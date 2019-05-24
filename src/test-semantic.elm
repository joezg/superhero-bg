module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Semantic as UI exposing (..)



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
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}
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
        [ ui <| inverted <| UI.menu []
        , container
            [ piledSegment
                [ verticalSegment Red
                    [ text "default"
                    , container [ text loreIpsum ]
                    ]
                , verticalSegment Green
                    [ text "left"
                    , alignedContainer Left [ text loreIpsum ]
                    ]
                , verticalSegment Blue
                    [ text "right"
                    , alignedContainer Right [ text loreIpsum ]
                    ]
                , verticalSegment Teal
                    [ text "center"
                    , alignedContainer Center [ text loreIpsum ]
                    ]
                , verticalSegment Pink
                    [ text "justified"
                    , alignedContainer Justified [ text loreIpsum ]
                    ]
                , verticalSegment Purple
                    [ text "fluid"
                    , ui <| fluid <| UI.container [ text loreIpsum ]
                    ]
                , verticalSegment Black
                    [ text "text"
                    , ui <| textual <| UI.container [ text loreIpsum ]
                    ]
                , verticalSegment Grey
                    [ text "combination"
                    , ui <| textual <| UI.align Right <| UI.container [ text loreIpsum ]
                    ]
                ]
            ]
        ]


container : List (Html msg) -> Html msg
container children =
    ui <| UI.container children


alignedContainer : Alignment -> List (Html msg) -> Html msg
alignedContainer a children =
    ui <| UI.align a <| UI.container children


verticalSegment : UI.Color -> List (Html msg) -> Html msg
verticalSegment c children =
    ui <| vertical <| inverted <| color c <| segment children


piledSegment : List (Html msg) -> Html msg
piledSegment children =
    ui <| piled <| segment children


loreIpsum : String
loreIpsum =
    "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa strong. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede link mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi."
