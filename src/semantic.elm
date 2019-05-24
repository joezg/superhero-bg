module Semantic exposing (Alignment(..), Color(..), align, color, container, fluid, inverted, menu, piled, placeholder, raised, segment, stacked, textual, ui, vertical)

import Html exposing (..)
import Html.Attributes exposing (..)


ui : Element state msg -> Html msg
ui element =
    element.r element.state


type alias Element s msg =
    { state : s
    , r : s -> Html msg
    }



-- ALIGNABLE


type alias Alignable e =
    { e
        | alignment : Alignment
    }


type Alignment
    = Left
    | Right
    | Center
    | Justified
    | NotAligned


align : Alignment -> Element (Alignable any) msg -> Element (Alignable any) msg
align a element =
    let
        { state } =
            element
    in
    { element | state = { state | alignment = a } }



-- INVERTABLE


type alias Invertable e =
    { e
        | inverted : Bool
    }


inverted : Element (Invertable any) msg -> Element (Invertable any) msg
inverted element =
    let
        { state } =
            element
    in
    { element | state = { state | inverted = True } }



-- COLORED


type alias Colored e =
    { e
        | color : Color
    }


type Color
    = NoColor
    | Red
    | Orange
    | Yellow
    | Olive
    | Green
    | Teal
    | Blue
    | Violet
    | Purple
    | Pink
    | Brown
    | Grey
    | Black


color : Color -> Element (Colored any) msg -> Element (Colored any) msg
color c element =
    let
        { state } =
            element
    in
    { element | state = { state | color = c } }


addColorModifier : Colored any -> String
addColorModifier state =
    case state.color of
        NoColor ->
            ""

        Red ->
            "red"

        Orange ->
            "orange"

        Yellow ->
            "yellow"

        Olive ->
            "olive"

        Green ->
            "green"

        Teal ->
            "teal"

        Blue ->
            "blue"

        Violet ->
            "violet"

        Purple ->
            "purple"

        Pink ->
            "pink"

        Brown ->
            "brown"

        Grey ->
            "grey"

        Black ->
            "black"



-- CONTAINER


type alias Container =
    Alignable
        { containerType : ContainerType
        }


type ContainerType
    = NormalContainer
    | Text
    | Fluid


renderContainer : List (Html msg) -> Container -> Html msg
renderContainer children { containerType, alignment } =
    let
        modifiers =
            [ case alignment of
                Left ->
                    "left aligned"

                Center ->
                    "center aligned"

                Right ->
                    "right aligned"

                Justified ->
                    "justified"

                NotAligned ->
                    ""
            , case containerType of
                NormalContainer ->
                    ""

                Text ->
                    "text"

                Fluid ->
                    "fluid"
            ]
    in
    div [ class <| createClass "container" modifiers ] children


container : List (Html msg) -> Element Container msg
container children =
    { state =
        { containerType = NormalContainer
        , alignment = NotAligned
        }
    , r = renderContainer children
    }


fluid : Element Container msg -> Element Container msg
fluid element =
    let
        { state } =
            element
    in
    { element | state = { state | containerType = Fluid } }


textual : Element Container msg -> Element Container msg
textual element =
    let
        { state } =
            element
    in
    { element | state = { state | containerType = Text } }



-- SEGMENT


type alias Segment =
    Colored (Invertable { raised : Bool, placeholder : Bool, stacked : Bool, piled : Bool, vertical : Bool })


renderSegment : List (Html msg) -> Segment -> Html msg
renderSegment children state =
    let
        modifiers =
            [ addBoolModifier "raised" (.raised state)
            , addBoolModifier "placeholder" (.placeholder state)
            , addBoolModifier "stacked" (.stacked state)
            , addBoolModifier "piled" (.piled state)
            , addBoolModifier "vertical" (.vertical state)
            , addBoolModifier "inverted" (.inverted state)
            , addColorModifier state
            ]
    in
    div [ class <| createClass "segment" modifiers ] children


segment : List (Html msg) -> Element Segment msg
segment children =
    { state = { raised = False, placeholder = False, stacked = False, piled = False, vertical = False, inverted = False, color = NoColor }
    , r = renderSegment children
    }


raised : Element Segment msg -> Element Segment msg
raised element =
    let
        { state } =
            element
    in
    { element | state = { state | raised = True } }


placeholder : Element Segment msg -> Element Segment msg
placeholder element =
    let
        { state } =
            element
    in
    { element | state = { state | placeholder = True } }


stacked : Element Segment msg -> Element Segment msg
stacked element =
    let
        { state } =
            element
    in
    { element | state = { state | stacked = True } }


piled : Element Segment msg -> Element Segment msg
piled element =
    let
        { state } =
            element
    in
    { element | state = { state | piled = True } }


vertical : Element Segment msg -> Element Segment msg
vertical element =
    let
        { state } =
            element
    in
    { element | state = { state | vertical = True } }



-- MENU


type alias Menu =
    Invertable {}


renderMenu : List (Html msg) -> Menu -> Html msg
renderMenu children state =
    let
        modifiers =
            [ addBoolModifier "inverted" (.inverted state)
            ]
    in
    div
        [ class <| createClass "menu" modifiers ]
        children


menu : List (Html msg) -> Element Menu msg
menu children =
    { state = { inverted = False }
    , r = renderMenu children
    }



-- HEADER


type alias Header =
    { disabled : Bool, dividing : Bool, block : Bool, inverted : Bool, sub : Bool }


renderHeader : List (Html msg) -> Header -> Html msg
renderHeader children state =
    let
        modifiers =
            [ addBoolModifier "disabled" (.disabled state)
            , addBoolModifier "dividing" (.dividing state)
            , addBoolModifier "block" (.block state)
            , addBoolModifier "inverted" (.inverted state)
            , addBoolModifier "sub" (.sub state)
            ]
    in
    div [ class <| createClass "segment" modifiers ] children


header : List (Html msg) -> Element Header msg
header children =
    { state = { disabled = False, dividing = False, block = False, inverted = False, sub = False }
    , r = renderHeader children
    }



-- HELPERS


addBoolModifier : String -> Bool -> String
addBoolModifier property switch =
    if switch then
        property

    else
        ""


createClass : String -> List String -> String
createClass base modifiers =
    String.join " " ("ui" :: base :: modifiers)
