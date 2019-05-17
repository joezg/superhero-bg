module Tile exposing (Color(..), Position, Size(..), Tile(..), addLimb, addSide, createTile, drawTile, tileSizeToPixels)

import Html exposing (..)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Tile
    = Complete TileParts
    | Incomplete TileParts
    | Invalid String


type alias TileParts =
    { limbs : List Limb
    , sides : List Side
    }


type alias Limb =
    Position


type alias Side =
    { size : Size
    , position : Position
    , color : Color
    }


type Size
    = Tiny
    | Small
    | Medium
    | Large
    | CustomSize Int


type Color
    = NoColor
    | Red
    | Green
    | Blue
    | Yellow
    | CustomColor String


type alias Angle =
    Int


type alias Position =
    Int


createTile : () -> Tile
createTile () =
    Incomplete (TileParts [] [])


addLimb : Position -> Tile -> Tile
addLimb position tile =
    case tile of
        Complete parts ->
            Invalid "To many parts"

        Invalid reason ->
            Invalid reason

        Incomplete parts ->
            validate { parts | limbs = position :: parts.limbs }


addSide : Position -> Size -> Color -> Tile -> Tile
addSide position size color tile =
    case size of
        CustomSize length ->
            Invalid "CustomSize is unsupported for a side"

        _ ->
            case tile of
                Complete parts ->
                    Invalid "To many parts"

                Invalid reason ->
                    Invalid reason

                Incomplete parts ->
                    validate { parts | sides = Side size position color :: parts.sides }


validate : TileParts -> Tile
validate parts =
    let
        allLimbPositions =
            parts.limbs

        allSidePositions =
            List.concat (List.map getNarrowSidePositions parts.sides)

        allPositions =
            allLimbPositions ++ allSidePositions

        positionSet =
            Set.fromList allPositions

        allWideSidePositions =
            List.concat (List.map getWideSidePositions parts.sides)

        sidesPositionSet =
            Set.fromList allWideSidePositions
    in
    if List.length parts.limbs > 8 then
        Invalid "Too many limbs"

    else if List.length allPositions /= Set.size positionSet then
        Invalid "Overlaping parts"

    else if List.length allWideSidePositions /= Set.size sidesPositionSet then
        Invalid "Adjacent sides"

    else if List.length allPositions < 12 then
        Incomplete parts

    else
        Complete parts


sideSizeToLength : Size -> Int
sideSizeToLength size =
    case size of
        Tiny ->
            1

        Small ->
            2

        Medium ->
            3

        Large ->
            4

        CustomSize int ->
            int


tileSizeToPixels : Size -> Int
tileSizeToPixels size =
    case size of
        Tiny ->
            50

        Small ->
            100

        Medium ->
            200

        Large ->
            300

        CustomSize int ->
            int


getNarrowSidePositions : Side -> List Position
getNarrowSidePositions =
    getSidePositions True


getWideSidePositions : Side -> List Position
getWideSidePositions =
    getSidePositions False


getSidePositions : Bool -> Side -> List Position
getSidePositions narrow { size, position } =
    let
        offset =
            if narrow then
                1

            else
                0

        sideEnd =
            modBy 12 <| position + sideSizeToLength size - offset
    in
    if sideEnd < position then
        List.range position 11 ++ List.range 0 sideEnd

    else
        List.range position sideEnd


drawTile : Size -> Tile -> Html msg
drawTile size tile =
    case tile of
        Incomplete parts ->
            Html.text "incomplete"

        Invalid reason ->
            Html.text ("invalid: " ++ reason)

        Complete parts ->
            svg
                [ viewBox "0 0 212.5984 212.5984"
                , height (tileSizeToPixels size |> String.fromInt)
                , width (tileSizeToPixels size |> String.fromInt)
                ]
                (renderTileBase () ++ renderLimbs parts.limbs ++ renderSides parts.sides)


renderSides : List Side -> List (Svg msg)
renderSides sides =
    List.foldr (++) [] (List.map sideToPath sides)


renderLimbs : List Angle -> List (Svg msg)
renderLimbs limbs =
    List.foldr (++) [] (List.map limbToPath limbs)


sideToPath : Side -> List (Svg msg)
sideToPath { size, position, color } =
    sideSizeToPath size (positionToAngle position) color


sideSizeToPath : Size -> Angle -> Color -> List (Svg msg)
sideSizeToPath size angle color =
    let
        render =
            renderPath angle color
    in
    case size of
        Tiny ->
            [ render "M152.281,26.6564l7.1678-12.415C142.6471,33.2579,111.3417,24.87,106.2992,0V14.3356A36.4012,36.4012,0,0,0,152.281,26.6564Z"
            ]

        Small ->
            [ render "M188.7212,58.7128a105.77,105.77,0,0,1-82.422-47.5864V0A100.101,100.101,0,0,0,198.357,53.15Z"
            ]

        Medium ->
            [ render "M106.2992,12.282a238.3936,238.3936,0,0,0,94.0172,94.0172h12.282A232.7237,232.7237,0,0,1,106.2992,0Z"
            ]

        Large ->
            [ render "M106.2992,0V16.5965A490.5355,490.5355,0,0,0,183.984,151.1506l14.373,8.2982A484.8629,484.8629,0,0,1,106.2992,0Z"
            ]

        CustomSize _ ->
            []


limbToPath : Position -> List (Svg msg)
limbToPath position =
    let
        render =
            renderPath (positionToAngle position) NoColor
    in
    List.map render
        [ "M133.8115,3.6221A104.9811,104.9811,0,0,0,106.2992,0q2.8346,2.9147,5.6693,5.8292a99.8469,99.8469,0,0,1,20.3756,3.269Q133.0778,6.36,133.8115,3.6221Z"
        , "M107.1087,23.7875a23.46,23.46,0,0,1-.8095-6.1486V0q2.8346,2.9147,5.6693,5.8292v11.81a17.8625,17.8625,0,0,0,.6163,4.6813Z"
        , "M112.5848,22.32a18.0873,18.0873,0,0,0,12.79,12.79q-.7336,2.7379-1.4674,5.4761a23.7565,23.7565,0,0,1-16.7983-16.7984Z"
        , "M125.3744,35.11a18.0872,18.0872,0,0,0,17.4708-4.6814l4.0088,4.0088a23.7564,23.7564,0,0,1-22.947,6.1487Q124.6407,37.8478,125.3744,35.11Z"
        , "M146.854,34.4372a23.46,23.46,0,0,0,3.7754-4.92l8.8194-15.2757-7.8243,2.2135L145.72,26.6825a17.859,17.859,0,0,1-2.8744,3.7459Z"
        , "M151.6245,16.4549a99.8587,99.8587,0,0,0-19.28-7.3567q.7337-2.7381,1.4674-5.4761a104.9789,104.9789,0,0,1,25.6373,10.6193Z"
        ]


renderPath : Angle -> Color -> String -> Svg msg
renderPath angle color d =
    Svg.path
        [ Svg.Attributes.class "cls-1"
        , Svg.Attributes.d d
        , transform ("rotate(" ++ String.fromInt angle ++ " 106.2992 106.2992)")
        , addColor color
        ]
        []


renderTileBase : () -> List (Svg msg)
renderTileBase _ =
    [ defs [] [ Svg.style [] [ Svg.text ".cls-1,.cls-2{fill:none;}.cls-1{stroke:#010101;stroke-miterlimit:10;stroke-width:0.2835px;}" ] ]
    , Svg.title [] [ Svg.text "sas_fight_digital" ]
    , circle
        [ Svg.Attributes.class "cls-2"
        , cx "106.2992"
        , cy "106.2992"
        , r "106.2992"
        ]
        []
    ]


addColor : Color -> Svg.Attribute msg
addColor color =
    case color of
        NoColor ->
            Svg.Attributes.style "fill: none;"

        Red ->
            Svg.Attributes.style "fill: red;"

        Green ->
            Svg.Attributes.style "fill: green;"

        Blue ->
            Svg.Attributes.style "fill: blue;"

        Yellow ->
            Svg.Attributes.style "fill: yellow;"

        CustomColor hex ->
            Svg.Attributes.style ("fill: #" ++ hex ++ ";")


positionToAngle : Position -> Angle
positionToAngle p =
    p * 30



-- , Svg.path
--     [ Svg.Attributes.class "cls-1"
--     --, d "M159.4488,14.2414A106.2992,106.2992,0,0,0,106.2992,0V17.6388a23.7565,23.7565,0,0,0,44.33,11.8783Z"
--     , d "M150.6293,29.5171a23.7564,23.7564,0,0,1-44.33-11.8782V0a106.3,106.3,0,0,1,53.15,14.2414Zm.9952-13.0621a97.6986,97.6986,0,0,0-39.656-10.6258v11.81A18.0871,18.0871,0,0,0,145.72,26.6825Z"
--     , transform "rotate(-30 106.2992 106.2992)"
--     ]
--     []
