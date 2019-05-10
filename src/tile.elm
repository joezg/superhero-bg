module Tile exposing (Color(..), Offset, Position, SideSize(..), Tile, addLimb, addSide, drawTile)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Tile =
    { limbs : List Angle
    , sides : List Side
    }


type Side
    = Side SideSize Angle
    | ColoredSide SideSize Angle Color


type SideSize
    = Tiny
    | Small
    | Medium
    | Large


type Color
    = NoColor
    | Red
    | Green
    | Blue
    | Custom String


type alias Angle =
    Int


type alias Position =
    Int


type alias Offset =
    { x : Float
    , y : Float
    }


addLimb : Position -> Tile -> Tile
addLimb position tile =
    { tile | limbs = positionToAngle position :: tile.limbs }


addSide : Position -> SideSize -> Color -> Tile -> Tile
addSide position size color tile =
    case color of
        NoColor ->
            { tile | sides = Side size (positionToAngle position) :: tile.sides }

        _ ->
            { tile | sides = ColoredSide size (positionToAngle position) color :: tile.sides }


positionToAngle : Position -> Angle
positionToAngle p =
    p * 30


drawTile : Tile -> Offset -> Html msg
drawTile tile offset =
    div []
        [ svg
            [ viewBox "0 0 500 500"
            ]
            (renderTileBase () ++ renderLimbs tile.limbs ++ renderSides tile.sides)
        ]


renderSides : List Side -> List (Svg msg)
renderSides sides =
    List.foldr (++) [] (List.map sideToPath sides)


renderLimbs : List Angle -> List (Svg msg)
renderLimbs limbs =
    List.foldr (++) [] (List.map limbToPath limbs)


sideToPath : Side -> List (Svg msg)
sideToPath side =
    case side of
        Side size angle ->
            sideSizeToPath size angle NoColor

        ColoredSide size angle color ->
            sideSizeToPath size angle color


sideSizeToPath : SideSize -> Angle -> Color -> List (Svg msg)
sideSizeToPath size angle color =
    case size of
        Tiny ->
            [ Svg.path
                [ addClass "cls-1"
                , d "M152.281,26.6564l7.1678-12.415C142.6471,33.2579,111.3417,24.87,106.2992,0V14.3356A36.4012,36.4012,0,0,0,152.281,26.6564Z"
                , addTransform angle
                , addColor color
                ]
                []
            ]

        Small ->
            [ Svg.path
                [ addClass "cls-1"
                , d "M188.7212,58.7128a105.77,105.77,0,0,1-82.422-47.5864V0A100.101,100.101,0,0,0,198.357,53.15Z"
                , addTransform angle
                , addColor color
                ]
                []
            ]

        Medium ->
            [ Svg.path
                [ addClass "cls-1"
                , d "M106.2992,12.282a238.3936,238.3936,0,0,0,94.0172,94.0172h12.282A232.7237,232.7237,0,0,1,106.2992,0Z"
                , addTransform angle
                , addColor color
                ]
                []
            ]

        Large ->
            [ Svg.path
                [ addClass "cls-1"
                , d "M106.2992,0V16.5965A490.5355,490.5355,0,0,0,183.984,151.1506l14.373,8.2982A484.8629,484.8629,0,0,1,106.2992,0Z"
                , addTransform angle
                , addColor color
                ]
                []
            ]


limbToPath : Angle -> List (Svg msg)
limbToPath angle =
    [ Svg.path
        [ addClass "cls-1"
        , d "M133.8115,3.6221A104.9811,104.9811,0,0,0,106.2992,0q2.8346,2.9147,5.6693,5.8292a99.8469,99.8469,0,0,1,20.3756,3.269Q133.0778,6.36,133.8115,3.6221Z"
        , fill "purple"
        , addTransform angle
        ]
        []
    , Svg.path
        [ addClass "cls-1"
        , d "M107.1087,23.7875a23.46,23.46,0,0,1-.8095-6.1486V0q2.8346,2.9147,5.6693,5.8292v11.81a17.8625,17.8625,0,0,0,.6163,4.6813Z"
        , addTransform angle
        ]
        []
    , Svg.path
        [ addClass "cls-1"
        , d "M112.5848,22.32a18.0873,18.0873,0,0,0,12.79,12.79q-.7336,2.7379-1.4674,5.4761a23.7565,23.7565,0,0,1-16.7983-16.7984Z"
        , addTransform angle
        ]
        []
    , Svg.path
        [ addClass "cls-1"
        , d "M125.3744,35.11a18.0872,18.0872,0,0,0,17.4708-4.6814l4.0088,4.0088a23.7564,23.7564,0,0,1-22.947,6.1487Q124.6407,37.8478,125.3744,35.11Z"
        , addTransform angle
        ]
        []
    , Svg.path
        [ addClass "cls-1"
        , d "M146.854,34.4372a23.46,23.46,0,0,0,3.7754-4.92l8.8194-15.2757-7.8243,2.2135L145.72,26.6825a17.859,17.859,0,0,1-2.8744,3.7459Z"
        , addTransform angle
        ]
        []
    , Svg.path
        [ addClass "cls-1"
        , d "M151.6245,16.4549a99.8587,99.8587,0,0,0-19.28-7.3567q.7337-2.7381,1.4674-5.4761a104.9789,104.9789,0,0,1,25.6373,10.6193Z"
        , addTransform angle
        ]
        []
    ]


addTransform : Angle -> Svg.Attribute msg
addTransform angle =
    transform ("rotate(" ++ String.fromInt angle ++ " 106.2992 106.2992)")


addClass : String -> Svg.Attribute msg
addClass class =
    Svg.Attributes.class class


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

        Custom hex ->
            Svg.Attributes.style ("fill: #" ++ hex ++ ";")



-- , Svg.path
--     [ Svg.Attributes.class "cls-1"
--     --, d "M159.4488,14.2414A106.2992,106.2992,0,0,0,106.2992,0V17.6388a23.7565,23.7565,0,0,0,44.33,11.8783Z"
--     , d "M150.6293,29.5171a23.7564,23.7564,0,0,1-44.33-11.8782V0a106.3,106.3,0,0,1,53.15,14.2414Zm.9952-13.0621a97.6986,97.6986,0,0,0-39.656-10.6258v11.81A18.0871,18.0871,0,0,0,145.72,26.6825Z"
--     , transform "rotate(-30 106.2992 106.2992)"
--     ]
--     []
