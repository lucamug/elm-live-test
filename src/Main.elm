module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick)
import Time


type alias Model =
    { composition : Composition
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { composition = Basic
      }
    , Cmd.none
    )


type Msg
    = ChangeComposition


cssSeparator : String
cssSeparator =
    ""


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeComposition ->
            ( { model | composition = nextComposition model.composition }, Cmd.none )


cssLine : String -> Int -> Shape -> String
cssLine figureName index s =
    let
        maybeShapeName =
            Array.get index shapesName

        translate =
            "translate("
                ++ adjust
                    (s.x
                        - 160
                        + (if String.startsWith name.basic figureName then
                            moveX.basic

                           else
                            moveX.crane
                          )
                    )
                ++ "px,"
                ++ adjust
                    (s.y
                        - 340
                        + (if String.startsWith name.basic figureName then
                            moveY.basic

                           else
                            moveY.crane
                          )
                    )
                ++ "px)"

        rotate =
            if s.rotate == 0 then
                ""

            else
                "rotate("
                    ++ String.fromInt s.rotate
                    ++ "deg)"

        skew =
            if s.skew == 0 then
                ""

            else
                "skew("
                    ++ String.fromInt s.skew
                    ++ "deg)"

        transform =
            translate ++ rotate ++ skew
    in
    if transform == "" then
        ""

    else
        let
            shapeName =
                Maybe.withDefault name.prefix maybeShapeName
        in
        String.join cssSeparator
            [ classCss ++ figureName ++ " " ++ classCss ++ shapeName ++ "{"
            , "transform:" ++ transform ++ ";"
            , if s.color == defaultColor then
                ""

              else if String.startsWith name.triangle shapeName then
                "border-left-color:" ++ s.color ++ ";"

              else
                "background-color:" ++ s.color ++ ";"
            , "}"
            ]


adjust : Int -> String
adjust value =
    String.fromInt (value // 2)


defaultColor : String
defaultColor =
    "#777"


classCss : String
classCss =
    "." ++ name.prefix ++ name.separator


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text <| css ]
        , div
            [ Html.Attributes.style "background-color" "black"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "flex-direction" "column"
            ]
            [ composition model.composition
            , composition <| nextComposition model.composition
            , composition <| nextComposition <| nextComposition model.composition
            ]
        ]


composition : Composition -> Html msg
composition c =
    div [ Html.Attributes.class (name.prefix ++ " " ++ name.prefix ++ "-" ++ compositionToString c), Html.Attributes.style "margin-top" "200px" ]
        [ div [ Html.Attributes.class <| name.prefix ++ "-tri " ++ name.prefix ++ "-tri-1" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-tri " ++ name.prefix ++ "-tri-2" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-tri " ++ name.prefix ++ "-tri-3" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-tri " ++ name.prefix ++ "-tri-4" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-tri " ++ name.prefix ++ "-tri-5" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-squ " ++ name.prefix ++ "-squ-6" ] []
        , div [ Html.Attributes.class <| name.prefix ++ "-par " ++ name.prefix ++ "-par-7" ] []
        ]


type Composition
    = Basic
    | Crane


nextComposition : Composition -> Composition
nextComposition c =
    case c of
        Basic ->
            Crane

        Crane ->
            Basic


compositionToString : Composition -> String
compositionToString c =
    case c of
        Basic ->
            "bas"

        Crane ->
            "cra"


css : String
css =
    (String.join cssSeparator <|
        List.concat <|
            List.map
                (\shape ->
                    List.indexedMap
                        (\index s -> cssLine shape.name index s)
                        shape.shape
                )
                shapes
    )
        ++ String.join cssSeparator
            [ classCss ++ name.triangle ++ "," ++ classCss ++ name.square ++ "," ++ classCss ++ name.parallelogram ++ "{"
            , "position:absolute;"
            , "transition:all 2s;"
            , "}"
            , classCss ++ name.triangle ++ "{"
            , "width:0;"
            , "height:0;"
            , "}"
            , classCss ++ name.triangle ++ name.separator ++ name.pink ++ "{"
            , "border-left:" ++ adjust 50 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 50 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 50 ++ "px solid transparent;"
            , "}"
            , classCss ++ name.triangle ++ name.separator ++ name.purple ++ "{"
            , "border-left:" ++ adjust 50 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 50 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 50 ++ "px solid transparent;"
            , "}"
            , classCss ++ name.triangle ++ name.separator ++ name.turquoise ++ "{"
            , "border-left:" ++ adjust 70 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 70 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 70 ++ "px solid transparent;"
            , "}"
            , classCss ++ name.triangle ++ name.separator ++ name.yellow ++ "{"
            , "border-left:" ++ adjust 100 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 100 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 100 ++ "px solid transparent;"
            , "}"
            , classCss ++ name.triangle ++ name.separator ++ name.red ++ "{"
            , "border-left:" ++ adjust 100 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 100 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 100 ++ "px solid transparent;"
            , "}"
            , classCss ++ name.square ++ name.separator ++ name.orange ++ "{"
            , "background-color:" ++ defaultColor ++ ";"
            , "width:" ++ adjust 70 ++ "px;"
            , "height:" ++ adjust 70 ++ "px;"
            , "}"
            , classCss ++ name.parallelogram ++ name.separator ++ name.green ++ "{"
            , "background-color:" ++ defaultColor ++ ";"
            , "width:" ++ adjust 64 ++ "px;"
            , "height:" ++ adjust 70 ++ "px;"
            , "}"
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Time.every 4000 (\_ -> ChangeComposition)
        }


type alias Shape =
    { x : Int
    , y : Int
    , rotate : Int
    , skew : Int
    , color : String
    }


name :
    { basic : String
    , green : String
    , orange : String
    , parallelogram : String
    , pink : String
    , purple : String
    , red : String
    , square : String
    , crane : String
    , triangle : String
    , turquoise : String
    , yellow : String
    , separator : String
    , prefix : String
    }
name =
    { triangle = "tri"
    , square = "squ"
    , parallelogram = "par"

    -- sub-types
    , pink = "1"
    , purple = "2"
    , turquoise = "3"
    , yellow = "4"
    , red = "5"
    , orange = "6"
    , green = "7"

    -- shapes
    , crane = "cra"
    , basic = "bas"

    -- separator
    , separator = "-"

    -- prefix
    , prefix = "xxx"
    }


shapesName : Array.Array String
shapesName =
    Array.fromList
        [ name.triangle ++ name.separator ++ name.pink
        , name.triangle ++ name.separator ++ name.purple
        , name.triangle ++ name.separator ++ name.turquoise
        , name.triangle ++ name.separator ++ name.yellow
        , name.triangle ++ name.separator ++ name.red
        , name.square ++ name.separator ++ name.orange
        , name.parallelogram ++ name.separator ++ name.green
        ]


moveX : { basic : Int, crane : Int }
moveX =
    { basic = 45
    , crane = 60
    }


moveY : { basic : Int, crane : Int }
moveY =
    { basic = 160
    , crane = 40
    }


shapes : List { name : String, shape : List Shape }
shapes =
    [ { -- basic
        name = name.basic
      , shape =
            [ Shape 160 -2 180 0 defaultColor
            , Shape 79 79 270 0 defaultColor
            , Shape 150 109 45 0 defaultColor
            , Shape 1 1 0 0 defaultColor
            , Shape 54 -52 90 0 defaultColor
            , Shape 122 66 45 0 "#aa0000"
            , Shape 45 146 -45 45 defaultColor
            ]
      }
    , { -- crane
        name = name.crane
      , shape =
            [ Shape -6 155 0 0 defaultColor
            , Shape 3 4 45 0 defaultColor
            , Shape -3 189 180 0 defaultColor
            , Shape 57 194 135 0 defaultColor
            , Shape 127 136 90 0 defaultColor
            , Shape 12 116 45 0 "#aa0000"
            , Shape 42 38 45 45 defaultColor
            ]
      }
    ]
