module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick)
import Time


type alias Model =
    { count : Int
    , composition : Composition
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { count = 0
      , composition = Basic
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
                        + (if String.startsWith names.cat figureName then
                            moveX.cat

                           else if String.startsWith names.rabbit figureName then
                            moveX.rabbit

                           else if String.startsWith names.swan figureName then
                            moveX.swan

                           else
                            moveX.basic
                          )
                    )
                ++ "px,"
                ++ adjust
                    (s.y
                        - 340
                        + (if String.startsWith names.cat figureName then
                            moveY.cat

                           else if String.startsWith names.rabbit figureName then
                            moveY.rabbit

                           else if String.startsWith names.swan figureName then
                            moveY.swan

                           else
                            moveY.basic
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
                Maybe.withDefault "xxx" maybeShapeName
        in
        String.join cssSeparator
            [ classCss ++ figureName ++ " " ++ classCss ++ shapeName ++ "{"
            , "transform:" ++ transform ++ ";"
            , if s.color == defaultColor then
                ""

              else if String.startsWith names.triangle shapeName then
                "border-left-color:" ++ s.color ++ ";"

              else
                "background-color:" ++ s.color ++ ";"
            , "}"
            ]


ratio : Int
ratio =
    2


adjust : Int -> String
adjust value =
    String.fromInt (value // 2)


defaultColor : String
defaultColor =
    "#000"


classCss : String
classCss =
    "." ++ names.prefix ++ names.separator


animationSpeed : String
animationSpeed =
    "500"


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text <| css ]
        , div
            [ Html.Attributes.style "background-color" "gray"
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
    div [ Html.Attributes.class ("xxx xxx-" ++ compositionToString c), Html.Attributes.style "margin-top" "200px" ]
        [ div [ Html.Attributes.class "xxx-tri xxx-tri-1" ] []
        , div [ Html.Attributes.class "xxx-tri xxx-tri-2" ] []
        , div [ Html.Attributes.class "xxx-tri xxx-tri-3" ] []
        , div [ Html.Attributes.class "xxx-tri xxx-tri-4" ] []
        , div [ Html.Attributes.class "xxx-tri xxx-tri-5" ] []
        , div [ Html.Attributes.class "xxx-squ xxx-squ-6" ] []
        , div [ Html.Attributes.class "xxx-par xxx-par-7" ] []
        ]


type Composition
    = Basic
    | Cat
    | Swan
    | Rabbit


nextComposition : Composition -> Composition
nextComposition c =
    case c of
        Basic ->
            Cat

        Cat ->
            Swan

        Swan ->
            Rabbit

        Rabbit ->
            Basic


compositionToString : Composition -> String
compositionToString c =
    case c of
        Basic ->
            "bas"

        Cat ->
            "cat"

        Swan ->
            "swa"

        Rabbit ->
            "rab"



-- "<div onclick='elmLive.hideCompiling()' style='background-color: rgba(0,0,0,0); position: fixed; top:0; left:0; bottom:0; right:0'></div>" +
-- '</div>' +
-- "<div id='loading'>" +
-- '</div>' +
-- "<div style='text-shadow:0px 0px 10px #fff, 0px 0px 10px #fff; text-align: center; color: #777; padding: 30px; font-size: 24px; font-weight: bold; font-family: sans-serif'>" +
-- (message ? message : "") +
-- '</div>' +
-- '</div>'


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
            [ classCss ++ names.triangle ++ "," ++ classCss ++ names.square ++ "," ++ classCss ++ names.parallelogram ++ "{"
            , "position:absolute;"
            , "transition:all 2s;"
            , "}"
            , classCss ++ names.triangle ++ "{"
            , "width:0;"
            , "height:0;"
            , "}"
            , classCss ++ names.triangle ++ names.separator ++ names.pink ++ "{"
            , "border-left:" ++ adjust 50 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 50 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 50 ++ "px solid transparent;"
            , "}"
            , classCss ++ names.triangle ++ names.separator ++ names.purple ++ "{"
            , "border-left:" ++ adjust 50 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 50 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 50 ++ "px solid transparent;"
            , "}"
            , classCss ++ names.triangle ++ names.separator ++ names.turquoise ++ "{"
            , "border-left:" ++ adjust 70 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 70 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 70 ++ "px solid transparent;"
            , "}"
            , classCss ++ names.triangle ++ names.separator ++ names.yellow ++ "{"
            , "border-left:" ++ adjust 100 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 100 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 100 ++ "px solid transparent;"
            , "}"
            , classCss ++ names.triangle ++ names.separator ++ names.red ++ "{"
            , "border-left:" ++ adjust 100 ++ "px solid " ++ defaultColor ++ ";"
            , "border-top:" ++ adjust 100 ++ "px solid transparent;"
            , "border-bottom:" ++ adjust 100 ++ "px solid transparent;"
            , "}"
            , classCss ++ names.square ++ names.separator ++ names.orange ++ "{"
            , "background-color:" ++ defaultColor ++ ";"
            , "width:" ++ adjust 70 ++ "px;"
            , "height:" ++ adjust 70 ++ "px;"
            , "}"
            , classCss ++ names.parallelogram ++ names.separator ++ names.green ++ "{"
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
        , subscriptions = \_ -> Time.every 2000 (\_ -> ChangeComposition)
        }


type alias Shape =
    { x : Int
    , y : Int
    , rotate : Int
    , skew : Int
    , color : String
    }


names :
    { basic : String
    , cat : String
    , green : String
    , orange : String
    , parallelogram : String
    , pink : String
    , purple : String
    , rabbit : String
    , red : String
    , square : String
    , swan : String
    , triangle : String
    , turquoise : String
    , yellow : String
    , separator : String
    , prefix : String
    }
names =
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
    , rabbit = "rab"
    , swan = "swa"
    , cat = "cat"
    , basic = "bas"

    -- separator
    , separator = "-"

    -- prefix
    , prefix = "xxx"
    }


shapesName : Array.Array String
shapesName =
    Array.fromList
        [ names.triangle ++ names.separator ++ names.pink
        , names.triangle ++ names.separator ++ names.purple
        , names.triangle ++ names.separator ++ names.turquoise
        , names.triangle ++ names.separator ++ names.yellow
        , names.triangle ++ names.separator ++ names.red
        , names.square ++ names.separator ++ names.orange
        , names.parallelogram ++ names.separator ++ names.green
        ]


moveX : { basic : Int, cat : Int, rabbit : Int, swan : Int }
moveX =
    { basic = 45
    , cat = 15
    , swan = 60
    , rabbit = 40
    }


moveY : { basic : Int, cat : Int, rabbit : Int, swan : Int }
moveY =
    { basic = 160
    , cat = 0
    , swan = 40
    , rabbit = -20
    }


shapes : List { name : String, shape : List Shape }
shapes =
    [ { -- basic
        name = names.basic
      , shape =
            [ Shape 160 -2 180 0 "#F0AD00"
            , Shape 79 79 270 0 "#F0AD00"
            , Shape 150 109 45 0 "#60B5CC"
            , Shape 1 1 0 0 "#5A6378"
            , Shape 54 -52 90 0 "#60B5CC"
            , Shape 122 66 45 0 "#7FD13B"
            , Shape 45 146 -45 45 "#7FD13B"
            ]
      }
    , { -- cat
        name = names.cat
      , shape =
            [ Shape 0 0 0 0 defaultColor
            , Shape 52 0 180 0 defaultColor
            , Shape 10 132 180 0 defaultColor
            , Shape 85 132 0 0 defaultColor
            , Shape 100 245 45 0 defaultColor
            , Shape 16 70 45 0 defaultColor
            , Shape 222 310 0 -45 defaultColor
            ]
      }
    , { -- swan
        name = names.swan
      , shape =
            [ Shape -6 155 0 0 defaultColor
            , Shape 3 4 45 0 defaultColor
            , Shape -3 189 180 0 defaultColor
            , Shape 57 194 135 0 defaultColor
            , Shape 127 136 90 0 defaultColor
            , Shape 12 116 45 0 defaultColor
            , Shape 42 38 45 45 defaultColor
            ]
      }
    , { -- rabbit
        name = names.rabbit
      , shape =
            [ Shape 61 333 45 0 defaultColor
            , Shape 20 210 180 0 defaultColor
            , Shape 99 307 135 0 defaultColor
            , Shape 60 120 135 0 defaultColor
            , Shape 130 196 -45 0 defaultColor
            , Shape 0 74 0 0 defaultColor
            , Shape 76 0 0 -45 defaultColor
            ]
      }
    ]



{-
   .container {
       width: 360px;
       height: 450px;
       background-color: #333;
       float: left;
   }

   .tangram {
       padding: 20px 30px;
       position: relative;
   }


   .tangram--basic .triangle--pink {
       transform: translate(160px, -2px) rotate(180deg);
   }

   .tangram--basic .triangle--purple {
       transform: translate(79px, 79px) rotate(270deg);
   }

   .tangram--basic .triangle--turquoise {
       transform: translate(150px, 109px) rotate(45deg);
   }

   .tangram--basic .triangle--yellow {
       transform: translate(0, 0);
   }

   .tangram--basic .triangle--red {
       transform: translate(54px, -52px) rotate(90deg);
   }

   .tangram--basic .square--orange {
       transform: translate(122px, 66px) rotate(45deg);
   }

   .tangram--basic .parallelogram--green {
       transform: translate(45px, 146px) rotate(-45deg) skew(45deg);
   }

   .tangram--cat .triangle--purple {
       transform: translate(52px, 0) rotate(180deg);
   }

   .tangram--cat .triangle--turquoise {
       transform: translate(10px, 132px) rotate(180deg);
   }

   .tangram--cat .triangle--yellow {
       transform: translate(85px, 132px);
   }

   .tangram--cat .triangle--red {
       transform: translate(100px, 245px) rotate(45deg);
   }

   .tangram--cat .square--orange {
       transform: translate(16px, 70px) rotate(45deg);
   }

   .tangram--cat .parallelogram--green {
       transform: translate(222px, 310px) skew(-45deg);
   }

   .tangram--swan .triangle--pink {
       transform: translate(-6px, 155px) rotate(0deg);
   }

   .tangram--swan .triangle--purple {
       transform: translate(3px, 4px) rotate(45deg);
   }

   .tangram--swan .triangle--turquoise {
       transform: translate(-3px, 189px) rotate(180deg);
   }

   .tangram--swan .triangle--yellow {
       transform: translate(57px, 194px) rotate(135deg);
   }

   .tangram--swan .triangle--red {
       transform: translate(127px, 136px) rotate(90deg);
   }

   .tangram--swan .square--orange {
       transform: translate(12px, 116px) rotate(45deg);
   }

   .tangram--swan .parallelogram--green {
       transform: translate(42px, 38px) rotate(45deg) skew(45deg);
   }

   .tangram--rabbit .triangle--pink {
       transform: translate(61px, 333px) rotate(45deg);
   }

   .tangram--rabbit .triangle--purple {
       transform: translate(20px, 210px) rotate(180deg);
   }

   .tangram--rabbit .triangle--turquoise {
       transform: translate(99px, 307px) rotate(135deg);
   }

   .tangram--rabbit .triangle--yellow {
       transform: translate(60px, 120px) rotate(135deg);
   }

   .tangram--rabbit .triangle--red {
       transform: translate(130px, 196px) rotate(-45deg);
   }

   .tangram--rabbit .square--orange {
       transform: translate(0, 74px) rotate(0deg);
   }

   .tangram--rabbit .parallelogram--green {
       transform: translate(76px, 0px) skew(-45deg);
   }

   .triangle {
       width: 0;
       height: 0;
       position: absolute;
       transition: all 2s;
   }

   .triangle--pink {
       border-left: 50px solid #F0AD00;
       border-top: 50px solid transparent;
       border-bottom: 50px solid transparent;
   }

   .triangle--purple {
       border-left: 50px solid #F0AD00;
       border-top: 50px solid transparent;
       border-bottom: 50px solid transparent;
   }

   .triangle--turquoise {
       border-left: 70px solid #60B5CC;
       border-top: 70px solid transparent;
       border-bottom: 70px solid transparent;
   }

   .triangle--yellow {
       border-left: 100px solid #5A6378;
       border-top: 100px solid transparent;
       border-bottom: 100px solid transparent;
   }

   .triangle--red {
       border-left: 100px solid #60B5CC;
       border-top: 100px solid transparent;
       border-bottom: 100px solid transparent;
   }

   .square--orange {
       background-color: #7FD13B;
       width: 70px;
       height: 70px;
   }

   .parallelogram--green {
       background-color: #7FD13B;
       width: 64px;
       height: 70px;
   }
-}
