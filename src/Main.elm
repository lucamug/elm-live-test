module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int
    }


initialModel : Model
initialModel =
    { count = 0
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


cssLine : String -> Int -> Shape -> String
cssLine figureName index s =
    let
        shapeName =
            Array.get index shapesName

        translate =
            "translate("
                ++ String.fromInt s.x
                ++ "px,"
                ++ String.fromInt s.y
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
        ".tangram--"
            ++ figureName
            ++ " ."
            ++ Maybe.withDefault "xxx" shapeName
            -- s.name
            ++ "{"
            ++ "transform:"
            ++ transform
            ++ "}"


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1 bbb" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text <| "-1" ]
        , Html.pre []
            [ text "<style>\n\n"
            , text <|
                String.join "" <|
                    List.concat <|
                        List.map
                            (\shape ->
                                List.indexedMap
                                    (\index s -> cssLine shape.name index s)
                                    shape.shape
                            )
                            shapes
            , text "\n\n</style>"
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Shape =
    { x : Int
    , y : Int
    , rotate : Int
    , skew : Int
    }


shapesName : Array.Array String
shapesName =
    Array.fromList
        [ "triangle--pink"
        , "triangle--purple"
        , "triangle--turquoise"
        , "triangle--yellow"
        , "triangle--red"
        , "square--orange"
        , "parallelogram--green"
        ]


shapes : List { name : String, shape : List Shape }
shapes =
    [ { name = "basic"
      , shape =
            [ Shape 160 -2 180 0
            , Shape 79 79 270 0
            , Shape 150 109 45 0
            , Shape 1 1 0 0
            , Shape 54 -52 90 0
            , Shape 122 66 45 0
            , Shape 45 146 -45 45
            ]
      }
    , { name = "cat"
      , shape =
            [ Shape 0 0 0 0
            , Shape 52 0 180 0
            , Shape 10 132 180 0
            , Shape 85 132 0 0
            , Shape 100 245 45 0
            , Shape 16 70 45 0
            , Shape 222 310 0 -45
            ]
      }
    , { name = "swan"
      , shape =
            [ Shape -6 155 0 0
            , Shape 3 4 45 0
            , Shape -3 189 180 0
            , Shape 57 194 135 0
            , Shape 127 136 90 0
            , Shape 12 116 45 0
            , Shape 42 38 45 45
            ]
      }
    , { name = "rabbit"
      , shape =
            [ Shape 61 333 45 0
            , Shape 20 210 180 0
            , Shape 99 307 135 0
            , Shape 60 120 135 0
            , Shape 130 196 -45 0
            , Shape 0 74 0 0
            , Shape 76 0 0 -45
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

   .square {
       position: absolute;
       transition: all 2s;
   }

   .square--orange {
       background-color: #7FD13B;
       width: 70px;
       height: 70px;
   }

   .parallelogram {
       position: absolute;
       transition: all 2s;
   }

   .parallelogram--green {
       background-color: #7FD13B;
       width: 64px;
       height: 70px;
   }
-}
