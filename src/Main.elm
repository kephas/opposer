module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Ev
import Browser.Navigation as Nav
import Element as El
import Element.Input as In
import Element.Background as Back
import Element.Border as Bord
import Element.Font as Font
import Html exposing (Html)
import String.Interpolate exposing(interpolate)
import Task
import Time
import Url



white = El.rgb 1 1 1
colorPrimary = El.rgb255 0x00 0x7b 0xff

nextMagnitude n =
    toFloat <| 10 ^ (logBase 10 n |> ceiling)

---- MODEL ----

type alias Model =
    { opposer : Float
    , opposee : Float
    , magnitude : Float
    }

adjustement = 7

probability model =
    let opposer = model.opposer
        opposee = model.opposee
        bigOpposer = opposer >= opposee
        ratio = if bigOpposer then opposer / opposee else opposee/opposer
        adjustedRatio = (ratio - 1) * adjustement + 1
        proba = 100 * adjustedRatio / (adjustedRatio + 1)
        realProba = if bigOpposer then proba else 100 - proba
    in
        String.fromInt (round realProba) ++ "%"

defaultValue = 20

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model defaultValue defaultValue (nextMagnitude defaultValue), Cmd.none )



---- UPDATE ----

type Msg
    = ChangeOpposer String
    | ChangeOpposee String
    | SlideOpposer Float
    | SlideOpposee Float


possiblyNumber numberStr =
    case String.toInt numberStr of
        Nothing -> 0
        Just number -> toFloat number

adjustMagnitude : Model -> Model
adjustMagnitude model =
    { model | magnitude = nextMagnitude <| max model.opposer model.opposee }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOpposer numberStr ->
            ( adjustMagnitude { model | opposer = possiblyNumber numberStr }, Cmd.none )

        ChangeOpposee numberStr ->
            ( adjustMagnitude { model | opposee = possiblyNumber numberStr }, Cmd.none )

        SlideOpposer number ->
            ( { model | opposer = number }, Cmd.none )

        SlideOpposee number ->
            ( { model | opposee = number }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

---- VIEW ----

input magnitude msgTxt msgSlide data label =
    El.row [ El.width El.fill, El.spacing 40 ]
        [ In.text [ El.width <| El.fillPortion 1 ]
              { onChange = msgTxt
              , placeholder = Nothing
              , text = if data == 0 then "" else String.fromInt <| round data
              , label = In.labelLeft [El.centerY] <| El.text label
              }
        , In.slider [ El.height <| El.px 40
                    , El.width <| El.fillPortion 4
                    ,  El.behindContent
                        (El.el
                             [ El.width El.fill
                             , El.height (El.px 2)
                             , El.centerY
                             , Back.color <| El.rgb 0.5 0.5 0.5
                             , Bord.rounded 2
                             ]
                             El.none
                        )
                    ]
            { onChange = msgSlide
            , label = In.labelHidden ""
            , min = 1
            , max = magnitude
            , step = Nothing
            , value = data
            , thumb = In.defaultThumb
            }
        ]

view : Model -> Browser.Document Msg
view model =
    { title = "Opposer"
    , body = [
           El.layout [El.padding 40, El.height El.fill ] <|
               El.column [El.spacing 40, El.width El.fill ]
                   [ input model.magnitude ChangeOpposer SlideOpposer model.opposer "Opposer"
                   , input model.magnitude ChangeOpposee SlideOpposee model.opposee "Opposee"
                   , El.el [ El.height El.fill, El.centerY, El.centerX ] <|
                       El.text (probability model)
                   ]
          ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
