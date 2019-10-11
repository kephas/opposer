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

---- MODEL ----

type alias Model =
    { opposer : Int
    , opposee : Int
    }

adjustement = 7

probability model =
    let opposer = toFloat model.opposer
        opposee = toFloat model.opposee
        ratio = opposer / opposee
        adjustedRatio = (ratio - 1) * adjustement + 1
    in
        String.fromInt (round <| 100 * adjustedRatio / (adjustedRatio + 1)) ++ "%"

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 10 10, Cmd.none )



---- UPDATE ----

type Msg
    = ChangeOpposer String
    | ChangeOpposee String


possiblyChange numberStr model resultFun =
    case String.toInt numberStr of
        Nothing ->
            ( resultFun 0, Cmd.none )
                        
        Just number ->
            ( resultFun number, Cmd.none ) 
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOpposer numberStr ->
            possiblyChange numberStr model (\n -> { model | opposer = n })

        ChangeOpposee numberStr ->
            possiblyChange numberStr model (\n -> { model | opposee = n })

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

---- VIEW ----

input msg data label =
    In.text [] { onChange = msg
               , placeholder = Nothing
               , text = if data == 0 then "" else String.fromInt data
               , label = In.labelLeft [El.centerY] <| El.text label
               }

view : Model -> Browser.Document Msg
view model =
    { title = "Opposer"
    , body = [
           El.layout [El.padding 40, El.height El.fill] <|
               El.column [El.spacing 40]
                   [ input ChangeOpposer model.opposer "Opposer"
                   , input ChangeOpposee model.opposee "Opposee"
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
