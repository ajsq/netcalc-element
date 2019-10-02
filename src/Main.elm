module Main exposing (main)

import Browser
import CalcValue as CV exposing (CalcFloat, CalcInt)
import Html exposing (Html, text)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { bandwidth : CalcInt
    , rtt : CalcInt
    , window : CalcInt
    }


init : Model
init =
    { bandwidth = CV.fromInt (50 * 1000 * 1000)
    , rtt = CV.fromInt 50
    , window = CV.fromInt 65535
    }


type Msg
    = UpdateBandwidth String
    | UpdateRtt String
    | UpdateWindow String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateBandwidth string ->
            { model
                | bandwidth =
                    CV.updateIntFromStr string model.bandwidth
            }

        UpdateRtt string ->
            { model
                | rtt =
                    CV.updateIntFromStr string model.rtt
            }

        UpdateWindow string ->
            { model
                | window =
                    CV.updateIntFromStr string model.window
            }


view : Model -> Html Msg
view model =
    Html.text "hi"
