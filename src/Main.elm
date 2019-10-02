module Main exposing (main)

import Browser
import CalcValue as CV exposing (CalcFloat, CalcInt, CalcValue(..))
import Html
    exposing
        ( Html
        , input
        , label
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes as HtmlAttr
    exposing
        ( colspan
        , for
        , id
        , type_
        , value
        )
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
    let
        (CalcValue bwStr bwVal bwConverted) =
            model.bandwidth

        (CalcValue rttStr rttVal rttConverted) =
            model.rtt

        reqWindow =
            bwVal * 8 * rttVal // 1000
    in
    table []
        [ tr []
            [ td []
                [ label [ for "bwInput" ] [ text "Bandwidth (bits/s)" ]
                ]
            , td []
                [ input
                    [ id "bwInput"
                    , type_ "string"
                    , value bwStr
                    , onInput UpdateBandwidth
                    ]
                    []
                ]
            , td []
                [ text (String.fromInt bwVal) ]
            ]
        , tr []
            [ td []
                [ label [ for "rttInput" ] [ text "RTT (ms)" ]
                ]
            , td []
                [ input
                    [ id "rttInput"
                    , type_ "string"
                    , value rttStr
                    , onInput UpdateRtt
                    ]
                    []
                ]
            , td []
                [ text (String.fromInt rttVal ++ " ms") ]
            ]
        , tr []
            [ td [] [ text "Required window" ]
            , td [ colspan 2 ]
                [ text (String.fromInt reqWindow ++ " bytes") ]
            ]
        ]
