module Main exposing (main)

import Browser
import CalcValue as CV exposing (CalcFloat, CalcInt, CalcValue(..))
import Css exposing (Style, border3, px, rgb, solid)
import Html.Styled
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
        , toUnstyled
        , tr
        )
import Html.Styled.Attributes as HtmlAttr
    exposing
        ( align
        , colspan
        , css
        , for
        , id
        , type_
        , value
        )
import Html.Styled.Events exposing (onInput)
import NumberSuffix as NS


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view >> toUnstyled
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
            bwVal * rttVal // 1000 // 8
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
                    , errorBorder (not bwConverted)
                    ]
                    []
                ]
            , td []
                [ text (NS.formatInt fmtConfig bwVal ++ "bit/s") ]
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
                    , errorBorder (not rttConverted)
                    ]
                    []
                ]
            , td []
                [ text (String.fromInt rttVal ++ " ms") ]
            ]
        , tr []
            [ td [] [ text "Required window" ]
            , td [ colspan 2, align "center" ]
                [ text (String.fromInt reqWindow ++ " bytes") ]
            ]
        ]


fmtConfig : NS.Config
fmtConfig =
    NS.standardConfig
        |> (\a ->
                { a
                    | getSuffix =
                        NS.suffixStandardShort
                            >> (\s -> "  " ++ s)
                    , minSuffix = 1000
                }
           )


errorBorder : Bool -> Html.Styled.Attribute Msg
errorBorder isErr =
    if isErr then
        css
            [ border3
                (px 2)
                solid
                (rgb 255 0 0)
            ]

    else
        css []
