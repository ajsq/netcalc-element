module CalcValue exposing
    ( CalcFloat
    , CalcInt
    , CalcValue(..)
    , asMaybe
    , asResult
    , floatFromStr
    , fromFloat
    , fromInt
    , getString
    , getValue
    , intFromStr
    , isSaved
    , setA
    , setStringFailedConv
    , updateFloatFromStr
    , updateIntFromStr
    , wrap
    )

{-| Wrap a String that might save to an _a_.
-}


type CalcValue a
    = CalcValue String a Bool


type alias CalcInt =
    CalcValue Int


type alias CalcFloat =
    CalcValue Float



-- GENERICS


isSaved : CalcValue a -> Bool
isSaved (CalcValue _ _ bool) =
    bool


wrap : String -> a -> CalcValue a
wrap string a =
    CalcValue
        string
        a
        True


getValue : CalcValue a -> a
getValue (CalcValue _ a _) =
    a


getString : CalcValue a -> String
getString (CalcValue str _ _) =
    str


asResult : CalcValue a -> Result String a
asResult aCalcValue =
    case aCalcValue of
        CalcValue str _ False ->
            Err str

        CalcValue _ a True ->
            Ok a


asMaybe : CalcValue a -> Maybe a
asMaybe =
    asResult
        >> Result.toMaybe


setA : a -> CalcValue a -> CalcValue a
setA a (CalcValue str _ bool) =
    CalcValue str a bool


setStringFailedConv : String -> CalcValue a -> CalcValue a
setStringFailedConv string (CalcValue _ a _) =
    CalcValue string a False



-- INT


fromInt : Int -> CalcInt
fromInt int =
    let
        stringRep =
            String.fromInt int
    in
    wrap stringRep int


intFromStr : String -> Maybe CalcInt
intFromStr str =
    String.toInt str
        |> Maybe.map
            (\int ->
                CalcValue str int True
            )


updateIntFromStr : String -> CalcInt -> CalcInt
updateIntFromStr string intCalcValue =
    String.toInt string
        |> Maybe.map (\int -> CalcValue string int True)
        |> Maybe.withDefault (setStringFailedConv string intCalcValue)



-- FLOAT


fromFloat : Float -> CalcValue Float
fromFloat float =
    let
        stringRep =
            String.fromFloat float
    in
    wrap stringRep float


floatFromStr : String -> Maybe CalcFloat
floatFromStr str =
    String.toFloat str
        |> Maybe.map
            (\float ->
                CalcValue str float True
            )


updateFloatFromStr : String -> CalcFloat -> CalcFloat
updateFloatFromStr string floatCalcValue =
    String.toFloat string
        |> Maybe.map (\float -> CalcValue string float True)
        |> Maybe.withDefault (setStringFailedConv string floatCalcValue)
