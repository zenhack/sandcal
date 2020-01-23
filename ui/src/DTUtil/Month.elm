module DTUtil.Month exposing (fromInt, numParser, toInt)

import Parser exposing (Parser)
import ParserUtils
import Time exposing (Month(..))


{-| Parser for a numeric month (1-12). Leading zeros are permitted.
-}
numParser : Parser Month
numParser =
    ParserUtils.decimalWithZeros
        |> Parser.andThen
            (\n ->
                case fromInt n of
                    Just m ->
                        Parser.succeed m

                    Nothing ->
                        Parser.problem <|
                            "The number "
                                ++ String.fromInt n
                                ++ " is not a valid month."
            )


{-| Convert an integer to a month. Returns `Nothing` if the number
is not in the range 1-12.
-}
fromInt : Int -> Maybe Month
fromInt n =
    case n of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


{-| Convert a month to an integer in the range 1-12.
-}
toInt : Month -> Int
toInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
