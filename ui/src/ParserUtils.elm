module ParserUtils exposing (decimalWithZeros)

import Parser exposing ((|.), (|=), Parser)


{-| Like `Parser.int`, but allows leading zeros.

FIXME: this treats the empty string as the number zero, which it should
reject. Not a problem for our current usage, but should fix.

-}
decimalWithZeros : Parser Int
decimalWithZeros =
    Parser.succeed identity
        |. Parser.chompWhile (\c -> c == '0')
        |= Parser.oneOf
            [ Parser.int
            , Parser.succeed 0
            ]
