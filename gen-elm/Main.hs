-- Script to generate the 'TimeZone' module for our frontend (Elm) code.
--
-- The generated module provides the elm equivalent of the haskell `tz`
-- package's `TZLabel` type, along with some utility functions.
module Main (main) where

import Data.List (intercalate)
import Zhp

import Data.Time.Zones.All

allZones :: [TZLabel]
allZones = [minBound..maxBound]

indent :: String -> String
indent = lines >>> map ("    " <>) >>> unlines

typeDecl = "type Label = " <> intercalate " | " (map show allZones)

fromStringCase :: TZLabel -> String
fromStringCase lbl = show (toTZName lbl) <> " -> Just " <> show lbl

toStringCase :: TZLabel -> String
toStringCase lbl = show lbl <> " -> " <> show (toTZName lbl)

fromStringDef = unlines
    [ "labelFromString : String -> Maybe Label"
    , "labelFromString s ="
    , "    case s of"
    , indent $ indent $ unlines
        (map fromStringCase allZones
            <> ["_ -> Nothing"])
    ]

toStringDef = unlines
    [ "labelToString : Label -> String"
    , "labelToString lbl ="
    , "   case lbl of"
    , indent $ indent $ unlines (map toStringCase allZones)
    ]

allZonesDef = unlines
    [ "allZones : List Label"
    , "allZones = " <> show allZones
    ]

encodeDef = unlines
    [ "encodeLabel : Label -> E.Value"
    , "encodeLabel = labelToString >> E.string"
    ]

decodeDef = unlines
    [ "decodeLabel : D.Decoder Label"
    , "decodeLabel ="
    , indent $ unlines
        [ "D.string |> D.andThen"
        , "(labelFromString"
        , ">> Maybe.map D.succeed"
        , ">> Maybe.withDefault (D.fail \"Unknown time zone name.\"))"
        ]
    ]


file = unlines
    [ "module TimeZone exposing"
    , indent $ unlines
        [ "( Label(..)"
        , ", allZones"
        , ", encodeLabel"
        , ", decodeLabel"
        , ", labelToString"
        , ", labelFromString"
        , ")"
        ]
    , "import Json.Decode as D"
    , "import Json.Encode as E"
    , typeDecl
    , allZonesDef
    , fromStringDef
    , toStringDef
    , decodeDef
    , encodeDef
    ]

main = writeFile "ui-gen/TimeZone.elm" file
