import Zhp

import Data.List (intercalate)

import qualified Data.Time.Zones.All as TZ

elmSrc :: String
elmSrc = mconcat
    [ "module GenTz exposing(tzLabels)\n"
    , "\n"
    , "tzLabels = ["
    , map (TZ.toTZName >>> show) [minBound..maxBound]
        & intercalate ", "
    , "]\n"
    ]

main = writeFile "ui/gen/GenTz.elm" elmSrc
