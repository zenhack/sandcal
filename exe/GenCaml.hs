import Zhp

import Data.List (intercalate)

import qualified Data.Time.Zones.All as TZ

camlSrc :: String
camlSrc = mconcat
    [ "let tz_labels = ["
    , map (TZ.toTZName >>> show) [minBound..maxBound]
        & intercalate "; "
    , "]\n"
    ]

main = writeFile "ui/src/gen_tz.ml" camlSrc
