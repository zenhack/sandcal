module SandCal.Route
    ( Route(..)
    , Method(..)
    , path
    , method
    , allRoutes
    ) where

import Zhp

data Route
    = Root
    | AllEvents
    | NewEvent Method
    deriving(Show, Read, Eq)

data Method = GET | POST
    deriving(Show, Read, Eq, Bounded, Enum)

allRoutes = mconcat
    [ [ Root
      , AllEvents
      ]
    , [ NewEvent m | m <- [minBound..maxBound] ]
    ]

path :: IsString a => Route -> a
path Root            = "/"
path AllEvents       = "/api/all-events.json"
path (NewEvent GET)  = "/event/new"
path (NewEvent POST) = "/api/event/new"

method :: Route -> Method
method Root         = GET
method AllEvents    = GET
method (NewEvent m) = m
