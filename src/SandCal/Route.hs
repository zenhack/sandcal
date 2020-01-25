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
    | Script
    | AllEvents
    | Event !Int
    | NewEvent Method
    deriving(Show, Read, Eq)

data Method = GET | POST
    deriving(Show, Read, Eq, Bounded, Enum)

allRoutes = mconcat
    [ [ Root
      , Script
      , AllEvents
      ]
    , [ NewEvent m | m <- [minBound..maxBound] ]
    ]

path :: IsString a => Route -> a
path Root            = "/"
path Script          = "/ui.js"
path AllEvents       = "/api/all-events.json"
path (Event eid)     = fromString $ "/event/" <> show eid
path (NewEvent GET)  = "/event/new"
path (NewEvent POST) = "/api/event/new"

method :: Route -> Method
method Script       = GET
method Root         = GET
method AllEvents    = GET
method (Event _)    = GET
method (NewEvent m) = m
