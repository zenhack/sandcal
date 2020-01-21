module Route
    ( Route(..)
    , Method(..)
    , path
    , method
    ) where

import Zhp

data Route
    = Root
    | AllEvents
    | NewEvent Method

data Method = GET | POST

path :: IsString a => Route -> a
path Root         = "/"
path AllEvents    = "/api/all-events.json"
path (NewEvent _) = "/event/new"

method :: Route -> Method
method Root         = GET
method AllEvents    = GET
method (NewEvent m) = m
