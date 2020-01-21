module Route
    ( Route(..)
    , Method(..)
    , path
    , method
    ) where

import Zhp

data Route
    = Root
    | NewEvent Method

data Method = GET | POST

path :: IsString a => Route -> a
path Root         = "/"
path (NewEvent _) = "/event/new"

method :: Route -> Method
method Root         = GET
method (NewEvent m) = m
