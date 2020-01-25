{-# LANGUAGE TemplateHaskell #-}
module GenElm
    ( elmSource
    ) where

import Zhp

import Data.Aeson (defaultOptions)
import Data.Text  (Text)

import Elminator
import ICal.Types       (Frequency)
import SandCal.ApiTypes

elmSource :: Text
elmSource =
    $(generateFor
        Elm0p19
        defaultOptions
        "SandCal.ApiTypes"
        (Just "./ui/gen/SandCal/ApiTypes.elm") $ do
            include (Proxy :: Proxy Event) $ Everything Mono
            include (Proxy :: Proxy Recur) $ Everything Mono
            include (Proxy :: Proxy Frequency) $ Everything Mono
    )
