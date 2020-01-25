module GenElm
    ( generate
    ) where

import Zhp

import Elm
import Servant.Elm

import ICal.Types       (Frequency)
import SandCal.ApiTypes

generate :: IO ()
generate = do
    let spec = Spec
            ["SandCal", "ApiTypes"]
            (mconcat
                [ [ defElmImports ]
                , generateElmForAPI (Proxy :: Proxy SandCalApi)
                , typeSpec (Proxy :: Proxy Event)
                , typeSpec (Proxy :: Proxy Recur)
                , typeSpec (Proxy :: Proxy Frequency)
                ]
            )
    specsToDir [spec] "ui/gen"


typeSpec proxy =
    [ toElmTypeSource proxy
    , toElmDecoderSource proxy
    , toElmEncoderSource proxy
    ]
