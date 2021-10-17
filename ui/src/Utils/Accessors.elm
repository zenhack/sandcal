module Utils.Accessors exposing (nth)

import Accessors
import List.Extra


nth i v =
    let
        getter =
            List.Extra.getAt i >> Maybe.withDefault v
    in
    Accessors.makeOneToOne
        getter
        (\change list ->
            List.Extra.setAt i (change (getter list)) list
        )
