module Main exposing (..)

{-| The main entry of application

# Definition
@docs main

-}

import App exposing (..)
import Html exposing (program)


{-|
-}
main : Program Never Model Msg
main =
    Html.program { view = view, init = init, update = update, subscriptions = subscriptions }
