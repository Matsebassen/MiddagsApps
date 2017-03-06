module Main exposing (..)

import App exposing (..)
import Html exposing (program)


main : Program Never Model Msg
main =
    Html.program { view = view, init = init, update = update, subscriptions = subscriptions }
