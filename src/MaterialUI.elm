module MaterialUI exposing (..)

import Html exposing (..)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield


dinnerInputMaterial : mdl -> String -> (String -> msg) -> model -> String -> Int -> Html msg
dinnerInputMaterial mdl placeHolder msg model defValue group =
    div []
        [ Textfield.render Mdl
            [ group ]
            mdl
            [ Textfield.label placeHolder
            , Textfield.floatingLabel
            , Textfield.text_
            , Options.onInput msg
            , Textfield.value defValue
            ]
            []
        ]

