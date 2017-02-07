module MaterialUI exposing (..)

import Material
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Color as Color

dinnerInputMaterial : String -> (String -> Msg) -> Model -> String -> Int -> Html Msg
dinnerInputMaterial placeHolder msg model defValue group =
    div []
        [ Textfield.render Mdl
            [ group ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.floatingLabel
            , Textfield.text_
            , Options.onInput msg
            , Options.attribute <| value defValue
            ]
            []
        ]



materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    div [] [ Button.render Mdl [ group ] model.mdl [ Button.raised, Button.colored, Button.ripple, Options.onClick msg ] [ text butText ] ]
