module Css exposing (..)
import Material.Options as Options exposing (css)
import Material.Grid as Grid

addDinnerCardCss : List (Options.Property c m)
addDinnerCardCss = 
    [css "width" "450px"
    , css "height" "auto"        
    , css "margin-top" "50px"
    , css "align-items" "center"]

flexFlowColumnAlignCenter : List (Options.Property c m)
flexFlowColumnAlignCenter = 
    [ css "display" "flex"
    , css "flex-flow" "column wrap"
    , css "align-items" "center"
    , css "justify-content" "center"
    ]

flexFlowRowAlignCenter : List (Options.Property c m)
flexFlowRowAlignCenter = 
    [ css "display" "flex"
    , css "flex-flow" "row wrap"
    , css "align-items" "center"
    , css "justify-content" "center"
    ]

cellStyle : Int -> List (Options.Style a)
cellStyle h =
    [ css "text-sizing" "border-box"
    , css "padding-left" "8px"
    , css "padding-top" "4px"
    , css "width" "256px"
    , Grid.size Grid.Tablet 6
    , Grid.size Grid.Desktop 12
    , Grid.size Grid.Phone 4
    ]