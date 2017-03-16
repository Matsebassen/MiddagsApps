module MaterialUI exposing (..)

import Html exposing (..)
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material.Button as Button

type MatrButton
    = MiniFab
    | MiniFabAccent
    | DiagOpen
    | Normal

materialButtonOptions : MatrButton -> msg -> String -> Int -> ((List Int), (List (Button.Property m)), (List (Html m)))
materialButtonOptions matrButton msg display group =
    case matrButton of
        Normal -> 
            ([2, group ]
            ,[                   
            Button.raised
            , Button.colored
            , Button.ripple  
            , Options.onClick msg
            , css "margin" "40px 12px"                        
            ]
            , [ text display ])

        MiniFab ->                    
            ([3, group ]
            ,[   
            Button.minifab
            , Button.colored
            , Options.onClick msg
            ]
            ,[ Icon.i display ])
                        

        MiniFabAccent ->
            ([4, group ]
            ,[   
            Button.minifab
            , Button.colored
            , Button.accent
            , Options.onClick msg
            ]
            ,[ Icon.i display ])
                    

        DiagOpen ->  
            ([5, group ]
            ,[   
            Button.minifab
            , Button.colored     
            , Dialog.openOn "click"
            , Options.onClick msg
            , css "margin-top" "-8px"        
            ]
            ,[ Icon.view display [ Icon.size36 ] ])

