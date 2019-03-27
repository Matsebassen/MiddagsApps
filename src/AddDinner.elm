module AddDinner exposing (..)

import ServerApi exposing (Dinner, TrineDinner, Ingredient, IngredientMember, DinnerMember, getRandomDinner, addNewDinner, getTrineDinner, handleHttpError)
import Css as Css exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on)
import Json.Decode as JsonD
import Html.Attributes exposing (..)
import Http exposing (..)
import Regex exposing (split)
import Array exposing (..)
import Material
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Material.Card as Card
import Material.Color as Color
import Material.Typography as Typo
import Material.Elevation as Elevation
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.Table as Table
import Material.Progress as Loading
import Material.Layout as Layout
import Dom.Scroll
import Task


--MODEL


type alias Model =
    { dinner : Dinner
    , ingredients : List Ingredient
    , inputIngredients : String
    , ingrCounter : Int
    , waiting : Bool
    , showImportTrine : String
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type Msg
    = JsonResponse (Result Http.Error String)
    | AddDinner
    | Mdl (Material.Msg Msg)
    | ImportTrineDinner
    | ImportResults (Result Http.Error (TrineDinner))
    | EditDinner DinnerMember String
    | EditIngredient IngredientMember Ingredient String
    | AddIngredient
    | AddIngredientsFromList
    | RemoveIngredient Ingredient
    | KeyDown Int
    | Snackbar (Snackbar.Msg Int)
    | Nop
    | IngredientsListInput String
    | IncrementCounter
    | AddToast String


type alias Mdl =
    Material.Model

type MatrButton
    = MiniFab
    | MiniFabAccent
    | DiagOpen
    | Normal
    | Trine


--INIT


init : Model
init =
    Model (Dinner "" "" "" "" "" 0) [ (Ingredient "" "" "" 1) ] "" 2 False "hidden" Snackbar.model Material.model



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDinner ->
            ( { model | waiting = True }, addNewDinner model.dinner model.ingredients JsonResponse )

        JsonResponse (Ok response) ->
            addToast (Snackbar.toast 1 response) (Model (Dinner "" "" "" "" "" 0) [ (Ingredient "" "" "" 1) ] "" 2 False "hidden" Snackbar.model model.mdl)

        JsonResponse (Err error) ->
            update (AddToast (handleHttpError error)) model

        ImportTrineDinner ->
            ( { model | waiting = True }, getTrineDinner (getUrl model.dinner) ImportResults )

        ImportResults (Ok dinnerFound) ->
            ( { model | dinner = (setTrineDinner dinnerFound), ingredients = dinnerFound.ingredients, waiting = False }, Cmd.none )

        ImportResults (Err error) ->
            update (AddToast (handleHttpError error)) model            

        EditDinner memberType newValue ->
            ( { model | dinner = editDinnerMember memberType newValue model.dinner, showImportTrine = isTrineUrl memberType newValue model.showImportTrine }, Cmd.none )

        EditIngredient memberType ingredient name ->
            ( { model | ingredients = (List.filterMap (editTableIngredientInList memberType name ingredient) model.ingredients) }, Cmd.none )

        AddIngredient ->
            ( { model | ingrCounter = model.ingrCounter + 1, ingredients = addNewIngredient model }, Task.attempt (always Nop) <| Dom.Scroll.toBottom Layout.mainId )

        RemoveIngredient ingredient ->
            ( { model | ingredients = (List.filterMap (removeIngredientFromList ingredient (List.length model.ingredients)) model.ingredients) }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                ( { model | ingrCounter = model.ingrCounter + 1, ingredients = addNewIngredient model }, Cmd.none )
            else
                ( model, Cmd.none )

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Nop ->
            ( model, Cmd.none )

        IngredientsListInput input ->
            ( { model | inputIngredients = input }, Cmd.none )

        AddIngredientsFromList ->
            ( { model | ingredients = List.append (arrayToIngredients model (listToNestedArray (String.lines model.inputIngredients))) model.ingredients, inputIngredients = "" }, Cmd.none )

        IncrementCounter ->
            ( { model | ingrCounter = model.ingrCounter + 1 }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model
        
        AddToast message ->
            addToast (Snackbar.toast 1 message) { model | waiting = False }



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ Options.div
            flexFlowColumnAlignCenter
            [ dinnerViewCard model
            , ingredientViewCard model
            , br [] []
            , if (model.waiting) then
                Loading.indeterminate
              else
                div [] []
            ]
        , p [] []
        , materialButton model Normal AddDinner "Add Dinner to DB" 2
        , Snackbar.view model.snackbar |> Html.map Snackbar
        , dialogView model
        ]


dinnerView : Model -> Html Msg
dinnerView model =
    div []
        [ (Options.styled p [ Typo.title ] [ text "New Dinner" ])
        , dinnerInputMaterial "Name of dinner" (EditDinner ServerApi.DinnerName) model model.dinner.name False 1
        , dinnerInputMaterial "Portions" (EditDinner ServerApi.Portions) model model.dinner.portions False 2
        , dinnerInputMaterial "Tags" (EditDinner ServerApi.Tags) model model.dinner.tags False 3
        , dinnerInputMaterial "Picture Url (optional)" (EditDinner ServerApi.PicUrl) model model.dinner.picUrl True 4
        , dinnerInputMaterial "Url (optional)" (EditDinner ServerApi.Url) model model.dinner.url True 5
        , materialButton model Trine ImportTrineDinner "Import Trine Dinner" 6           
        ]        


dinnerViewCard : Model -> Html Msg
dinnerViewCard model =
    Card.view
        (Elevation.e8 :: Css.addDinnerCardCss)
        [ Card.text [ Card.expand ] []
          -- Filler
        , Card.text
            [ css "background" "rgba(0, 0, 0, 0)" ]
            -- Non-gradient scrim
            [ Options.span
                [ black, Typo.title, Typo.contrast 1.0 ]
                [ dinnerView model ]
            ]
        ]


ingredientView : Model -> Html Msg
ingredientView model =
    div []
        [ Options.div Css.flexFlowRowAlignCenter
            [ (Options.styled p [ Typo.title ] [ text "Ingredients" ])
            , materialButton model DiagOpen  Nop "playlist_add" 1
            ]
        , ingredientsTable model
        ]


ingredientViewCard : Model -> Html Msg
ingredientViewCard model =
    Card.view
        ([ Elevation.e8 ] ++ Css.addDinnerCardCss)
        [ Card.text [ Card.expand ] []
          -- Filler
        , Card.text
            []
            -- Non-gradient scrim
            [ Options.span
                [ black, Typo.title, Typo.contrast 1.0 ]
                [ Options.div
                    Css.flexFlowRowAlignCenter
                    [ div []
                        [ ingredientView model
                        , materialButton model MiniFab  AddIngredient "add_circle" 1
                        ]
                    ]
                ]
            ]
        ]


ingredientsTable : Model -> Html Msg
ingredientsTable model =
    Table.table []
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Quantity" ]
                , Table.th [] [ text "Unit" ]
                , Table.th [] []
                ]
            ]
        , Table.tbody [] (List.map (renderIngredients model) (List.reverse model.ingredients))
        ]


renderIngredients : Model -> Ingredient -> Html Msg
renderIngredients model ingr =
    Table.tr []
        [ Table.td [] [ ingredientInputMaterial "Name" (EditIngredient ServerApi.IngredientName ingr) model ingr.name False 1 ingr.id 10 ]
        , Table.td [] [ ingredientInputMaterial "Qty" (EditIngredient ServerApi.Qty ingr) model ingr.qty True 2 ingr.id 3 ]
        , Table.td [] [ ingredientInputMaterial "Unit" (EditIngredient ServerApi.Unit ingr) model ingr.unit True 3 ingr.id 3 ]
        , Table.td [] [ materialButton model MiniFabAccent (RemoveIngredient ingr) "remove_circle" 1 ]
        ]


dialogView : Model -> Html Msg
dialogView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "List of ingredients" ]
        , Dialog.content []
            [ p []
                [ div []
                    [ Options.styled p [ Typo.body2 ] [ text "FORMAT: Quantity  Unit  Name" ]
                    , Textfield.render Mdl
                        [ 10 ]
                        model.mdl
                        [ Textfield.label "Paste the recipe here..."
                        , Textfield.floatingLabel
                        , Textfield.textarea
                        , Textfield.rows 10
                        , Options.onInput IngredientsListInput
                        , Textfield.value model.inputIngredients
                        ]
                        []
                    ]
                ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick AddIngredientsFromList
                ]
                [ text "Add" ]
            ]
        ]



-- HELPER FUNCTIONS


radio : String -> msg -> Html msg
radio value msg =
    label
        [ style []
        ]
        [ input [ type_ "radio", name "font-size", onClick msg ] []
        , text value
        ]


black : Options.Property c m
black =
    Color.text Color.black


dinnerInputMaterial : String -> (String -> Msg) -> Model -> String -> Bool -> Int -> Html Msg
dinnerInputMaterial placeHolder msg model defValue canBeEmpty group =
    div []
        [ Textfield.render Mdl
            [ 1, group ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.text_
            , Textfield.floatingLabel
            , Options.onInput msg
            , Textfield.value defValue
            , (Textfield.error ("Can't be empty") 
                |> Options.when (String.length defValue == 0 && (List.length model.ingredients > 1))) |> Options.when (not <| canBeEmpty )
            ]
            []
        ]


ingredientInputMaterial : String -> (String -> Msg) -> Model -> String -> Bool -> Int -> Int -> Int -> Html Msg
ingredientInputMaterial placeHolder msg model defValue canBeEmpty x y txtWidth =
    div []
        [ Textfield.render Mdl
            [ 2, x, y ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.text_            
            , Options.onInput msg
            , Textfield.value defValue
            , Options.attribute (onKeyDown KeyDown)
            , css "width" (toString txtWidth ++ "rem")
            , css "margin-top" "-1rem"
            , css "margin-bottom" "-1rem"
            , (Textfield.error ("Can't be empty") 
                |> Options.when (String.length defValue == 0  && (List.length model.ingredients > 1))) |> Options.when (not <| canBeEmpty )
            ]
            []
        ]

materialButton : Model -> MatrButton -> Msg -> String -> Int -> Html Msg
materialButton model matrButton msg display group =
    let 
        (idGroup ,buttonOptions, displayOptions) = 
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

                Trine -> 
                    ([6, group ]
                    ,[                   
                    Button.raised
                    , Button.colored
                    , Button.ripple  
                    , Options.onClick msg
                    , css "visibility" model.showImportTrine                   
                    ]
                    , [text display])                       
    in         
        Button.render Mdl
        idGroup        
        model.mdl
        buttonOptions
        displayOptions     


addToast : Snackbar.Contents Int -> Model -> ( Model, Cmd Msg )
addToast f model =
    let
        ( snackbar_, effect ) =
            Snackbar.add (f) model.snackbar
                |> map2nd (Cmd.map Snackbar)

        model_ =
            { model
                | snackbar = snackbar_
            }
    in
        ( model_
        , effect
        )


listToNestedArray : List String -> List (List String)
listToNestedArray list =
    List.map (Regex.split (Regex.All) (Regex.regex " ")) list


arrayToIngredients : Model -> List (List String) -> List Ingredient
arrayToIngredients model ingredientList =
    List.map (arrayToIngredient model) ingredientList


arrayToIngredient : Model -> List String -> Ingredient
arrayToIngredient model ingrArray =
    Ingredient (sumIngrName (List.drop 2 ingrArray)) (getIngrPart ingrArray 0) (getIngrPart ingrArray 1) 0


fromJust : Maybe String -> String
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            ""


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JsonD.map tagger keyCode)



-- GETTERS & SETTERS


sumIngrName : List String -> String
sumIngrName lst =
    case lst of
        [] ->
            ""
        x :: xs ->
            x ++ " " ++ sumIngrName (xs)


getIngrPart : List String -> Int -> String
getIngrPart array partNo =
    (fromJust (Array.get partNo (Array.fromList array)))


getUrl : Dinner -> String
getUrl dinner = 
    dinner.url


setTrineDinner : TrineDinner -> Dinner
setTrineDinner trineDinner = 
    Dinner trineDinner.name trineDinner.url trineDinner.tags trineDinner.portions trineDinner.picUrl trineDinner.id    

removeIngredientFromList : Ingredient -> Int -> Ingredient -> Maybe Ingredient
removeIngredientFromList ingrToCheck nrOfIngredients ingr =
    if ingrToCheck == ingr && nrOfIngredients > 1 then
        Nothing
    else
        Just ingr


editTableIngredientInList : IngredientMember -> String -> Ingredient -> Ingredient -> Maybe Ingredient
editTableIngredientInList memberType newValue ingrToEdit ingr =
    if ingrToEdit == ingr then
        case memberType of
            ServerApi.IngredientName ->
                Just { ingrToEdit | name = newValue }

            ServerApi.Qty ->
                Just { ingrToEdit | qty = newValue }

            ServerApi.Unit ->
                Just { ingrToEdit | unit = newValue }
    else
        Just ingr


addNewIngredient : Model -> List Ingredient
addNewIngredient model =
    (Ingredient "" "" "" model.ingrCounter) :: model.ingredients


editDinnerMember : DinnerMember -> String -> Dinner -> Dinner
editDinnerMember memberType newValue dinner =
    case memberType of
        ServerApi.DinnerName ->
            { dinner | name = newValue }

        ServerApi.Url ->
            { dinner | url = newValue }

        ServerApi.Tags ->
            { dinner | tags = newValue }

        ServerApi.Portions ->
            { dinner | portions = newValue }

        ServerApi.PicUrl ->
            { dinner | picUrl = newValue }

isTrineUrl : DinnerMember -> String -> String -> String
isTrineUrl memberType newValue currentShowTrine =
    case memberType of
        ServerApi.DinnerName ->
            currentShowTrine

        ServerApi.Url ->
            if (String.contains "trinesmatblogg.no" newValue) then
                "visible"
            else
                "hidden"

        ServerApi.Tags ->
            currentShowTrine

        ServerApi.Portions ->
            currentShowTrine

        ServerApi.PicUrl ->
            currentShowTrine
