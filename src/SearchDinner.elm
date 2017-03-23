module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, IngredientMember, DinnerMember, getRandomDinner, searchDinners, addNewDinner, addDinnerToShopList, getIngredients, editDinner, editDinnerIngredients, handleHttpError)
import Html exposing (..)
import Css as Css exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as JsonD
import Material
import Task
import Material.Elevation as Elevation
import Material.Dialog as Dialog
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Textfield as Textfield
import Material.Color as Color
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Snackbar as Snackbar
import Material.Spinner as Loading
import Material.Icon as Icon
import Material.Table as Table
import Material.Typography as Typo


--MODEL


type alias Model =
    { currentDinner : Dinner
    , dinners : List Dinner
    , ingredients : List Ingredient
    , searchText : String
    , raised : Int
    , ingrCounter : Int
    , waiting : Bool
    , dialogType : DialogType
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type DialogType
    = ShowIngredientsDia
    | EditDinnerDia
    | EditIngredientsDia

type MatrButton
    = MiniFab
    | MiniFabAccent
    | DiagClose
    | Normal



--INIT


init : Model
init =
    Model (Dinner "" "" "" "" "" 0) [] [] "" -1 -1 False ShowIngredientsDia Snackbar.model Material.model



--MSG


type Msg
    = GetRandomDinner
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
    | SearchIngredients Dinner
    | SearchIngredientsResult (Result Http.Error (List Ingredient))
    | AddIngredient
    | EditIngredient IngredientMember Ingredient String
    | EditIngrInDb
    | RemoveIngredient Ingredient
    | AddDinnerToShopList
    | JsonResponse (Result Http.Error (String))
    | EditIngredientsView
    | EditDinnerDialog Dinner
    | EditDinner DinnerMember String
    | EditDinnerInDb
    | EditDinnerResult (Result Http.Error String)
    | KeyDown Int
    | Raise Int
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
    | AddToast String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomDinner ->
            ( { model | waiting = True }, getRandomDinner SearchResults )

        SearchResults (Ok dinnersFound) ->
            ( { model | dinners = dinnersFound, waiting = False }, Cmd.none )

        SearchResults (Err error) ->
            update (AddToast (handleHttpError error)) model

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )

        SearchDinners ->
            ( { model | waiting = True }, searchDinners model.searchText SearchResults )

        SearchIngredients dinner ->
            ( { model | dialogType = ShowIngredientsDia, currentDinner = dinner }, getIngredients dinner.id SearchIngredientsResult )

        SearchIngredientsResult (Ok ingredientsFound) ->
            ( { model | ingredients = ingredientsFound }, Cmd.none )

        SearchIngredientsResult (Err error) ->
            ( model, Cmd.none )

        AddIngredient ->
            ( { model | ingrCounter = model.ingrCounter - 1, ingredients = addNewIngredient model }, Cmd.none )

        EditIngredient memberType ingredient value ->
            ( { model | ingredients = (List.filterMap (editTableIngredientInList memberType value ingredient) model.ingredients) }, Cmd.none )

        EditIngrInDb ->
            ( { model | waiting = True }, editDinnerIngredients model.currentDinner model.ingredients EditDinnerResult )

        RemoveIngredient ingredient ->
            ( { model | ingredients = (List.filterMap (removeIngredientFromList ingredient (List.length model.ingredients)) model.ingredients) }, Cmd.none )

        AddDinnerToShopList ->
            (model, addDinnerToShopList model.currentDinner model.ingredients JsonResponse )

        JsonResponse (Ok response) ->
            update (AddToast response) model 

        JsonResponse (Err error) ->
            update (AddToast (handleHttpError error)) model          

        EditIngredientsView ->
            ( { model | dialogType = EditIngredientsDia }, Cmd.none )

        EditDinnerDialog dinner ->
            ( { model | dialogType = EditDinnerDia, currentDinner = dinner }, Cmd.none )

        EditDinner memberType newValue ->
            ( { model | currentDinner = editDinnerMember memberType newValue model.currentDinner }, Cmd.none )

        EditDinnerInDb ->
            ( { model | waiting = True }, editDinner model.currentDinner [] EditDinnerResult )

        EditDinnerResult (Ok response) ->
            --addToast (Snackbar.toast 1 response) { model | waiting = False }
            --(update (AddToast "Bad webservice URL") model)
            (update SearchDinners { model | waiting = False })

        EditDinnerResult (Err error) ->
            update (AddToast (handleHttpError error)) model

        KeyDown key ->
            if key == 13 then
                ( { model | waiting = True }, searchDinners model.searchText SearchResults )
            else
                ( model, Cmd.none )

        Raise k ->
            { model | raised = k } ! []

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model

        AddToast message ->
            addToast (Snackbar.toast 1 message) { model | waiting = False }


view : Model -> Html Msg
view model =
    Options.div
        Css.flexFlowColumnAlignCenter
        [ h3 [ style [ ( "padding", "0rem" ) ] ] []
        , Options.div
            Css.flexFlowRowAlignCenter
            [ materialInput "Search..." SearchText model model.searchText 1
            , materialButton model MiniFab SearchDinners "search" 2
            ]
        , materialButton model Normal GetRandomDinner "I feel lucky" 2
        , br [] []
        , Loading.spinner [ Loading.active model.waiting ]
        , List.map2 (dinnerCardCell model) model.dinners (List.range 1 (List.length model.dinners)) |> grid [ css "justify-content" "center" ]
        , dialogView model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


dialogView : Model -> Html Msg
dialogView model =
    case model.dialogType of
        ShowIngredientsDia ->
            showIngredientView model

        EditDinnerDia ->
            editDinnerView model

        EditIngredientsDia ->
            editIngredientView model


showIngredientView : Model -> Html Msg
showIngredientView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Ingredients" ]
        , Dialog.content []
            [ p []
                [ ingredientsTable model ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Close" ]
            , Button.render Mdl
                [ 0 ]
                model.mdl
                [ Options.onClick EditIngredientsView ]
                [ text "Edit" ]
            , materialButton model DiagClose AddDinnerToShopList "Buy" 3
            ]
        ]


editDinnerView : Model -> Html Msg
editDinnerView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Edit Dinner" ]
        , Dialog.content []
            [ materialInput "Name" (EditDinner ServerApi.DinnerName) model model.currentDinner.name 1
            , materialInput "Portions" (EditDinner ServerApi.Portions) model model.currentDinner.portions 2
            , materialInput "Tags" (EditDinner ServerApi.Tags) model model.currentDinner.tags 3
            , materialInput "Picture Url" (EditDinner ServerApi.PicUrl) model model.currentDinner.picUrl 4
            , materialInput "Url (optional)" (EditDinner ServerApi.Url) model model.currentDinner.url 5
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 20, 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            , materialButton model DiagClose EditDinnerInDb "Save" 2
            ]
        ]


dinnerCardCell : Model -> Dinner -> Int -> Material.Grid.Cell Msg
dinnerCardCell model dinner i =
    cell (Css.cellStyle "256")
        [ cardView model dinner i
        ]



cardView : Model -> Dinner -> Int -> Html Msg
cardView model dinner i =
    div []
        [ Card.view
            [ dynamic i model
            , css "width" "256px"
            , css "height" "340px"
            ]
            [ Card.title
                [ css "background" ("url('" ++ dinner.picUrl ++ "') center / cover")
                , css "height" "256px"
                , css "padding" "0"
                , Options.onClick (EditDinnerDialog dinner)
                , Dialog.openOn "click"
                  -- Clear default padding to encompass scrim
                ]
                [ Card.head
                    [ white
                    , Options.scrim 0.75
                    , css "padding" "16px"
                      -- Restore default padding inside scrim
                    , css "width" "100%"
                    ]
                    [ text dinner.name ]
                ]
            , Card.text []
                [ text (dinner.portions ++ " portions - " ++ dinner.tags) ]
            , Card.actions
                [ Card.border ]
                [ Button.render Mdl
                    [ 1, 0, i ]
                    model.mdl
                    [ Button.ripple, Button.accent, Options.onClick (SearchIngredients dinner), Dialog.openOn "click" ]
                    [ text "Ingredients" ]
                , Button.render Mdl
                    [ 1, 1, i ]
                    model.mdl
                    [ Button.ripple
                    , if (String.length dinner.url < 1) then
                        Button.disabled
                      else
                        Button.link dinner.url
                    , Button.accent
                    , Options.attribute <| Html.Attributes.target "_blank"
                    ]
                    [ text "Website" ]
                ]
            ]
        ]

ingredientsTable : Model -> Html Msg
ingredientsTable model =
    table [ align "center" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Quantity" ]
                , th [] [ text "Unit" ]
                , th [] []
                ]
            ]
        , tbody [] (List.map renderIngredients model.ingredients)
        ]


renderIngredients : Ingredient -> Html Msg
renderIngredients ingredient =
    tr []
        [ td [ align "left" ] [ text ingredient.name ]
        , td [ align "left" ] [ text ingredient.qty ]
        , td [ align "left" ] [ text ingredient.unit ]
        ]


editIngredientView : Model -> Html Msg
editIngredientView model =
    Dialog.view
        [ css "width" "420px" ]
        [ Dialog.title [] [ text "Ingredients" ]
        , Dialog.content []
            [ div []
                [ Options.div Css.flexFlowRowAlignCenter
                    [ (Options.styled p [ Typo.title ] [ ])
                    ]
                , editIngredientsTable model
                , materialButton model MiniFab AddIngredient "add_circle" 1
                ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Close" ]
            , materialButton model DiagClose EditIngrInDb "Save" 15
            ]
        ]


editIngredientsTable : Model -> Html Msg
editIngredientsTable model =
    Table.table [css "margin-left" "-15px"]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Quantity" ]
                , Table.th [] [ text "Unit" ]
                , Table.th [] []
                ]
            ]
        , Table.tbody [] (List.map (editRenderIngredients model) (List.reverse model.ingredients))
        ]


editRenderIngredients : Model -> Ingredient -> Html Msg
editRenderIngredients model ingr =
    Table.tr []
        [ Table.td [] [ ingredientInputMaterial "Name" (EditIngredient ServerApi.IngredientName ingr) model ingr.name 1 ingr.id 8 ]
        , Table.td [] [ ingredientInputMaterial "Qty" (EditIngredient ServerApi.Qty ingr) model ingr.qty 2 ingr.id 2 ]
        , Table.td [] [ ingredientInputMaterial "Unit" (EditIngredient ServerApi.Unit ingr) model ingr.unit 3 ingr.id 2 ]
        , Table.td [] [ materialButton model MiniFabAccent (RemoveIngredient ingr) "remove_circle" 1 ]
        ]


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
        , (effect)
        )


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JsonD.map tagger keyCode)



-- MATERIAL UI CONSTRUCTORS

materialButton : Model -> MatrButton -> Msg -> String -> Int -> Html Msg
materialButton model matrButton msg display group =
    let 
        (idGroup ,buttonOptions, displayOptions) = 
            case matrButton of
                Normal -> 
                    ([2, group ]
                    , [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick msg
                    , css "margin-top" "12px"
                    ]
                    ,[ text display ] )      

                MiniFab ->                    
                    ([3, group ]
                    ,[ Options.onClick (msg)
                    , Button.minifab
                    , Button.colored
                    , css "margin-top" "15px"
                    ]
                    ,[ Icon.view display [ Icon.size24 ] ])
                        

                MiniFabAccent ->
                    ([4, group ]
                    ,[ Options.onClick (msg)
                    , Button.minifab
                    , Button.colored
                    , Button.accent
                    ]
                    ,[ Icon.i display ])
                    

                DiagClose ->  
                    ([5, group ]
                    ,[ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick msg
                    , Dialog.closeOn "click"
                    ]
                    ,[ text display ])
    in         
        Button.render Mdl
        idGroup        
        model.mdl
        buttonOptions
        displayOptions     


materialInput : String -> (String -> Msg) -> Model -> String -> Int -> Html Msg
materialInput placeHolder msg model defValue group =
    div []
        [ Textfield.render Mdl
            [ 2, group ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.floatingLabel
            , Textfield.text_
            , Options.onInput msg
            , Options.attribute (onKeyDown KeyDown)
            , Textfield.value defValue
            ]
            []
        ]


ingredientInputMaterial : String -> (String -> Msg) -> Model -> String -> Int -> Int -> Int -> Html Msg
ingredientInputMaterial placeHolder msg model defValue x y txtWidth =
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
            ]
            []
        ]



-- GETTERS & SETTERS

dynamic : Int -> Model -> Options.Style Msg
dynamic k model =
    [ if model.raised == k then
        Elevation.e16
      else
        Elevation.e2
    , Elevation.transition 250
    , Options.onMouseEnter (Raise k)
    , Options.onMouseLeave (Raise -1)
    ]
        |> Options.many


white : Options.Property c m
white =
    Color.text Color.white

addNewIngredient : Model -> List Ingredient
addNewIngredient model =
    (Ingredient "" "" "" model.ingrCounter) :: model.ingredients


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


removeIngredientFromList : Ingredient -> Int -> Ingredient -> Maybe Ingredient
removeIngredientFromList ingrToCheck nrOfIngredients ingr =
    if ingrToCheck == ingr && nrOfIngredients > 1 then
        Nothing
    else
        Just ingr


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
