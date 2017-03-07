module AddDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, addNewDinner)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
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


--MODEL


type alias Model =
    { dinner : Dinner
    , ingredients : List Ingredient
    , inputIngredients : List String
    , ingredientFormat : IngredientFormat
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type Msg
    = JsonResponse (Result Http.Error String)
    | AddDinner
    | Mdl (Material.Msg Msg)
    | DinnerName String
    | DinnerUrl String
    | DinnerPicUrl String
    | DinnerTags String
    | DinnerPortions String
    | IngredientName Ingredient String
    | IngredientQty Ingredient String
    | IngredientUnit Ingredient String
    | AddIngredient
    | RemoveIngredient Ingredient
    | Snackbar (Snackbar.Msg Int)
    | InputAsList
    | IngredientsListInput String
    | SwitchFormat IngredientFormat


type alias Mdl =
    Material.Model


type IngredientFormat
    = NameQtyUnit
    | QtyUnitName
    | UnitQtyName



--INIT


init : Model
init =
    Model (Dinner "" "" "" "" "") [ (Ingredient "" "" "") ] [] NameQtyUnit Snackbar.model Material.model



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDinner ->
            ( model, addNewDinner model.dinner model.ingredients JsonResponse )

        JsonResponse (Ok response) ->
            addToast (Snackbar.toast 1 response) (Model (Dinner "" "" "" "" "") [ (Ingredient "" "" "") ] [] NameQtyUnit Snackbar.model model.mdl)

        JsonResponse (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    addToast (Snackbar.toast 1 ("That was a shitty url. Message: " ++ badUrlMsg)) model

                Http.Timeout ->
                    addToast (Snackbar.toast 1 "The request timed out") model

                Http.NetworkError ->
                    addToast (Snackbar.toast 1 "Can't contact server") model

                Http.BadStatus badResponse ->
                    addToast (Snackbar.toast 1 "Bad response code from webservice") model

                Http.BadPayload debugMessage badResponse ->
                    addToast (Snackbar.toast 1 "Bad payload. Perhaps wrong JSON format?") model

        DinnerName newName ->
            ( { model | dinner = setDinnerName newName model.dinner }, Cmd.none )

        DinnerUrl newUrl ->
            ( { model | dinner = setDinnerUrl newUrl model.dinner }, Cmd.none )

        DinnerPicUrl newUrl ->
            ( { model | dinner = setDinnerPicUrl newUrl model.dinner }, Cmd.none )

        DinnerTags newTags ->
            ( { model | dinner = setDinnerTags newTags model.dinner }, Cmd.none )

        DinnerPortions newPortions ->
            ( { model | dinner = setDinnerPortions newPortions model.dinner }, Cmd.none )

        IngredientName ingredient name ->
            ( { model | ingredients = (List.filterMap (editIngredientNameInList name ingredient) model.ingredients) }, Cmd.none )

        IngredientQty ingredient qty ->
            ( { model | ingredients = (List.filterMap (editIngredientQtyInList qty ingredient) model.ingredients) }, Cmd.none )

        IngredientUnit ingredient unit ->
            ( { model | ingredients = (List.filterMap (editIngredientUnitInList unit ingredient) model.ingredients) }, Cmd.none )

        AddIngredient ->
            ( { model | ingredients = addNewIngredient model }, Cmd.none )

        RemoveIngredient ingredient ->
            ( { model | ingredients = (List.filterMap (removeIngredientFromList ingredient (List.length model.ingredients)) model.ingredients) }, Cmd.none )

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        InputAsList ->
            ( model, Cmd.none )

        SwitchFormat format ->
            ( { model | ingredientFormat = format }, Cmd.none )

        IngredientsListInput input ->
            ( { model | ingredients = arrayToIngredients (listToNestedArray (String.lines input)) model.ingredientFormat }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ Options.div
            [ css "display" "flex"
            , css "flex-flow" "column wrap"
            , css "align-items" "center"
            , css "width" "100%"
            , css "margin-top" "2rem"
            , css "justify-content" "center"
            ]
            [ dinnerViewCard model
            , ingredientViewCard model
            ]
        , p [] []
        , materialButton model AddDinner "Add Dinner to DB" 2
        , Snackbar.view model.snackbar |> Html.map Snackbar
        , dialogView model
        ]


dinnerView : Model -> Html Msg
dinnerView model =
    div []
        [ (Options.styled p [ Typo.title ] [ text "New Dinner" ])
        , dinnerInputMaterial "Name of dinner" DinnerName model model.dinner.name 1
        , dinnerInputMaterial "Portions" DinnerPortions model model.dinner.portions 2
        , dinnerInputMaterial "Tags" DinnerTags model model.dinner.tags 3
        , dinnerInputMaterial "Picture Url" DinnerPicUrl model model.dinner.picUrl 4
        , dinnerInputMaterial "Url (optional)" DinnerUrl model model.dinner.url 5
        ]


dinnerViewCard : Model -> Html Msg
dinnerViewCard model =
    Card.view
        [ css "width" "500px"
        , css "height" "auto"
        , Elevation.e8
        , css "margin" "0"
        , css "align-items" "center"
        ]
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
        [ Options.div [ css "display" "flex", css "flex-flow" "row wrap", css "justify-content" "center" ]
            [ (Options.styled p [ Typo.title ] [ text "Ingredients" ])
            , materialButtonDiagOpen model InputAsList "playlist_add"
            ]
        , ingredientsTable model
        ]


ingredientViewCard : Model -> Html Msg
ingredientViewCard model =
    Card.view
        [ css "width" "500px"
        , css "height" "auto"
        , Elevation.e8
        , css "margin" "50px 50px 50px 50px "
        , css "align-items" "center"
        , css "justify-content" "center"
        ]
        [ Card.text [ Card.expand ] []
          -- Filler
        , Card.text
            [ css "background" "rgba(0, 0, 0, 0)" ]
            -- Non-gradient scrim
            [ Options.span
                [ black, Typo.title, Typo.contrast 1.0 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-direction" "row"
                    , css "width" "100%"
                    ]
                    [ div []
                        [ ingredientView model
                        , materialMiniFab model AddIngredient "add_circle"
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
renderIngredients model ingredient =
    Table.tr []
        [ Table.td [] [ ingredientInputMaterial "Name" (IngredientName ingredient) model ingredient.name (List.length model.ingredients) 10 ]
        , Table.td [] [ ingredientInputMaterial "Qty" (IngredientQty ingredient) model ingredient.qty (List.length model.ingredients) 3 ]
        , Table.td [] [ ingredientInputMaterial "Unit" (IngredientUnit ingredient) model ingredient.unit (List.length model.ingredients) 3 ]
        , Table.td [] [ materialMiniFabAccent model (RemoveIngredient ingredient) "remove_circle" ]
        ]


dialogView : Model -> Html Msg
dialogView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "List of ingredients" ]
        , Dialog.content []
            [ p []
                [ div []
                    [ fieldset []
                        [ radio "Name - Qty - Unit" (SwitchFormat NameQtyUnit)
                        , br [] []
                        , radio "Qty - Unit - Name" (SwitchFormat QtyUnitName)
                        , br [] []
                        , radio "Unit - Qty - Name" (SwitchFormat UnitQtyName)
                        ]
                    , Textfield.render Mdl
                        [ 10 ]
                        model.mdl
                        [ Textfield.label "Paste the recipe here..."
                        , Textfield.floatingLabel
                        , Textfield.textarea
                        , Textfield.rows 10
                        , Options.onInput IngredientsListInput
                        ]
                        []
                    ]
                ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
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


dinnerInputMaterial : String -> (String -> Msg) -> Model -> String -> Int -> Html Msg
dinnerInputMaterial placeHolder msg model defValue group =
    div []
        [ Textfield.render Mdl
            [ 1, group ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.text_
            , Textfield.floatingLabel
            , Options.onInput msg
            , Textfield.value defValue
            ]
            []
        ]


ingredientInputMaterial : String -> (String -> Msg) -> Model -> String -> Int -> Int -> Html Msg
ingredientInputMaterial placeHolder msg model defValue group txtWidth =
    div []
        [ Textfield.render Mdl
            [ 2, group ]
            model.mdl
            [ Textfield.label placeHolder
            , Textfield.text_
            , Options.onInput msg
            , Textfield.value defValue
            , css "width" (toString txtWidth ++ "rem")
            , css "margin-top" "-1rem"
            , css "margin-bottom" "-1rem"
            ]
            []
        ]


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    Button.render Mdl
        [ group ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick msg
        , css "margin" "0 12px"
        ]
        [ text butText ]


materialButtonDiagOpen : Model -> Msg -> String -> Html Msg
materialButtonDiagOpen model msg icon =
    Button.render Mdl
        [ 5, 1 ]
        model.mdl
        [ Button.minifab
        , Button.colored
        , Options.onClick msg
        , Dialog.openOn "click"
        , css "margin-top" "-8px"
        ]
        [ Icon.view icon [ Icon.size36 ] ]


materialMiniFabAccent : Model -> Msg -> String -> Html Msg
materialMiniFabAccent model msg icon =
    Button.render Mdl
        [ 3, 1 ]
        model.mdl
        [ Options.onClick (msg)
        , Button.minifab
        , Button.colored
        , Button.accent
        ]
        [ Icon.i icon ]


materialMiniFab : Model -> Msg -> String -> Html Msg
materialMiniFab model msg icon =
    Button.render Mdl
        [ 4, 1 ]
        model.mdl
        [ Options.onClick (msg)
        , Button.minifab
        , Button.colored
        ]
        [ Icon.view icon [ Icon.size36 ] ]


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
    List.map (Regex.split (Regex.AtMost 3) (Regex.regex " ")) list


arrayToIngredients : List (List String) -> IngredientFormat -> List Ingredient
arrayToIngredients ingredientList format =
    List.map (arrayToIngredient format) ingredientList


arrayToIngredient : IngredientFormat -> List String -> Ingredient
arrayToIngredient format ingredientArray =
    case format of
        NameQtyUnit ->
            Ingredient (fromJust (Array.get 0 (Array.fromList ingredientArray))) (fromJust (Array.get 1 (Array.fromList ingredientArray))) (fromJust (Array.get 2 (Array.fromList ingredientArray)))

        QtyUnitName ->
            Ingredient (fromJust (Array.get 2 (Array.fromList ingredientArray))) (fromJust (Array.get 0 (Array.fromList ingredientArray))) (fromJust (Array.get 1 (Array.fromList ingredientArray)))

        UnitQtyName ->
            Ingredient (fromJust (Array.get 2 (Array.fromList ingredientArray))) (fromJust (Array.get 1 (Array.fromList ingredientArray))) (fromJust (Array.get 0 (Array.fromList ingredientArray)))


fromJust : Maybe String -> String
fromJust x =
    case x of
        Just y ->
            y

        Nothing ->
            ""



-- GETTERS & SETTERS


removeIngredientFromList : Ingredient -> Int -> Ingredient -> Maybe Ingredient
removeIngredientFromList x nrOfIngredients y =
    if x == y && nrOfIngredients > 1 then
        Nothing
    else
        Just y


editIngredientNameInList : String -> Ingredient -> Ingredient -> Maybe Ingredient
editIngredientNameInList newName x y =
    if x == y then
        Just { x | name = newName }
    else
        Just y


editIngredientQtyInList : String -> Ingredient -> Ingredient -> Maybe Ingredient
editIngredientQtyInList newQty x y =
    if x == y then
        Just { x | qty = newQty }
    else
        Just y


editIngredientUnitInList : String -> Ingredient -> Ingredient -> Maybe Ingredient
editIngredientUnitInList newUnit x y =
    if x == y then
        Just { x | unit = newUnit }
    else
        Just y


addNewIngredient : Model -> List Ingredient
addNewIngredient model =
    (Ingredient "" "" "") :: model.ingredients


setDinnerName : String -> Dinner -> Dinner
setDinnerName value dinner =
    { dinner | name = value }


setDinnerUrl : String -> Dinner -> Dinner
setDinnerUrl value dinner =
    { dinner | url = value }


setDinnerPicUrl : String -> Dinner -> Dinner
setDinnerPicUrl value dinner =
    { dinner | picUrl = value }


setDinnerPortions : String -> Dinner -> Dinner
setDinnerPortions value dinner =
    { dinner | portions = value }


setDinnerTags : String -> Dinner -> Dinner
setDinnerTags value dinner =
    { dinner | tags = value }
