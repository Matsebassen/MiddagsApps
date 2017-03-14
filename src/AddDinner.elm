module AddDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, addNewDinner)
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
    , ingredients : List TableIngredient
    , inputIngredients : String
    , ingrCounter : Int
    , waiting : Bool
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
    | IngredientName TableIngredient String
    | IngredientQty TableIngredient String
    | IngredientUnit TableIngredient String
    | AddIngredient
    | AddIngredientsFromList
    | RemoveIngredient TableIngredient
    | KeyDown Int
    | Snackbar (Snackbar.Msg Int)
    | Nop
    | InputAsList
    | IngredientsListInput String
    | IncrementCounter


type alias Mdl =
    Material.Model


type alias TableIngredient =
    { index : Int
    , ingredient : Ingredient
    }



--INIT


init : Model
init =
    Model (Dinner "" "" "" "" "" 0) [ (TableIngredient 1 (Ingredient "" "" "")) ] "" 2 False Snackbar.model Material.model



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDinner ->
            ( { model | waiting = True }, addNewDinner model.dinner (tableIngredientsToIngredients model.ingredients) JsonResponse )

        JsonResponse (Ok response) ->
            addToast (Snackbar.toast 1 response) (Model (Dinner "" "" "" "" "" 0) [ (TableIngredient 1 (Ingredient "" "" "")) ] "" 2 False Snackbar.model model.mdl)

        JsonResponse (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    addToast (Snackbar.toast 1 ("That was a shitty url. Message: " ++ badUrlMsg)) { model | waiting = False }

                Http.Timeout ->
                    addToast (Snackbar.toast 1 "The request timed out") { model | waiting = False }

                Http.NetworkError ->
                    addToast (Snackbar.toast 1 "Can't contact server") { model | waiting = False }

                Http.BadStatus badResponse ->
                    addToast (Snackbar.toast 1 (badResponse.body)) { model | waiting = False }

                Http.BadPayload debugMessage badResponse ->
                    addToast (Snackbar.toast 1 "Bad payload. Perhaps wrong JSON format?") { model | waiting = False }

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

        InputAsList ->
            ( model, Cmd.none )

        IngredientsListInput input ->
            ( { model | inputIngredients = input }, Cmd.none )

        AddIngredientsFromList ->
            ( { model | ingredients = List.append (arrayToIngredients model (listToNestedArray (String.lines model.inputIngredients))) model.ingredients, inputIngredients = "" }, Cmd.none )

        IncrementCounter ->
            ( { model | ingrCounter = model.ingrCounter + 1 }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



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
            , materialButtonDiagOpen model InputAsList "playlist_add"
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


renderIngredients : Model -> TableIngredient -> Html Msg
renderIngredients model ingr =
    Table.tr []
        [ Table.td [] [ ingredientInputMaterial "Name" (IngredientName ingr) model ingr.ingredient.name 1 ingr.index 10 ]
        , Table.td [] [ ingredientInputMaterial "Qty" (IngredientQty ingr) model ingr.ingredient.qty 2 ingr.index 3 ]
        , Table.td [] [ ingredientInputMaterial "Unit" (IngredientUnit ingr) model ingr.ingredient.unit 3 ingr.index 3 ]
        , Table.td [] [ materialMiniFabAccent model (RemoveIngredient ingr) "remove_circle" ]
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


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    Button.render Mdl
        [ group ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick msg
        , css "margin" "40px 12px"
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
    List.map (Regex.split (Regex.All) (Regex.regex " ")) list


arrayToIngredients : Model -> List (List String) -> List TableIngredient
arrayToIngredients model ingredientList =
    List.map (arrayToIngredient model) ingredientList


arrayToIngredient : Model -> List String -> TableIngredient
arrayToIngredient model ingrArray =
    TableIngredient 0 (Ingredient (sumIngrName (List.drop 2 ingrArray)) (getIngrPart ingrArray 0) (getIngrPart ingrArray 1))


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


removeIngredientFromList : TableIngredient -> Int -> TableIngredient -> Maybe TableIngredient
removeIngredientFromList ingrToCheck nrOfIngredients ingr =
    if ingrToCheck == ingr && nrOfIngredients > 1 then
        Nothing
    else
        Just ingr


editIngredientNameInList : String -> TableIngredient -> TableIngredient -> Maybe TableIngredient
editIngredientNameInList newName ingrToEdit ingr =
    if ingrToEdit == ingr then
        Just { ingrToEdit | ingredient = setIngredientName newName ingrToEdit.ingredient }
    else
        Just ingr


editIngredientQtyInList : String -> TableIngredient -> TableIngredient -> Maybe TableIngredient
editIngredientQtyInList newQty ingrToEdit ingr =
    if ingrToEdit == ingr then
        Just { ingrToEdit | ingredient = setIngredientQty newQty ingrToEdit.ingredient }
    else
        Just ingr


editIngredientUnitInList : String -> TableIngredient -> TableIngredient -> Maybe TableIngredient
editIngredientUnitInList newUnit ingrToEdit ingr =
    if ingrToEdit == ingr then
        Just { ingrToEdit | ingredient = setIngredientUnit newUnit ingrToEdit.ingredient }
    else
        Just ingr


addNewIngredient : Model -> List TableIngredient
addNewIngredient model =
    (TableIngredient model.ingrCounter (Ingredient "" "" "")) :: model.ingredients


tableIngredientsToIngredients : List TableIngredient -> List Ingredient
tableIngredientsToIngredients tableingredients =
    List.map getIngredient tableingredients


getIngredient : TableIngredient -> Ingredient
getIngredient tableIngredient =
    tableIngredient.ingredient


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


setIngredientName : String -> Ingredient -> Ingredient
setIngredientName value ingredient =
    { ingredient | name = value }


setIngredientQty : String -> Ingredient -> Ingredient
setIngredientQty value ingredient =
    { ingredient | qty = value }


setIngredientUnit : String -> Ingredient -> Ingredient
setIngredientUnit value ingredient =
    { ingredient | unit = value }
