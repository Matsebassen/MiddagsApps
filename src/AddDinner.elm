module AddDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, addNewDinner)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Material
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Snackbar as Snackbar
import Material.Card as Card
import Material.Color as Color
import Material.Typography as Typo
import Material.Elevation as Elevation


--MODEL


type alias Model =
    { dinner : Dinner
    , ingredients : List Ingredient
    , currentIngredient : Ingredient
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type Msg
    = JsonResponse (Result Http.Error String)
    | AddDinner
    | Mdl (Material.Msg Msg)
    | DinnerName String
    | DinnerUrl String
    | DinnerTags String
    | DinnerPortions String
    | IngredientName String
    | IngredientQty String
    | IngredientUnit String
    | AddIngredient
    | RemoveIngredient Ingredient
    | Snackbar (Snackbar.Msg Int)
    | EditIngredient Ingredient


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model (Dinner "" "" "" "") [] (Ingredient "" "" "") Snackbar.model Material.model



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDinner ->
            ( model, addNewDinner model.dinner model.ingredients JsonResponse )

        JsonResponse (Ok response) ->
            addToast (Snackbar.toast 1 response) (Model (Dinner "" "" "" "") [] (Ingredient "" "" "") Snackbar.model model.mdl)

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

        DinnerTags newTags ->
            ( { model | dinner = setDinnerTags newTags model.dinner }, Cmd.none )

        DinnerPortions newPortions ->
            ( { model | dinner = setDinnerPortions newPortions model.dinner }, Cmd.none )

        IngredientName name ->
            ( { model | currentIngredient = setIngredientName name model.currentIngredient }, Cmd.none )

        IngredientQty qty ->
            ( { model | currentIngredient = setIngredientQty qty model.currentIngredient }, Cmd.none )

        IngredientUnit unit ->
            ( { model | currentIngredient = setIngredientUnit unit model.currentIngredient }, Cmd.none )

        AddIngredient ->
            ( { model | ingredients = addNewIngredient model, currentIngredient = (Ingredient "" "" "") }, Cmd.none )

        RemoveIngredient ingredient ->
            ( { model | ingredients = (List.filterMap (removeIngredientFromList ingredient.name ingredient.qty ingredient.unit) model.ingredients) }, Cmd.none )

        EditIngredient ingredient ->
            ( { model | currentIngredient = ingredient, ingredients = (List.filterMap (removeIngredientFromList ingredient.name ingredient.qty ingredient.unit) model.ingredients) }, Cmd.none )

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ Options.div
            [ css "display" "flex"
            , css "flex-flow" "row wrap"
            , css "align-items" "center"
            , css "width" "100%"
            , css "margin-top" "4rem"
            , css "justify-content" "center"
            ]
            [ dinnerViewCard model
            , ingredientViewCard model
            ]
        , materialButton model AddDinner "Add Dinner to DB" 2
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


dinnerView : Model -> Html Msg
dinnerView model =
    div []
        [ (Options.styled p [ Typo.title ] [ text "New Dinner" ])
        , dinnerInputMaterial "Name of dinner" DinnerName model model.dinner.name 1
        , dinnerInputMaterial "Portions" DinnerPortions model model.dinner.portions 2
        , dinnerInputMaterial "Tags" DinnerTags model model.dinner.tags 3
        , dinnerInputMaterial "Url (optional)" DinnerUrl model model.dinner.url 4
        ]


dinnerViewCard : Model -> Html Msg
dinnerViewCard model =
    Card.view
        [ --css "width" "auto"
          css "height" "auto"
        , Elevation.e2
        , css "margin" "0"
        , css "align-items" "center"
          --, css "background" "url('assets/elm.png') center / cover"
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


black : Options.Property c m
black =
    Color.text Color.black


ingredientView : Model -> Html Msg
ingredientView model =
    div []
        [ (Options.styled p [ Typo.title ] [ text "Ingredients" ])
        , dinnerInputMaterial "Name of Ingredient" IngredientName model model.currentIngredient.name 5
        , dinnerInputMaterial "Quantity" IngredientQty model model.currentIngredient.qty 6
        , dinnerInputMaterial "Unit" IngredientUnit model model.currentIngredient.unit 7
        ]


ingredientViewCard : Model -> Html Msg
ingredientViewCard model =
    Card.view
        [ css "width" "800px"
        , css "height" "auto"
        , Elevation.e2
        , css "margin" "50px 50px 50px 50px "
        , css "align-items" "center"
          --, css "background" "url('assets/elm.png') center / cover"
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
                    , css "margin-top" "4rem"
                    ]
                    [ div []
                        [ ingredientView model
                        , materialButton model AddIngredient "Add" 1
                        ]
                    , ingredientsTable model
                    ]
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
        , td [ align "left" ] [ button [ onClick (EditIngredient ingredient) ] [ text "edit" ] ]
        , td [ align "left" ] [ button [ onClick (RemoveIngredient ingredient) ] [ text "remove" ] ]
        ]



-- HELPER FUNCTIONS


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
            , Textfield.value defValue
            ]
            []
        ]


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    div [] [ Button.render Mdl [ group ] model.mdl [ Button.raised, Button.colored, Button.ripple, Options.onClick msg ] [ text butText ] ]


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



-- GETTERS & SETTERS


removeIngredientFromList : String -> String -> String -> Ingredient -> Maybe Ingredient
removeIngredientFromList name qty unit i =
    if name == i.name && qty == i.qty && unit == i.unit then
        Nothing
    else
        Just i


addNewIngredient : Model -> List Ingredient
addNewIngredient model =
    model.currentIngredient :: model.ingredients


setDinnerName : String -> Dinner -> Dinner
setDinnerName value dinner =
    { dinner | name = value }


setDinnerUrl : String -> Dinner -> Dinner
setDinnerUrl value dinner =
    { dinner | url = value }


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
