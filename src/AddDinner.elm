module AddDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, addNewDinner)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Material
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Textfield as Textfield


--MODEL


type alias Model =
    { statusMessage : String
    , dinner : Dinner
    , ingredients : List Ingredient
    , currentIngredient : Ingredient
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
    | EditIngredient Ingredient


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model "" (Dinner "" "" "" "") [] (Ingredient "" "" "") Material.model



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDinner ->
            ( model, addNewDinner model.dinner model.ingredients JsonResponse )

        JsonResponse (Ok response) ->
            ( Model response (Dinner "" "" "" "") [] (Ingredient "" "" "") model.mdl, Cmd.none )

        JsonResponse (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    ( { model | statusMessage = "That was a shitty url. Message: " ++ badUrlMsg }, Cmd.none )

                Http.Timeout ->
                    ( { model | statusMessage = "The request timed out" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | statusMessage = "There seems to be a network error, Sir" }, Cmd.none )

                Http.BadStatus badResponse ->
                    ( { model | statusMessage = "Bad status. Does that make sense to you? " }, Cmd.none )

                Http.BadPayload debugMessage badResponse ->
                    ( { model | statusMessage = "My payload is bad. Really bad. Also, I got a message for you: " ++ debugMessage }, Cmd.none )

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

        Mdl msg_ ->
            Material.update Mdl msg_ model



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ text model.statusMessage
        , dinnerView model
        , materialButton model AddDinner "Add dinner" 2
        , br [] []
        , table [ align "center" ]
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
        , ingredientView model
        , div [] [ Button.render Mdl [ 0 ] model.mdl [ Button.minifab, Button.colored, Options.onClick AddIngredient ] [ Icon.i "add" ] ]
        ]


dinnerView : Model -> Html Msg
dinnerView model =
    div []
        [ dinnerInputMaterial "Name of dinner" DinnerName model model.dinner.name 1
        , dinnerInputMaterial "Portions" DinnerPortions model model.dinner.portions 2
        , dinnerInputMaterial "Tags" DinnerTags model model.dinner.tags 3
        , dinnerInputMaterial "Url (optional)" DinnerUrl model model.dinner.url 4
        ]


ingredientView : Model -> Html Msg
ingredientView model =
    div []
        [ dinnerInputMaterial "Name of Ingredient" IngredientName model model.currentIngredient.name 5
        , dinnerInputMaterial "Quantity" IngredientQty model model.currentIngredient.qty 6
        , dinnerInputMaterial "Unit" IngredientUnit model model.currentIngredient.unit 7
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
            , Options.attribute <| value defValue
            ]
            []
        ]


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    div [] [ Button.render Mdl [ group ] model.mdl [ Button.raised, Button.colored, Button.ripple, Options.onClick msg ] [ text butText ] ]



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
