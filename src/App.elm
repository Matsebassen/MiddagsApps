module App exposing (..)

--

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as JsonD
import Json.Encode as Encode exposing (..)


type alias Model =
    { title : String
    , dinner : Dinner
    , ingredients : List Ingredient
    , currentIngredient : Ingredient
    }


type alias Dinner =
    { name : String
    , url : String
    , tags : String
    , portions : String
    }


type alias Ingredient =
    { name : String
    , qty : String
    , unit : String
    }


init : String -> ( Model, Cmd Msg )
init blabla =
    ( Model "Mats lærer elm" (Dinner "" "" "" "") [] (Ingredient "" "" ""), Cmd.none )



-- UPDATE


type Msg
    = GetRandomDinner
    | NewDinner (Result Http.Error Dinner)
    | AddDinner
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomDinner ->
            ( model, getRandomDinner )

        AddDinner ->
            ( model, addNewDinner model )

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

        NewDinner (Ok newDinner) ->
            ( { model | dinner = newDinner }, Cmd.none )

        NewDinner (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    ( { model | title = "That was a shitty url. Message: " ++ badUrlMsg }, Cmd.none )

                Http.Timeout ->
                    ( { model | title = "The request timed out" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | title = "There seems to be a network error, Sir" }, Cmd.none )

                Http.BadStatus badResponse ->
                    ( { model | title = "Bad status. Does that make sense to you?" }, Cmd.none )

                Http.BadPayload debugMessage badResponse ->
                    ( { model | title = "My payload is bad. Really bad. Also, I got a message for you: " ++ debugMessage }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , label [] [ text model.dinner.name ]
        , br [] []
        , text model.dinner.url
        , div [] [ button [ onClick GetRandomDinner ] [ text "Get Random Dinner" ] ]
        , br [] []
        , dinnerView model
        , div [] [ button [ onClick AddDinner ] [ text "Add dinner" ] ]
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
        , div [] [ button [ onClick AddIngredient ] [ text "Add ingredient" ] ]
        ]


dinnerView : Model -> Html Msg
dinnerView model =
    div []
        [ dinnerInput "Name of dinner" DinnerName model model.dinner.name
        , dinnerInput "Portions" DinnerPortions model model.dinner.portions
        , dinnerInput "Tags" DinnerTags model model.dinner.tags
        , dinnerInput "Url (optional)" DinnerUrl model model.dinner.url
        ]


ingredientView : Model -> Html Msg
ingredientView model =
    div []
        [ dinnerInput "Name of Ingredient" IngredientName model model.currentIngredient.name
        , dinnerInput "Quantity" IngredientQty model model.currentIngredient.qty
        , dinnerInput "Unit" IngredientUnit model model.currentIngredient.unit
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


dinnerInput : String -> (String -> Msg) -> Model -> String -> Html Msg
dinnerInput placeHolder msg model defValue =
    div [] [ input [ type_ "text", placeholder placeHolder, onInput msg, value defValue ] [] ]


getRandomDinner : Cmd Msg
getRandomDinner =
    let
        url =
            "http://localhost:49203/API/MiddagsApp/GetRandomDinner"

        --"http://middagbackend.azurewebsites.net/API/MiddagsApp/GetRandomDinner"
        request =
            Http.get url dinnerDecoder
    in
        Http.send NewDinner request


dinnerDecoder : JsonD.Decoder Dinner
dinnerDecoder =
    JsonD.map4 Dinner
        (JsonD.field "Name" JsonD.string)
        (JsonD.field "Url" JsonD.string)
        (JsonD.field "Tags" JsonD.string)
        (JsonD.field "Portions" JsonD.string)


dinnerEncoder : Model -> Encode.Value
dinnerEncoder model =
    Encode.object
        [ ( "name", Encode.string model.dinner.name )
        , ( "url", Encode.string model.dinner.url )
        , ( "tags", Encode.string model.dinner.tags )
        , ( "portions", Encode.string model.dinner.portions )
        , ( "ingredients", Encode.list <| List.map ingredientEncoder model.ingredients )
        ]


ingredientEncoder : Ingredient -> Encode.Value
ingredientEncoder ingredient =
    Encode.object
        [ ( "name", Encode.string ingredient.name )
        , ( "qty", Encode.string ingredient.qty )
        , ( "unit", Encode.string ingredient.unit )
        ]


addNewDinner : Model -> Cmd Msg
addNewDinner model =
    let
        url =
            "http://localhost:49203/API/MiddagsApp/AddNewDinner"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder model)) dinnerDecoder
    in
        Http.send NewDinner request


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
