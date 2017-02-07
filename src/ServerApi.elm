module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Http


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
        (JsonD.field "name" JsonD.string)
        (JsonD.field "url" JsonD.string)
        (JsonD.field "tags" JsonD.string)
        (JsonD.field "portions" JsonD.string)


ingredientDecoder : JsonD.Decoder Ingredient
ingredientDecoder =
    JsonD.map3 Ingredient
        (JsonD.field "name" JsonD.string)
        (JsonD.field "qty" JsonD.string)
        (JsonD.field "unit" JsonD.string)


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


jsonResponseDecoder : JsonD.Decoder String
jsonResponseDecoder =
    JsonD.string


addNewDinner : Model -> Cmd Msg
addNewDinner model =
    let
        url =
            "http://localhost:49203/API/MiddagsApp/AddNewDinner"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder model)) jsonResponseDecoder
    in
        Http.send JsonResponse request
