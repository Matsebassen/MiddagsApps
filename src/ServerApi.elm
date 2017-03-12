module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as Encode
import Http exposing (..)


type alias Dinner =
    { name : String
    , url : String
    , tags : String
    , portions : String
    , picUrl : String
    }


type alias Ingredient =
    { name : String
    , qty : String
    , unit : String
    }


getRandomDinner : (Result Http.Error (List Dinner) -> msg) -> Cmd msg
getRandomDinner msg =
    let
        url =
            "http://middagsapp.azurewebsites.net/API/MiddagsApp/GetRandomDinner"

        --"http://middagsapp.azurewebsites.net/API/MiddagsApp/GetRandomDinner"
        request =
            Http.get url (JsonD.list dinnerDecoder)

        --request =
        --    Http.get url dinnerDecoder
    in
        Http.send msg request


addNewDinner : Dinner -> List Ingredient -> (Result Http.Error String -> msg) -> Cmd msg
addNewDinner dinner ingredients msg =
    let
        url =
            "http://middagsapp.azurewebsites.net/API/MiddagsApp/AddNewDinner"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder dinner ingredients)) jsonResponseDecoder
    in
        Http.send msg request


searchDinners : String -> (Result Http.Error (List Dinner) -> msg) -> Cmd msg
searchDinners searchText msg =
    let
        url =
            "http://middagsapp.azurewebsites.net/API/MiddagsApp/SearchDinner"

        request =
            Http.post url (Http.jsonBody (Encode.string searchText)) (JsonD.list dinnerDecoder)
    in
        Http.send msg request


getIngredients : String -> (Result Http.Error (List Ingredient) -> msg) -> Cmd msg
getIngredients dinnerName msg =
    let
        url =
            "http://middagsapp.azurewebsites.net/API/MiddagsApp/GetIngredients"

        request =
            Http.post url (Http.jsonBody (Encode.string dinnerName)) (JsonD.list ingredientDecoder)
    in
        Http.send msg request


dinnerDecoder : JsonD.Decoder Dinner
dinnerDecoder =
    JsonD.map5 Dinner
        (JsonD.field "name" JsonD.string)
        (JsonD.field "url" JsonD.string)
        (JsonD.field "tags" JsonD.string)
        (JsonD.field "portions" JsonD.string)
        (JsonD.field "picUrl" JsonD.string)


ingredientDecoder : JsonD.Decoder Ingredient
ingredientDecoder =
    JsonD.map3 Ingredient
        (JsonD.field "name" JsonD.string)
        (JsonD.field "qty" JsonD.string)
        (JsonD.field "unit" JsonD.string)


dinnerEncoder : Dinner -> List Ingredient -> Encode.Value
dinnerEncoder dinner ingredients =
    Encode.object
        [ ( "name", Encode.string dinner.name )
        , ( "url", Encode.string dinner.url )
        , ( "tags", Encode.string dinner.tags )
        , ( "portions", Encode.string dinner.portions )
        , ( "picUrl", Encode.string dinner.picUrl )
        , ( "ingredients", Encode.list <| List.map ingredientEncoder ingredients )
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
