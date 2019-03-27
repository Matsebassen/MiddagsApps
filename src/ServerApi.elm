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
    , id : Int
    }


type alias Ingredient =
    { name : String
    , qty : String
    , unit : String
    , id : Int
    }

type alias ShopIngredient =
    { name : String
    , desc : String
    , haveBought : Bool
    , id : Int
    }

type alias TrineDinner = 
    { name : String
    , url : String
    , tags : String
    , portions : String
    , picUrl : String
    , id : Int
    , ingredients : List Ingredient
    }



type IngredientMember
    = IngredientName
    | Qty
    | Unit


type DinnerMember
    = DinnerName
    | Url
    | Tags
    | Portions
    | PicUrl


webServiceURl : String
webServiceURl =
    "https://middagsapp.azurewebsites.net/API/MiddagsApp/"
    --"http://localhost:49203/API/MiddagsApp/"


getRandomDinner : (Result Http.Error (List Dinner) -> msg) -> Cmd msg
getRandomDinner msg =
    let
        url =
            webServiceURl ++ "GetRandomDinner"

        request =
            Http.get url (JsonD.list dinnerDecoder)
    in
        Http.send msg request

getTrineDinner : String -> (Result Http.Error (TrineDinner) -> msg) -> Cmd msg
getTrineDinner trineUrl msg =
    let
        url =
            webServiceURl ++ "GetTrineDinner"

        request =
            Http.post url (Http.jsonBody (Encode.string trineUrl)) (trineDinnerDecoder) 
    in
        Http.send msg request

addNewDinner : Dinner -> List Ingredient -> (Result Http.Error String -> msg) -> Cmd msg
addNewDinner dinner ingredients msg =
    let
        url =
            webServiceURl ++ "AddNewDinner"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder dinner ingredients)) jsonResponseDecoder
    in
        Http.send msg request


editDinner : Dinner -> List Ingredient -> (Result Http.Error String -> msg) -> Cmd msg
editDinner dinner ingredients msg =
    let
        url =
            webServiceURl ++ "EditDinner"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder dinner ingredients)) jsonResponseDecoder
    in
        Http.send msg request

addDinnerToShopList : Dinner -> List Ingredient -> (Result Http.Error String -> msg) -> Cmd msg
addDinnerToShopList dinner ingredients msg =
    let
        url =
            webServiceURl ++ "addDinnerToShopList"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder dinner ingredients)) jsonResponseDecoder
    in
        Http.send msg request


editDinnerIngredients : Dinner -> List Ingredient -> (Result Http.Error String -> msg) -> Cmd msg
editDinnerIngredients dinner ingredients msg =
    let
        url =
            webServiceURl ++ "EditDinnerIngredients"

        request =
            Http.post url (Http.jsonBody (dinnerEncoder dinner ingredients)) jsonResponseDecoder
    in
        Http.send msg request


searchDinners : String -> (Result Http.Error (List Dinner) -> msg) -> Cmd msg
searchDinners searchText msg =
    let
        url =
            webServiceURl ++ "SearchDinner"

        request =
            Http.post url (Http.jsonBody (Encode.string searchText)) (JsonD.list dinnerDecoder)
    in
        Http.send msg request


getIngredients : Int -> (Result Http.Error (List Ingredient) -> msg) -> Cmd msg
getIngredients dinnerId msg =
    let
        url =
            webServiceURl ++ "GetIngredients"

        request =
            Http.post url (Http.jsonBody (Encode.int dinnerId)) (JsonD.list ingredientDecoder)
    in
        Http.send msg request

addShopIngredient : ShopIngredient -> (Result Http.Error (List ShopIngredient) -> msg) -> Cmd msg
addShopIngredient shopIngredient msg =
    let
        url =
            webServiceURl ++ "AddIngredientToShoppingList"

        request =
            Http.post url (Http.jsonBody (shopIngredientEncoder shopIngredient)) (JsonD.list shopIngredientDecoder)
    in
        Http.send msg request

editShopIngredient : ShopIngredient -> (Result Http.Error (List ShopIngredient) -> msg) -> Cmd msg
editShopIngredient shopIngredient msg =
    let
        url =
            webServiceURl ++ "EditShopIngredient"

        request =
            Http.post url (Http.jsonBody (shopIngredientEncoder shopIngredient)) (JsonD.list shopIngredientDecoder)
    in
        Http.send msg request

getShoppingList : (Result Http.Error (List ShopIngredient) -> msg) -> Cmd msg
getShoppingList  msg =
    let
        url =
            webServiceURl ++ "GetShoppingList"

        request =
            Http.get url (JsonD.list shopIngredientDecoder)
    in
        Http.send msg request


dinnerDecoder : JsonD.Decoder Dinner
dinnerDecoder =
    JsonD.map6 Dinner
        (JsonD.field "name" JsonD.string)
        (JsonD.field "url" JsonD.string)
        (JsonD.field "tags" JsonD.string)
        (JsonD.field "portions" JsonD.string)
        (JsonD.field "picUrl" JsonD.string)
        (JsonD.field "id" JsonD.int)

trineDinnerDecoder : JsonD.Decoder TrineDinner 
trineDinnerDecoder =
    JsonD.map7 TrineDinner
        (JsonD.field "name" JsonD.string)
        (JsonD.field "url" JsonD.string)
        (JsonD.field "tags" JsonD.string)
        (JsonD.field "portions" JsonD.string)
        (JsonD.field "picUrl" JsonD.string)    
        (JsonD.field "id" JsonD.int)    
        (JsonD.field "ingredients" (JsonD.list ingredientDecoder))


ingredientDecoder : JsonD.Decoder Ingredient
ingredientDecoder =
    JsonD.map4 Ingredient
        (JsonD.field "name" JsonD.string)
        (JsonD.field "qty" JsonD.string)
        (JsonD.field "unit" JsonD.string)
        (JsonD.field "id" JsonD.int)

shopIngredientDecoder : JsonD.Decoder ShopIngredient
shopIngredientDecoder =
    JsonD.map4 ShopIngredient
        (JsonD.field "name" JsonD.string)
        (JsonD.field "desc" JsonD.string)
        (JsonD.field "haveBought" JsonD.bool)
        (JsonD.field "id" JsonD.int)

dinnerEncoder : Dinner -> List Ingredient -> Encode.Value
dinnerEncoder dinner ingredients =
    Encode.object
        [ ( "name", Encode.string dinner.name )
        , ( "url", Encode.string dinner.url )
        , ( "tags", Encode.string dinner.tags )
        , ( "portions", Encode.string dinner.portions )
        , ( "picUrl", Encode.string dinner.picUrl )
        , ( "id", Encode.int dinner.id )
        , ( "ingredients", Encode.list <| List.map ingredientEncoder ingredients )
        ]   


ingredientEncoder : Ingredient -> Encode.Value
ingredientEncoder ingredient =
    Encode.object
        [ ( "name", Encode.string ingredient.name )
        , ( "qty", Encode.string ingredient.qty )
        , ( "unit", Encode.string ingredient.unit )
        , ( "id", Encode.int ingredient.id )
        ]


shopIngredientEncoder : ShopIngredient -> Encode.Value
shopIngredientEncoder shopIngredient =
    Encode.object
        [ ( "name", Encode.string shopIngredient.name )
        , ( "desc", Encode.string shopIngredient.desc )
        , ( "haveBought", Encode.bool shopIngredient.haveBought )
        , ( "id", Encode.int shopIngredient.id )
        ]


jsonResponseDecoder : JsonD.Decoder String
jsonResponseDecoder =
    JsonD.string

handleHttpError : Http.Error -> String
handleHttpError error =
    case error of
        Http.BadUrl badUrlMsg ->
            "Bad webservice URL"

        Http.Timeout ->
            "The request timed out"

        Http.NetworkError ->
            "Can't contact server"

        Http.BadStatus badResponse ->
            "Bad status from Webservice: " ++ badResponse.body

        Http.BadPayload debugMessage badResponse ->
            debugMessage ++ " - " ++  badResponse.body
