module ShoppingList exposing (..)

import ServerApi exposing (ShopIngredient, handleHttpError, getShoppingList, addShopIngredient, changeShopHaveBought)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on)
import Http exposing (..)
import Css as Css exposing (..)
import Json.Decode as JsonD
import Time exposing (Time)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield

--MODEL


type alias Model =
    { searchText : String
    , ingredients : List ShopIngredient
    , raised : Int
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }

type Msg 
    = AddIngredient
    | ChangeHaveBought ShopIngredient
    | SearchText String
    | GetShoppingList Time
    | SearchResult (Result Http.Error (List ShopIngredient))
    | KeyDown Int
    | Raise Int
    | AddToast String
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)

type alias Mdl =
    Material.Model


--INIT


init : Model
init =
    Model "" [] -1 Snackbar.model Material.model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddIngredient ->
            (model,  addShopIngredient model.searchText SearchResult)

        ChangeHaveBought ingredient ->
            (  model, changeShopHaveBought ingredient SearchResult)

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )   
        
        GetShoppingList time ->
            ( model, getShoppingList SearchResult)

        SearchResult (Ok shopIngredients) ->
            ( { model | ingredients = shopIngredients }, Cmd.none )

        SearchResult (Err error) ->
            update (AddToast (handleHttpError error)) model        

        KeyDown key ->
            if key == 13 then
                ( model, addShopIngredient model.searchText SearchResult )
            else
                ( model, Cmd.none )                 

        Raise k ->
            { model | raised = k } ! []                

        AddToast message ->
            addToast (Snackbar.toast 1 message) model

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model        


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 3000 GetShoppingList        

--VIEW


view : Model -> Html Msg
view model =
    Options.div
        Css.flexFlowColumnAlignCenter
        [ h3 [ style [ ( "padding", "0rem" ) ] ] []
        , Options.div
            Css.flexFlowRowAlignCenter
            [ materialInput "Search..." SearchText model model.searchText 1
            , materialButton model AddIngredient "search" 2
            ]
        , List.map (ingredientCardCell model) (List.filterMap (haveBought False) model.ingredients) |> grid [ css "justify-content" "center" ]
        , hr[] []
        , text "Recent"
        , List.map (ingredientCardCell model) (List.filterMap (haveBought True) model.ingredients)  |> grid [ css "justify-content" "center" ]
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]

ingredientCardCell : Model -> ShopIngredient -> Material.Grid.Cell Msg
ingredientCardCell model ingredient=
    let
      width = "128"
    in      
        cell (Css.cellStyle width)
            [ cardView model ingredient width
            ]


cardView : Model -> ShopIngredient -> String -> Html Msg
cardView model ingredient width = 
    div []
    [Card.view
    [ css "height" (width ++ "px")
    , css "width" (width ++ "px")
    , if (ingredient.haveBought) then
         Color.background (Color.color Color.Teal Color.S500)
    else
        Color.background (Color.color Color.Red Color.S500)
    , dynamic ingredient.id model
    , Options.onClick (ChangeHaveBought ingredient)
    ]
    [ Card.title [] 
        [ Card.head [ white ] [ text ingredient.name ] ] ]
    ]




-- HELPERS

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JsonD.map tagger keyCode)

white : Options.Property c m
white =
    Color.text Color.white

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

haveBought : Bool -> ShopIngredient -> Maybe ShopIngredient
haveBought haveBought ingredient =
  if ingredient.haveBought == haveBought then
    Just ingredient

  else
    Nothing

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


-- GETTERS & SETTERS

changeHaveBought : ShopIngredient -> ShopIngredient -> Maybe ShopIngredient
changeHaveBought ingrToEdit ingr =
    if ingrToEdit == ingr then        
        Just { ingrToEdit | haveBought = not ingrToEdit.haveBought }
    else
        Just ingr



-- MATERIAL UI

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

materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg display group =
        Button.render Mdl
        [3, group ]       
        model.mdl
        [ Options.onClick (msg)
                    , Button.minifab
                    , Button.colored
                    , css "margin-top" "15px"
        ]
        [ Icon.view display [ Icon.size24 ] ]   