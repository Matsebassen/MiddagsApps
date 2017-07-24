module ShoppingList exposing (..)

import ServerApi exposing (ShopIngredient, handleHttpError, getShoppingList, addShopIngredient, editShopIngredient)
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
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield
import Material.Typography as Typo


--MODEL


type alias Model =
    { searchText : String
    , ingredients : List ShopIngredient
    , currentIngredient : ShopIngredient
    , raised : Int
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type Msg
    = AddIngredient
    | ChangeHaveBought ShopIngredient
    | SearchText String
    | SetCurrentIngredient ShopIngredient
    | EditDesc String
    | SaveDescInDb
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
    Model "" [] (ShopIngredient "" "" False 0) -1 Snackbar.model Material.model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddIngredient ->
            ( { model | searchText = "" }, addShopIngredient (ShopIngredient model.searchText "" False 0) SearchResult )

        ChangeHaveBought ingredient ->
            ( model, editShopIngredient ingredient SearchResult )

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )

        SetCurrentIngredient ingredient ->
            ( { model | currentIngredient = ingredient }, Cmd.none )

        EditDesc newDesc ->
            ( { model
                | currentIngredient =
                    (let
                        currIngr =
                            model.currentIngredient
                     in
                        { currIngr | desc = newDesc }
                    )
              }
            , Cmd.none
            )

        SaveDescInDb ->
            ( model, editShopIngredient model.currentIngredient SearchResult )

        GetShoppingList time ->
            ( model, getShoppingList SearchResult )

        SearchResult (Ok shopIngredients) ->
            ( { model | ingredients = shopIngredients }, Cmd.none )

        SearchResult (Err error) ->
            update (AddToast (handleHttpError error)) model

        KeyDown key ->
            if key == 13 then
                ( { model | searchText = "" }, addShopIngredient (ShopIngredient model.searchText "" False 0) SearchResult )
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
        [ h4 [ style [ ( "padding", "0rem" ) ] ] []
        , Options.div
            Css.flexFlowRowAlignCenter
            [ materialInput "Search..." SearchText model model.searchText 1
            , materialButton model AddIngredient "add_shopping_cart" 2
            ]
        , List.map (ingredientCardCell model) (List.filterMap (haveBought False) model.ingredients) |> grid [ css "justify-content" "center" ]
        , hr [] []
        , text "Recently used"
        , List.map (ingredientCardCell model) (List.filterMap (haveBought True) model.ingredients) |> grid [ css "justify-content" "center" ]
        , dialogView model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


ingredientCardCell : Model -> ShopIngredient -> Material.Grid.Cell Msg
ingredientCardCell model ingredient =
    let
        height =
            "128"

        width =
            "116"
    in
        cell (Css.shopListCellStyle height width)
            [ cardView model ingredient height
            ]


cardView : Model -> ShopIngredient -> String -> Html Msg
cardView model ingredient height =
    div []
        [ Card.view
            [ css "height" (height ++ "px")
            , css "width" (height ++ "px")
            , dynamic ingredient.id model
            ]
            [ Card.title
                [ Options.onClick (ChangeHaveBought { ingredient | haveBought = not ingredient.haveBought })
                , css "height"
                    "100px"
                , backgroundColor (String.slice 0 1 (String.toLower ingredient.name)) ingredient.haveBought
                ]
                [ Card.subhead [ white, Typo.subhead, Typo.nowrap, css "margin" "0 0 -10px -12px" ] [ text ingredient.name ] ]
            , Card.media
                [ Card.border
                , Color.background (Color.color Color.Grey Color.S300)
                , Options.onClick (SetCurrentIngredient ingredient)
                , Dialog.openOn "click"
                , css "height" "28px"
                , Typo.subhead
                ]
                [ text ingredient.desc ]
            ]
        ]


dialogView : Model -> Html Msg
dialogView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Edit Ingredient" ]
        , Dialog.content []
            [ materialInput "Description" (EditDesc) model model.currentIngredient.desc 2
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 20, 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            , Button.render Mdl
                [ 20, 2 ]
                model.mdl
                [ Options.onClick SaveDescInDb, Dialog.closeOn "click" ]
                [ text "Save" ]
            ]
        ]



-- HELPERS


backgroundColor : String -> Bool -> Options.Property c m
backgroundColor firstCharacter haveBought =
    let
        url =
            if (haveBought) then
                "https://middagsappbilder.blob.core.windows.net/middag/" ++ firstCharacter ++ "-true.png"
            else
                "https://middagsappbilder.blob.core.windows.net/middag/" ++ firstCharacter ++ "-false.png"
    in
        css "background" ("url('" ++ url ++ "') center / cover")


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JsonD.map tagger keyCode)


white : Options.Property c m
white =
    Color.text Color.white


black : Options.Property c m
black =
    Color.text Color.black


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
        [ 3, group ]
        model.mdl
        [ Options.onClick (msg)
        , Button.minifab
        , Button.colored
        ]
        [ Icon.view display [ Icon.size24 ] ]
