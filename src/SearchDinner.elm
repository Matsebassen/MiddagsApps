module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, searchDinners, addNewDinner, getIngredients, editDinner)
import Html exposing (..)
import Css as Css exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as JsonD
import Material
import Task exposing (..)
import Material.Elevation as Elevation
import Material.Dialog as Dialog
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Textfield as Textfield
import Material.Color as Color
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Snackbar as Snackbar
import Material.Spinner as Loading
import Material.Icon as Icon


--MODEL


type alias Model =
    { currentDinner : Dinner
    , dinners : List Dinner
    , ingredients : List Ingredient
    , searchText : String
    , raised : Int
    , waiting : Bool
    , dialogType : DialogType
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type DialogType
    = ShowIngredients
    | EditDinner



--INIT


init : Model
init =
    Model (Dinner "" "" "" "" "" 0) [] [] "" -1 False ShowIngredients Snackbar.model Material.model



--MSG


type Msg
    = GetRandomDinner
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
    | SearchIngredients Dinner
    | SearchIngredientsResult (Result Http.Error (List Ingredient))
    | EditDinnerDialog Dinner
    | EditDinnerInDb
    | EditDinnerResult (Result Http.Error String)
    | KeyDown Int
    | Raise Int
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
    | DinnerName String
    | DinnerUrl String
    | DinnerPicUrl String
    | DinnerTags String
    | DinnerPortions String
    | AddToast String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomDinner ->
            ( { model | waiting = True }, getRandomDinner SearchResults )

        SearchResults (Ok dinnersFound) ->
            ( { model | dinners = dinnersFound, waiting = False }, Cmd.none )

        SearchResults (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    update (AddToast "Bad webservice URL") model

                Http.Timeout ->
                    update (AddToast "The request timed out") model

                Http.NetworkError ->
                    update (AddToast "Can't contact server") model

                Http.BadStatus badResponse ->
                    update (AddToast badResponse.body) model

                Http.BadPayload debugMessage badResponse ->
                    update (AddToast badResponse.body) model

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )

        SearchDinners ->
            ( { model | waiting = True }, searchDinners model.searchText SearchResults )

        SearchIngredients dinner ->
            ( { model | dialogType = ShowIngredients, currentDinner = dinner }, getIngredients dinner.name SearchIngredientsResult )

        SearchIngredientsResult (Ok ingredientsFound) ->
            ( { model | ingredients = ingredientsFound }, Cmd.none )

        SearchIngredientsResult (Err error) ->
            ( model, Cmd.none )

        EditDinnerDialog dinner ->
            ( { model | dialogType = EditDinner, currentDinner = dinner }, Cmd.none )

        EditDinnerInDb ->
            ( model, editDinner model.currentDinner [] EditDinnerResult )

        EditDinnerResult (Ok response) ->
            --addToast (Snackbar.toast 1 response) { model | waiting = False }
            (update SearchDinners { model | waiting = False })

        EditDinnerResult (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    update (AddToast "Bad webservice URL") model

                Http.Timeout ->
                    update (AddToast "The request timed out") model

                Http.NetworkError ->
                    update (AddToast "Can't contact server") model

                Http.BadStatus badResponse ->
                    update (AddToast badResponse.body) model

                Http.BadPayload debugMessage badResponse ->
                    update (AddToast badResponse.body) model

        KeyDown key ->
            if key == 13 then
                ( { model | waiting = True }, searchDinners model.searchText SearchResults )
            else
                ( model, Cmd.none )

        Raise k ->
            { model | raised = k } ! []

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model

        DinnerName newName ->
            ( { model | currentDinner = setDinnerName newName model.currentDinner }, Cmd.none )

        DinnerUrl newUrl ->
            ( { model | currentDinner = setDinnerUrl newUrl model.currentDinner }, Cmd.none )

        DinnerPicUrl newUrl ->
            ( { model | currentDinner = setDinnerPicUrl newUrl model.currentDinner }, Cmd.none )

        DinnerTags newTags ->
            ( { model | currentDinner = setDinnerTags newTags model.currentDinner }, Cmd.none )

        DinnerPortions newPortions ->
            ( { model | currentDinner = setDinnerPortions newPortions model.currentDinner }, Cmd.none )

        AddToast message ->
            addToast (Snackbar.toast 1 message) { model | waiting = False }


view : Model -> Html Msg
view model =
    Options.div
        Css.flexFlowColumnAlignCenter
        [ h3 [ style [ ( "padding", "0rem" ) ] ] []
        , Options.div
            Css.flexFlowRowAlignCenter
            [ materialInput "Search..." SearchText model model.searchText 1
            , materialMiniFab model SearchDinners "search"
            ]
        , materialButton model GetRandomDinner "I feel lucky" 2
        , br [] []
        , Loading.spinner [ Loading.active model.waiting ]
        , List.map2 (dinnerCardCell model) model.dinners (List.range 1 (List.length model.dinners)) |> grid [ css "justify-content" "center" ]
        , dialogView model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


dialogView : Model -> Html Msg
dialogView model =
    case model.dialogType of
        ShowIngredients ->
            showIngredientView model

        EditDinner ->
            editDinnerView model


showIngredientView : Model -> Html Msg
showIngredientView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Ingredients" ]
        , Dialog.content []
            [ p []
                [ ingredientTable model ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Close" ]
            ]
        ]


editDinnerView : Model -> Html Msg
editDinnerView model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Edit Dinner" ]
        , Dialog.content []
            [ materialInput "Name" DinnerName model model.currentDinner.name 1
            , materialInput "Portions" DinnerPortions model model.currentDinner.portions 2
            , materialInput "Tags" DinnerTags model model.currentDinner.tags 3
            , materialInput "Picture Url" DinnerPicUrl model model.currentDinner.picUrl 4
            , materialInput "Url (optional)" DinnerUrl model model.currentDinner.url 5
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
                [ Options.onClick EditDinnerInDb
                , Dialog.closeOn "click"
                ]
                [ text "Save" ]
            ]
        ]


dinnerCardCell : Model -> Dinner -> Int -> Material.Grid.Cell Msg
dinnerCardCell model dinner i =
    cell (Css.cellStyle 256)
        [ cardView model dinner i
        ]


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


cardView : Model -> Dinner -> Int -> Html Msg
cardView model dinner i =
    div []
        [ Card.view
            [ dynamic i model
            , css "width" "256px"
            , css "height" "340px"
            ]
            [ Card.title
                [ css "background" ("url('" ++ dinner.picUrl ++ "') center / cover")
                , css "height" "256px"
                , css "padding" "0"
                , Options.onClick (EditDinnerDialog dinner)
                , Dialog.openOn "click"
                  -- Clear default padding to encompass scrim
                ]
                [ Card.head
                    [ white
                    , Options.scrim 0.75
                    , css "padding" "16px"
                      -- Restore default padding inside scrim
                    , css "width" "100%"
                    ]
                    [ text dinner.name ]
                ]
            , Card.text []
                [ text (dinner.portions ++ " portions - " ++ dinner.tags) ]
            , Card.actions
                [ Card.border ]
                [ Button.render Mdl
                    [ 1, 0, i ]
                    model.mdl
                    [ Button.ripple, Button.accent, Options.onClick (SearchIngredients dinner), Dialog.openOn "click" ]
                    [ text "Ingredients" ]
                , Button.render Mdl
                    [ 1, 1, i ]
                    model.mdl
                    [ Button.ripple
                    , if (String.length dinner.url < 1) then
                        Button.disabled
                      else
                        Button.link dinner.url
                    , Button.accent
                    , Options.attribute <| Html.Attributes.target "_blank"
                    ]
                    [ text "Website" ]
                ]
            ]
        ]


white : Options.Property c m
white =
    Color.text Color.white


ingredientTable : Model -> Html Msg
ingredientTable model =
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
        ]


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
        , (Cmd.batch [ effect ])
        )


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JsonD.map tagger keyCode)


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    Button.render Mdl
        [ group ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick msg
        , css "margin-top" "12px"
        ]
        [ text butText ]


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


materialMiniFab : Model -> Msg -> String -> Html Msg
materialMiniFab model msg icon =
    Button.render Mdl
        [ 4, 1 ]
        model.mdl
        [ Options.onClick (msg)
        , Button.minifab
        , Button.colored
        , css "margin-top" "15px"
        ]
        [ Icon.view icon [ Icon.size24 ] ]


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
