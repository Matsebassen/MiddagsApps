module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, searchDinners, addNewDinner, getIngredients)
import Html exposing (..)
import Css as Css exposing (..) 
import Html.Events exposing (onClick, onInput, keyCode, on)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as JsonD
import Material
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
    { statusMessage : String
    , dinner : Dinner
    , dinners : List Dinner
    , ingredients : List Ingredient
    , searchText : String
    , raised : Int
    , waiting : Bool
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model "" (Dinner "" "" "" "" "") [] [] "" -1 False Snackbar.model Material.model



--MSG


type Msg
    = GetRandomDinner
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
    | SearchIngredients Dinner
    | SearchIngredientsResult (Result Http.Error (List Ingredient))
    | KeyDown Int
    | Raise Int
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomDinner ->
            ( {model | waiting = True}, getRandomDinner SearchResults )

        SearchResults (Ok dinnersFound) ->
            ( { model | dinners = dinnersFound, waiting = False }, Cmd.none )

        SearchResults (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    ( { model | statusMessage = "That was a shitty url. Message: " ++ badUrlMsg }, Cmd.none )

                Http.Timeout ->
                    ( { model | statusMessage = "The request timed out" }, Cmd.none )

                Http.NetworkError ->
                    addToast (Snackbar.toast 1 "Can't contact server") model

                Http.BadStatus badResponse ->
                    ( { model | statusMessage = "Bad status. Does that make sense to you?" ++ toString badResponse }, Cmd.none )

                Http.BadPayload debugMessage badResponse ->
                    ( { model | statusMessage = "My payload is bad. Really bad. Also, I got a message for you: " ++ debugMessage }, Cmd.none )

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )

        SearchDinners ->
            ( { model | waiting = True }, searchDinners model.searchText SearchResults )

        SearchIngredients dinner ->
            ( model, getIngredients dinner.name SearchIngredientsResult )

        SearchIngredientsResult (Ok ingredientsFound) ->
            ( { model | ingredients = ingredientsFound }, Cmd.none )

        SearchIngredientsResult (Err error) ->
            case error of
                Http.BadUrl badUrlMsg ->
                    ( { model | statusMessage = "That was a shitty url. Message: " ++ badUrlMsg }, Cmd.none )

                Http.Timeout ->
                    ( { model | statusMessage = "The request timed out" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | statusMessage = "There seems to be a network error, Sir" }, Cmd.none )

                Http.BadStatus badResponse ->
                    ( { model | statusMessage = "Bad status. Does that make sense to you?" ++ toString badResponse }, Cmd.none )

                Http.BadPayload debugMessage badResponse ->
                    ( { model | statusMessage = "My payload is bad. Really bad. Also, I got a message for you: " ++ debugMessage }, Cmd.none )
        
        KeyDown key ->
            if key == 13 then
                ( { model | waiting = True }, searchDinners model.searchText SearchResults )
            else
                (model, Cmd.none)

        Raise k ->
            { model | raised = k } ! []

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Model -> Html Msg
view model =
    Options.div
            Css.flexFlowColumnAlignCenter
        [ h3 [ style [ ( "padding", "0rem" ) ] ] [ text model.statusMessage ]        
        , Options.div
            Css.flexFlowRowAlignCenter         
            [materialInput "Search..." SearchText model model.searchText 1        
            ,materialMiniFab model SearchDinners "search"             
            ]
        , materialButton model GetRandomDinner "I feel lucky" 2
        , br[] []
        , Loading.spinner [Loading.active model.waiting]
        , List.map2 (dinnerCardCell model) model.dinners (List.range 1 (List.length model.dinners)) |> grid [css "justify-content" "center"]         
        , dialogView model
        , Snackbar.view model.snackbar |> Html.map Snackbar        
        ]


dialogView : Model -> Html Msg
dialogView model =
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
                    [ Button.ripple, Button.accent, Dialog.openOn "click", Options.onClick (SearchIngredients dinner) ]
                    [ text "Ingredients" ]
                , Button.render Mdl
                    [ 1, 1, i ]
                    model.mdl
                    [ Button.ripple
                    ,if (String.length dinner.url < 1) then  Button.disabled else Button.link dinner.url
                    ,Button.accent,  Options.attribute <| Html.Attributes.target "_blank" ]
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
        , effect
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
            [ group ]
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
        [ Icon.view icon [ Icon.size24] ]
