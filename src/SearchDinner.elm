module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, searchDinners, addNewDinner, getIngredients)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Material
import Material.Elevation as Elevation
import Material.Dialog as Dialog
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Textfield as Textfield
import Material.Color as Color


--MODEL


type alias Model =
    { statusMessage : String
    , dinner : Dinner
    , dinners : List Dinner
    , ingredients : List Ingredient
    , searchText : String
    , raised : Int
    , mdl : Material.Model    
    }


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model "" (Dinner "" "" "" "") [] [] "" -1 Material.model



--MSG


type Msg
    = GetRandomDinner
    | NewDinner (Result Http.Error Dinner)
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
    | SearchIngredients Dinner
    | SearchIngredientsResult (Result Http.Error (List Ingredient))
    | Raise Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomDinner ->
            ( model, getRandomDinner NewDinner )

        NewDinner (Ok newDinner) ->
            ( { model | dinner = newDinner }, Cmd.none )

        NewDinner (Err error) ->
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

        SearchResults (Ok dinnersFound) ->
            ( { model | dinners = dinnersFound }, Cmd.none )

        SearchResults (Err error) ->
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

        SearchText searchTxt ->
            ( { model | searchText = searchTxt }, Cmd.none )

        SearchDinners ->
            ( model, searchDinners model.searchText SearchResults )
        
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

        Raise k ->
            { model | raised = k } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Model -> Html Msg
view model =
    div []
        [ h3 [ style [ ( "padding", "0rem" ) ] ] [ text model.statusMessage ]
        , materialInput "Search..." SearchText model model.searchText 1
        , materialButton model SearchDinners "Search" 1
        , materialButton model GetRandomDinner "I feel lucky" 2 
        , div[] 
        [ List.map2 (dinnerCardCell model) model.dinners (List.range 1 (List.length model.dinners)) |> grid[]]
        , element model
        ]


element : Model -> Html Msg
element model = 
  Dialog.view
    [ ]
    [ Dialog.title [] [ text "Ingredients" ]
    , Dialog.content [] 
        [ p [] [ text "All the ingredients will be shown here" ]
        ]
    , Dialog.actions [ ]
      [ Button.render Mdl [0] model.mdl
          [ Dialog.closeOn "click" ]
          [ text "Close" ]
      ]
    ]


dinnerCardCell :  Model -> Dinner -> Int -> Material.Grid.Cell Msg
dinnerCardCell model dinner i =
    cell (cellStyle 256)
    [
        cardView model dinner i
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


cardView :  Model -> Dinner -> Int -> Html Msg
cardView model dinner i =
    div []
    [
    Card.view
        [ dynamic i  model,
            css "width" "256px"
        --, Color.background (Color.color Color.Teal Color.S400)
        ]
        [ Card.title
            [ css "background" "url('http://caspario.com/tanker2010/mars/cowboymat.jpg') center / cover"
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
            [ text (dinner.portions ++ " portions - "++ dinner.tags) ]
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
                [ Button.ripple, Button.accent ]
                [ text "Website" ]
            ]
        ]
        ]


white : Options.Property c m
white =
    Color.text Color.white

cellStyle : Int -> List (Options.Style a)
cellStyle h = 
  [ css "text-sizing" "border-box"
  --, css "background-color" "teal"
  , css "padding-left" "8px"
  , css "padding-top" "4px"
  , css "color" "teal"
  , css "width" "256px"
  , Material.Grid.size Tablet 6
  , Material.Grid.size Desktop 12
  , Material.Grid.size Phone 4
  ]


materialButton : Model -> Msg -> String -> Int -> Html Msg
materialButton model msg butText group =
    Button.render Mdl
        [ group ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick msg
        , css "margin" "0 12px"
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
            , Options.attribute <| value defValue
            ]
            []
        ]
