module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, searchDinners, addNewDinner)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Material
import Material.Elevation as Elevation
import Material.Table as Table
import Material.Button as Button
import Material.Card as Card
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Color as Color


--MODEL


type alias Model =
    { statusMessage : String
    , dinner : Dinner
    , dinners : List Dinner
    , searchText : String
    , raised : Int
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model "" (Dinner "" "" "" "") [] "" -1 Material.model



--MSG


type Msg
    = GetRandomDinner
    | NewDinner (Result Http.Error Dinner)
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
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
        , div []
            [ dinnerTable model ]
        , cardView model "http://caspario.com/tanker2010/mars/cowboymat.jpg" "Cowboygryte" "Cowboygryte er nam nam"
        ]


dinnerTable : Model -> Html Msg
dinnerTable model =
    Table.table [ css "align" "center" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Url" ]
                , Table.th [] [ text "Tags" ]
                , Table.th [] [ text "Portions" ]
                ]
            ]
        , Table.tbody [] (List.map renderDinners model.dinners)
        ]


renderDinners : Dinner -> Html Msg
renderDinners dinner =
    Table.tr []
        [ Table.td [ css "align" "left" ] [ text dinner.name ]
        , Table.td [ css "align" "left" ] [ text dinner.url ]
        , Table.td [ css "align" "left" ] [ text dinner.tags ]
        , Table.td [ css "align" "left" ] [ text dinner.portions ]
        ]


dynamic : Int -> Msg -> Model -> Options.Style Msg
dynamic k showcode model =
    [ if model.raised == k then
        Elevation.e8
      else
        Elevation.e2
    , Elevation.transition 250
    , Options.onMouseEnter (Raise k)
    , Options.onMouseLeave (Raise -1)
    , Options.onClick showcode
    ]
        |> Options.many


cardView : Model -> String -> String -> String -> Html Msg
cardView model picUrl header desc =
    Card.view
        [ css "width" "256px"
        , css "padding" "20 20 20 20"
        ]
        [ Card.title
            [ css "background" ("url('" ++ picUrl ++ "') center / cover")
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
                [ text header ]
            ]
        , Card.text []
            [ text desc ]
        , Card.actions
            [ Card.border ]
            [ Button.render Mdl
                [ 1, 0 ]
                model.mdl
                [ Button.ripple, Button.accent ]
                [ text "Ingredients" ]
            , Button.render Mdl
                [ 1, 1 ]
                model.mdl
                [ Button.ripple, Button.accent ]
                [ text "Website" ]
            ]
        ]


white : Options.Property c m
white =
    Color.text Color.white


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
