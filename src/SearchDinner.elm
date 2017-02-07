module SearchDinner exposing (..)

import ServerApi exposing (Dinner, Ingredient, getRandomDinner, searchDinners, addNewDinner)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http exposing (..)
import Material
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Textfield as Textfield


--MODEL


type alias Model =
    { statusMessage : String
    , dinner : Dinner
    , dinners : List Dinner
    , searchText : String
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model



--INIT


init : Model
init =
    Model "" (Dinner "" "" "" "") [] "" Material.model



--MSG


type Msg
    = GetRandomDinner
    | NewDinner (Result Http.Error Dinner)
    | SearchResults (Result Http.Error (List Dinner))
    | SearchText String
    | SearchDinners
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
        ]


dinnerTable : Model -> Html Msg
dinnerTable model =
    table [ align "center" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Url" ]
                , th [] [ text "Tags" ]
                , th [] [ text "Portions" ]
                ]
            ]
        , tbody [] (List.map renderDinners model.dinners)
        ]


renderDinners : Dinner -> Html Msg
renderDinners dinner =
    tr []
        [ td [ align "left" ] [ text dinner.name ]
        , td [ align "left" ] [ text dinner.url ]
        , td [ align "left" ] [ text dinner.tags ]
        , td [ align "left" ] [ text dinner.portions ]
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
