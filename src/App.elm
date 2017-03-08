module App exposing (..)

--

import AddDinner
import SearchDinner
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Layout as Layout
import Material.Color as Color
import Spinner


type alias Model =
    { selectedTab : Int
    , mdl : Material.Model
    , addDinnerModel : AddDinner.Model
    , searchDinnerModel : SearchDinner.Model
    }


type alias Mdl =
    Material.Model


init : ( Model, Cmd Msg )
init =
    ( Model 0 Material.model AddDinner.init SearchDinner.init, Cmd.none )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | AddDinnerMsg AddDinner.Msg
    | SearchDinnerMsg SearchDinner.Msg
    | SpinnerMsgAdd Spinner.Msg
    | SpinnerMsgSearch Spinner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectTab num ->
            { model | selectedTab = num } ! []

        AddDinnerMsg m ->
            let
                ( subMdl, subCmd ) =
                    AddDinner.update m model.addDinnerModel
            in
                { model | addDinnerModel = subMdl }
                    ! [ Cmd.map AddDinnerMsg subCmd ]

        SearchDinnerMsg m ->
            let
                ( subMdl, subCmd ) =
                    SearchDinner.update m model.searchDinnerModel
            in
                { model | searchDinnerModel = subMdl }
                    ! [ Cmd.map SearchDinnerMsg subCmd ]

        SpinnerMsgAdd msg ->
            let
                spinnerModel =
                    Spinner.update msg model.addDinnerModel.spinner
            in
                { model | addDinnerModel = setAddDinnerModel spinnerModel model.addDinnerModel } ! []

        SpinnerMsgSearch msg ->
            let
                spinnerModel =
                    Spinner.update msg model.searchDinnerModel.spinner
            in
                { model | searchDinnerModel = setSearchDinnerModel spinnerModel model.searchDinnerModel } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selectedTab of
        0 ->
            case model.searchDinnerModel.waiting of
                True ->
                    Sub.map SpinnerMsgSearch Spinner.subscription

                False ->
                    Sub.none

        1 ->
            Sub.map SpinnerMsgAdd Spinner.subscription

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    --Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.onSelectTab SelectTab
        ]
        { header = [ h2 [ style [ ( "padding", "0rem" ) ] ] [ text "Middag" ] ]
        , drawer = []
        , tabs = ( [ text "Search", text "Add Dinner" ], [ Color.background (Color.color Color.Teal Color.S600) ] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            div []
                [ br [] []
                , Html.map SearchDinnerMsg <| SearchDinner.view model.searchDinnerModel
                ]

        1 ->
            div []
                [ br [] []
                , Html.map AddDinnerMsg <| AddDinner.view model.addDinnerModel
                ]

        _ ->
            div []
                [ br [] []
                , Html.map SearchDinnerMsg <| SearchDinner.view model.searchDinnerModel
                ]


setAddDinnerModel : Spinner.Model -> AddDinner.Model -> AddDinner.Model
setAddDinnerModel spinnerModel dinnerModel =
    { dinnerModel | spinner = spinnerModel }


setSearchDinnerModel : Spinner.Model -> SearchDinner.Model -> SearchDinner.Model
setSearchDinnerModel spinnerModel dinnerModel =
    { dinnerModel | spinner = spinnerModel }
