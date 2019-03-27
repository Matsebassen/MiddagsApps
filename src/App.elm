module App exposing (..)

--

import AddDinner
import SearchDinner
import ShoppingList
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Button as Button
import Material.Options as Options exposing (css)


type alias Model =
    { selectedTab : Int
    , mdl : Material.Model
    , addDinnerModel : AddDinner.Model
    , searchDinnerModel : SearchDinner.Model
    , shoppingListModel : ShoppingList.Model
    }


type alias Mdl =
    Material.Model


init : ( Model, Cmd Msg )
init =
    update (ShoppingListMsg (ShoppingList.GetShoppingList 0)) (Model 2 Material.model AddDinner.init SearchDinner.init ShoppingList.init)



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | AddDinnerMsg AddDinner.Msg
    | SearchDinnerMsg SearchDinner.Msg
    | ShoppingListMsg ShoppingList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectTab num ->
            if ( num == 2) then
                update (ShoppingListMsg (ShoppingList.GetShoppingList 0)) ({ model | selectedTab = num })
                --update (Mdl Layout.ToggleDrawer) model
            else
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

        ShoppingListMsg m ->
            let
                ( subMdl, subCmd ) =
                    ShoppingList.update m model.shoppingListModel
            in
                { model | shoppingListModel = subMdl }
                    ! [ Cmd.map ShoppingListMsg subCmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selectedTab of
        2 ->
            Sub.map ShoppingListMsg (ShoppingList.subscriptions model.shoppingListModel)

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.onSelectTab SelectTab
        ]
        { header = [ h2 [ style [ ( "padding", "0rem" ) ] ] [ text "Middag" ] ]
        , drawer = [ materialFlatButton model "Search" 0, materialFlatButton model "Add Dinner" 1, materialFlatButton model "Shopping List" 2 ]
        , tabs = ( [ text "Search", text "Add Dinner", text "Shopping List" ], [ Color.background (Color.color Color.Teal Color.S600) ] )
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

        2 ->
            div []
                [ br [] []
                , Html.map ShoppingListMsg <| ShoppingList.view model.shoppingListModel
                ]

        _ ->
            div []
                [ br [] []
                , Html.map ShoppingListMsg <| ShoppingList.view model.shoppingListModel
                ]


materialFlatButton : Model -> String -> Int -> Html Msg
materialFlatButton model butTxt tabNo =
    Button.render Mdl
        [ tabNo ]
        model.mdl
        [ Button.colored
        , Button.ripple
        , Options.onClick (SelectTab tabNo)
        , css "margin-top" "20px"
        ]
        [ text butTxt ]
