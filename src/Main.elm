module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Requests
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type InputValue
    = BoolInputValue Bool
    | DateInputValue String
    | EnumInputValue String
    | FloatInputValue Float
    | IntInputValue Int


type alias Individual =
    { inputValues : Dict String InputValue
    , roles : Dict String String
    }


type alias Model =
    { displayDisclaimer : Bool
    , entitiesWebData : WebData (Dict String Entity)
    , individuals : List Individual
    , variablesWebData : WebData VariablesResponse
    }


initialModel : Model
initialModel =
    { displayDisclaimer = True
    , entitiesWebData = NotAsked
    , individuals =
        [ { inputValues =
                Dict.fromList
                    [ ( "salaire_de_base", FloatInputValue 10000 )
                    ]
          , roles =
                Dict.fromList
                    [ ( "parent", "famille" )
                    ]
          }
        ]
    , variablesWebData = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    let
        baseUrl =
            "http://localhost:2001/api"

        newModel =
            { initialModel | variablesWebData = Loading }

        entitiesCmd =
            Requests.entities baseUrl
                |> RemoteData.sendRequest
                |> Cmd.map EntitiesResult

        variablesCmd =
            Requests.variables baseUrl
                |> RemoteData.sendRequest
                |> Cmd.map VariablesResult
    in
        -- TODO Load baseUrl and displayDisclaimer from flags and store setting in localStorage.
        newModel ! [ entitiesCmd, variablesCmd ]



-- UPDATE


type Msg
    = CloseDisclaimer
    | EntitiesResult (WebData (Dict String Entity))
    | SetInputValue Int String InputValue
    | VariablesResult (WebData VariablesResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseDisclaimer ->
            ( model, Cmd.none )

        EntitiesResult webData ->
            let
                newModel =
                    { model | entitiesWebData = webData }
            in
                ( newModel, Cmd.none )

        SetInputValue index name inputValue ->
            let
                newIndividuals =
                    model.individuals
                        |> List.indexedMap
                            (\index1 individual ->
                                if index == index1 then
                                    let
                                        newInputValues =
                                            Dict.insert name inputValue individual.inputValues
                                    in
                                        { individual | inputValues = newInputValues }
                                else
                                    individual
                            )

                newModel =
                    { model | individuals = newIndividuals }
            in
                ( newModel, Cmd.none )

        VariablesResult webData ->
            let
                newModel =
                    { model | variablesWebData = webData }
            in
                ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ viewNavBar
         , div [ class "container-fluid" ]
            ((if model.displayDisclaimer then
                [ viewDisclaimer ]
              else
                []
             )
                ++ (case RemoteData.append model.entitiesWebData model.variablesWebData of
                        NotAsked ->
                            []

                        Loading ->
                            [ p [] [ text "Loading data..." ] ]

                        Failure err ->
                            let
                                _ =
                                    Debug.log "Load data failure" err
                            in
                                [ div [ class "alert alert-danger" ]
                                    [ h4 [] [ text "We are sorry" ]
                                    , p [] [ text "There was an error loading data." ]
                                    ]
                                ]

                        Success ( entities, variablesResponse ) ->
                            [ viewIndividuals entities variablesResponse model.individuals ]
                   )
            )
         ]
        )


viewDisclaimer : Html Msg
viewDisclaimer =
    div [ class "alert alert-info" ]
        [ button
            [ attribute "aria-hidden" "true"
            , attribute "data-dismiss" "alert"
            , class "close"
            , onClick CloseDisclaimer
            , type_ "button"
            ]
            [ text "×" ]
          -- TODO Translate in english
        , h4 [] [ text "À propos de cet outil" ]
        , ul []
            [ li [] [ text "OpenFisca est un simulateur socio-fiscal en cours de développement." ]
            , li [] [ text "Les résultats des simulations peuvent comporter des erreurs, et vous pouvez commencer à contribuer au projet en les faisant remonter si vous les détectez." ]
            , li [] [ text "Cet outil est un démonstrateur pour OpenFisca qui illustre ses possibilités concernant des cas types." ]
            , li [] [ text "Les données que vous saisissez ne sont jamais stockées." ]
            ]
        , p []
            [ strong [] [ text "Les résultats affichés n'ont en aucun cas un caractère officiel." ] ]
          -- , button -- TODO Store setting in localStorage
          --     [ class "btn btn-link"
          --     , onClick CloseDisclaimer
          --     ]
          --     [ text "J'ai compris, ne plus afficher" ]
        ]


viewIndividual : VariablesResponse -> Int -> Individual -> Html Msg
viewIndividual variablesResponse index individual =
    div [ class "panel panel-default" ]
        [ div [ class "panel-heading" ]
            [ h3 [ class "panel-title" ]
                -- TODO i18n
                [ text ("Individual " ++ (toString (index + 1))) ]
            ]
        , ul [ class "list-group" ]
            [ li [ class "list-group-item" ]
                [ div [ class "form-horizontal" ]
                    (individual.inputValues
                        |> Dict.toList
                        |> List.map
                            (\( name, inputValue ) ->
                                let
                                    label =
                                        variablesResponse.variables
                                            |> Dict.get name
                                            |> Maybe.andThen (variableCommonFields >> .label)
                                            |> Maybe.withDefault name
                                in
                                    viewInputValue index name label inputValue
                            )
                    )
                ]
            , li [ class "list-group-item" ]
                [ p [] [ text "Roles" ] ]
            ]
        ]


viewIndividuals : Dict String Entity -> VariablesResponse -> List Individual -> Html Msg
viewIndividuals entities variablesResponse individuals =
    div []
        (individuals
            |> List.indexedMap (viewIndividual variablesResponse)
        )


viewInputValue : Int -> String -> String -> InputValue -> Html Msg
viewInputValue index name label inputValue =
    div [ class "form-group" ]
        [ Html.label [ class "col-sm-2 control-label" ]
            [ text label ]
        , div [ class "col-sm-10" ]
            [ case inputValue of
                BoolInputValue bool ->
                    text "TODO"

                DateInputValue string ->
                    text "TODO"

                EnumInputValue string ->
                    text "TODO"

                FloatInputValue float ->
                    input
                        [ class "form-control"
                        , onInput
                            (\str ->
                                let
                                    newInputValue =
                                        case String.toFloat str of
                                            Ok newFloat ->
                                                FloatInputValue newFloat

                                            Err _ ->
                                                FloatInputValue float
                                in
                                    SetInputValue index name newInputValue
                            )
                        , step "any"
                        , type_ "number"
                        , value (toString float)
                        ]
                        []

                IntInputValue int ->
                    input
                        [ class "form-control"
                        , onInput
                            (\str ->
                                let
                                    newInputValue =
                                        case String.toInt str of
                                            Ok newInt ->
                                                IntInputValue newInt

                                            Err _ ->
                                                IntInputValue int
                                in
                                    SetInputValue index name newInputValue
                            )
                        , step "1"
                        , type_ "number"
                        , value (toString int)
                        ]
                        []
            ]
        ]


viewNavBar : Html msg
viewNavBar =
    nav [ class "navbar navbar-inverse navbar-static-top", attribute "role" "navigation" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ button
                    [ class "navbar-toggle"
                    , attribute "data-target" "#topbar-collapse"
                    , attribute "data-toggle" "collapse"
                    , type_ "button"
                    ]
                    [ span [ class "sr-only" ]
                        [ text "Basculer la navigation" ]
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    ]
                , a [ class "navbar-brand", href "https://ui.openfisca.fr/" ]
                    -- TODO Add to program flags
                    [ text "Démonstrateur OpenFisca" ]
                ]
            , div [ class "collapse navbar-collapse", id "topbar-collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    []
                , ul [ class "nav navbar-nav navbar-right" ]
                    [ li []
                        [ a [ href "https://www.openfisca.fr/" ]
                            -- TODO Add to program flags
                            [ text "Retour à l'accueil" ]
                        ]
                    , li [ class "visible-xs-block" ]
                        [ a [ href "http://stats.data.gouv.fr/index.php?idSite=4" ]
                            -- TODO Add to program flags
                            [ text "Statistiques du site" ]
                        ]
                    , li [ class "visible-xs-block" ]
                        [ a [ href "https://www.openfisca.fr/mentions-legales" ]
                            -- TODO Add to program flags
                            [ text "Mentions légales" ]
                        ]
                      -- , li [ class "visible-xs-block" ] -- TODO
                      --     [ a [ href "/privacy-policy" ]
                      --         [ text "Politique de confidentialité" ]
                      --     ]
                      -- , li [ class "dropdown" ] -- TODO
                      --     [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", href "#" ]
                      --         [ text "Français "
                      --         , span [ class "caret" ]
                      --             []
                      --         ]
                      --     , ul [ class "dropdown-menu", attribute "role" "menu" ]
                      --         [ li [ class "dropdown-header", attribute "role" "presentation" ]
                      --             [ text "France" ]
                      --         , li []
                      --             [ a [ href "/" ]
                      --                 [ text "Français" ]
                      --             ]
                      --         , li []
                      --             [ a [ href "/en" ]
                      --                 [ text "English" ]
                      --             ]
                      --         ]
                      --     ]
                    ]
                ]
            ]
        ]
