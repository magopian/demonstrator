module Main exposing (..)

import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Requests
import Task
import Time
import Types exposing (..)


type alias Flags =
    { apiBaseUrl : Maybe String
    , displayDisclaimer : Maybe Bool
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { apiBaseUrl : String
    , debounce : Debounce ( Period, List Individual )
    , displayDisclaimer : Bool
    , displayRoles : Bool
    , entitiesWebData : WebData (Dict String Entity)
    , individuals : List Individual
    , period : String
    , simulateWebData : WebData SimulateNode
    , variablesWebData : WebData VariablesResponse
    , waterfallIndex : Int
    }


initialModel : Model
initialModel =
    { apiBaseUrl = "//localhost:2000/api"
    , debounce = Debounce.init
    , displayDisclaimer = True
    , displayRoles = False
    , entitiesWebData = NotAsked
    , individuals = []
    , period =
        -- TODO Add UI to set period
        "2015"
    , simulateWebData = NotAsked
    , variablesWebData = NotAsked
    , waterfallIndex = 0
    }


initialIndividuals : Dict String Entity -> List Individual
initialIndividuals entities =
    let
        salaireVariableName =
            -- TODO Do not hardcode
            "salaire_de_base"

        initialSalaire =
            ( salaireVariableName, FloatInputValue 0 )

        firstOrSecondRole : List Role -> Maybe Role
        firstOrSecondRole roles =
            roles
                |> List.head
                |> Maybe.andThen
                    (\firstRole ->
                        let
                            secondRole =
                                roles
                                    |> List.drop 1
                                    |> List.head
                        in
                            case firstRole.max of
                                Nothing ->
                                    Just firstRole

                                Just firstRoleMax ->
                                    if firstRoleMax > 1 then
                                        Just firstRole
                                    else
                                        secondRole
                    )
    in
        [ { inputValues = Dict.fromList [ initialSalaire ]
          , roles = initialRoles entities List.head
          }
        , { inputValues = Dict.fromList [ initialSalaire ]
          , roles = initialRoles entities firstOrSecondRole
          }
        ]


initialRoles : Dict String Entity -> (List Role -> Maybe Role) -> Dict String String
initialRoles entities getter =
    entities
        |> Dict.values
        |> List.filterMap
            (\entity ->
                if entity.isPersonsEntity then
                    Nothing
                else
                    entity.roles
                        |> getter
                        |> Maybe.map (\firstRole -> ( pluralOrKey entity, pluralOrKey firstRole ))
            )
        |> Dict.fromList


initialCmds : String -> List (Cmd Msg)
initialCmds apiBaseUrl =
    let
        entitiesCmd =
            Requests.entities apiBaseUrl
                |> RemoteData.sendRequest
                |> Cmd.map EntitiesResult

        variablesCmd =
            Requests.variables apiBaseUrl
                |> RemoteData.sendRequest
                |> Cmd.map VariablesResult
    in
        [ entitiesCmd, variablesCmd ]


init : Flags -> ( Model, Cmd Msg )
init { apiBaseUrl, displayDisclaimer } =
    let
        newModel =
            { initialModel
                | apiBaseUrl = apiBaseUrl |> Maybe.withDefault initialModel.apiBaseUrl
                , displayDisclaimer = displayDisclaimer |> Maybe.withDefault initialModel.displayDisclaimer
                , entitiesWebData = Loading
                , variablesWebData = Loading
            }
    in
        newModel ! initialCmds newModel.apiBaseUrl



-- UPDATE


type Msg
    = CloseDisclaimer
    | DebounceMsg Debounce.Msg
    | EntitiesResult (WebData (Dict String Entity))
    | ResetApplication
    | SetDisplayRoles Bool
    | SetInputValue Int String InputValue
    | SetRole Int String String
    | SetWaterfallIndex Int
    | Simulate ( Period, List Individual )
    | SimulateResult (WebData SimulateNode)
    | VariablesResult (WebData VariablesResponse)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (1 * Time.second)
    , transform = DebounceMsg
    }


simulate : ( Period, List Individual ) -> Cmd Msg
simulate pair =
    Task.perform Simulate (Task.succeed pair)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        renderWaterfallCmd webData =
            case webData of
                Success simulateNode ->
                    let
                        data =
                            Ports.waterfallData model.waterfallIndex simulateNode
                    in
                        Ports.renderWaterfall data

                _ ->
                    Cmd.none
    in
        case msg of
            CloseDisclaimer ->
                let
                    newModel =
                        { model | displayDisclaimer = False }

                    localStorageCmd =
                        Ports.writeToLocalStorage
                            { key = "display-disclaimer"
                            , value = Just (Encode.bool False)
                            }

                    cmds =
                        [ localStorageCmd, renderWaterfallCmd model.simulateWebData ]
                in
                    newModel ! cmds

            DebounceMsg msg ->
                let
                    ( debounce, cmd ) =
                        Debounce.update debounceConfig (Debounce.takeLast simulate) msg model.debounce

                    newModel =
                        { model | debounce = debounce }
                in
                    ( newModel, cmd )

            EntitiesResult webData ->
                let
                    newIndividuals =
                        case webData of
                            Success entities ->
                                initialIndividuals entities

                            _ ->
                                []

                    newModel =
                        { model
                            | entitiesWebData =
                                webData
                                    |> RemoteData.mapError (Debug.log "model.entitiesWebData Failure")
                            , individuals = newIndividuals
                        }

                    cmd =
                        simulate ( model.period, newIndividuals )
                in
                    ( newModel, cmd )

            ResetApplication ->
                let
                    localStorageCmd =
                        Ports.writeToLocalStorage
                            { key = "display-disclaimer"
                            , value = Nothing
                            }

                    cmds =
                        localStorageCmd :: initialCmds model.apiBaseUrl
                in
                    initialModel ! cmds

            SetDisplayRoles bool ->
                let
                    newModel =
                        { model | displayRoles = bool }
                in
                    ( newModel, Cmd.none )

            SetInputValue index variableName inputValue ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.indexedMap
                                (\index1 individual ->
                                    if index == index1 then
                                        let
                                            newInputValues =
                                                Dict.insert variableName inputValue individual.inputValues
                                        in
                                            { individual | inputValues = newInputValues }
                                    else
                                        individual
                                )

                    ( newDebounce, cmd ) =
                        Debounce.push debounceConfig ( model.period, newIndividuals ) model.debounce

                    newModel =
                        { model
                            | debounce = newDebounce
                            , individuals = newIndividuals
                        }
                in
                    ( newModel, cmd )

            SetRole index entityId roleId ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.indexedMap
                                (\index1 individual ->
                                    if index == index1 then
                                        let
                                            newRoles =
                                                Dict.insert entityId roleId individual.roles
                                        in
                                            { individual | roles = newRoles }
                                    else
                                        individual
                                )

                    ( newDebounce, cmd ) =
                        Debounce.push debounceConfig ( model.period, newIndividuals ) model.debounce

                    newModel =
                        { model
                            | debounce = newDebounce
                            , individuals = newIndividuals
                        }
                in
                    ( newModel, cmd )

            SetWaterfallIndex index ->
                let
                    newModel =
                        { model | waterfallIndex = index }

                    cmd =
                        renderWaterfallCmd model.simulateWebData
                in
                    ( newModel, cmd )

            Simulate ( period, individuals ) ->
                let
                    newModel =
                        { model | simulateWebData = Loading }

                    cmd =
                        Requests.simulate model.apiBaseUrl individuals period
                            |> RemoteData.sendRequest
                            |> Cmd.map SimulateResult
                in
                    ( newModel, cmd )

            SimulateResult webData ->
                let
                    newModel =
                        { model
                            | simulateWebData =
                                webData
                                    |> RemoteData.mapError (Debug.log "model.simulateWebData Failure")
                        }

                    cmd =
                        renderWaterfallCmd webData
                in
                    ( newModel, cmd )

            VariablesResult webData ->
                let
                    newModel =
                        { model
                            | variablesWebData =
                                webData
                                    |> RemoteData.mapError (Debug.log "model.variablesWebData Failure")
                        }
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
        [ viewNavBar
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
                            [ p []
                                [ text "Loading data..."
                                  -- TODO i18n
                                ]
                            ]

                        Failure err ->
                            [ div [ class "alert alert-danger" ]
                                -- TODO i18n
                                [ h4 [] [ text "We are sorry" ]
                                , p [] [ text "There was an error while loading data." ]
                                , p [] [ text "If you're a technical person, you can look at your browser console to see the detailed error." ]
                                ]
                            ]

                        Success ( entities, variablesResponse ) ->
                            [ div [ class "row" ]
                                (viewIndividuals model.displayRoles
                                    entities
                                    variablesResponse
                                    model.waterfallIndex
                                    model.individuals
                                    :: [ div [ class "col-sm-8" ]
                                            (case model.simulateWebData of
                                                NotAsked ->
                                                    []

                                                Loading ->
                                                    [ p []
                                                        [ text "Calculation in progress..."
                                                          -- TODO i18n
                                                        ]
                                                    ]

                                                Failure err ->
                                                    [ div [ class "alert alert-danger" ]
                                                        -- TODO i18n
                                                        [ h4 [] [ text "We are sorry" ]
                                                        , p [] [ text "There was an error while calculating result." ]
                                                        , p [] [ text "If you're a technical person, you can look at your browser console to see the detailed error." ]
                                                        ]
                                                    ]

                                                Success simulateNode ->
                                                    [ viewDecomposition model.waterfallIndex simulateNode ]
                                            )
                                       ]
                                )
                            ]
                   )
                ++ [ button
                        [ class "btn btn-default pull-right"
                        , onClick ResetApplication
                        , title "Clear all custom data and make the application look like the first time it was loaded."
                        ]
                        [ text "Reset application"
                        ]
                   ]
            )
        , viewFooter
        ]


viewDecomposition : Int -> SimulateNode -> Html Msg
viewDecomposition waterfallIndex simulateNode =
    let
        viewSimulateNode : SimulateNode -> Html Msg
        viewSimulateNode (SimulateNode fields) =
            div []
                (case
                    List.getAt waterfallIndex fields.values
                        |> Maybe.andThen
                            (\value ->
                                if value == 0 then
                                    Nothing
                                else
                                    Just value
                            )
                 of
                    Nothing ->
                        []

                    Just value ->
                        [ text fields.name
                        , text " "
                        , samp [] [ text (toString value) ]
                        ]
                            ++ (if List.isEmpty fields.children then
                                    []
                                else
                                    [ div [ style [ ( "margin-left", "1em" ) ] ]
                                        (List.map viewSimulateNode fields.children)
                                    ]
                               )
                )
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ text "Decomposition of household income"
                      -- TODO i18n
                    ]
                ]
            , ul [ class "list-group" ]
                [ li [ class "list-group-item" ]
                    [ div [ id "waterfall" ]
                        [-- Filled by "renderWaterfall" port
                        ]
                    ]
                , li [ class "list-group-item" ]
                    [ viewSimulateNode simulateNode ]
                ]
            ]


viewDisclaimer : Html Msg
viewDisclaimer =
    div [ class "alert alert-info" ]
        [ button
            [ attribute "aria-hidden" "true"
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
        , button
            [ class "btn btn-link"
            , onClick CloseDisclaimer
            ]
            [ text "J'ai compris, ne plus afficher" ]
        ]


viewFooter : Html msg
viewFooter =
    footer
        [ style
            [ ( "padding-top", "50px" )
            , ( "padding-bottom", "50px" )
            , ( "margin-top", "100px" )
            , ( "color", "#99979c" )
            , ( "text-align", "left" )
            , ( "background-color", "#2a2730" )
            ]
        ]
        [ div [ class "container" ]
            [ ul
                [ style
                    [ ( "padding-left", "0" )
                    , ( "margin-bottom", "20px" )
                    ]
                ]
                [ li
                    [ style [ ( "display", "inline-block" ) ] ]
                    [ a
                        [ href "https://www.openfisca.fr/"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "Home"
                          -- TODO i18n
                        ]
                    ]
                , li
                    [ style [ ( "display", "inline-block" ), ( "margin-left", "15px" ) ] ]
                    [ a
                        [ href "https://twitter.com/OpenFisca"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "Twitter" ]
                    ]
                , li
                    [ style [ ( "display", "inline-block" ), ( "margin-left", "15px" ) ] ]
                    -- TODO Do not hardcode URL
                    [ a
                        [ href "https://github.com/openfisca/demonstrator"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "GitHub" ]
                    ]
                ]
            , p []
                [ text "Designed and built with all the love in the world by "
                , a [ href "https://twitter.com/mdo", target "_blank" ]
                    [ text "@mdo" ]
                , text " and "
                , a [ href "https://twitter.com/fat", target "_blank" ]
                    [ text "@fat" ]
                , text ". Maintained by the "
                , a [ href "https://github.com/orgs/twbs/people" ]
                    [ text "core team" ]
                , text " with the help of "
                , a [ href "https://github.com/twbs/bootstrap/graphs/contributors" ]
                    [ text "our contributors" ]
                , text "."
                ]
            , p []
                [ text "Code licensed "
                  -- TODO Fix link
                , a
                    [ href "https://github.com/openfisca/demonstrator/blob/master/LICENSE"
                    , target "_blank"
                    , rel "license"
                    ]
                    -- TODO Fix license name
                    [ text "GNU Affero GPL version 3+" ]
                ]
            ]
        ]


viewIndividual : Bool -> Dict String Entity -> VariablesResponse -> Int -> Int -> Individual -> Html Msg
viewIndividual displayRoles entities variablesResponse waterfallIndex index individual =
    let
        individualLabel =
            entities
                |> Dict.toList
                |> List.filterMap
                    (\( _, entity ) ->
                        if entity.isPersonsEntity then
                            Just entity.label
                        else
                            Nothing
                    )
                |> List.head
                |> -- TODO i18n
                   Maybe.withDefault "Individual"

        viewIndividualRoles roles =
            div []
                (roles
                    |> Dict.toList
                    |> List.filterMap
                        (\( entityId, roleId ) ->
                            findEntity entityId entities
                                |> Maybe.andThen
                                    (\entity ->
                                        findRole roleId entity.roles
                                            |> Maybe.map (viewIndividualRole entities index entityId entity)
                                    )
                        )
                )
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ text (individualLabel ++ " " ++ (toString (index + 1))) ]
                ]
            , ul [ class "list-group" ]
                ([ li [ class "list-group-item" ]
                    [ div []
                        (individual.inputValues
                            |> Dict.toList
                            |> List.map
                                (\( variableName, inputValue ) ->
                                    let
                                        label =
                                            variableLabel variablesResponse.variables variableName
                                    in
                                        viewInputValue index variableName label inputValue waterfallIndex
                                )
                        )
                    ]
                 ]
                    ++ (if displayRoles then
                            [ li [ class "list-group-item" ]
                                [ viewIndividualRoles individual.roles ]
                            ]
                        else
                            []
                       )
                )
            ]


viewIndividualRole : Dict String Entity -> Int -> String -> Entity -> Role -> Html Msg
viewIndividualRole entities index entityId entity role =
    div [ class "form-group" ]
        [ label [] [ text entity.label ]
        , select
            [ class "form-control"
            , on "change"
                (targetValue |> Decode.map (SetRole index entityId))
            ]
            (entity.roles
                |> List.map
                    (\role1 ->
                        option
                            [ selected (role1.key == role.key)
                            , value (pluralOrKey role1)
                            ]
                            [ text role1.label ]
                    )
            )
        ]


viewIndividuals : Bool -> Dict String Entity -> VariablesResponse -> Int -> List Individual -> Html Msg
viewIndividuals displayRoles entities variablesResponse waterfallIndex individuals =
    div [ class "col-sm-4" ]
        ((individuals
            |> List.indexedMap (viewIndividual displayRoles entities variablesResponse waterfallIndex)
         )
            ++ [ div [ class "checkbox" ]
                    [ label []
                        [ input
                            [ onCheck SetDisplayRoles
                            , type_ "checkbox"
                            ]
                            []
                        , text " Display roles"
                          -- TODO i18n
                        ]
                    ]
               ]
        )


viewInputValue : Int -> VariableName -> String -> InputValue -> Int -> Html Msg
viewInputValue index variableName variableLabel inputValue waterfallIndex =
    div [ class "form-group" ]
        [ let
            truncatedLabel =
                String.left 25 variableLabel
          in
            label
                [ title variableLabel
                  -- Let the user hover the truncated label to see the full label
                ]
                [ text (truncatedLabel ++ "...") ]
        , if index == 0 && variableName == "salaire_de_base" then
            -- TODO Do not hardcode variable name
            div []
                [ input
                    [ -- TODO Do not hardcode (see axis in Requests.elm)
                      Html.Attributes.max "49"
                    , Html.Attributes.min "0"
                    , onInput (SetWaterfallIndex << (String.toInt >> Result.withDefault 0))
                    , type_ "range"
                    , value (toString waterfallIndex)
                    ]
                    []
                  -- TODO Do not hardcode (see axis in Requests.elm)
                , samp [] [ text (toString ((toFloat waterfallIndex) * 100000 / 49)) ]
                ]
          else
            case inputValue of
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
                                        if String.isEmpty str then
                                            FloatInputValue 0
                                        else
                                            case String.toFloat str of
                                                Ok newFloat ->
                                                    FloatInputValue newFloat

                                                Err _ ->
                                                    FloatInputValue float
                                in
                                    SetInputValue index variableName newInputValue
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
                                        if String.isEmpty str then
                                            IntInputValue 0
                                        else
                                            case String.toInt str of
                                                Ok newInt ->
                                                    IntInputValue newInt

                                                Err _ ->
                                                    IntInputValue int
                                in
                                    SetInputValue index variableName newInputValue
                            )
                        , step "1"
                        , type_ "number"
                        , value (toString int)
                        ]
                        []
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
