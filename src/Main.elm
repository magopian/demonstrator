module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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


type alias Model =
    { displayDisclaimer : Bool
    , variablesWebData : WebData VariablesResponse
    }


initialModel : Model
initialModel =
    { displayDisclaimer = True
    , variablesWebData = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    let
        baseUrl =
            "http://localhost:2001/api"

        newModel =
            { initialModel | variablesWebData = Loading }

        variablesCmd =
            Http.send VariablesResult (Requests.variables baseUrl)
    in
        -- TODO Load baseUrl and displayDisclaimer from flags and store setting in localStorage.
        ( newModel, variablesCmd )



-- UPDATE


type Msg
    = CloseDisclaimer
    | VariablesResult (Result Http.Error VariablesResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseDisclaimer ->
            ( model, Cmd.none )

        VariablesResult result ->
            let
                newModel =
                    { model | variablesWebData = RemoteData.fromResult result }
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
                ++ [ case model.variablesWebData of
                        NotAsked ->
                            text ""

                        Loading ->
                            p [] [ text "Loading variables data..." ]

                        Failure err ->
                            let
                                _ =
                                    Debug.log "variablesWebData Failure" err
                            in
                                div [ class "alert alert-danger" ]
                                    [ h4 [] [ text "We are sorry" ]
                                    , p [] [ text "There was an error loading variables data." ]
                                    ]

                        Success variablesResponse ->
                            p [] [ text "OK" ]
                   ]
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
