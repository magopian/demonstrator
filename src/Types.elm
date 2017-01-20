module Types exposing (..)

import Dict exposing (Dict)
import List.Extra as List


-- INDIVIDUALS


type alias Individual =
    { inputValues : Dict VariableName InputValue
    , roles : Dict String String
    }


type InputValue
    = BoolInputValue Bool
    | DateInputValue String
    | EnumInputValue String
    | FloatInputValue Float
    | IntInputValue Int


individualId : Int -> String
individualId index =
    "individual_" ++ (toString (index + 1))



-- ENTITIES


type alias Role =
    { key : String
    , label : String
    , max : Maybe Int
    , plural : String
    , subroles : List String
    }


type alias Entity =
    { isPersonsEntity : Bool
    , key : String
    , label : String
    , plural : String
    , roles : List Role
    }


findEntity : String -> Dict String Entity -> Maybe Entity
findEntity entityId entities =
    -- A simple `Dict.get` does not the job: we want to find an entity by its "plural-or-key".
    entities
        |> Dict.values
        |> List.find (\entity -> pluralOrKey entity == entityId)


findRole : String -> List Role -> Maybe Role
findRole roleId roles =
    List.find (\role -> pluralOrKey role == roleId) roles


pluralOrKey : { a | key : String, plural : String } -> String
pluralOrKey record =
    if String.isEmpty record.plural then
        record.key
    else
        record.plural



-- GROUP ENTITIES (sub-part of a test case)


type alias GroupEntity =
    Dict String (List String)


groupEntities : List Individual -> Maybe (Dict String GroupEntity)
groupEntities individuals =
    let
        mergeTestCases : Dict String GroupEntity -> Dict String GroupEntity -> Dict String GroupEntity
        mergeTestCases testCase1 testCase2 =
            Dict.merge
                Dict.insert
                (\entityId roles1 roles2 individualAccu ->
                    individualAccu |> Dict.insert entityId (mergeEntities roles1 roles2)
                )
                Dict.insert
                testCase1
                testCase2
                Dict.empty

        mergeEntities : Dict String (List String) -> Dict String (List String) -> Dict String (List String)
        mergeEntities entities1 entities2 =
            Dict.merge
                Dict.insert
                (\roleId individualIds1 individualIds2 entityAccu ->
                    entityAccu |> Dict.insert roleId (individualIds1 ++ individualIds2)
                )
                Dict.insert
                entities1
                entities2
                Dict.empty
    in
        individuals
            |> List.indexedMap
                (\index { roles } ->
                    roles
                        |> Dict.map
                            (\entityId roleId ->
                                Dict.singleton roleId [ individualId index ]
                            )
                )
            |> List.foldl1 mergeTestCases



-- AXES


type alias Axis =
    { count : Int
    , individualIndex : Int
    , max : Float
    , min : Float
    , variableName : VariableName
    , selectedIndex : Int
    }


axisValue : Axis -> Float
axisValue axis =
    ((toFloat axis.selectedIndex) * axis.max) / (axis.count - 1 |> toFloat)


getValue : List Float -> List Axis -> Maybe Float
getValue values axes =
    let
        index =
            axes
                |> List.map .selectedIndex
                -- TODO Really take into account selected indexes when multiple axes
                |>
                    List.head
                |> Maybe.withDefault 0
    in
        List.getAt index values
            |> Maybe.andThen
                (\value ->
                    if value == 0 then
                        Nothing
                    else
                        Just value
                )


findAxis : Int -> VariableName -> List Axis -> Maybe Axis
findAxis individualIndex variableName axes =
    List.find (\axis -> axis.individualIndex == individualIndex && axis.variableName == variableName) axes



-- PERIODS


type alias Period =
    Int



-- SIMULATE


type SimulateNode
    = SimulateNode SimulateNodeFields


type alias SimulateNodeFields =
    { children : List SimulateNode
    , code : String
    , color : ( Int, Int, Int )
    , name :
        -- Not really a VariableName (could be a decomposition node which does not correspond to a variable)
        String
    , shortName : String
    , values : List Float
    }



-- VARIABLES


type alias VariableName =
    String


type alias VariableCommonFields =
    { entity : String
    , label : Maybe String
    , name : VariableName
    , sourceCode : String
    , sourceFilePath : String
    , startLineNumber : Int
    , type_ : String
    }


type alias BoolVariableFields =
    { default : Bool }


type alias DateVariableFields =
    { default : String }


type alias EnumVariableFields =
    { default : String
    , labels : Dict String String
    }


type alias FloatVariableFields =
    { default : Float }


type alias IntVariableFields =
    { default : Int }


type alias StringVariableFields =
    { default : String }


type Variable
    = BoolVariable ( VariableCommonFields, BoolVariableFields )
    | DateVariable ( VariableCommonFields, DateVariableFields )
    | EnumVariable ( VariableCommonFields, EnumVariableFields )
    | FloatVariable ( VariableCommonFields, FloatVariableFields )
    | IntVariable ( VariableCommonFields, IntVariableFields )
    | StringVariable ( VariableCommonFields, StringVariableFields )


type alias VariablesResponse =
    { countryPackageName : String
    , countryPackageVersion : String
    , currency : String
    , variables : Dict String Variable
    }


variableCommonFields : Variable -> VariableCommonFields
variableCommonFields variable =
    case variable of
        (BoolVariable ( x, _ )) as variable ->
            x

        (DateVariable ( x, _ )) as variable ->
            x

        (EnumVariable ( x, _ )) as variable ->
            x

        (FloatVariable ( x, _ )) as variable ->
            x

        (IntVariable ( x, _ )) as variable ->
            x

        (StringVariable ( x, _ )) as variable ->
            x


variableLabel : Dict String Variable -> VariableName -> String
variableLabel variables variableName =
    variables
        |> Dict.get variableName
        |> Maybe.andThen (variableCommonFields >> .label)
        |> Maybe.withDefault variableName
