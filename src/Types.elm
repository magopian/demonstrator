module Types exposing (..)

import Dict exposing (Dict)
import List.Extra exposing (..)


-- HELPERS


find : (a -> Bool) -> List a -> Maybe a
find f xs =
    xs
        |> List.filter f
        |> List.head



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


findRole : List Role -> String -> Maybe Role
findRole roles roleKey =
    find (\role -> pluralOrKey role == roleKey) roles


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
            |> foldl1 mergeTestCases



-- CALCULATE


type alias Period =
    String


type alias CalculateValue =
    Dict VariableName (Dict Period (List Float))



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


type Variable
    = BoolVariable ( VariableCommonFields, BoolVariableFields )
    | DateVariable ( VariableCommonFields, DateVariableFields )
    | EnumVariable ( VariableCommonFields, EnumVariableFields )
    | FloatVariable ( VariableCommonFields, FloatVariableFields )
    | IntVariable ( VariableCommonFields, IntVariableFields )


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
