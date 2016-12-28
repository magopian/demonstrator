module Types exposing (..)

import Dict exposing (Dict)


-- HELPERS


find : (a -> Bool) -> List a -> Maybe a
find f xs =
    xs
        |> List.filter f
        |> List.head



-- INDIVIDUALS


type alias Individual =
    { inputValues : Dict String InputValue
    , roles : Dict String String
    }


type InputValue
    = BoolInputValue Bool
    | DateInputValue String
    | EnumInputValue String
    | FloatInputValue Float
    | IntInputValue Int



-- ENTITIES


type alias Role =
    { key : String
    , label : String
    , plural : String
    , subroles : List String
    }


type alias Entity =
    { isPersonsEntity : Bool
    , label : String
    , roles : List Role
    }


findRole : List Role -> String -> Maybe Role
findRole roles roleKey =
    find
        (\{ key, plural } ->
            let
                pluralOrKey =
                    if String.isEmpty plural then
                        key
                    else
                        plural
            in
                pluralOrKey == roleKey
        )
        roles


pluralOrKey : Role -> String
pluralOrKey role =
    if String.isEmpty role.plural then
        role.key
    else
        role.plural



-- VARIABLES


type alias VariableCommonFields =
    { entity : String
    , label : Maybe String
    , name : String
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
