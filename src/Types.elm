module Types exposing (..)

import Dict exposing (Dict)


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
