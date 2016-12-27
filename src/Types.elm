module Types exposing (..)

import Dict exposing (Dict)


type alias VariableCommonFields =
    { entity : String
    , name : String
    , sourceCode : String
    , sourceFilePath : String
    , startLineNumber : Int
    , type_ : String
    }


type alias BoolVariableFields =
    { default : Bool
    , label : Maybe String
    }


type alias DateVariableFields =
    { default : String
    , label : Maybe String
    }


type alias EnumVariableFields =
    { default : String
    , labels : Dict String String
    }


type alias FloatVariableFields =
    { default : Float
    , label : Maybe String
    }


type alias IntVariableFields =
    { default : Int
    , label : Maybe String
    }


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
    , variables : List Variable
    }
