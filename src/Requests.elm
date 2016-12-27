module Requests exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (..)
import Types exposing (..)


-- DECODERS


variableCommonFieldsDecoder : Decoder VariableCommonFields
variableCommonFieldsDecoder =
    map7 VariableCommonFields
        (field "entity" string)
        (maybe (field "label" string))
        (field "name" string)
        (field "source_code" string)
        (field "source_file_path" string)
        (field "start_line_number" int)
        (field "@type" string)


variableDecoder : Decoder Variable
variableDecoder =
    variableCommonFieldsDecoder
        |> andThen
            (\variableCommonFields ->
                case variableCommonFields.type_ of
                    "Boolean" ->
                        map BoolVariableFields
                            (field "default" bool)
                            |> map (\fields -> BoolVariable ( variableCommonFields, fields ))

                    "Date" ->
                        map DateVariableFields
                            (field "default" string)
                            |> map (\fields -> DateVariable ( variableCommonFields, fields ))

                    "Enumeration" ->
                        map2 EnumVariableFields
                            (field "default" string)
                            (field "labels" (dict string))
                            |> map (\fields -> EnumVariable ( variableCommonFields, fields ))

                    "Float" ->
                        map FloatVariableFields
                            (field "default" float)
                            |> map (\fields -> FloatVariable ( variableCommonFields, fields ))

                    "Integer" ->
                        map IntVariableFields
                            (field "default" int)
                            |> map (\fields -> IntVariable ( variableCommonFields, fields ))

                    _ ->
                        fail ("Unsupported type: " ++ variableCommonFields.type_)
            )


variablesResponseDecoder : Decoder VariablesResponse
variablesResponseDecoder =
    map4 VariablesResponse
        (field "country_package_name" string)
        (field "country_package_version" string)
        (field "currency" string)
        (field "variables"
            (list variableDecoder
                |> map
                    (List.map
                        (\variable ->
                            ( variableCommonFields variable |> .name
                            , variable
                            )
                        )
                        >> Dict.fromList
                    )
            )
        )



-- REQUESTS


variables : String -> Http.Request VariablesResponse
variables baseUrl =
    Http.get (baseUrl ++ "/1/variables") variablesResponseDecoder
