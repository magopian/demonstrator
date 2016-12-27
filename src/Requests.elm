module Requests exposing (..)

import Http
import Json.Decode exposing (..)
import Types exposing (..)


-- DECODERS


variableCommonFieldsDecoder : Decoder VariableCommonFields
variableCommonFieldsDecoder =
    map6 VariableCommonFields
        (field "entity" string)
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
                        map2 BoolVariableFields
                            (field "default" bool)
                            (maybe (field "label" string))
                            |> map (\fields -> BoolVariable ( variableCommonFields, fields ))

                    "Date" ->
                        map2 DateVariableFields
                            (field "default" string)
                            (maybe (field "label" string))
                            |> map (\fields -> DateVariable ( variableCommonFields, fields ))

                    "Enumeration" ->
                        map2 EnumVariableFields
                            (field "default" string)
                            (field "labels" (dict string))
                            |> map (\fields -> EnumVariable ( variableCommonFields, fields ))

                    "Float" ->
                        map2 FloatVariableFields
                            (field "default" float)
                            (maybe (field "label" string))
                            |> map (\fields -> FloatVariable ( variableCommonFields, fields ))

                    "Integer" ->
                        map2 IntVariableFields
                            (field "default" int)
                            (maybe (field "label" string))
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
        (field "variables" (list variableDecoder))



-- REQUESTS


variables : String -> Http.Request VariablesResponse
variables baseUrl =
    Http.get (baseUrl ++ "/1/variables") variablesResponseDecoder
