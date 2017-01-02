port module Ports exposing (..)

import Json.Encode as Encode
import List.Extra as List
import Types exposing (..)


-- LOCAL STORAGE


type alias LocalStorageOptions =
    { key : String
    , value : Maybe Encode.Value
    }


port writeToLocalStorage : LocalStorageOptions -> Cmd msg



-- WATERFALL


type alias WaterfallDataItem =
    { isSubtotal : Bool
    , name : String
    , value : Float
    }


waterfallData : Int -> SimulateNode -> List WaterfallDataItem
waterfallData waterfallIndex (SimulateNode fields) =
    case
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
            if List.isEmpty fields.children then
                [ { isSubtotal = False
                  , name = fields.shortName
                  , value = value
                  }
                ]
            else
                (List.concatMap (waterfallData waterfallIndex) fields.children)
                    ++ [ { isSubtotal = True
                         , name = fields.shortName
                         , value = value
                         }
                       ]


port renderWaterfall : List WaterfallDataItem -> Cmd msg
