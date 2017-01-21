port module Ports exposing (..)

import Json.Encode as Encode
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


waterfallData : List Axis -> SimulateNode -> List WaterfallDataItem
waterfallData axes (SimulateNode fields) =
    case getValue fields.values axes of
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
                (List.concatMap (waterfallData axes) fields.children)
                    ++ [ { isSubtotal = True
                         , name = fields.shortName
                         , value = value
                         }
                       ]


port renderWaterfall : List WaterfallDataItem -> Cmd msg
