port module Ports exposing (..)

import Json.Encode as Encode
import ListHelpers as List
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


waterfallData : SimulateNode -> List WaterfallDataItem
waterfallData (SimulateNode fields) =
    case List.firstNonZeroValue fields.values of
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
                (List.concatMap waterfallData fields.children)
                    ++ [ { isSubtotal = True
                         , name = fields.shortName
                         , value = value
                         }
                       ]


port renderWaterfall : List WaterfallDataItem -> Cmd msg
