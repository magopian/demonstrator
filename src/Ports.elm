port module Ports exposing (..)

import ListHelpers as List
import Types exposing (..)


type alias WaterfallDataItem =
    { isSubtotal : Bool
    , name : String
    , value : Float
    }


type alias WaterfallOptions =
    { data : List WaterfallDataItem
    , yMin : Float
    , yMax : Float
    , xLabel : String
    , yLabel : String
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


port renderWaterfall : WaterfallOptions -> Cmd msg
